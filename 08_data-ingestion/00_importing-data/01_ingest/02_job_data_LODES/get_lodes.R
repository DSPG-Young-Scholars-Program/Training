# Get LODES job count data
devtools::install_git("https://github.com/hrbrmstr/lodes.git")
library(lodes)
library(DBI)
library(ggplot2)

lodes_ia_2015 <- data.table::setDT(read_lodes("ia", "od", "main", "JT00", "2015", "__Personal__/2019MBDSS/data/Census/LODES/"), )
lodes_ia_2015[, w_geocode := as.character(w_geocode)]
lodes_ia_2015[, h_geocode := as.character(h_geocode)]
head(lodes_ia_2015, n=50)
lodes_ia_2015[S000 > 1][order(-S000)]







# Aggregate jobs per block
blk_jobs <- lodes_ia_2015[, .(blk_jobs = sum(S000)), w_geocode]

bgs <- unique(lodes_ia_2015[substr(w_geocode, 1, 5) == "19127", .(bg_geoid = substr(w_geocode, 1, 12))])


bg <- bgs[1, bg_geoid]
bgs_str <- paste0(bgs$bg_geoid, collapse = "','")
dist_mi <- 2
sql <-
  paste0(
    "SELECT a.geoid geoid_bg, b.geoid geoid_block, ST_Distance(ST_Transform(a.geometry::geometry, 900913), ST_Transform(b.geometry::geometry, 900913))/1609.344 dist_mi
     FROM tl_2018_19_bg_centerpoints a
     JOIN tl_2018_19_block_centerpoints b
     ON ST_Distance(ST_Transform(a.geometry::geometry, 900913), ST_Transform(b.geometry::geometry, 900913)) < (", dist_mi,"*1609.344)
     WHERE a.geoid IN ('", bgs_str, "')"
  )

con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
                 dbname = "user1",
                 host = "postgis",
                 port = "5432",
                 user = "user1",
                 password = "user1")
bg_dists <- dbGetQuery(con, sql)
dbDisconnect(con)

library(data.table)
bg_blk_jobs <- merge(bg_dists, blk_jobs, by.x = "geoid_block", by.y = "w_geocode")
setDT(bg_blk_jobs)
bg_blk_jobs[, dist_sqr := dist_mi^2]
bg_blk_jobs[dist_sqr == 0, dist_sqr := 0.01]


bg_idx <- bg_blk_jobs[, .(sum_e = sum(blk_jobs/dist_sqr)), geoid_bg]
# need to normalize
bg_idx$bgidx_lg <- log(bg_idx$sum_e)

bg_idx$bgidx_lg_scl <- scale(bg_idx$bgidx_lg,center=min(bg_idx$bgidx_lg),scale=diff(range(bg_idx$bgidx_lg)))

bg_idx$rank <-  cut(bg_idx$bgidx_lg,breaks=quantile(bg_idx$bgidx_lg,probs=seq(0,1,by=0.2)),labels=1:5,include.lowest=TRUE)

bg_geos_idx <- merge(bg_geos, bg_idx, by.x = "geoid", by.y = "geoid_bg")


plot(bg_geos_idx[, c("rank")])



# PLOT SUPER FANCY

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
EVI_palette<-cbPalette[c(6,3,1,5,7)]
EVI_palette<-cbPalette[c(7,5,1,3,6)]

p <- ggplot(data = bg_geos_idx) +
  theme_minimal() +
  theme(
    text=element_text(family="mono", color="#22211d"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.line=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    # panel.grid.minor=element_line(color="#ebebe5", size=0.2),
    panel.grid.major=element_line(color="#ebebe5", size=0.2),
    panel.grid.minor=element_blank(),
    plot.background=element_rect(fill="#f5f5f2", color = NA),
    panel.background=element_rect(fill="#f5f5f2", color = NA),
    legend.background=element_rect(fill="#f5f5f2", color = NA),
    panel.border=element_blank()
  ) +
  geom_sf(aes(fill = rank), color = "grey20") +
  # scale_fill_viridis_c()
  theme(legend.position = "bottom") +
  scale_fill_manual(
    values = EVI_palette,
    name = "Employment Access Index Quantiles",
    drop = FALSE,
    labels = c("Low Access", "", "", "", "High Access"),
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(30 / length(labels), units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Employment Access Index Quantiles",
    subtitle = "Marshalltown, Census LODES, 2015",
    caption = "Geometries: Census Block Groups, 2018")

p
