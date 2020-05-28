library(sf)
library(dplyr)
library(ggplot2) # latest github version
library(magrittr)

cd <- st_read('congressional_districts.shp', stringsAsFactors = FALSE)
head(cd)

plot(cd)

cts <- read.csv('congressional_trump_scores.csv', stringsAsFactors = FALSE)[ , -1] %>%
  mutate(district = as.character(district))
head(cts)

dat <- left_join(cd, cts)

#plot(dat, col = dat[, c("district")])

# first define a set of layout/design parameters to re-use in each map
mapTheme <- function() {
  theme_void() + 
    theme(
      text = element_text(size = 7),
      plot.title = element_text(size = 11, color = "#1c5074", hjust = 0, vjust = 2, face = "bold"), 
      plot.subtitle = element_text(size = 8, color = "#3474A2", hjust = 0, vjust = 0),
      axis.ticks = element_blank(), 
      legend.direction = "vertical", 
      legend.position = "right",
      plot.margin = margin(1, 1, 1, 1, 'cm'),
      legend.key.height = unit(1, "cm"), legend.key.width = unit(0.2, "cm")
    ) 
}
ggplot(dat) + 
  # plot a map with ggplot
  geom_sf(aes(fill = trump_score), color = NA) +
  # specify the projection to use
  coord_sf(crs = st_crs(102003)) +
  scale_fill_gradient2('Trump Score \n', low='#0099ff', mid = '#ffffff', high = '#ff6666', midpoint = 0.5) +
  labs(
    title = 'Where have U.S. Representatives voted with and against President Trump?',
    subtitle = "Mapping FiveThirtyEight's 'Trump Score' of House of Representative's voting records",
    caption = "Source: Azavea, Data: FiveThirtyEight"
  ) +
  mapTheme()


upper_mw <- dat %>%
  # select a few states using dplyr::filter
  filter(state %in% c('MN', 'IA', 'WI')) %>%
  # re-project to an appropriate coordinate system
  st_transform(2289) 
upper_mw_coords <- upper_mw %>%
  # find polygon centroids (sf points object)
  st_centroid %>%
  # extract the coordinates of these points as a matrix
  st_coordinates
# insert centroid long and lat fields as attributes of polygons
upper_mw$long <- upper_mw_coords[,1]
upper_mw$lat <- upper_mw_coords[,2]
ggplot(upper_mw) + 
  # map districts by Trump Score
  geom_sf(aes(fill = trump_score), color = 'white') +
  # add labels according to locations of each polygon centroid
  geom_label(aes(long, lat, color = party, label = name), alpha = 0.75, size = 2) +
  scale_fill_gradient2('Trump Score \n', low='#0099ff', mid = '#ffffff', high = '#ff6666', midpoint = 0.5) +
  scale_color_manual('Political Party', values = c('Blue', 'Red')) +
  labs(
    title = "Congressional support for President Trump's policies in the Upper Midwest",
    subtitle = "Mapping FiveThirtyEight's 'Trump Score' of House of Representative's voting records",
    caption = "Source: Azavea, Data: FiveThirtyEight"
  ) +
  mapTheme()


by_state <- dat %>%
  group_by(state) %>%
  summarise(avg_trump_score = mean(na.omit(trump_score)), 
            districts = n_distinct(district))
head(by_state)

ggplot(by_state) + 
  geom_sf(aes(fill = avg_trump_score), color = 'white') +
  scale_fill_gradient2('Average \nTrump Score \nby State \n', low='#0099ff', mid = '#ffffff', high = '#ff6666', midpoint = 0.5) +
  coord_sf(crs = st_crs(102003)) +
  labs(
    title = "Which states' congressional delegations have supported President Trump's policies?",
    subtitle = "Mapping FiveThirtyEight's 'Trump Score' of House of Representative's voting records",
    caption = "Source: Azavea, Data: FiveThirtyEight"
  ) +
  mapTheme()



con <- sdalr::con_db("sdal")
pop <- st_read(con, c("apps$dashboard", "va_bg_pop_by_sex_15"))
pop <- pop[pop$item_geoparent=="Arlington County" & pop$item_by_value=="Female",]
pop_simple <- st_simplify(pop, preserveTopology = TRUE, dTolerance = 100)

colnames(pop)[colnames(pop)=="wkb_geometry"] <- "geometry"
st_geometry(pop) <- "geometry"
ggplot(pop) + 
  geom_sf(aes(fill = item_value), color = 'white') +
  scale_fill_gradient2('Population \n', low = "red", mid = "white", high = "blue", midpoint = 0) +
  coord_sf(crs = st_crs(102003)) +
  labs(
    title = "Female Population by Block Group",
    subtitle = "Arlington County, VA",
    caption = "Source: American Community Survey"
  ) +
  mapTheme()


con <- sdalr::con_db("sdal")
evic <- st_read(con, c("apps$dashboard", "va_bg_evictions_00_16"))
evic <- evic[evic$item_geoparent=="Arlington County" & evic$item_year=="2016",]

colnames(evic)[colnames(evic)=="wkb_geometry"] <- "geometry"
st_geometry(evic) <- "geometry"

ggplot(evic) + 
  geom_sf(aes(fill = item_value), color = 'white') +
  #scale_fill_gradient(low = "white", high = "black") +
  coord_sf(crs = st_crs(102003)) +
  labs(
    title = "Female Population by Block Group",
    subtitle = "Arlington County, VA",
    caption = "Source: American Community Survey"
  ) +
  mapTheme()


