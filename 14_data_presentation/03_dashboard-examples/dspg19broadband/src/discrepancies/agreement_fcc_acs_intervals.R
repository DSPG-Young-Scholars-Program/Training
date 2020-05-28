library(IRanges)


# Note: continued from agreement_fcc_acs

#
# Prepare data -------------------------------------------------------------------------------------
#

# Need two-column dataframes with nothing else
test_acs <- data %>% select(bbandmin, bbandmax) %>% 
  st_set_geometry(NULL)

test_fcc10 <- data %>% select(conn10min, conn10max) %>% 
  st_set_geometry(NULL)

test_fcc200 <- data %>% select(conn200min, conn200max) %>% 
  st_set_geometry(NULL)

test_fcc <- data %>% select(connmin, connmax) %>% 
  st_set_geometry(NULL)

# Can only work with whole numbers (multiply to preserve all decimals)
startacs <- test_acs$bbandmin*10000000
endacs <- test_acs$bbandmax*10000000
test_acs <- IRanges(start = startacs, end = endacs)

startfcc10 <- test_fcc10$conn10min*10000000
endfcc10 <- test_fcc10$conn10max*10000000
test_fcc10 <- IRanges(start = startfcc10, end = endfcc10)

startfcc200 <- test_fcc200$conn200min*10000000
endfcc200 <- test_fcc200$conn200max*10000000
test_fcc200 <- IRanges(start = startfcc200, end = endfcc200)

startfcc <- test_fcc$connmin*10000000
endfcc <- test_fcc$connmax*10000000
test_fcc <- IRanges(start = startfcc, end = endfcc)

# Test
countOverlaps(test_acs[1], test_fcc10[1])
countOverlaps(test_acs[2], test_fcc10[2])
countOverlaps(test_acs[3], test_fcc10[3])


#
# Get intervals -------------------------------------------------------------------------------------
#

# Any overlap at all
# overlap_any <- as.data.frame(countOverlaps(test_acs[1], test_fcc10[1]))
# for(i in 2:length(test_acs@start)){
#   tmp <- countOverlaps(test_acs[i], test_fcc10[i])
#   overlap_any <- rbind(overlap_any, tmp)
# }
# names(overlap_any)[1] <- "overlap_any"
# overlap_any$overlap_any <- as.factor(overlap_any$overlap_any)
# 
# # FCC is completely within ACS
# overlap_fccwithinacs <- as.data.frame(countOverlaps(test_fcc10[1], test_acs[1]), type = "within")
# for(i in 2:length(test_acs@start)){
#   tmp <- countOverlaps(test_fcc10[i], test_acs[i], type = "within")
#   overlap_fccwithinacs <- rbind(overlap_fccwithinacs, tmp)
# }
# names(overlap_fccwithinacs)[1] <- "fcc_within_acs"
# overlap_fccwithinacs$fcc_within_acs <- as.factor(overlap_fccwithinacs$fcc_within_acs)

# ACS is completely within FCC 10
overlap_acswithinfcc10 <- as.data.frame(countOverlaps(test_acs[1], test_fcc10[1]), type = "within")
for(i in 2:length(test_acs@start)){
  tmp <- countOverlaps(test_acs[i], test_fcc10[i], type = "within")
  overlap_acswithinfcc10 <- rbind(overlap_acswithinfcc10, tmp)
}
names(overlap_acswithinfcc10)[1] <- "acs_within_fcc10"
overlap_acswithinfcc10$acs_within_fcc10 <- as.factor(overlap_acswithinfcc10$acs_within_fcc10)

# ACS is completely within FCC200
overlap_acswithinfcc200 <- as.data.frame(countOverlaps(test_acs[1], test_fcc200[1]), type = "within")
for(i in 2:length(test_acs@start)){
  tmp <- countOverlaps(test_acs[i], test_fcc200[i], type = "within")
  overlap_acswithinfcc200 <- rbind(overlap_acswithinfcc200, tmp)
}
names(overlap_acswithinfcc200)[1] <- "acs_within_fcc200"
overlap_acswithinfcc200$acs_within_fcc200 <- as.factor(overlap_acswithinfcc200$acs_within_fcc200)

# ACS is completely within FCC10min-FCC200max
overlap_acswithinfcc <- as.data.frame(countOverlaps(test_acs[1], test_fcc[1]), type = "within")
for(i in 2:length(test_acs@start)){
  tmp <- countOverlaps(test_acs[i], test_fcc[i], type = "within")
  overlap_acswithinfcc <- rbind(overlap_acswithinfcc, tmp)
}
names(overlap_acswithinfcc)[1] <- "acs_within_fcc"
overlap_acswithinfcc$acs_within_fcc <- as.factor(overlap_acswithinfcc$acs_within_fcc)


#
# Put back -------------------------------------------------------------------------------------
#

# Create df with intervals
overlap_geo <- as.data.frame(data$GEOID)
names(overlap_geo)[1] <- "GEOID"
overlap_geo$GEOID <- as.character(data$GEOID)

overlap_df <- cbind(overlap_geo, overlap_acswithinfcc200, overlap_acswithinfcc10, overlap_acswithinfcc)

# Create categorical variable
overlap_df <- overlap_df %>% mutate(overlaptype = case_when(acs_within_fcc200 == 1 & acs_within_fcc10 == 0 ~ "Yes FCC 200kbps & No FCC 10mbps",
                                                            acs_within_fcc200 == 1 & acs_within_fcc10 == 1 ~ "Yes FCC 200kbps & Yes FCC 10mbps",
                                                            acs_within_fcc200 == 0 & acs_within_fcc10 == 1 ~ "No FCC 200kbps & Yes FCC 10mbps",
                                                            acs_within_fcc200 == 0 & acs_within_fcc10 == 0 ~ "No FCC 200kbps & No FCC 10mbps"))
overlap_df$overlaptype <- factor(overlap_df$overlaptype, levels = c("Yes FCC 200kbps & No FCC 10mbps", "No FCC 200kbps & Yes FCC 10mbps", 
                                                                    "Yes FCC 200kbps & Yes FCC 10mbps", "No FCC 200kbps & No FCC 10mbps"))

# Left join with data (that has geography)
data_int <- left_join(data, overlap_df, by = "GEOID")

# Add conditional urbanicity indicators for plots
data_int <- data_int %>% mutate(urban_fcc200 = case_when(acs_within_fcc200 == 0 ~ NA_character_,
                                                         acs_within_fcc200 == 1 & urbanicity == "Rural" ~ "Rural", 
                                                         acs_within_fcc200 == 1 & urbanicity == "Small town" ~ "Small town", 
                                                         acs_within_fcc200 == 1 & urbanicity == "Micropolitan" ~ "Micropolitan", 
                                                         acs_within_fcc200 == 1 & urbanicity == "Metropolitan" ~ "Metropolitan"),
                                urban_fcc10 = case_when(acs_within_fcc10 == 0 ~ NA_character_,
                                                        acs_within_fcc10 == 1 & urbanicity == "Rural" ~ "Rural", 
                                                        acs_within_fcc10 == 1 & urbanicity == "Small town" ~ "Small town", 
                                                        acs_within_fcc10 == 1 & urbanicity == "Micropolitan" ~ "Micropolitan", 
                                                        acs_within_fcc10 == 1 & urbanicity == "Metropolitan" ~ "Metropolitan"),
                                urban_any = case_when(acs_within_fcc == 0 ~ NA_character_,
                                                      acs_within_fcc == 1 & urbanicity == "Rural" ~ "Rural", 
                                                      acs_within_fcc == 1 & urbanicity == "Small town" ~ "Small town", 
                                                      acs_within_fcc == 1 & urbanicity == "Micropolitan" ~ "Micropolitan", 
                                                      acs_within_fcc == 1 & urbanicity == "Metropolitan" ~ "Metropolitan"))
data_int$urban_fcc200 <- factor(data_int$urban_fcc200, levels = c("Rural", "Small town", "Micropolitan", "Metropolitan"))
data_int$urban_fcc10 <- factor(data_int$urban_fcc10, levels = c("Rural", "Small town", "Micropolitan", "Metropolitan"))
data_int$urban_any <- factor(data_int$urban_any, levels = c("Rural", "Small town", "Micropolitan", "Metropolitan"))

# write_rds(data_int, "./data/working/intervals/data_int.Rds")

#
# Select data -------------------------------------------------------------------------------------
#

int_contig <- data_int %>% filter(STATEFP != "02" & STATEFP != "15" & STATEFP != "60" & STATEFP != "66" & STATEFP != "69" & STATEFP != "72" & STATEFP != "78")
int_alaska <- data_int %>% filter(STATEFP == "02")
int_hawaii <- data_int %>% filter(STATEFP == "15")

# Save for Shiny
# int_alaska_save <- int_alaska %>% select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAME.x, NAME.y, 
#                                          bband, bbandmin, bbandmax, conn10min, conn10max, conn200min, 
#                                          conn200max, connmin, connmax, primRUCA, TractPop10, LandSqmile10, 
#                                          PopDens10, urbanicity, acs_within_fcc200, acs_within_fcc10, 
#                                          acs_within_fcc, geometry, urban_fcc200, urban_fcc10, urban_any)
# int_hawaii_save <- int_hawaii %>% select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAME.x, NAME.y, 
#                                          bband, bbandmin, bbandmax, conn10min, conn10max, conn200min, 
#                                          conn200max, connmin, connmax, primRUCA, TractPop10, LandSqmile10, 
#                                          PopDens10, urbanicity, acs_within_fcc200, acs_within_fcc10, 
#                                          acs_within_fcc, geometry, urban_fcc200, urban_fcc10, urban_any)
# int_contig_save <- int_contig %>% select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAME.x, NAME.y, 
#                                          bband, bbandmin, bbandmax, conn10min, conn10max, conn200min, 
#                                          conn200max, connmin, connmax, primRUCA, TractPop10, LandSqmile10, 
#                                          PopDens10, urbanicity, acs_within_fcc200, acs_within_fcc10, 
#                                          acs_within_fcc, geometry, urban_fcc200, urban_fcc10, urban_any)
# 
# saveRDS(int_alaska_save, file = "./src/discrepancies/agreementapp/data/alaska.Rds")
# saveRDS(int_hawaii_save, file = "./src/discrepancies/agreementapp/data/hawaii.Rds")
# saveRDS(int_contig_save, file = "./src/discrepancies/agreementapp/data/contig.Rds")


#
# Plot: ANY OVERLAP [min10, max200] -------------------------------------------------------------------------------------
#

# Plot contiguous states
plot_main <- ggplot() +
  geom_sf(data = int_contig, aes(fill = urban_any), size = 0.001) +
  theme_map() +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000)) +
  labs(title = "ACS and FCC Broadband Subscription Estimate Congruence by Tract [min10mbps, max200kbps]", 
       subtitle = "Tracts with incongruent estimate ranges shown in grey.",
       caption = "Note: FCC = Federal Communications Commission, December 2015. ACS = American Community Survey, 2013-17.\nAlaska and Hawaii not to scale.") +
  scale_fill_manual(name = "Urbanicity", values = c("#fed98e", "#fe9929", "#d95f0e", "#993404"), na.value = "#f0f0f0") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "top")

# Plot Hawaii
plot_hawaii <- ggplot() +
  geom_sf(data = int_hawaii, aes(fill = urban_any), size = 0.001)  +
  theme_map() +
  coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE) +
  scale_fill_manual(name = "Urbanicity", values = c("#fed98e", "#fe9929", "#d95f0e", "#993404"), na.value = "#f0f0f0") +
  theme(legend.position = "none")

# Plot Alaska
plot_alaska <- ggplot() +
  geom_sf(data = int_alaska, aes(fill = urban_any), size = 0.001) +
  theme_map() +
  coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE) +
  scale_fill_manual(name = "Urbanicity", values = c("#fed98e", "#fe9929", "#d95f0e", "#993404"), na.value = "#f0f0f0") +
  theme(legend.position = "none")

# Plot all
plot_main +
  annotation_custom(grob = ggplotGrob(plot_alaska),
                    xmin = -3350000,
                    xmax = -3350000 + (1600000 - (-2400000))/1.8,
                    ymin = -2450000,
                    ymax = -2450000 + (2500000 - 200000)/1.8) +
  annotation_custom(grob = ggplotGrob(plot_hawaii),
                    xmin = -1700000,
                    xmax = -1700000 + (-154 - (-161))*230000,
                    ymin = -2450000,
                    ymax = -2450000 + (23 - 18)*230000)


#
# Plot: WITHIN FCC 200 -------------------------------------------------------------------------------------
#

# Plot contiguous states
plot_main <- ggplot() +
  geom_sf(data = int_contig, aes(fill = urban_fcc200), size = 0.001) +
  theme_map() +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000)) +
  labs(title = "ACS and FCC 200kbps Broadband Subscription Estimate Congruence by Tract", 
       subtitle = "Tracts with ACS estimates incongruent with the FCC 200kbps range shown in grey.",
       caption = "Note: FCC = Federal Communications Commission, December 2015. ACS = American Community Survey, 2013-17.\nAlaska and Hawaii not to scale.") +
  scale_fill_manual(name = "Urbanicity", values = c("#fed98e", "#fe9929", "#d95f0e", "#993404"), na.value = "#f0f0f0") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "top")

# Plot Hawaii
plot_hawaii <- ggplot() +
  geom_sf(data = int_hawaii, aes(fill = urban_fcc200), size = 0.001)  +
  theme_map() +
  coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE) +
  scale_fill_manual(name = "Urbanicity", values = c("#fed98e", "#fe9929", "#d95f0e", "#993404"), na.value = "#f0f0f0") +
  theme(legend.position = "none")

# Plot Alaska
plot_alaska <- ggplot() +
  geom_sf(data = int_alaska, aes(fill = urban_fcc200), size = 0.001) +
  theme_map() +
  coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE) +
  scale_fill_manual(name = "Urbanicity", values = c("#fed98e", "#fe9929", "#d95f0e", "#993404"), na.value = "#f0f0f0") +
  theme(legend.position = "none")

# Plot all
plot_main +
  annotation_custom(grob = ggplotGrob(plot_alaska),
                    xmin = -3350000,
                    xmax = -3350000 + (1600000 - (-2400000))/1.8,
                    ymin = -2450000,
                    ymax = -2450000 + (2500000 - 200000)/1.8) +
  annotation_custom(grob = ggplotGrob(plot_hawaii),
                    xmin = -1700000,
                    xmax = -1700000 + (-154 - (-161))*230000,
                    ymin = -2450000,
                    ymax = -2450000 + (23 - 18)*230000)


#
# Plot: ACS WITHIN FCC 10 -------------------------------------------------------------------------------------
#

# Plot contiguous states
plot_main <- ggplot() +
  geom_sf(data = int_contig, aes(fill = urban_fcc10), size = 0.001) +
  theme_map() +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000)) +
  labs(title = "ACS and FCC 10mbps Broadband Subscription Estimate Congruence by Tract", 
       subtitle = "Tracts with ACS estimates incongruent with the FCC 10mbps range shown in grey.",
       caption = "Note: FCC = Federal Communications Commission, December 2015. ACS = American Community Survey, 2013-17.\nAlaska and Hawaii not to scale.") +
  scale_fill_manual(name = "Urbanicity", values = c("#fed98e", "#fe9929", "#d95f0e", "#993404"), na.value = "#f0f0f0") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "top")

# Plot Hawaii
plot_hawaii <- ggplot() +
  geom_sf(data = int_hawaii, aes(fill = urban_fcc10), size = 0.001)  +
  theme_map() +
  coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE) +
  scale_fill_manual(name = "Urbanicity", values = c("#fed98e", "#fe9929", "#d95f0e", "#993404"), na.value = "#f0f0f0") +
  theme(legend.position = "none")

# Plot Alaska
plot_alaska <- ggplot() +
  geom_sf(data = int_alaska, aes(fill = urban_fcc10), size = 0.001) +
  theme_map() +
  coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE) +
  scale_fill_manual(name = "Urbanicity", values = c("#fed98e", "#fe9929", "#d95f0e", "#993404"), na.value = "#f0f0f0") +
  theme(legend.position = "none")

# Plot all
plot_main +
  annotation_custom(grob = ggplotGrob(plot_alaska),
                    xmin = -3350000,
                    xmax = -3350000 + (1600000 - (-2400000))/1.8,
                    ymin = -2450000,
                    ymax = -2450000 + (2500000 - 200000)/1.8) +
  annotation_custom(grob = ggplotGrob(plot_hawaii),
                    xmin = -1700000,
                    xmax = -1700000 + (-154 - (-161))*230000,
                    ymin = -2450000,
                    ymax = -2450000 + (23 - 18)*230000)


#
# Check urbanicity ------------------------------------------------------------------
#

# Urbanicity not coded for 84 tracts

# Urbanicity of min10 - max200 congruence
table(data_int$acs_within_fcc)
table(data_int$acs_within_fcc, data_int$urbanicity)
round(prop.table(table(data_int$acs_within_fcc, data_int$urbanicity)), 4)

round(prop.table(table(data_int$urbanicity, data_int$acs_within_fcc), margin = 2), 4)
round(prop.table(table(data_int$urbanicity, data_int$acs_within_fcc), margin = 1), 4)

# States with the highest proportion of congruent tracts
statesmax <- data_int %>% mutate(acs_within_fcc = as.numeric(acs_within_fcc),
                                 acs_within_fcc = acs_within_fcc - 1) %>%
  group_by(State) %>% 
  transmute(NAME.y = NAME.y,
            acs_within_fcc = acs_within_fcc,
            tractnumber = n(),
            tractcong = sum(acs_within_fcc),
            tractcongprop = tractcong/tractnumber) %>%
  st_set_geometry(NULL)
statesmax <- statesmax %>% select(State, tractcongprop) %>% 
  unique() %>%
  arrange(desc(tractcongprop))
head(statesmax, 10)
tail(statesmax, 10)

# Counties with the highest proportion of congruent tracts
countiesmax <- data_int %>% mutate(acs_within_fcc = as.numeric(acs_within_fcc),
                                   acs_within_fcc = acs_within_fcc - 1) %>%
  group_by(County) %>% 
  transmute(State = State,
            acs_within_fcc = acs_within_fcc,
            tractnumber = n(),
            tractcong = sum(acs_within_fcc),
            tractcongprop = tractcong/tractnumber) %>%
  st_set_geometry(NULL)
countiesmax <- countiesmax %>% select(State, County, tractcongprop) %>% 
  unique()

countiesmax <- countiesmax %>% group_by(State) %>%
  mutate(countiesnumber = n()) %>%
  ungroup() %>%
  arrange(desc(tractcongprop))

ggplot(countiesmax, aes(x = tractcongprop)) +
  geom_histogram(bins = 15) +
  labs(title = "Histogram of proportion congruent tracts within counties", x = "Proportion congruent tracts", y = "Number of counties") +
  scale_y_continuous(breaks = seq(0, 550, 50))

# Which counties are made up 100% by tracts with congruent estimates? Which states are they in?
countiesmax_1 <- countiesmax %>% 
  filter(tractcongprop == 1) %>%
  group_by(State) %>%
  mutate(nperstate = n())  %>%
  ungroup() %>%
  arrange(desc(nperstate))

countiesmax_1_table <- countiesmax_1 %>% select(State, nperstate) %>% unique()

# Looks like some of these are counties that are also only 1 census tract.

# Check in original data. Texas, Concho County
test <- data_int %>% filter(State == "TX" & County == "Concho County")
test <- data_int %>% filter(str_detect(NAME.y, "Concho"))

test <- data_int %>% filter(str_detect(NAME.y, "Brewster"))


#
# Check width of ACS intervals ------------------------------------------------------------------
#

# Distribution of ACS-MOE
hist(data_int[data_int$urbanicity == "Rural", ]$bbandmin, freq = TRUE, breaks = 10)
hist(data_int[data_int$urbanicity == "Small town", ]$bbandmin, freq = TRUE, breaks = 10)
hist(data_int[data_int$urbanicity == "Micropolitan", ]$bbandmin, freq = TRUE, breaks = 10)
hist(data_int[data_int$urbanicity == "Metropolitan", ]$bbandmin, freq = TRUE, breaks = 10)

# Distance btween ACS-MOE, ACS+MOE
data_int$bbanddist <- data_int$bbandmax - data_int$bbandmin

hist(data_int[data_int$urbanicity == "Rural", ]$bbanddist, freq = TRUE, breaks = 10)
hist(data_int[data_int$urbanicity == "Small town", ]$bbanddist, freq = TRUE, breaks = 10)
hist(data_int[data_int$urbanicity == "Micropolitan", ]$bbanddist, freq = TRUE, breaks = 10)
hist(data_int[data_int$urbanicity == "Metropolitan", ]$bbanddist, freq = TRUE, breaks = 10)

summary(data_int[data_int$urbanicity == "Rural", ]$bbanddist)
summary(data_int[data_int$urbanicity == "Small town", ]$bbanddist)
summary(data_int[data_int$urbanicity == "Micropolitan", ]$bbanddist)
summary(data_int[data_int$urbanicity == "Metropolitan", ]$bbanddist)


test <- data_int %>% select(NAME.y, bbandmin, bbandmax, bbanddist, urbanicity, connmin, connmax, acs_within_fcc)