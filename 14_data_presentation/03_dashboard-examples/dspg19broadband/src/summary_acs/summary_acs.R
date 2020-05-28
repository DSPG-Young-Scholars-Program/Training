library(tidyverse)
library(dplyr)
library(sf)
library(tidyr)

acs <- read.csv("data/original/acs5yr/acsB28002.csv")
sp <- acs

sp <- sp[,-2]
sp <-sp[,-4]

sp <- reshape(sp, idvar = "GEOID", timevar = "variable", direction = "wide")


sp <- sp %>%
  rename(
  B28002_001 = estimate.B28002_001,
  B28002_002 = estimate.B28002_002,
  B28002_003 = estimate.B28002_003,
  B28002_004 = estimate.B28002_004,
  B28002_005 = estimate.B28002_005,
  B28002_006 = estimate.B28002_006,
  B28002_007 = estimate.B28002_007,
  B28002_008 = estimate.B28002_008,
  B28002_009 = estimate.B28002_009,
  B28002_010 = estimate.B28002_010,
  B28002_011 = estimate.B28002_011,
  B28002_012 = estimate.B28002_012,
  B28002_013 = estimate.B28002_013
)


percent <- function(x){
  (x/sp$B28002_001) *100
}


sp$B28002_002_per = percent(sp$B28002_002)
sp$B28002_003_per = percent(sp$B28002_003)
sp$B28002_004_per = percent(sp$B28002_004)
sp$B28002_005_per = percent(sp$B28002_005)
sp$B28002_006_per = percent(sp$B28002_006)
sp$B28002_007_per = percent(sp$B28002_007)
sp$B28002_008_per = percent(sp$B28002_008)
sp$B28002_009_per = percent(sp$B28002_009)
sp$B28002_010_per = percent(sp$B28002_010)
sp$B28002_011_per = percent(sp$B28002_011)
sp$B28002_012_per = percent(sp$B28002_012)
sp$B28002_013_per = percent(sp$B28002_013)

sp$GEOID = as.character(as.numeric(sp$GEOID))

str(sp)

#separating geoid into block group, census tract, county, and state
sp <-separate(sp, GEOID, into = c("remove", "block_group"), sep= -1, remove = FALSE)
sp <-separate(sp, remove, into = c("remove2", "census_tract"), sep= -6, remove = FALSE)
sp <-separate(sp, remove2, into = c("remove3", "county"), sep= -3, remove = FALSE)
colnames(sp)[colnames(sp)=="remove3"] <- "state"
sp <- sp[,-(2:3)]

sp$state <- ifelse(nchar(sp$state)!=2,paste0("0",sp$state),sp$state)

#second ACS Table
acs2 <- read.csv("data/original/acs5yr/acsB28003.csv")
sp2 <- acs2

sp2 <- sp2[,-2]
sp2 <-sp2[,-4]

sp2 <- reshape(sp2, idvar = "GEOID", timevar = "variable", direction = "wide")

sp2 <- sp2 %>%
  rename(
    B28003_001 = estimate.B28003_001,
    B28003_002 = estimate.B28003_002,
    B28003_003 = estimate.B28003_003,
    B28003_004 = estimate.B28003_004,
    B28003_005 = estimate.B28003_005,
    B28003_006 = estimate.B28003_006,
  )

percent2 <- function(x){
  (x/sp2$B28003_001) *100
}


sp2$B28003_002_per = percent2(sp2$B28003_002)
sp2$B28003_003_per = percent2(sp2$B28003_003)
sp2$B28003_004_per = percent2(sp2$B28003_004)
sp2$B28003_005_per = percent2(sp2$B28003_005)
sp2$B28003_006_per = percent2(sp2$B28003_006)

sp2$GEOID = as.character(as.numeric(sp2$GEOID))

sp <- merge(sp, sp2, by = "GEOID", all=TRUE)


###RURAL/URBAN
library(readxl)
ru <- read_excel("src/summary_acs/ruralurbancodes2013 (2).xls")

#remove irrelevant columns
ruralurban <- ru[, -(2:4)]
ruralurban <- ruralurban[, -3]

#create FIPS from acs data
sp$FIPS = paste(sp$state, sp$county, sep= "")

sp <- merge(sp, ruralurban, by= "FIPS")

#I noticed that 13 observations were lost when merging
#unique(sp$FIPS[!(sp$FIPS %in% test$FIPS)])
# FIPS 02158 Kusilvak Census Area, 46102 Oglala Lakota County, SD

#filter(ru, ru$County_Name %like% "Kusilvak")
#filter(ru, ru$County_Name %like% "Oglala")

#filter(sp, sp$FIPS == "46102")
#9 observations
#filter(sp, sp$FIPS == "02158")
#4 observations


#write.csv(sp, "data/working/summary_acs.csv")
