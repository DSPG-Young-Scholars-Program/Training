library(dplyr)
sp<- read.csv("data/working/summary_acs.csv")

sp <- sp[-c(1,3,7,21:32, 39:44)]

sp <- aggregate(list(sp[5:23]), by = list(FIPS = sp$FIPS, state = sp$state, county = sp$county, census_tract = sp$census_tract), FUN=sum)

sp$census_tract <- ifelse(nchar(sp$census_tract)==3,paste0("000",sp$census_tract),sp$census_tract)
sp$census_tract <- ifelse(nchar(sp$census_tract)==4,paste0("00",sp$census_tract),sp$census_tract)
sp$census_tract <- ifelse(nchar(sp$census_tract)==5,paste0("0",sp$census_tract),sp$census_tract)

sp$county <- ifelse(nchar(sp$county)==2,paste0("0",sp$county),sp$county)
sp$county <- ifelse(nchar(sp$county)==1,paste0("00",sp$county),sp$county)

sp$state <- ifelse(nchar(sp$state)==1,paste0("0",sp$state),sp$state)

sp$FIPS <- ifelse(nchar(sp$FIPS)==4,paste0("0",sp$FIPS),sp$FIPS)

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

percent2 <- function(x){
  (x/sp$B28003_001) *100
}


sp$B28003_002_per = percent2(sp$B28003_002)
sp$B28003_003_per = percent2(sp$B28003_003)
sp$B28003_004_per = percent2(sp$B28003_004)
sp$B28003_005_per = percent2(sp$B28003_005)
sp$B28003_006_per = percent2(sp$B28003_006)


sp<- sp[c(1:17,24:35, 18:23, 36:40)]

#write.csv(sp, "data/working/summary_acs_census_tract.csv")
