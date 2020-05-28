library(tidyverse)
library(dplyr)
library(sf)
library(tidyr)


#EDUCATION
edu <- read.csv("data/original/acs5yr/acsB15003.csv")

edu <- edu[,-c(1, 6)]

edu <- reshape(edu, idvar = c("GEOID", "NAME"), timevar = "variable", direction = "wide")

edu <- edu %>%
  rename(
    B15003_001 = estimate.B15003_001,
    B15003_002 = estimate.B15003_002,
    B15003_003 = estimate.B15003_003,
    B15003_004 = estimate.B15003_004,
    B15003_005 = estimate.B15003_005,
    B15003_006 = estimate.B15003_006,
    B15003_007 = estimate.B15003_007,
    B15003_008 = estimate.B15003_008,
    B15003_009 = estimate.B15003_009,
    B15003_010 = estimate.B15003_010,
    B15003_011 = estimate.B15003_011,
    B15003_012 = estimate.B15003_012,
    B15003_013 = estimate.B15003_013,
    B15003_014 = estimate.B15003_014,
    B15003_015 = estimate.B15003_015,
    B15003_016 = estimate.B15003_016,
    B15003_017 = estimate.B15003_017,
    B15003_018 = estimate.B15003_018,
    B15003_019 = estimate.B15003_019,
    B15003_020 = estimate.B15003_020,
    B15003_021 = estimate.B15003_021,
    B15003_022 = estimate.B15003_022,
    B15003_023 = estimate.B15003_023,
    B15003_024 = estimate.B15003_024,
    B15003_025 = estimate.B15003_025
  )


percent <- function(x){
  (x/edu$B15003_001) *100
}


edu$B15003_002_per = percent(edu$B15003_002)
edu$B15003_003_per = percent(edu$B15003_003)
edu$B15003_004_per = percent(edu$B15003_004)
edu$B15003_005_per = percent(edu$B15003_005)
edu$B15003_006_per = percent(edu$B15003_006)
edu$B15003_007_per = percent(edu$B15003_007)
edu$B15003_008_per = percent(edu$B15003_008)
edu$B15003_009_per = percent(edu$B15003_009)
edu$B15003_010_per = percent(edu$B15003_010)
edu$B15003_011_per = percent(edu$B15003_011)
edu$B15003_012_per = percent(edu$B15003_012)
edu$B15003_013_per = percent(edu$B15003_013)
edu$B15003_014_per = percent(edu$B15003_014)
edu$B15003_015_per = percent(edu$B15003_015)
edu$B15003_016_per = percent(edu$B15003_016)

edu$B15003_017_per = percent(edu$B15003_017)
edu$B15003_018_per = percent(edu$B15003_018)
edu$B15003_019_per = percent(edu$B15003_019)
edu$B15003_020_per = percent(edu$B15003_020)
edu$B15003_021_per = percent(edu$B15003_021)
edu$B15003_022_per = percent(edu$B15003_022)
edu$B15003_023_per = percent(edu$B15003_023)
edu$B15003_024_per = percent(edu$B15003_024)
edu$B15003_025_per = percent(edu$B15003_025)

write.csv(edu, "data/working/acs_transform/acs_edu.csv")

# HISPANIC OR LATINO ETHNICITY

ethn <- read.csv("data/original/acs5yr/acsB03003.csv")

ethn <- ethn[,-c(1, 6)]

ethn <- reshape(ethn, idvar = c("GEOID", "NAME"), timevar = "variable", direction = "wide")

ethn <- ethn %>%
  rename(
    B03003_001 = estimate.B03003_001,
    B03003_002 = estimate.B03003_002,
    B03003_003 = estimate.B03003_003,

  )

percent <- function(x){
  (x/ethn$B03003_001) *100
}

ethn$B03003_002_per = percent(ethn$B03003_002)
ethn$B03003_003_per = percent(ethn$B03003_003)

write.csv(ethn, "data/working/acs_transform/acs_ethn.csv")


#EMPLOYMENT

empl <- read.csv("data/original/acs5yr/acsB23025.csv")

empl <- empl[,-c(1, 6)]

empl <- reshape(empl, idvar = c("GEOID", "NAME"), timevar = "variable", direction = "wide")

empl <- empl %>%
  rename(
    B23025_001 = estimate.B23025_001,
    B23025_002 = estimate.B23025_002,
    B23025_003 = estimate.B23025_003,
    B23025_004 = estimate.B23025_004,
    B23025_005 = estimate.B23025_005,
    B23025_006 = estimate.B23025_006,
    B23025_007 = estimate.B23025_007
  )


percent <- function(x){
  (x/empl$B23025_001) *100
}


empl$B23025_002_per = percent(empl$B23025_002)
empl$B23025_003_per = percent(empl$B23025_003)
empl$B23025_004_per = percent(empl$B23025_004)
empl$B23025_005_per = percent(empl$B23025_005)
empl$B23025_006_per = percent(empl$B23025_006)
empl$B23025_007_per = percent(empl$B23025_007)


write.csv(empl, "data/working/acs_transform/acs_empl.csv")

#RACE

race <- read.csv("data/original/acs5yr/acsB02001.csv")

race <- race[,-c(1, 6)]

race <- reshape(race, idvar = c("GEOID", "NAME"), timevar = "variable", direction = "wide")

race <- race %>%
  rename(
    B02001_001 = estimate.B02001_001,
    B02001_002 = estimate.B02001_002,
    B02001_003 = estimate.B02001_003,
    B02001_004 = estimate.B02001_004,
    B02001_005 = estimate.B02001_005,
    B02001_006 = estimate.B02001_006,
    B02001_007 = estimate.B02001_007,
    B02001_008 = estimate.B02001_008,
    B02001_009 = estimate.B02001_009,
    B02001_010 = estimate.B02001_010
  )


percent <- function(x){
  (x/race$B02001_001) *100
}


race$B02001_002_per = percent(race$B02001_002)
race$B02001_003_per = percent(race$B02001_003)
race$B02001_004_per = percent(race$B02001_004)
race$B02001_005_per = percent(race$B02001_005)
race$B02001_006_per = percent(race$B02001_006)
race$B02001_007_per = percent(race$B02001_007)
race$B02001_008_per = percent(race$B02001_008)
race$B02001_009_per = percent(race$B02001_009)
race$B02001_010_per = percent(race$B02001_010)

write.csv(race, "data/working/acs_transform/acs_race.csv")

#INCOME TO POVERTY LEVEL RATIO

pov <- read.csv("data/original/acs5yr/acsB17026.csv")

pov <- pov[,-c(1, 6)]

pov <- reshape(pov, idvar = c("GEOID", "NAME"), timevar = "variable", direction = "wide")

pov <- pov %>%
  rename(
    B17026_001 = estimate.B17026_001,
    B17026_002 = estimate.B17026_002,
    B17026_003 = estimate.B17026_003,
    B17026_004 = estimate.B17026_004,
    B17026_005 = estimate.B17026_005,
    B17026_006 = estimate.B17026_006,
    B17026_007 = estimate.B17026_007,
    B17026_008 = estimate.B17026_008,
    B17026_009 = estimate.B17026_009,
    B17026_010 = estimate.B17026_010,
    B17026_011 = estimate.B17026_011,
    B17026_012 = estimate.B17026_012,
    B17026_013 = estimate.B17026_013
  )


percent <- function(x){
  (x/pov$B17026_001) *100
}


pov$B17026_002_per = percent(pov$B17026_002)
pov$B17026_003_per = percent(pov$B17026_003)
pov$B17026_004_per = percent(pov$B17026_004)
pov$B17026_005_per = percent(pov$B17026_005)
pov$B17026_006_per = percent(pov$B17026_006)
pov$B17026_007_per = percent(pov$B17026_007)
pov$B17026_008_per = percent(pov$B17026_008)
pov$B17026_009_per = percent(pov$B17026_009)
pov$B17026_010_per = percent(pov$B17026_010)
pov$B17026_011_per = percent(pov$B17026_011)
pov$B17026_012_per = percent(pov$B17026_012)
pov$B17026_013_per = percent(pov$B17026_013)

write.csv(pov, "data/working/acs_transform/acs_pov.csv")
x <- read.csv("data/working/acs_transform/acs_pov.csv")
