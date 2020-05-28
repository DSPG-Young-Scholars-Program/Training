library(rvest)
library(dplyr)
library(stringr)
library(readr)
library(xml2)
library(purrr)
library(DescTools)



bbnow <- read.csv("./data/working/merged_bbnow.csv")

ruralurban <- read.csv("./data/working/ruralurban2013.csv")

df1 = data.frame(bbnow)

df2 = data.frame(ruralurban)

df3 = merge(df1,df2,by.x = c("county_fips","stateid"),by.y = c("FIPS","State"))

write_csv(df3, path = "./data/working/merged_by_rural_urban.csv", append = FALSE, col_names = TRUE)

