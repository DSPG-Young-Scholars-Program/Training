library(httr)
library(here)
library(maptools)
library(gpclib)
library(sp)
library(sf)
library(ggplot2)
library(ggmap)
library(osmdata)
library(tidyverse)
library(tigris)
library(acs)
library(data.table)
library(maditr)
library(viridis)
library(usmap)

## Read in FCC data and create GEOID from BlockCode
fcc <-  fread('unzip -p  ~/dspg19broadband/data/original/FCC/FCC-Dec-2015/US-Fixed-without-Satellite-Dec2015.zip', colClasses = c(BlockCode = "character"))
fcc <- fcc[,c('HocoNum', 'BlockCode', 'MaxAdDown', 'MaxCIRDown', 'Consumer', 'Business', 'StateAbbr')]
fcc[, GEOID := substr(BlockCode, 1, 12)] 
fcc[, county := substr(BlockCode, 1, 5)] 
names(fcc) <- tolower(names(fcc))

## Population - from Decennial Census (2010)
pop <- fread('~/dspg19broadband/data/working/population_by_census_block_2010.csv', colClasses = c(GEOID = "character"))
# correct for data being stupid about leading 0's
pop$GEOID <- ifelse(nchar(pop$GEOID) !=15 ,gsub(" ", "", paste("0",pop$GEOID), fixed = TRUE), pop$GEOID)
pop[, county := substr(GEOID, 1, 5)] 
names(pop) <- tolower(names(pop))

## Merge fcc and population data by block
fcc_pop <- merge(fcc, pop, by.x = c('blockcode', 'county'), by.y = c('geoid', 'county'),all.x = TRUE, all.y = FALSE)

## Create MaxAdDown threshold
fcc_threshold <- fcc_pop %>% dt_mutate(threshold_addown = maxaddown > 20) %>% dt_mutate(threshold_cirdown = maxcirdown > 20) 

# bring up to block level first to get block level populations
fcc_block <- fcc_threshold[, .(state = max(stateabbr), county = max(county), consumer = max(consumer), business = max(business), 
                                threshold_addown = max(threshold_addown),  threshold_cirdown = max(threshold_cirdown), 
                               maxaddown = max(maxaddown), maxcirdown = max(maxcirdown),
                               block_population = mean(value)), .(blockcode)]

fcc_county <- fcc_block[, .(state = max(state), consumer = max(consumer), business = max(business),
                            maxaddown = max(maxaddown), availability_adv = sum(threshold_addown*block_population)/sum(block_population),
                            availability_cont = sum(threshold_cirdown*block_population)/sum(block_population)), .(county)]

## Parse GEOID into State, County, Tract
fcc_county$state_fips <- usmap::fips(fcc_county$stateabbr)

## Bring In Subscription Data
connections <- fread('~/dspg19broadband/data/original/FCC/tract_map_dec_2015/tract_map_dec_2015.csv', colClasses = c(tractcode = "character"))
connections$county <- substr(connections$tractcode, 1,5)
connections = subset(connections, select = -c(tractcode) )


# convert numbers to %s: 0 - 0%, 1 - min=0% max=200/1000, 2 - min=200/1000, max=400/1000, 
# 3 - min=400/1000, max=600/1000, 4 - min=600/1000, max=800/1000, 5 - min=800/1000, max=NA
connections %>% dt_mutate(pcat_all_pct_min = case_when(pcat_all == 0 ~ 0,
                                                       pcat_all == 1 ~ 0,
                                                       pcat_all == 2 ~ 200/1000,
                                                       pcat_all == 3 ~ 400/1000,
                                                       pcat_all == 4 ~ 600/1000,
                                                       pcat_all == 5 ~ 800/1000)) %>%
  dt_mutate(pcat_all_pct_max = case_when(pcat_all == 0 ~ 0,
                                         pcat_all == 1 ~ 200/1000,
                                         pcat_all == 2 ~ 400/1000,
                                         pcat_all == 3 ~ 600/1000,
                                         pcat_all == 4 ~ 800/1000,
                                         pcat_all == 5 ~ 1)) %>%
  dt_mutate(pcat_all_10x1_min = case_when(pcat_10x1 == 0 ~ 0,
                                          pcat_10x1 == 1 ~ 0,
                                          pcat_10x1 == 2 ~ 200/1000,
                                          pcat_10x1 == 3 ~ 400/1000,
                                          pcat_10x1 == 4 ~ 600/1000,
                                          pcat_10x1 == 5 ~ 800/1000)) %>%
  dt_mutate(pcat_all_10x1_max = case_when(pcat_10x1 == 0 ~ 0,
                                          pcat_10x1 == 1 ~ 200/1000,
                                          pcat_10x1 == 2 ~ 400/1000,
                                          pcat_10x1 == 3 ~ 600/1000,
                                          pcat_10x1 == 4 ~ 800/1000,
                                          pcat_10x1 == 5 ~ 1))

connections <- connections %>% group_by(county) %>% 
  summarise(max_pcat_all_per = max(pcat_all_pct_max), min_pcat_all_per= min(pcat_all_pct_min), 
            max_pcat_10x1_per = max(pcat_all_10x1_max), min_pcat_10x1_per = min(pcat_all_10x1_min), 
            max_pcat_all = max(pcat_all), min_pcat_all = min(pcat_all), max_pcat_10x1=max(pcat_10x1), 
            min_pcat_10x1=min(pcat_10x1))

fcc_full <- merge(fcc_county, connections, by = 'county', all.x = TRUE)


#rural/urban
r_u <- fread('~/dspg19broadband/data/working/ruralurban2013.csv', colClasses = 'character')
fcc_final  <- merge(fcc_full, r_u, by.x = 'county', by.y = 'FIPS', all.x = TRUE, all.y = FALSE)
write.csv(fcc_final, '~/dspg19broadband/data/working/fcc_processed_county_updated.csv')







