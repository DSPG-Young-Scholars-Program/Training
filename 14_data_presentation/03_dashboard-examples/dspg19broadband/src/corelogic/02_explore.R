library(RPostgreSQL)
library(DBI)
library(sf)
library(here)
library(data.table)
library(dplyr)
library(naniar)
library(ggplot2)
library(maps)
library(ggthemes)
library(viridis)

options(scipen = 99999999)


#
# Connect to database ---------------------------------------------------------------------
#

con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
                 dbname = "corelogic",
                 host = "localhost",
                 port = "5436",
                 user = Sys.getenv("db_userid"),
                 password = Sys.getenv("db_pwd"))


#
# Get column names --------------------------------------------------------------------------------
#

# Raw data for whole country 2014
deed14us <- dbGetQuery(con, "SELECT * FROM usda_deed_2014 LIMIT 1")
colnames(deed14us)

# Data for Virginia with tract info and geometry (does not include 43 tracts that had no properties with lat/lon)
deed14va <- st_read(con, query = "SELECT * FROM usda_deed_2014_51_tract LIMIT 1")
colnames(deed14va)

# Median sales per tract in Virginia (includes all 1900 tracts with those with no properties to calculate set to NA)
deed14vasale <- st_read(con, query = "SELECT * FROM usda_deed_2014_51_median_sales_tract LIMIT 1")
colnames(deed14vasale)


#
# Get some VA data --------------------------------------------------------------------------------
#

# Median sales per tract in Virginia (includes all 1900 tracts with those with no properties to calculate set to NA)
deed14vasale <- st_read(con, query = "SELECT * FROM usda_deed_2014_51_median_sales_tract")
colnames(deed14vasale)

# Data for Virginia with tract info and geometry (does not include 43 tracts that had no properties with lat/lon)
deed14va <- st_read(con, query = "SELECT \"tract_fips\", \"corporate.indicator\", \"absentee.owner.status\", \"situs.city\", \"situs.state\", \"multi.apn\", 
                   \"sale.code\", \"sale.amount\", \"transaction.type\", \"mortgage.amount\", \"mortgage.interest.rate\", \"mortgage.loan.type.code\",
                   \"mortgage.deed.type\", \"mortgage.term.code\", \"mortgage.term\", \"ownership.transfer.percentage\", \"land.use\", \"inter.family\", 
                   \"mortgage.interest.rate.type\", \"resale.new.construction\", \"foreclosure\", \"geometry\" FROM \"usda_deed_2014_51_tract\"")
colnames(deed14va)


#
# Missingness --------------------------------------------------------------------------------
#

gg_miss_var(deed14vasale)

gg_miss_var(deed14va)

table(deed14va$ownership.transfer.percentage)
table(deed14va$absentee.owner.status)
hist(deed14va$sale.amount)
table(deed14va$sale.code)
table(deed14va$transaction.type)
table(deed14va$mortgage.term.code)
table(deed14va$multi.apn)
table(deed14va$resale.new.construction)
table(deed14va$inter.family)
table(deed14va$land.use)
table(deed14va$mortgage.interest.rate.type)
table(deed14va$foreclosure)
hist(deed14va$mortgage.amount)
table(deed14va$corporate.indicator)


#
# Median sales data plot --------------------------------------------------------------------------------
#

# Distribution
summary(deed14vasale$median_sale_amount)
boxplot(deed14vasale$median_sale_amount)

# Get deciles
deed14vasale$medsale_decile <- cut(deed14vasale$median_sale_amount, quantile(deed14vasale$median_sale_amount, seq(0, 1, by = 0.10), na.rm = TRUE), include.lowest = TRUE,
                                   labels = c("[5,050 - 67,000]", 
                                              "(67,000 - 109,000]",
                                              "(109,000 - 139,000]",
                                              "(139,000 - 168,000]",
                                              "(168,000 - 201,000]",
                                              "(201,000 - 245,000]",
                                              "(245,000 - 309,000]",
                                              "(309,000 - 395,000]",
                                              "(395,000 - 529,000]",
                                              "(529,000 - 80,400,000]"))
  
# Plot deciles                            
ggplot(deed14vasale, aes(fill = medsale_decile)) +
  geom_sf() +
  labs(title = "Median property sale amount (USD) deciles in Virginia by tract", caption = "Source: CoreLogic 2014.") +
  scale_fill_viridis(option = "magma", direction = -1, discrete = TRUE) +
  ggthemes::theme_map()

# Get log sale
deed14vasale$medsale_log <- log(deed14vasale$median_sale_amount)

# Plot deciles                            
ggplot(deed14vasale, aes(fill = medsale_log)) +
  geom_sf(size = 0.1) +
  labs(title = "Logged median property sale amount in Virginia by tract", 
       caption = "Source: CoreLogic deed data, 2014.\nData includes all 1,907 tracts. Those with no sold property records set to NA.", 
       fill = "Logged median sale price (USD)") +
  scale_fill_viridis(option = "magma", direction = -1, discrete = FALSE) +
  ggthemes::theme_map() + 
  theme(legend.position = c(0.03, 0.65),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 16, face = "bold"),
        plot.caption = element_text(size = 10))



#
# Disconnect -----------------------------------------------------------------------------
#

dbDisconnect(con)
