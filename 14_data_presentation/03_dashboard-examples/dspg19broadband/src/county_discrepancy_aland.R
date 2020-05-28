library(dplyr)
library(sp)
library(sf)
library(usmap)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(ggpubr)

#FCC DATA
fcc_original<- read.csv('~/dspg19broadband/data/working/fcc_processed_county_25.csv')
fcc=fcc_original[, c("county", "availability_adv", "Population_2010", "RUCC_2013")]
fcc$county <- ifelse(nchar(fcc$county)==4,paste0("0",fcc$county),fcc$county)
colnames(fcc)[colnames(fcc)=="county"] <- "FIPS"
fcc$availability_adv = fcc$availability_adv*100

#ACS DATA
acs <- read.csv("data/working/summary_acs_county.csv")
acs=acs[, c("FIPS", "B28002_007_per")]
acs$FIPS <- ifelse(nchar(acs$FIPS)==4,paste0("0",acs$FIPS),acs$FIPS)

#MICROSOFT DATA
ms <- read.csv("data/original/microsoft/microsoft.csv")
ms$COUNTY.ID = as.character(as.integer(ms$COUNTY.ID))
ms$COUNTY.ID <- ifelse(nchar(ms$COUNTY.ID)==4,paste0("0",ms$COUNTY.ID),ms$COUNTY.ID)
colnames(ms)[colnames(ms)=="COUNTY.ID"] <- "FIPS"
ms$BROADBAND.USAGE = as.character(as.factor(ms$BROADBAND.USAGE))
ms$BROADBAND.USAGE = as.numeric(as.character(ms$BROADBAND.USAGE))
ms$BROADBAND.USAGE = ms$BROADBAND.USAGE*100
ms = ms[, c("FIPS", "BROADBAND.USAGE")]

#DETERMINING WHICH FIPS ARE DISCLUDED
#matched <- intersect(fcc$FIPS, acs$FIPS)
#all <-  union(fcc$FIPS, acs$FIPS)
#non.matched <- all[!all %in% matched]

#matched2 <- intersect(acs$FIPS, ms$FIPS)
#all2 <- union(acs$FIPS, ms$FIPS)
#non.matched2 <- all2[!all2 %in% matched2]

#02270 is an old FIPS code
#46113 is an old FIPS code
#51515 city is no longer a city, is now part of a county
#FCC includes American Samoa, Guam, Northern Mariana Islands, Puerto Rico, Virgin Islands

discr<- merge(fcc, acs, by= "FIPS", all.y=TRUE)
discr <- merge(discr, ms, by = "FIPS", all.y = TRUE)
discr$discr_metr = round((discr$availability_adv) - discr$B28002_007_per,1)
discr$abs_discr = abs(discr$discr_metr)
discr$ru_binary = ifelse(discr$RUCC_2013>3, "nonmetro", "metro")
discr$RUCC_2013 =as.factor(discr$RUCC_2013)

#RETRIEVING LAND AREA DATA
con <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                      dbname = "gis",
                      host = "postgis_1",
                      port = "5432",
                      user = "sm9dv",
                      password = "sm9dv")

geo = st_read(con, c("census_cb", "cb_2016_us_county_500k"))
geo$fips = paste(geo$STATEFP, geo$COUNTYFP, sep="")

DBI::dbDisconnect(con)

geo$FIPS <- paste(geo$STATEFP, geo$COUNTYFP, sep="")
geo <- geo %>% select("FIPS", "ALAND")

discr = merge(discr, geo, by="FIPS", all.y=FALSE)

#####Violin Plots#####

#ACS Violin Plot RUCC Code
discr %>% 
  filter(!is.na(B28002_007_per)) %>%
  ggplot(aes(x=RUCC_2013, y=B28002_007_per, fill = RUCC_2013)) + 
  geom_violin()+
  geom_boxplot(width=0.1)+
  scale_y_continuous(limits=c(0, 100)) +
  labs(title= "County Distribution of Percent of Consumers Reporting Broadband Access by RUCC Code (ACS)", x= "RUCC Code (1-3 Urban, 4-9 Rural)", y="Percent of Consumers Reporting Broadband Access")

discr %>% 
  filter(!is.na(B28002_007_per)) %>%
  ggplot(aes(x=ru_binary, y=B28002_007_per, fill = ru_binary)) + 
  geom_violin()+
  geom_boxplot(width=0.1)+
  scale_y_continuous(limits=c(0, 100)) +
  labs(title= "County Distribution of Percent of Consumers Reporting Broadband Access by Metro/Non-Metro", x= "Metro/Non-Metro", y="Percent of Consumers Reporting Broadband Access")

#FCC Violin Plot RUCC Code
discr %>% 
  filter(!is.na(availability_adv)) %>%
  ggplot(aes(x=RUCC_2013, y=availability_adv, fill = RUCC_2013)) + 
  geom_violin()+
  geom_boxplot(width=0.1)+
  scale_y_continuous(limits=c(0, 100)) +
  labs(title= "County Distribution of Percent of Population with Access to Broadband (at least 25 Mbps Download Speed) by Rucc Code (FCC)", x= "RUCC Code (1-3 Urban, 4-9 Rural)", y="Percent of Population with Access to Broadband (at least 25 Mbps Download Speed )")

discr %>% 
  filter(!is.na(availability_adv)) %>%
  ggplot(aes(x=ru_binary, y=availability_adv, fill = ru_binary)) + 
  geom_violin()+
  geom_boxplot(width=0.1)+
  scale_y_continuous(limits=c(0, 100)) +
  labs(title= "County Distribution of Percent of Population with Access to Broadband (at least 25 Mbps Download Speed) by Metro/Non-Metro", x= "Metro/Non-Metro", y="Percent of Population with Access to Broadband (at least 25 Mbps Download Speed )")

#Microsoft Violin Plot RUCC Code
discr %>% 
  filter(!is.na(BROADBAND.USAGE)) %>%
  ggplot(aes(x=RUCC_2013, y=BROADBAND.USAGE, fill= RUCC_2013))+
  geom_violin()+
  geom_boxplot(width=0.1) +
  scale_y_continuous(limits=c(0, 100))+
  labs(title= "County Distribution of Percent of Broadband Usage by RUCC Code (Microsoft)", x= "RUCC Code (1-3 Urban, 4-9 Rural)", y="Percent of Broadband Usage")

discr %>% 
  filter(!is.na(BROADBAND.USAGE)) %>%
  ggplot(aes(x=ru_binary, y=BROADBAND.USAGE, fill= ru_binary))+
  geom_violin()+
  geom_boxplot(width=0.1) +
  scale_y_continuous(limits=c(0, 100))+
  labs(title= "County Distribution of Percent of Broadband Usage by Metro/Non-Metro (Microsoft)", x= "Metro/Non-Metro", y="Percent of Broadband Usage")

#Discrepancy Violin Plot RUCC Code
discr %>% 
  filter(!is.na(abs_discr)) %>%
  ggplot(aes(x=RUCC_2013, y=abs_discr, fill= RUCC_2013))+
  geom_violin()+
  geom_boxplot(width=0.1) +
  scale_y_continuous(limits=c(0, 100))+
  labs(title= "County Distribution of Percent Discrepeancy Between FCC and ACS Broadband Measures by RUCC Code", x= "RUCC Code (1-3 Urban, 4-9 Rural)", y="Percent Discrepeancy")

discr %>% 
  filter(!is.na(discr_metr)) %>%
  ggplot(aes(x=ru_binary, y=discr_metr, fill= ru_binary))+
  geom_violin()+
  geom_boxplot(width=0.1) +
  scale_y_continuous(limits=c(-100, 100))+
  labs(title= "County Distribution of Percent Discrepeancy Between FCC and ACS Broadband Measures by Metro/Non-Metro", x= "Metro/Non-Metro", y="Percent Discrepeancy")

#Compare Three Different Sources Overall
discr_rs <- discr[, c("FIPS", "availability_adv", "B28002_007_per", "BROADBAND.USAGE", "RUCC_2013", "ru_binary")]
discr_rs <-  reshape(discr_rs, idvar = c("FIPS", "RUCC_2013", "ru_binary"), timevar = "variable", varying= c("availability_adv", "B28002_007_per", "BROADBAND.USAGE"), times= c("availability_adv", "B28002_007_per", "BROADBAND.USAGE"), v.names = "percent", direction = "long")
discr_rs$variable[discr_rs$variable=="availability_adv"] <- "FCC"
discr_rs$variable[discr_rs$variable=="B28002_007_per"] <- "ACS"
discr_rs$variable[discr_rs$variable=="BROADBAND.USAGE"] <- "Microsoft"

discr_rs$variable <- factor(discr_rs$variable, levels= c("FCC", "ACS", "Microsoft"))

discr_rs_discr <- discr[, c("FIPS", "RUCC_2013","discr_metr", "abs_discr")]
discr_rs_discr <-  reshape(discr_rs_discr, idvar = c("FIPS", "RUCC_2013"), timevar = "discr", varying= c("discr_metr", "abs_discr"), times= c("discr_metr", "abs_discr"), v.names = "percent", direction = "long")
discr_rs_discr$discr[discr_rs_discr$discr == "discr_metr"] <- "Discrepancy"
discr_rs_discr$discr[discr_rs_discr$discr == "abs_discr"] <- "Absolute Discrepancy"
discr_rs_discr$discr <- factor(discr_rs_discr$discr, levels= c("Discrepancy", "Absolute Discrepancy"))




######## FINAL PLOTS ################
ggplot(discr_rs, 
       aes(
         x = "", 
         y = percent, 
         fill = variable)) +
  geom_violin()+
  geom_boxplot(
    alpha = 0, 
    position = position_dodge(width=.9)) +
  labs(
    title = "Boxplots for County Distribution of Broadband Access by Data Source", 
    x = "",
    y = "Percent Within County With Broadband Access", 
    caption ="ACS: Households Reporting Broadband Access, American Community Survey (2013-2017) \nFCC: Provider-Reported Population with Access to Broadband (at least 25 Mbps Download Speed), Federal Communications Commission Form 477 (2015) \nMicrosoft: Broadband Usage,  Microsoft Airband Initiative (2018)") +
  theme(
    plot.title = element_text(hjust = 0.5, size=20, face = "bold"), 
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size=14),
    legend.position = "none",
    plot.caption = element_text(hjust = 0), 
    panel.background = element_blank(),
    strip.text = element_text(size=16)) +
  facet_wrap(~variable, strip.position = "bottom") +
  scale_fill_manual(values =c("#A9D5A5", "#DD7B7A", "#6DD4DB")) 


ggplot(discr_rs, 
  aes(
    x = "", 
    y = percent, 
    fill = ru_binary)) +
  geom_violin()+
  geom_boxplot(
    alpha = 0, 
    position = position_dodge(width=.9)) +
  labs(
    title = "Boxplots for County Distribution of Broadband Access by Urbanicity and Data Source", 
    x = "",
    y = "Percent Within County With Broadband Access", 
    caption ="Urbanicity Categorized Using USDA Rural-Urban Continuum Codes (Metro = Codes 1-3; Non-Metro = Codes 4-9) \nACS: Households Reporting Broadband Access, American Community Survey (2013-2017) \nFCC: Provider-Reported Population with Access to Broadband (at least 25 Mbps Download Speed), Federal Communications Commission Form 477 (2015) \nMicrosoft: Broadband Usage,  Microsoft Airband Initiative (2018)") +
  theme(
    plot.title = element_text(hjust = 0.5, size=20, face="bold"), 
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size=14),
    legend.title = element_blank(), 
    legend.position = "bottom",
    legend.text = element_text(size = 14), 
    legend.direction ="horizontal", 
    legend.key.size = unit(.5, "cm"),
    legend.spacing.x = unit(.2, "cm"),
    plot.caption = element_text(hjust = 0), 
    panel.background = element_blank(),
    strip.text = element_text(size=16)) +
  facet_wrap(~variable, strip.position = "bottom") +
  scale_fill_manual(values =c("#F17E1D", "#02ABD6"), labels = c("Metro", "Non-Metro")) 


ggplot(discr_rs, 
  aes(
    x = "", 
    y = percent, 
    fill = RUCC_2013)) +
  geom_violin()+
  geom_boxplot(
    alpha = 0, 
    position = position_dodge(width=.9)) +
  labs(
    title = "Boxplots for County Distribution of Broadband Access by Urbanicity and Data Source", 
    x = "",
    y = "Percent Within County With Broadband Access", 
    caption ="Urbanicity Categorized Using USDA Rural-Urban Continuum Codes (Metro = Codes 1-3; Non-Metro = Codes 4-9) \nACS: Households Reporting Broadband Access, American Community Survey (2013-2017) \nFCC: Provider-Reported Population with Access to Broadband (at least 25 Mbps Download Speed), Federal Communications Commission Form 477 (2015) \nMicrosoft: Broadband Usage,  Microsoft Airband Initiative (2018)") +
  theme(
    plot.title = element_text(hjust = 0.5, size=20, face = "bold"), 
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size=14),
    legend.title = element_blank(), 
    legend.position = "bottom",
    legend.text = element_text(size = 14), 
    legend.direction ="horizontal", 
    legend.key.size = unit(.5, "cm"),
    legend.spacing.x = unit(.2, "cm"),
    plot.caption = element_text(hjust = 0), 
    panel.background = element_blank(),
    strip.text = element_text(size=16)) +
  facet_wrap(~variable, strip.position = "bottom")+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  scale_fill_manual(values =c("#F17E1D", "#F39546", "#F6AC6F","#8CD8EC","#5EC9E4","#30BADD","#02ABD6", "#028CB0", "#026D89"), labels = c("Metro: 1", "Metro: 2", "Metro: 3", "Non-Metro: 4", "Non-Metro: 5", "Non-Metro: 6", "Non-Metro: 7", "Non-Metro: 8", "Non-Metro: 9")) 


ggplot(discr_rs_discr, 
       aes(
         x = "", 
         y = percent, 
         fill = RUCC_2013)) +
  geom_violin()+
  geom_boxplot(
    alpha = 0, 
    position = position_dodge(width=.9)) +
  labs(
    title = "Boxplots for County Distribution of Discrepancy Measure by Urbanicity", 
    x = "",
    y = "Percent Discrepancy by County", 
    caption ="Discrepancy Measure Calculates Difference Between Provider-Reported Population with Access to Broadband (at least 25 Mbps Download Speed) from the Federal Communications Commission Form 477 (2015) \nand Households Reporting Broadband Access from the American Community Survey (2013-2017) \nUrbanicity Categorized Using USDA Rural-Urban Continuum Codes (Metro = Codes 1-3; Non-Metro = Codes 4-9)") +
  theme(
    plot.title = element_text(hjust = 0.5, size=20, face = "bold"), 
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size=14),
    legend.title = element_blank(), 
    legend.position = "bottom",
    legend.text = element_text(size = 14), 
    legend.direction ="horizontal", 
    legend.key.size = unit(.5, "cm"),
    legend.spacing.x = unit(.2, "cm"),
    plot.caption = element_text(hjust = 0), 
    panel.background = element_blank(),
    strip.text = element_text(size=16)) +
  facet_wrap(~discr, strip.position = "bottom")+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  scale_fill_manual(values =c("#F17E1D", "#F39546", "#F6AC6F","#8CD8EC","#5EC9E4","#30BADD","#02ABD6", "#028CB0", "#026D89"), labels = c("Metro: 1", "Metro: 2", "Metro: 3", "Non-Metro: 4", "Non-Metro: 5", "Non-Metro: 6", "Non-Metro: 7", "Non-Metro: 8", "Non-Metro: 9"))+
  scale_y_continuous(breaks = seq(-100, 100, 50))+
  expand_limits(y=c(-100, 100))






######Correlation with Land Area#####
cor.test(x=log(discr$ALAND), y=discr$abs_discr)

discr %>% 
  ggscatter(x = "ALAND", y = "abs_discr", color= "#F17E1D", alpha = 0.1,
          add = "reg.line", conf.int = TRUE, add.params = list(color = "blue", fill = "lightgray"), 
          cor.coef = FALSE, cor.method = "pearson", fullrange = TRUE)+
  xscale("log10", .format = TRUE)+
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  labs(title = "Correlation of Land Area (Log) and Percent Discrepancy", x= "Land Area (log)", y = "Percent Discrepancy")

discr %>%
  ggscatter(x = "ALAND", y = "abs_discr", alpha = 0.1,
          color = "RUCC_2013", 
          facet.by = "RUCC_2013", 
          add = "reg.line", conf.int = TRUE,  cor.method = "pearson", cor.coef = FALSE, fullrange = TRUE) +
  xscale("log10", .format = TRUE)+
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  labs(title = "Correlation of Land Area (Log) and Percent Discrepancy by RUCC Code", x= "Land Area (log)", y = "Percent Discrepancy")

discr %>%
  ggscatter(x = "ALAND", y = "abs_discr", alpha = 0.1,
          color = "ru_binary", 
          facet.by = "ru_binary", 
          add = "reg.line", conf.int = TRUE,  cor.method = "pearson", cor.coef = FALSE, fullrange = TRUE) +
  xscale("log10", .format = TRUE)+
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  labs(title = "Correlation of Land Area (Log) and Percent Discrepancy by Metro/Non-Metro", x= "Land Area (log)", y = "Percent Discrepancy")

#####Correlation to Unemployed#####
empl<- read.csv("data/working/acs_transform/acs_empl.csv")
empl$GEOID <- as.character(as.integer(empl$GEOID))
empl$GEOID <- ifelse(nchar(empl$GEOID)==4,paste0("0",empl$GEOID),empl$GEOID)
discr <- merge(discr, empl[,c("B23025_005_per", "GEOID")], by.x="FIPS", by.y="GEOID", all.x = TRUE)
colnames(discr)[colnames(discr)=="B23025_005_per"] <- "unempl"

cor.test(x=discr$unempl, y=discr$abs_discr)

discr %>% 
  ggscatter(x = "unempl", y = "abs_discr", color= "#F17E1D", alpha = 0.1,
            add = "reg.line", conf.int = TRUE, add.params = list(color = "blue", fill = "lightgray"), 
            cor.coef = FALSE, cor.method = "pearson", fullrange = TRUE)+
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  labs(title = "Correlation of Percent Unemployed and Percent Discrepancy", x= "Percent Unemployed", y = "Percent Discrepancy")

discr %>%
  ggscatter(x = "unempl", y = "abs_discr", alpha = 0.1,
            color = "RUCC_2013", 
            facet.by = "RUCC_2013", 
            add = "reg.line", conf.int = TRUE,  cor.method = "pearson", cor.coef = FALSE, fullrange = TRUE) +
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  labs(title = "Correlation of Percent Unemployed and Percent Discrepancy by RUCC Code", x= "Percent Unemployed", y = "Percent Discrepancy")

discr %>%
  ggscatter(x = "unempl", y = "abs_discr", alpha = 0.1,
            color = "ru_binary", 
            facet.by = "ru_binary", 
            add = "reg.line", conf.int = TRUE,  cor.method = "pearson", cor.coef = FALSE, fullrange = TRUE) +
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  labs(title = "Correlation of Percent Unemployed and Percent Discrepancy by Metro/Non-Metro", x= "Percent Unemployed", y = "Percent Discrepancy")

######Discrepancy to Hispanic/Latino#####
ethn<- read.csv("data/working/acs_transform/acs_ethn.csv")
ethn$GEOID <- as.character(as.integer(ethn$GEOID))
ethn$GEOID <- ifelse(nchar(ethn$GEOID)==4,paste0("0",ethn$GEOID),ethn$GEOID)
discr <- merge(discr, ethn[, c("B03003_003_per", "GEOID")], by.x="FIPS", by.y="GEOID", all.x = TRUE)
colnames(discr)[colnames(discr)=="B03003_003_per"] <- "his_lat"

cor.test(x=discr$his_lat, y=discr$abs_discr)

discr %>% 
  ggscatter(x = "his_lat", y = "abs_discr", color= "#F17E1D", alpha = 0.1,
            add = "reg.line", conf.int = TRUE, add.params = list(color = "blue", fill = "lightgray"), 
            cor.coef = FALSE, cor.method = "pearson", fullrange = TRUE)+
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  labs(title = "Correlation of Percent Hispanic/Latino Population and Percent Discrepancy", x= "Percent Hispanic/Latino Population", y = "Percent Discrepancy")

discr %>%
  ggscatter(x = "his_lat", y = "abs_discr", alpha = 0.1,
            color = "RUCC_2013", 
            facet.by = "RUCC_2013", 
            add = "reg.line", conf.int = TRUE,  cor.method = "pearson", cor.coef = FALSE, fullrange = TRUE) +
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  labs(title = "Correlation of Percent Hispanic/Latino Population and Percent Discrepancy by RUCC Code", x= "Percent Hispanic/Latino Population", y = "Percent Discrepancy")

discr %>%
  ggscatter(x = "his_lat", y = "abs_discr", alpha = 0.1,
            color = "ru_binary", 
            facet.by = "ru_binary", 
            add = "reg.line", conf.int = TRUE,  cor.method = "pearson", cor.coef = FALSE, fullrange = TRUE) +
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  labs(title = "Correlation of Percent Hispanic/Latino Population and Percent Discrepancy by Metro/Non-Metro", x= "Percent Hispanic/Latino Population", y = "Percent Discrepancy")

#####Discrepancy to Race#####
race<- read.csv("data/working/acs_transform/acs_race.csv")
race$minority_per <- race$B02001_003_per + race$B02001_004_per + race$B02001_005_per + race$B02001_006_per + race$B02001_007_per + race$B02001_008_per
race$GEOID <- as.character(as.integer(race$GEOID))
race$GEOID <- ifelse(nchar(race$GEOID)==4,paste0("0",race$GEOID),race$GEOID)
discr <- merge(discr, race[, c("GEOID", "minority_per")], by.x="FIPS", by.y = "GEOID", all.x = TRUE)

cor.test(x=discr$minority_per, y=discr$abs_discr)

discr %>% 
  ggscatter(x = "minority_per", y = "abs_discr", color= "#F17E1D", alpha = 0.1,
            add = "reg.line", conf.int = TRUE, add.params = list(color = "blue", fill = "lightgray"), 
            cor.coef = FALSE, cor.method = "pearson", fullrange = TRUE)+
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  labs(title = "Correlation of Percent Minority Population and Percent Discrepancy", x= "Percent Minority Population", y = "Percent Discrepancy")

discr %>%
  ggscatter(x = "minority_per", y = "abs_discr", alpha = 0.1,
            color = "RUCC_2013", 
            facet.by = "RUCC_2013", 
            add = "reg.line", conf.int = TRUE,  cor.method = "pearson", cor.coef = FALSE, fullrange = TRUE) +
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  labs(title = "Correlation of Percent Minority Population and Percent Discrepancy by RUCC Code", x= "Percent Minority Population", y = "Percent Discrepancy")

discr %>%
  ggscatter(x = "minority_per", y = "abs_discr", alpha = 0.1,
            color = "ru_binary", 
            facet.by = "ru_binary", 
            add = "reg.line", conf.int = TRUE,  cor.method = "pearson", cor.coef = FALSE, fullrange = TRUE) +
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  labs(title = "Correlation of Percent Minority Population and Percent Discrepancy by Metro/Non-Metro", x= "Percent Minority Population", y = "Percent Discrepancy")

#####Discrepancy to Poverty######
pov<- read.csv("data/working/acs_transform/acs_pov.csv")
pov$under_1 <- pov$B17026_002_per + pov$B17026_003_per + pov$B17026_004_per
pov$GEOID <- as.character(as.integer(pov$GEOID))
pov$GEOID <- ifelse(nchar(pov$GEOID)==4,paste0("0",pov$GEOID),pov$GEOID)
discr <- merge(discr, pov[, c("GEOID", "under_1")], by.x="FIPS", by.y = "GEOID", all.x = TRUE)

cor.test(x=discr$under_1, y=discr$abs_discr)

discr %>% 
  ggscatter(x = "under_1", y = "abs_discr", color= "#F17E1D", alpha = 0.1,
            add = "reg.line", conf.int = TRUE, add.params = list(color = "blue", fill = "lightgray"), 
            cor.coef = FALSE, cor.method = "pearson", fullrange = TRUE)+
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  labs(title = "Correlation of Percent of Population with Income to Poverty Ratio Under 1 and Percent Discrepancy", x= "Percent of Population with Income to Poverty Ratio Under 1", y = "Percent Discrepancy")

discr %>%
  ggscatter(x = "under_1", y = "abs_discr", alpha = 0.1,
            color = "RUCC_2013", 
            facet.by = "RUCC_2013", 
            add = "reg.line", conf.int = TRUE,  cor.method = "pearson", cor.coef = FALSE, fullrange = TRUE) +
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  labs(title = "Correlation of Percent of Population with Income to Poverty Ratio Under 1 and Percent Discrepancy by RUCC Code", x= "Percent of Population with Income to Poverty Ratio Under 1", y = "Percent Discrepancy")

discr %>%
  ggscatter(x = "under_1", y = "abs_discr", alpha = 0.1,
            color = "ru_binary", 
            facet.by = "ru_binary", 
            add = "reg.line", conf.int = TRUE,  cor.method = "pearson", cor.coef = FALSE, fullrange = TRUE) +
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  labs(title = "Correlation of Percent of Population with Income to Poverty Ratio Under 1 and Percent Discrepancy by Metro/Non-Metro", x= "Percent of Population with Income to Poverty Ratio Under 1", y = "Percent Discrepancy")

#####Discrepancy to Education
edu<- read.csv("data/working/acs_transform/acs_edu.csv")
edu$no_hs <- edu$B15003_002_per + edu$B15003_003_per + edu$B15003_004_per + edu$B15003_005_per + edu$B15003_006_per + edu$B15003_007_per + edu$B15003_008_per + edu$B15003_009_per + edu$B15003_010_per + edu$B15003_011_per + edu$B15003_012_per + edu$B15003_013_per + edu$B15003_014_per + edu$B15003_015_per + edu$B15003_016_per
edu$GEOID <- as.character(as.integer(edu$GEOID))
edu$GEOID <- ifelse(nchar(edu$GEOID)==4,paste0("0",edu$GEOID),edu$GEOID)
discr <- merge(discr, edu[, c("GEOID", "no_hs")], by.x="FIPS", by.y = "GEOID", all.x = TRUE)

cor.test(x=discr$no_hs, y=discr$abs_discr)

discr %>% 
  ggscatter(x = "no_hs", y = "abs_discr", color= "#F17E1D", alpha = 0.1,
            add = "reg.line", conf.int = TRUE, add.params = list(color = "blue", fill = "lightgray"), 
            cor.coef = FALSE, cor.method = "pearson", fullrange = TRUE)+
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  labs(title = "Correlation of Percent of Population Without High School Diploma and Percent Discrepancy", x= "Percent of Population Without High School Diploma", y = "Percent Discrepancy")

discr %>%
  ggscatter(x = "no_hs", y = "abs_discr", alpha = 0.1,
            color = "RUCC_2013", 
            facet.by = "RUCC_2013", 
            add = "reg.line", conf.int = TRUE,  cor.method = "pearson", cor.coef = FALSE, fullrange = TRUE) +
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  labs(title = "Correlation of Percent of Population Without High School Diploma and Percent Discrepancy by RUCC Code", x= "Percent of Population Without High School Diploma", y = "Percent Discrepancy")

discr %>%
  ggscatter(x = "no_hs", y = "abs_discr", alpha = 0.1,
            color = "ru_binary", 
            facet.by = "ru_binary", 
            add = "reg.line", conf.int = TRUE,  cor.method = "pearson", cor.coef = FALSE, fullrange = TRUE) +
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  labs(title = "Correlation of Percent of Population Without High School Diploma and Percent Discrepancy by Metro/Non-Metro", x= "Percent of Population Without High School Diploma", y = "Percent Discrepancy")

#####FCC vs ACS#####
cor.test(x=discr$availability_adv, y=discr$B28002_007_per)

discr %>% 
ggscatter(x = "availability_adv", y = "B28002_007_per", color= "#F17E1D", alpha = 0.1,
          add = "reg.line", conf.int = TRUE, add.params = list(color = "blue", fill = "lightgray"), 
          cor.coef = FALSE, cor.method = "pearson", fullrange = TRUE)+
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  labs(title = "Correlation of FCC and ACS Broadband Availability", x= "Percent of FCC Availability", y = "Percent of ACS Availability")

discr %>%
  ggscatter(x = "availability_adv", y = "B28002_007_per", alpha = 0.1,
            color = "RUCC_2013", 
            facet.by = "RUCC_2013", 
            add = "reg.line", conf.int = TRUE,  cor.method = "pearson", cor.coef = FALSE, fullrange = TRUE) +
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  labs(title = "Correlation of FCC and ACS Broadband Availability by RUCC Code", x= "Percent of FCC Availability", y = "Percent of ACS Availability")

discr %>%
  ggscatter(x = "availability_adv", y = "B28002_007_per", alpha = 0.1,
            color = "ru_binary", 
            facet.by = "ru_binary", 
            add = "reg.line", conf.int = TRUE,  cor.method = "pearson", cor.coef = FALSE, fullrange = TRUE) +
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  labs(title = "Correlation of FCC and ACS Broadband Availability by Metro/Non-Metro", x= "Percent of FCC Availability", y = "Percent of ACS Availability")

#####Density Plots#####
ggplot(discr, aes(x = abs_discr, fill= ru_binary, color= ru_binary)) +
  geom_density(alpha = .8)+
  labs(title= "Density Plot of Discrepancy Measures for Metro vs Nonmetro Counties")+ 
  scale_x_continuous(name="Discrepancy", limits=c(0, 100)) +
  scale_y_continuous(name="Density", limits=c(0, .04)) +
  scale_fill_manual(values=c("#02ABD6", "#F17E1D")) +
  theme_classic()+
  scale_color_manual(values=c("#02ABD6", "#F17E1D"))

va <- discr %>% filter(FIPS %like% '^51.')
ggplot(va, aes(x = abs_discr, fill= ru_binary, color = ru_binary)) +
  geom_density( alpha = .8) +
  labs(title= "Density Plot of Discrepancy Measures for Metro vs Nonmetro Counties in VA")+
  scale_x_continuous(name="Discrepancy", limits=c(0, 100)) +
  scale_y_continuous(name="Density", limits=c(0, 0.04))+
  scale_fill_manual(values=c("#02ABD6", "#F17E1D")) +
  theme_classic()+
  scale_color_manual(values=c("#02ABD6", "#F17E1D"))


######REGRESSION#####

discr_rs2 <- discr[, c("FIPS", "abs_discr", "RUCC_2013", "ru_binary", "unempl", "his_lat", "minority_per", "under_1", "no_hs")]
discr_rs2 <-  reshape(discr_rs2, idvar = c("FIPS", "abs_discr", "RUCC_2013", "ru_binary"), timevar = "variable", varying= c("unempl", "his_lat", "minority_per", "under_1", "no_hs"), times= c("unempl", "his_lat", "minority_per", "under_1", "no_hs"), v.names = "percent", direction = "long")


discr_rs2 %>%
  ggscatter(x = "percent", y = "abs_discr", alpha = 0.1,
    color = "ru_binary", 
    palette = c("#F17E1D", "#02ABD6"),
    facet.by = c("ru_binary", "variable"), 
    add = "reg.line",
    add.params = list(color = "#1B3766", fill = "lightgray"),
    conf.int = TRUE,  
    panel.labs = list(variable = c("Percent Hispanic/Latino", "Percent Minority", "Percent Without \nHigh School Diploma", "Percent With Poverty/Income \nRatio Less Than 1", "Percent Unemployed"), ru_binary = c("Metro", "Non-Metro")),
    cor.method = "pearson", 
    cor.coef = FALSE, 
    fullrange = TRUE) +
  stat_cor( aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  theme(
    plot.title = element_text(hjust = 0.5, size=20, face = "bold"), 
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size=14),
    legend.position = "none",
    plot.caption = element_text(hjust = 0), 
    panel.background = element_blank(),
    strip.text = element_text(size=12))+
  labs(
    title = "County-Level Correlation Between Sociodemographic Incidators and Percent Discrepancy", 
    y = "Percent Discrepancy", 
    x = "", 
    caption ="Sociodemographic Data from the American Community Survey (2013-2017) \nDiscrepancy Measure Calculates Difference Between Provider-Reported Population with Access to Broadband (at least 25 Mbps Download Speed) from the Federal Communications Commission Form 477 (2015) \nand Households Reporting Broadband Access from the American Community Survey (2013-2017)") +  
  scale_y_continuous(
    limits=c(0, 100))
#+
 # scale_fill_manual(
  #  values =c("#F17E1D", "#02ABD6"))
  


  