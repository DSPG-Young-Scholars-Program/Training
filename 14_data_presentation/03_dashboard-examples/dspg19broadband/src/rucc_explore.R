library(ggplot2)

bbnow <- read.csv("./data/working/merged_by_rural_urban.csv")

#my_data <- read.table(file = "clipboard",sep = "\t", header=TRUE)


rucc_9 <- subset(bbnow, bbnow$RUCC_2013 == "9")
rucc_8 <- subset(bbnow, bbnow$RUCC_2013 == "8")
rucc_7 <- subset(bbnow, bbnow$RUCC_2013 == "7")
rucc_6 <- subset(bbnow, bbnow$RUCC_2013 == "6")
rucc_5 <- subset(bbnow, bbnow$RUCC_2013 == "5")
rucc_4 <- subset(bbnow, bbnow$RUCC_2013 == "4")
rucc_3 <- subset(bbnow, bbnow$RUCC_2013 == "3")
rucc_2 <- subset(bbnow, bbnow$RUCC_2013 == "2")
rucc_1 <- subset(bbnow, bbnow$RUCC_2013 == "1")



library(ggplot2)
theme_set(theme_classic())

# Histogram on a Continuous (Numeric) Variable
g <- ggplot(bbnow, aes(bbnow$stateid)) + scale_fill_brewer(palette = "Spectral")

g + geom_histogram(aes(fill=class), 
                   binwidth = .1, 
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram with Auto Binning", 
       subtitle="Engine Displacement across Vehicle Classes")  

g + geom_histogram(aes(fill=bbnow$RUCC_2013), 
                   bins=5, 
                   col="black", 
                   size=.1) +   # change number of bins
  labs(title="Histogram with Fixed Bins", 
       subtitle="Engine Displacement across Vehicle Classes")

meltDF$variable=as.numeric(levels(meltDF$variable))[meltDF$variable]

#bbnow$RUCC_2013 = as.numeric(levels(bbnow$RUCC_2013))[bbnow$RUCC_2013]
#bbnow$Population_2010 = as.numeric(levels(bbnow$Population_2010))[bbnow$]
gg <- ggplot(bbnow, aes(x=bbnow$RUCC_2013, y=bbnow$Population_2010)) + 
  geom_point(aes(colour = bbnow$RUCC_2013)) + 
  xlim(c(0, 0.1)) + 
  ylim(c(0, 500000)) + 
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot", 
       caption = "Source: midwest")

plot(gg)
