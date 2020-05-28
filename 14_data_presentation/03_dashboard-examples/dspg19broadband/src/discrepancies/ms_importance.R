library(dplyr)
library(ggplot2)
library(readr)
library(forcats)
library(ggthemes)
library(viridis)


# Read in and prepare
data <- read_csv("./data/working/discrepancies/fcc_ms_vimp.csv", col_names = TRUE)
data$variable <- as.factor(data$variable)
data$counties <- as.factor(data$counties)
data$sigtype <- as.factor(data$sigtype)

# Plot - bar
ggplot(data, aes(y = importance, x = fct_reorder(variable, importance), fill = counties, color = counties)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  coord_flip() +
  scale_fill_viridis(discrete = TRUE) +
  theme_hc() +
  labs(title = "Imputation-based variable importance in predicting\ncounty-level broadband coverage discrepancies",
       x = " Variable", y = "Importance", fill = "County model")
ggsave("./doc/discrepancies/counties_all_discr_imp_compare_bar.png", plot = last_plot(), device = "png")

# Plot - lollipop
ggplot(data, aes(importance, fct_reorder(variable, importance), color = counties)) +
  geom_segment(aes(x = 0, y = fct_reorder(variable, importance), xend = importance, yend = fct_reorder(variable, importance)), color = "grey50") +
  geom_point(size = 3) +
  scale_color_colorblind() +
  theme_hc() +
  labs(title = "Imputation-based variable importance in predicting\ncounty-level broadband coverage discrepancies",
       x = "Importance", y = "Variable", color = "County model")
ggsave("./doc/discrepancies/counties_all_discr_imp_compare_lol.png", plot = last_plot(), device = "png")


