library(tidyverse)
library(corrplot)
library(distances)

comparison <- read_csv("forsyth_comparison.csv")

# correlation between variables ---------------

# create correlation plot between variable
cor_columns <- c("total_pop.place", "total_pop.county", 
                  "age_pop.place", "age_pop.county", 
                  "white_perc.place", "white_perc.county", 
                  "aa_perc.place", "aa_perc.county", 
                  "hisp_perc.place", "hisp_perc.county")

col_columns_county <- c("total_pop.county","age_pop.county", 
                        "white_perc.county","aa_perc.county", 
                        "hisp_perc.county")

corrplot(cor(comparison[col_columns_county], use = "complete.obs"), 
         type = "upper", method = "number")

# plot densities of populations and areas

density_plot <- function(x_var, color_fill, main_title) {
  
  ggplot(comparison, aes(x = {{ x_var }})) +
    geom_density(fill=color_fill, color=color_fill, alpha = 0.4) +
    labs(x = main_title) +
    theme_minimal()
  
}

county_pop <- density_plot(total_pop.county, "#eb4034", "County Populations")
county_pop_log <- density_plot(total_pop_log.county, "#0f4d94", "Log of County Populations")

density <- density_plot(pop_density.county, "#eb4034", "County Population Density (Sq. Km)")
density_log <- density_plot(pop_density_log.county, "#0f4d94", "Log of County Population Density (Sq. Km)")

# calculate euclidean distance -----------------------------

comp_dist <- distances(data = as.matrix(comparison[col_columns_county]), 
                       normalize = "studentize", weights = c(3,3,1,1,1)) %>%
  as.matrix() %>%
  as.data.frame()

# find which row forsyth is, so we can extract it from distance DF
forsyth_row <- which(comparison$county_name == "Forsyth")

# only keep this column of distance matrix
forsyth_dist <- comp_dist[forsyth_row]

# add county names to forsyth distance matrix
forsyth_dist$county <- comparison$county_name

colnames(forsyth_dist) <- c("distance", "county")

# to verify counties align, Forsyth's distance should be zero
forsyth_dist[forsyth_dist$county == "Forsyth", ]

# remove Forsyth and rank counties from smallest to largest
forsyth_dist <- forsyth_dist %>%
  filter(county != "Forsyth") %>%
  arrange(distance) %>%
  select(county, distance)

