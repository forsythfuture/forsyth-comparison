library(tidyverse)
library(corrplot)
library(distances)
library(gridArrange)

comparison <- read_csv("forsyth_comparison.csv")

# correlation between variables ---------------

# create vectors of columns to be used
county_col <- c("age_pop.county", "white_perc.county","aa_perc.county", "hisp_perc.county")
place_col <- c("age_pop.place", "white_perc.place","aa_perc.place", "hisp_perc.place")
county_log <- c("total_pop_log.county", "pop_density_log.county")
place_log <- c("total_pop_log.place", "pop_density_log.place")

a <- rcorr(as.matrix(comparison[county_col]))[["r"]]
#corrplot(cor(comparison[col_columns_county], use = "complete.obs"), 
#         type = "upper", method = "number")

# plot densities of populations and areas

density_plot <- function(x_var_county, x_var_place, main_title) {
  
  ggplot(comparison, aes(x = {{ x_var }})) +
    geom_density(fill=color_fill, color=color_fill, alpha = 0.4) +
    labs(x = main_title) +
    theme_minimal()
  
}

density_plot <- function(df, x_place, x_county, x_title) {
  
  ggplot(df) +
    geom_density(aes(x={{ x_place }}, fill="#4287f5"), alpha=0.4) +
    geom_density(aes(x={{ x_county }}, fill="#f57542"), alpha=0.4) +
    labs(x = x_title) +
    theme_minimal() +
    theme(legend.position="none")

}
density_plot(comparison, total_pop.place, total_pop.county, "total population")

county_pop <- density_plot(total_pop.county, "#eb4034", "County Populations")
county_pop_log <- density_plot(total_pop_log.county, "#0f4d94", "Log of County Populations")

density <- density_plot(pop_density.county, "#eb4034", "County Population Density (Sq. Km)")
density_log <- density_plot(pop_density_log.county, "#0f4d94", "Log of County Population Density (Sq. Km)")

grid.arrange(county_pop, county_pop_log, density, density_log, nrow=4)

# calculate euclidean distance -----------------------------

# create function to calculate distances and order results
euc_dist <- function(dist_columns) {
  
  df <- comparison[dist_columns] %>%
    mutate_all(scale) %>%
    as.matrix()
  
  dist <- distances(data = df, 
                    #normalize = "studentize", 
                    weights = c(1,1,.3,.3,.3,1)) %>%
    as.matrix() %>%
    as.data.frame()
  
  # find which row forsyth is, so we can extract it from distance DF
  forsyth_row <- which(comparison$county_name == "Forsyth")
  
  # only keep this column of distance matrix
  forsyth_dist <- dist[forsyth_row]
  
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
  
  return(forsyth_dist)

}

euc_dist(col_columns_county)
