library(tidycensus)
library(FFtools)
library(tigris)
library(rgeos)
library(sp)
library(sf)
library(raster)
library(tidyverse)

select <- dplyr::select

# find out which places are in which counties -------------------------

# download place and county shape files from census API
plc <- places(state = "NC")
cnty <- counties(state = "NC")

# calculate whether places are wholly contained within counties
# store results in list, where each list element is a different county
# and the element shows all places within the county
county_places <- gContains(cnty, plc, byid=TRUE, returnDense=FALSE)

places_counties <- function(county) {
  
  # if there are no palces in the county (value is null) then enter NA in data frame
  # otherwise, enter the palces
  places_within <- if (is.null(county_places[[county]])) NA else county_places[[county]]
  
  data.frame(county = county,
             place = places_within)
}

# create single long dataframe where each row is a place / county
# combination
place_long <- map_df(seq_along(county_places), places_counties)

# the places and counties are currently identified by numbers, which represent row
# numbers in the original dataframes we want to add labels

# create dataframes that are the only the row numebrs and labels of the places
# and counties, these will then be merged on to the places_long dataframe
# these dataframes serve as lookup tables between numbers and names
convert_to_names <- function(df, col_name) {
  
  df <- df[c("GEOID", "NAME")]
  colnames(df) <- c("GEOID", col_name)
  df$row_num <- seq(1, nrow(df))
  return(df)
  
}

# create lookup tables
place_names <- convert_to_names(plc@data, "place_name")
county_names <- convert_to_names(cnty@data, "county_name")

# merge lookup tables that contain names with table that contains numbers
# and which places are within what counties
place_table <- place_long %>%
  left_join(county_names, by = c("county" = "row_num")) %>%
  left_join(place_names, by = c("place" = "row_num")) %>%
  select(-county, -place) %>%
  rename(GEOID_county = GEOID.x, GEOID_place = GEOID.y)

# population ---------------------------------

# load census variable names
# needed so we can identify which tables and variables to pull
datasets <- c("acs1", "acs1/subject", "acs1/profile")

vars <- map_df(datasets, function(x) load_variables(year = 2017, dataset = x))

# import county and place population datasets -------------------

# import palce and identify largest population place in each county
pop_place <- get_acs(geography = "place",
                     state = 'nc',
                     year = 2017,
                     table = "DP05") %>%
                 mutate(geography = "place") %>%
  # merge variable descriptions
  left_join(vars, by = c("variable" = "name"))

# identify largest palce in county
place_total_pop <- pop_place %>%
  # isolate total place populations
  filter(label == "Estimate!!SEX AND AGE!!Total population") %>%
  # join county lookup table, so we know what county the palce is in
  left_join(place_table, by = c("GEOID" = "GEOID_place")) %>%
  select(-place_name)

# we'll manually add counties to cities with population over 10000 and werent matched
# these represent places in more than one county
manual_add_county <- place_total_pop %>%
  filter((is.na(county_name) & estimate > 15000)) %>%
  select(GEOID)

add_counties <- c(37001, 37183, 37135, 37101, 37063, 37139, 37035,37081,
                  37025, 37067, 37179, 37183, 37183, 37065, 37057, 37069)

manual_add_county$GEOID_county <- add_counties

# add missing county values
place_total_pop <- place_total_pop %>%
  left_join(manual_add_county, by = "GEOID") %>%
  mutate(GEOID_county.x = ifelse(is.na(GEOID_county.x), GEOID_county.y, GEOID_county.x)) %>%
  select(-GEOID_county.y) %>%
  rename(GEOID_county = GEOID_county.x) %>%
  # now we need to add the county names for these missing values
  left_join(county_names, by = c("GEOID_county" = "GEOID")) %>%
  mutate(county_name.x = ifelse(is.na(county_name.x), county_name.y, county_name.x)) %>%
  select(-county_name.y, -row_num) %>%
  rename(county_name = county_name.x)

# select most populated city in each county
city_county_lookup <- place_total_pop %>%
  group_by(GEOID_county) %>%
  # create column showing the most populated city
  mutate(most_pop = max(estimate)) %>%
  # filter for this population
  filter(estimate == most_pop,
         # remove NA
         !is.na(GEOID_county)) %>%
  ungroup() %>%
  select(GEOID, NAME, GEOID_county, county_name, estimate)

# filter our primary dataset to only keep the most populace places
pop_place <- pop_place %>%
  filter(GEOID %in% city_county_lookup$GEOID) %>%
  # add county to dataset
  left_join(city_county_lookup[, c("GEOID", "GEOID_county", "county_name")], by = "GEOID")
  
# remove unneeded items
rm(county_places, manual_add_county, place_long, place_names, place_table,
   place_total_pop, add_counties, county_names)

# import county population data and merge with population

# import county data
pop_county <- get_acs(geography = "county",
                     state = 'nc',
                     year = 2017,
                     table = "DP05") %>%
  mutate(geography = "county") %>%
  # merge variable descriptions
  left_join(vars, by = c("variable" = "name"))

# stack county data to place data
pop <- bind_rows(pop_place, pop_county)

# filter population data ------------------------

# we'll filter the data by pulling out the total population, 
# race, and age data separately
# then, we will recombine

# total population
total <- pop %>%
  # isolate total place populations
  filter(label == "Estimate!!SEX AND AGE!!Total population")

# median age
age <- pop %>%
  filter(label == "Estimate!!SEX AND AGE!!Median age (years)")

# race / ethnicity
white <- pop %>%
  filter(str_detect(label, "^Percent.*!!Not Hispanic or Latino!!White alone"))

aa <- pop %>%
  filter(label == "Percent!!RACE!!One race!!Black or African American")

hisp <- pop %>%
  filter(str_detect(label, "^Percent!!HISPANIC.*of any race.$"))

# recombine population data into one dataset -----------------------

# create the skeleton dataset that the other datasets will be attached to
# it will be based on the total dataset
cleaned_df <- total %>%
  select(GEOID, NAME, total_pop = estimate, total_moe = moe, GEOID_county, county_name)

# create a function to merge the other datasets to the cleaned on
merge_demo <- function(df, estimate_col, moe_col) {
  
  df <- df %>%
    select(GEOID, estimate, moe)
  
  colnames(df) <- c("GEOID", estimate_col, moe_col)
  
  cleaned <- cleaned_df %>%
    left_join(df, by = "GEOID")
  
  return(cleaned)
  
}
cleaned_df <- merge_demo(age, "age_pop", "age_moe")
cleaned_df <- merge_demo(white, "white_perc", "white_moe")
cleaned_df <- merge_demo(aa, "aa_perc", "aa_moe")
cleaned_df <- merge_demo(hisp, "hisp_perc", "hisp_moe")

# break off places as rows and add as columns -------------------

# create separate dataframes of jsut palces and counties
places <- cleaned_df %>%
  filter(!is.na(GEOID_county))

county <- cleaned_df %>%
  filter(is.na(GEOID_county)) %>%
  select(-GEOID_county, -county_name)

# merge places and county dataframes back into cleaned dataframe
cleaned_df <- left_join(places, county, by = c("GEOID_county"="GEOID"),
                   suffix = c(".place", ".county")) %>%
  select(-NAME.county) %>%
  select(GEOID_place = GEOID, place_name = NAME.place, GEOID_county, county_name, everything())

rm(total, age, white, aa, hisp, pop_county, pop)

# find population density ------------------------------------

# calculate areas of counties and places and palce in data frame
# use function for both

calc_area <- function(spatial_object) {
  
  data.frame(
    GEOID = spatial_object@data$GEOID,
    name = spatial_object@data$NAME,
    area = area(spatial_object) / 1000000 # area in sq kilometers
  ) 
  
}

cnty_areas <- calc_area(cnty)
place_areas <- calc_area(plc)
  
# merge county and place areas to final dataset,
# so we can calculate population densities
# we'll also log populations while we're here
pop_dens <- cleaned_df %>%
  left_join(place_areas[c("GEOID", "area")], by = c("GEOID_place" = "GEOID")) %>%
  mutate(total_pop_log.place = log(total_pop.place),
         pop_density.place = total_pop.place / area,
         pop_density_log.place = total_pop_log.place / area) %>%
  select(-area) %>%
  left_join(cnty_areas[c("GEOID", "area")], by = c("GEOID_county" = "GEOID")) %>%
  mutate(total_pop_log.county = log(total_pop.county),
         pop_density.county = total_pop.county / area,
         pop_density_log.county = total_pop_log.county / area) %>%
  select(-area)

# write out final dataset
write_csv(pop_dens, "forsyth_comparison.csv")
