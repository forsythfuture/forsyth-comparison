library(tidyverse)
library(tidycensus)
library(FFtools)
library(tigris)
library(rgeos)
library(sp)

# find out which places are in which counties -------------------------

# download place and county shape files from census API
plc <- places(state = "NC")
cnty <- counties(state = "nc")

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

# import county and place population datasets
df <- map_df(c("county", "place"),
             function (x) {
               get_acs(geography = x,
                       state = 'nc',
                       year = 2017,
                       table = "DP05") %>%
                 mutate(geography = !! x)
              }
             ) %>%
  # merge variable descriptions
  left_join(vars, by = c("variable" = "name"))
