# data cleaner
library(tidyverse)
library(janitor)
library(sf)
library(data.table)
library(lubridate)

# check objects in environment
if (exists("raw_geo_export_3f3122c2-4cfc-489b-8bd1-590a8876ad0b")) {
  print("All the necessary objects available in environment.")
  } else {
    print("Need to run the read script...")
    source("./1-load.R")
    print("...all the necessary objects now available in environment.")
    }

ls()

#study frame https://www.abs.gov.au/websitedbs/D3310114.nsf/Home/2016%20DataPacks
city_of_melbourne <- raw_LGA_2016_AUST %>%
  st_as_sf() %>%
  clean_names() %>%
  filter(lga_name16 == "Melbourne (C)") %>%
  mutate(city = "City of Melbourne") %>%
  select(lga_name16, city)

st_write(city_of_melbourne, "../outputs/city_of_melbourne.shp", driver="ESRI Shapefile", delete_layer =TRUE)
st_write(city_of_melbourne, "../city_of_melbourne.shp", driver="ESRI Shapefile", delete_layer =TRUE)


# CLUE Data https://data.melbourne.vic.gov.au/stories/s/CLUE/rt3z-vy3t?src=hdr
clue_blocks <- get('raw_geo_export_3f3122c2-4cfc-489b-8bd1-590a8876ad0b') %>%
  st_as_sf() %>%
  st_transform(st_crs(city_of_melbourne)) %>%
  select(block_id) %>%
  mutate(block_id = as.numeric(block_id))

st_write(clue_blocks, "../outputs/clue_blocks.shp", driver="ESRI Shapefile", delete_layer =TRUE)

st_write(clue_blocks, "../clue_blocks.shp", driver="ESRI Shapefile", delete_layer =TRUE)

## Cafes and Restaurants Seating https://data.melbourne.vic.gov.au/Business/Cafes-and-restaurants-with-seating-capacity/xt2y-tnn9
clue_data <- get("raw_Cafes_and_restaurants__with_seating_capacity") %>%
  as_tibble() %>%
  clean_names() %>%
  group_by(block_id, census_year, seating_type) %>%
  summarise(number_of_seats = sum(number_of_seats)) %>%
  as_tibble() %>%
  mutate(seating_type = str_to_lower(seating_type, locale = "en"),
         seating_type = gsub(" - ", "_", seating_type),
         seating_type = paste("cafe_restaurant", seating_type, sep = "_"),
         join = paste(block_id, census_year, sep = "_" )) %>%
  pivot_wider(names_from = seating_type, values_from = number_of_seats)

# Bar and Pub Capacity https://data.melbourne.vic.gov.au/Business/Bars-and-pubs-with-patron-capacity/mffi-m9yn
clue_data <- get("raw_Bars_and_pubs__with_patron_capacity") %>%
  as_tibble() %>%
  clean_names() %>%
  group_by(block_id, census_year) %>%
  summarise(bar_pub_patron_limit = sum(number_of_patrons)) %>%
  as_tibble() %>%
  mutate(join = paste(block_id, census_year, sep = "_" )) %>%
  select(join, bar_pub_patron_limit) %>%
  full_join(clue_data, by = "join")

# Residential Dwellings https://data.melbourne.vic.gov.au/Property/Residential-dwellings/44kh-ty54
clue_data <- get("raw_Residential_dwellings") %>%
  as_tibble() %>%
  clean_names() %>%
  group_by(block_id, census_year, dwelling_type) %>%
  summarise(dwelling_number = sum(dwelling_number)) %>%
  as_tibble() %>%
  mutate(dwelling_type = str_to_lower(dwelling_type, locale = "en"),
         dwelling_type = gsub(" ", "_", dwelling_type),
         dwelling_type = gsub("/", "_", dwelling_type),
         dwelling_type = paste("dwelling_", dwelling_type, sep = ""),
         join = paste(block_id, census_year, sep = "_" )) %>%
  pivot_wider(names_from = dwelling_type, values_from = dwelling_number) %>%
  select(join, dwelling_residential_apartments, dwelling_house_townhouse, dwelling_student_apartments) %>%
  full_join(clue_data, by = "join")

# Off Street Parking https://data.melbourne.vic.gov.au/Transport/Off-street-car-parks-with-capacity-and-type/krh5-hhjn
clue_data <- get("raw_Off-street_car_parks_with_capacity_and_type") %>%
  as_tibble() %>%
  clean_names() %>%
  group_by(block_id, census_year, parking_type) %>%
  summarise(parking_spaces = sum(parking_spaces)) %>%
  as_tibble() %>%
  mutate(parking_type = str_to_lower(parking_type, locale = "en"),
         parking_type = gsub(" ", "_", parking_type),
         parking_type = gsub("/", "_", parking_type),
         parking_type = paste("parking_n", parking_type, sep = "_"),
         join = paste(block_id, census_year, sep = "_" )) %>%
  pivot_wider(names_from = parking_type, values_from = parking_spaces) %>%
  select(join, parking_n_commercial, parking_n_private, parking_n_residential) %>%
  full_join(clue_data, by = "join")

# Business Establishments https://data.melbourne.vic.gov.au/Business/Business-establishments-per-block-by-ANZSIC/jsun-ttri
clue_data <- get("raw_Business_establishments_per_block_by_ANZSIC") %>%
  as_tibble() %>%
  clean_names() %>%
  rename_at(.vars = vars(ends_with("_services")),
            .funs = funs(sub("_services", "", .))) %>%
  rename_all(function(x) paste0("services_", x)) %>%
  mutate(join = paste(services_block_id, services_census_year, sep = "_" )) %>%
  select(-services_block_id, -services_census_year, -services_total_establishments_in_block, -services_clue_small_area) %>%
  full_join(clue_data, by = "join")

# Floor Space https://data.melbourne.vic.gov.au/Business/Floor-space-by-use-by-block/234q-gg83
clue_data <- get("raw_Floor_space_by_use_by_block") %>%
  as_tibble() %>%
  clean_names() %>%
  rename_all(function(x) paste0("m2_", x)) %>%
  mutate(join = paste(m2_block_id, m2_census_year, sep = "_" )) %>%
  select(-m2_census_year, -m2_block_id, -m2_clue_small_area, -m2_total_space_in_block) %>%
  full_join(clue_data, by = "join") %>%
  replace(is.na(.), 0) %>%
  arrange(., block_id, census_year) %>%
  select(block_id, census_year, order(colnames(.)), -join) %>%
  filter(block_id != 0) %>%
  mutate(block_year = paste0(block_id,"_", census_year))

saveRDS(clue_data, file = "../outputs/clue_data.RData")

write.csv(clue_data, file = "../clue_blocks.csv")

# Development Applications https://data.melbourne.vic.gov.au/Property/Development-Activity-Monitor/gh7s-qda8
development_applications <- raw_com_dev_app %>%
  clean_names() %>%
  transmute(block_year = paste0(clue_block,"_", year_completed),
            development_key,
            year_completed, clue_block, street_address,
            resi_dwellings, studio_dwe, one_bdrm_dwe, two_bdrm_dwe, three_bdrm_dwe,
            student_apartments, student_beds, student_accommodation_units,
            car_spaces, bike_spaces,
            longitude, latitude) %>%
  na.omit(year_completed) %>%
  inner_join(clue_data, by = "block_year")

write.csv(development_applications, file = "../development_applications.csv")

sf1 <- development_applications %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(city_of_melbourne)) %>%
  select(development_key)

st_write(sf1, "../development_applications.shp", driver="ESRI Shapefile", delete_layer =TRUE)
