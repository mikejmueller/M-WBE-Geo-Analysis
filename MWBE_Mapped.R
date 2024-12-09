library (ggplot2)
library(tidyverse)
library(janitor)

#Cleans MWBE Data
mwbe_clean <- read.csv("~/Desktop/Data Visualization/Final Assignment/MWBE_Map_Data Sources/Original Data/SBS_Full_MWBE_Directory.csv") %>% 
  clean_names()

mwbe_clean_locations <- mwbe_clean %>%
  mutate(vendor_formal_name = 
           ifelse(vendor_dba != "", vendor_dba, vendor_formal_name)) %>% 
  rename(vendor_name = vendor_formal_name) %>% 
  select(vendor_name,
         address_line_1, city, state, zip,
         ethnicity, certification, 
         naics_sector, naics_subsector, largest_value_of_contract
         ) %>% 
  filter(address_line_1 != "" & !is.na(address_line_1))

#Separates MWBEs by Borough, required to geo-code addresses under 2,500 daily geo-code limits.
mwbe_clean_locations_nys <- mwbe_clean_locations %>%
  filter(state == "New York")
mwbe_clean_locations_nyc <- mwbe_clean_locations %>%
  distinct() %>% 
  filter(city %in% c("New York","Brooklyn", "Queens", "Bronx", "Staten Island"))
mwbe_clean_locations_brooklyn <- mwbe_clean_locations %>%
  filter(city == "Brooklyn")
mwbe_clean_locations_manhattan <- mwbe_clean_locations %>%
  filter(city == "New York")
mwbe_clean_locations_bronx <- mwbe_clean_locations %>%
  filter(city == "Bronx")
mwbe_clean_locations_queens <- mwbe_clean_locations %>%
  filter(city == "Queens")
mwbe_clean_locations_staten_island <- mwbe_clean_locations %>%
  filter(city == "Staten Island")

#Writes csvs to be geo-coded
write.csv(mwbe_clean_locations_brooklyn, "~/Desktop/Data Visualization/Final Assignment/MWBE_MAP_Data Sources/Clean_MWBE_Locations/Clean_MWBE_Locations_Brooklyn.csv")
write.csv(mwbe_clean_locations_manhattan, "~/Desktop/Data Visualization/Final Assignment/MWBE_MAP_Data Sources/Clean_MWBE_Locations/Clean_MWBE_Locations_Manhattan.csv")
write.csv(mwbe_clean_locations_bronx, "~/Desktop/Data Visualization/Final Assignment/MWBE_MAP_Data Sources/Clean_MWBE_Locations/Clean_MWBE_Locations_Bronx.csv")
write.csv(mwbe_clean_locations_queens, "~/Desktop/Data Visualization/Final Assignment/MWBE_MAP_Data Sources/Clean_MWBE_Locations/Clean_MWBE_Locations_Queens.csv")
write.csv(mwbe_clean_locations_staten_island, "~/Desktop/Data Visualization/Final Assignment/MWBE_MAP_Data Sources/Clean_MWBE_Locations/Clean_MWBE_Locations_Staten_Island.csv")

#Inputs and cleans geo-coded csvs
mwbe_geocode_manhattan <- read_csv("~/Desktop/Data Visualization/Final Assignment/MWBE_Map_Data Sources/Geocoded_MWBE/Geocoded_MWBE_Manhattan.csv") %>% 
  clean_names() %>% 
  select(vendor_name, ethnicity, city, latitude, longitude)
mwbe_geocode_bronx <- read_csv("~/Desktop/Data Visualization/Final Assignment/MWBE_Map_Data Sources/Geocoded_MWBE/Geocoded_MWBE_Bronx.csv") %>% 
  clean_names() %>% 
  select(vendor_name, city, latitude, longitude)
mwbe_geocode_brooklyn <- read_csv("~/Desktop/Data Visualization/Final Assignment/MWBE_Map_Data Sources/Geocoded_MWBE/Geocoded_MWBE_Brooklyn.csv") %>% 
  clean_names() %>% 
  select(vendor_name, city, latitude, longitude) 
mwbe_geocode_queens <- read_csv("~/Desktop/Data Visualization/Final Assignment/MWBE_Map_Data Sources/Geocoded_MWBE/Geocoded_MWBE_Queens.csv") %>% 
  clean_names() %>% 
  select(vendor_name, city, latitude, longitude)
mwbe_geocode_staten_island <- read_csv("~/Desktop/Data Visualization/Final Assignment/MWBE_Map_Data Sources/Geocoded_MWBE/Geocoded_MWBE_Staten_Island.csv") %>% 
  clean_names() %>% 
  select(vendor_name, city, latitude, longitude)

#Joins geo-coded csvs with original demographic data
mwbe_map_brooklyn <- mwbe_geocode_brooklyn %>% 
  clean_names() %>% 
  distinct(vendor_name, .keep_all = TRUE) %>% 
  select(vendor_name, city, latitude, longitude) %>%
  left_join(mwbe_clean_locations_brooklyn, by = "vendor_name") %>% 
  select(!city.y) %>%
  select(!state) %>% 
  rename(city = city.x)
mwbe_map_queens <- mwbe_geocode_queens %>% 
  clean_names() %>% 
  distinct(vendor_name, .keep_all = TRUE) %>% 
  select(vendor_name, city, latitude, longitude) %>%
  left_join(mwbe_clean_locations_queens, by = "vendor_name") %>% 
  select(!city.y) %>%
  select(!state) %>% 
  rename(city = city.x)
mwbe_map_staten_island <- mwbe_geocode_staten_island %>% 
  clean_names() %>% 
  distinct(vendor_name, .keep_all = TRUE) %>% 
  select(vendor_name, city, latitude, longitude) %>%
  left_join(mwbe_clean_locations_staten_island, by = "vendor_name") %>% 
  select(!city.y) %>%
  select(!state) %>% 
  rename(city = city.x)
mwbe_map_manhattan <- mwbe_geocode_manhattan %>%
  clean_names() %>% 
  distinct(vendor_name, .keep_all = TRUE) %>% 
  select(vendor_name, city, latitude, longitude, ethnicity) %>%
  left_join(mwbe_clean_locations_brooklyn, by = "vendor_name") %>% 
  select(!city.y) %>%
  select(!state) %>% 
  rename(city = city.x)
mwbe_map_bronx <- mwbe_geocode_bronx %>% 
  clean_names() %>% 
  distinct(vendor_name, .keep_all = TRUE) %>% 
  select(vendor_name, city, latitude, longitude) %>%
  left_join(mwbe_clean_locations_bronx, by = "vendor_name") %>% 
  select(!city.y) %>%
  select(!state) %>% 
  rename(city = city.x)

#Writes csvs to be mapped
write.csv(mwbe_map_brooklyn, "~/Desktop/Data Visualization/Final Assignment/MWBE_Map_Data Sources/MWBE_Map_Locations/MWBE_Map_Locations_Brooklyn.csv")
write.csv(mwbe_map_manhattan, "~/Desktop/Data Visualization/Final Assignment/MWBE_Map_Data Sources/MWBE_Map_Locations/MWBE_Map_Locations_Manhattan.csv")
write.csv(mwbe_map_bronx, "~/Desktop/Data Visualization/Final Assignment/MWBE_Map_Data Sources/MWBE_Map_Locations/MWBE_Map_Locations_Bronx.csv")
write.csv(mwbe_map_queens, "~/Desktop/Data Visualization/Final Assignment/MWBE_Map_Data Sources/MWBE_Map_Locations/MWBE_Map_Locations_Queens.csv")
write.csv(mwbe_map_staten_island, "~/Desktop/Data Visualization/Final Assignment/MWBE_Map_Data Sources/MWBE_Map_Locations/MWBE_Map_Locations_Staten_Island.csv")

    
#Data Analysis: Boroughs by Ethnicity
#Counts Borough M/WBEs by ethnicity
count_brooklyn <- mwbe_map_brooklyn %>%
  distinct() %>% 
  group_by(ethnicity) %>% 
  summarise( 
    Asian_Pacific_Count = sum (ethnicity == "Asian-Pacific", na.rm = T),
    Asian_India_Count = sum (ethnicity == "Asian-Indian", na.rm = T),
    White_Count = sum (ethnicity == "Non-Minority", na.rm = T),
    Black_Count = sum (ethnicity == "Black", na.rm = T),
    Hispanic_Count = sum (ethnicity == "Hispanic", na.rm = T))
count_bronx <- mwbe_map_bronx %>%
  distinct() %>% 
  group_by(ethnicity) %>% 
  summarise( 
    Asian_Pacific_Count = sum (ethnicity == "Asian-Pacific", na.rm = T),
    Asian_India_Count = sum (ethnicity == "Asian-Indian", na.rm = T),
    White_Count = sum (ethnicity == "Non-Minority", na.rm = T),
    Black_Count = sum (ethnicity == "Black", na.rm = T),
    Hispanic_Count = sum (ethnicity == "Hispanic", na.rm = T))
count_manhattan <- mwbe_map_manhattan %>%
  distinct() %>% 
  rename("ethnicity" = "ethnicity.x") %>% 
  group_by(ethnicity) %>% 
  summarise( 
    Asian_Pacific_Count = sum (ethnicity == "Asian-Pacific", na.rm = T),
    Asian_India_Count = sum (ethnicity == "Asian-Indian", na.rm = T),
    White_Count = sum (ethnicity == "Non-Minority", na.rm = T),
    Black_Count = sum (ethnicity == "Black", na.rm = T),
    Hispanic_Count = sum (ethnicity == "Hispanic", na.rm = T))
count_staten_island <- mwbe_map_staten_island %>%
  distinct() %>% 
  group_by(ethnicity) %>% 
  summarise( 
    Asian_Pacific_Count = sum (ethnicity == "Asian-Pacific", na.rm = T),
    Asian_India_Count = sum (ethnicity == "Asian-Indian", na.rm = T),
    White_Count = sum (ethnicity == "Non-Minority", na.rm = T),
    Black_Count = sum (ethnicity == "Black", na.rm = T),
    Hispanic_Count = sum (ethnicity == "Hispanic", na.rm = T))
count_queens <- mwbe_map_queens %>%
  distinct() %>% 
  group_by(ethnicity) %>% 
  summarise( 
    Asian_Pacific_Count = sum (ethnicity == "Asian-Pacific", na.rm = T),
    Asian_India_Count = sum (ethnicity == "Asian-Indian", na.rm = T),
    White_Count = sum (ethnicity == "Non-Minority", na.rm = T),
    Black_Count = sum (ethnicity == "Black", na.rm = T),
    Hispanic_Count = sum (ethnicity == "Hispanic", na.rm = T))
    

#Data Analysis: Boroughs by Ethnicity 2
#Counts Borough M/WBEs by ethnicity (a cleaner format)
count_queens <- mwbe_map_queens %>%
  distinct() %>%
  group_by(ethnicity) %>% 
  summarise( 
    Count = n()
  ) %>%
  filter(ethnicity %in% c("Asian-Pacific", "Asian-Indian", "Non-Minority", "Black", "Hispanic")) %>% 
  mutate(Region = "Queens")
count_bronx <- mwbe_map_bronx %>%
  distinct() %>%
  group_by(ethnicity) %>% 
  summarise( 
    Count = n()
  ) %>%
  filter(ethnicity %in% c("Asian-Pacific", "Asian-Indian", "Non-Minority", "Black", "Hispanic")) %>% 
  mutate(Region = "Bronx")
count_manhattan <- mwbe_map_manhattan %>%
  distinct() %>%
  rename(ethnicity = ethnicity.x) %>% 
  group_by(ethnicity) %>% 
  summarise( 
    Count = n()
  ) %>%
  filter(ethnicity %in% c("Asian-Pacific", "Asian-Indian", "Non-Minority", "Black", "Hispanic")) %>% 
  mutate(Region = "Manhattan")
count_brooklyn <- mwbe_map_brooklyn%>%
  distinct() %>%
  group_by(ethnicity) %>% 
  summarise( 
    Count = n()
  ) %>%
  filter(ethnicity %in% c("Asian-Pacific", "Asian-Indian", "Non-Minority", "Black", "Hispanic")) %>% 
  mutate(Region = "Brooklyn")
count_staten_island <- mwbe_map_staten_island %>%
  distinct() %>%
  group_by(ethnicity) %>% 
  summarise( 
    Count = n()
  ) %>%
  filter(ethnicity %in% c("Asian-Pacific", "Asian-Indian", "Non-Minority", "Black", "Hispanic")) %>% 
  mutate(Region = "Staten Island")
#Combines above summaries to a single chart.
combined_counts <- bind_rows(count_queens, count_bronx, count_manhattan, count_staten_island, count_brooklyn) %>% 
  pivot_wider(names_from = Region, values_from = Count)
#Writes CSV for analysis
write.csv(combined_counts, "~/Desktop/Data Visualization/Final Assignment/MWBE_Graph_Data_Sources/MWBE_Count.csv")


#Data Analysis: Number of Black M/WBEs in predominantly Black Neighborhoods.
#Counts Black M/WBEs in predominantly Black Brooklyn neighborhoods. Neighborhoods/PUMAs are identified in QGIS.
PUMA_MWBE_BK <- read.csv("~/Desktop/Data Visualization/Final Assignment/MWBE_Graph_Data_Sources/PUMA_MWBE_BK.csv") %>% 
  clean_names() %>% 
  distinct() %>% 
  filter(ethnicity == "Black") %>% 
  filter(puma %in% c("4009", "4008", "4007", "4010", "4011", "4003", "4006")) %>%
  group_by(puma) %>% 
  summarise(vendor_count = n())

#Data Analysis: Pie chart of all M/WBE ethnicities.
#Counts the number of rows per ethnicity
mwbe_clean_locations_nyc_per_race <- mwbe_clean_locations %>% 
  group_by(ethnicity) %>% 
  summarise(count = n(), .groups = "drop")

  

