
# load libraries ----------------------------------------------------------

library(arrow)
library(dplyr)
library(data.table)
library(tidyverse)


# load data ---------------------------------------------------------------

sd_data <- open_dataset("Data_and_Scripts/Data/output/sd_file_arrow")
glimpse(sd_data)

ia_data <- open_dataset("Data_and_Scripts/Data/output/ia_file_arrow")
glimpse(ia_data)

mn_data <- open_dataset("Data_and_Scripts/Data/output/mn_file_arrow")
glimpse(mn_data)

wi_data <- open_dataset("Data_and_Scripts/Data/output/wi_file_arrow")
glimpse(wi_data)

mi_data <- open_dataset("Data_and_Scripts/Data/output/mi_file_arrow")
glimpse(mi_data)

il_data <- open_dataset("Data_and_Scripts/Data/output/il_file_arrow")
glimpse(il_data)

in_data <- open_dataset("Data_and_Scripts/Data/output/in_file_arrow")
glimpse(in_data)


in_data %>% count() %>% collect() %>% {. ->> a}
il_data %>% count() %>% collect() %>% {. ->> a[2,]}
ia_data %>% count() %>% collect() %>% {. ->> a[3,]}
mn_data %>% count() %>% collect() %>% {. ->> a[4,]}
sd_data %>% count() %>% collect() %>% {. ->> a[5,]}
wi_data %>% count() %>% collect() %>% {. ->> a[6,]}
mi_data %>% count() %>% collect() %>% {. ->> a[7,]}

colSums(a)
# number of fish sampled by gear within state -----------------------------------------



sd_data %>% 
  group_by(state, sampling_method) %>%
  filter(!is.na(sampling_method)) %>% 
  summarise(
    n_effort_idents = n_distinct(total_effort_ident),
    n_lakes = n_distinct(paste(lake_name,county)),
    n_walleye = sum(species_1 == "walleye", na.rm = T),
    n_yellow_perch = sum(species_1 == "yellow_perch", na.rm = T),
    n_black_crappie = sum(species_1 == "black_crappie", na.rm = T),
    n_bluegill = sum(species_1 == "bluegill", na.rm = T),
    n_smallmouth_bass = sum(species_1 == "smallmouth_bass", na.rm = T),
    n_largemouth_bass = sum(species_1 == "largemouth_bass", na.rm = T),
    n_northern_pike = sum(species_1 == "northern_pike", na.rm = T),
    n_cisco = sum(species_1 == "cisco", na.rm = T)
  ) %>%
  collect() %>% 
  arrange(-n_effort_idents) %>% 
  {. ->> sd_sample_sz}


# iowa 

  ia_data %>% group_by(species_1) %>% count() %>% collect() %>% print(n = nrow(.))


ia_data %>% 
  group_by(state, sampling_method) %>%
  summarise(
    n_effort_idents = n_distinct(total_effort_ident),
    n_lakes = n_distinct(paste(lake_name,county)),
    n_walleye = sum(species_1 == "walleye", na.rm = T),
    n_yellow_perch = sum(species_1 == "yellow_perch", na.rm = T),
    n_black_crappie = sum(species_1 == "black_crappie", na.rm = T),
    n_bluegill = sum(species_1 == "bluegill", na.rm = T),
    n_smallmouth_bass = sum(species_1 == "smallmouth_bass", na.rm = T),
    n_largemouth_bass = sum(species_1 == "largemouth_bass", na.rm = T),
    n_northern_pike = sum(species_1 == "northern_pike", na.rm = T),
    n_cisco = sum(species_1 == "cisco", na.rm = T)
  ) %>%
  collect() %>% 
  arrange(-n_effort_idents) %>% 
  {. ->> ia_sample_sz}

#minnesota

mn_data %>% group_by(species_1) %>% count() %>% collect() %>% print(n = nrow(.))

mn_data %>% 
  group_by(state, sampling_method, sampling_method_2) %>%
  summarise(
    n_effort_idents = n_distinct(total_effort_ident),
    n_lakes = n_distinct(paste(lake_name,county)),
    n_walleye = sum(species_1 == "walleye", na.rm = T),
    n_yellow_perch = sum(species_1 == "yellow_perch", na.rm = T),
    n_black_crappie = sum(species_1 == "black_crappie", na.rm = T),
    n_bluegill = sum(species_1 == "bluegill", na.rm = T),
    n_smallmouth_bass = sum(species_1 == "smallmouth_bass", na.rm = T),
    n_largemouth_bass = sum(species_1 == "largemouth_bass", na.rm = T),
    n_northern_pike = sum(species_1 == "northern_pike", na.rm = T),
    n_cisco = sum(species_1 == "cisco", na.rm = T)
  ) %>%
  collect() %>%
  mutate(sampling_method_2 = case_when( is.na(sampling_method_2) ~ sampling_method,
                                        .default = sampling_method_2)) %>%
  arrange(-n_effort_idents) %>% 
  ungroup() %>% 
  select(!sampling_method) %>% 
  rename(sampling_method = sampling_method_2) %>% 
  {. ->> mn_sample_sz}

#wisconsin

wi_data %>% group_by(species) %>% count() %>% collect() %>% print(n = nrow(.))

wi_data %>% 
  filter(!is.na(sampling_method)) %>% 
  group_by(state, sampling_method) %>%
  summarise(
    n_effort_idents = n_distinct(total_effort_ident),
    n_lakes = n_distinct(paste(lake_name,county)),
    n_walleye = sum(species == "walleye", na.rm = T),
    n_yellow_perch = sum(species == "yellow_perch", na.rm = T),
    n_black_crappie = sum(species == "black_crappie", na.rm = T),
    n_bluegill = sum(species == "bluegill", na.rm = T),
    n_smallmouth_bass = sum(species == "smallmouth_bass", na.rm = T),
    n_largemouth_bass = sum(species == "largemouth_bass", na.rm = T),
    n_northern_pike = sum(species == "northern_pike", na.rm = T),
    n_cisco = sum(species %in% c("ciscoes_&_whitefishes", "cisco/lake_herring", "blackfin_cisco", "longjaw_cisco"), na.rm = T)
  ) %>%
  collect() %>% 
  arrange(-n_effort_idents) %>% 
  {. ->> wi_sample_sz}


#michigan
mi_data %>% group_by(species.1) %>% count() %>% collect() %>% print(n = nrow(.))

mi_data %>% 
  mutate(state = "Michigan") %>% 
  group_by(state, sampling_method) %>%
  summarise(
    n_effort_idents = n_distinct(effort_ident),
    n_lakes = n_distinct(paste(lake_name.1,county)),
    n_walleye = sum(species.1 == "walleye", na.rm = T),
    n_yellow_perch = sum(species.1 == "yellow_perch", na.rm = T),
    n_black_crappie = sum(species.1 == "black_crappie", na.rm = T),
    n_bluegill = sum(species.1 == "bluegill", na.rm = T),
    n_smallmouth_bass = sum(species.1 == "smallmouth_bass", na.rm = T),
    n_largemouth_bass = sum(species.1 == "largemouth_bass", na.rm = T),
    n_northern_pike = sum(species.1 == "northern_pike", na.rm = T),
    n_cisco = sum(species.1 %in% c("cisco"), na.rm = T)
  ) %>%
  collect() %>% 
  arrange(-n_effort_idents) %>% 
  {. ->> mi_sample_sz}



#illinois
il_data %>% 
  mutate(state = "Illinois") %>% 
  group_by(state, sampling_method) %>%
  summarise(
    n_effort_idents = n_distinct(total_effort_ident),
    n_lakes = n_distinct(paste(lake_name,county)),
    n_walleye = sum(species_1 == "walleye", na.rm = T),
    n_yellow_perch = sum(species_1 == "yellow_perch", na.rm = T),
    n_black_crappie = sum(species_1 == "black_crappie", na.rm = T),
    n_bluegill = sum(species_1 == "bluegill", na.rm = T),
    n_smallmouth_bass = sum(species_1 == "smallmouth_bass", na.rm = T),
    n_largemouth_bass = sum(species_1 == "largemouth_bass", na.rm = T),
    n_northern_pike = sum(species_1 == "northern_pike", na.rm = T),
    n_cisco = sum(species_1 == "cisco", na.rm = T)
  ) %>%
  collect() %>% 
  arrange(-n_effort_idents) %>% 
  {. ->> il_sample_sz}


#indiana
in_data %>% group_by(species_1) %>% count() %>% collect() %>% print(n = nrow(.))

in_data %>% 
  filter(sampling_method != "unknown sampling method") %>% 
  group_by(state, sampling_method, sampling_method_2) %>%
  summarise(
    n_effort_idents = n_distinct(total_effort_ident),
    n_lakes = n_distinct(paste(lake_name,county)),
    n_walleye = sum(species_1 == "walleye", na.rm = T),
    n_yellow_perch = sum(species_1 == "yellow_perch", na.rm = T),
    n_black_crappie = sum(species_1 == "black_crappie", na.rm = T),
    n_bluegill = sum(species_1 == "bluegill", na.rm = T),
    n_smallmouth_bass = sum(species_1 == "smallmouth_bass", na.rm = T),
    n_largemouth_bass = sum(species_1 == "largemouth_bass", na.rm = T),
    n_northern_pike = sum(species_1 == "northern_pike", na.rm = T),
    n_cisco = sum(species_1 == "cisco", na.rm = T)
  ) %>%
  collect() %>%
  mutate(sampling_method_2 = paste(sampling_method, sampling_method_2, sep = ": ")) %>%
  arrange(-n_effort_idents) %>%
  ungroup() %>%
  select(!sampling_method) %>%
  {. ->> in_sample_sz}

state_gear_sample_sizes <- rbindlist(list(ia_sample_sz,
                                          il_sample_sz,
                                          in_sample_sz,
                                          mi_sample_sz,
                                          mn_sample_sz,
                                          sd_sample_sz,wi_sample_sz), fill = TRUE)
# fwrite(state_gear_sample_sizes, file = "Data_and_Scripts/Data/output/all_state_gear_ns.csv")



# number of surveys per species -------------------------------------------


sd_data %>% 
  group_by(state, sampling_method) %>%
  # mutate(total_effort_ident = paste(lake_name_1, lake_id, nhdhr_id, date_1, sampling_method, total_effort_1, total_effort_1_units)) %>% 
  summarise(
    n_effort_idents = n_distinct(total_effort_ident),
    n_lakes = n_distinct(paste(lake_name, lake_id, nhdhr_id)),
    n_present_effort_walleye =     n_distinct(ifelse(species_1 == "walleye", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_yellow_perch = n_distinct(ifelse(species_1 == "yellow_perch", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_black_crappie = n_distinct(ifelse(species_1 == "black_crappie", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_bluegill = n_distinct(ifelse(species_1 == "bluegill", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_smallmouth_bass = n_distinct(ifelse(species_1 == "smallmouth_bass", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_largemouth_bass = n_distinct(ifelse(species_1 == "largemouth_bass", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_northern_pike = n_distinct(ifelse(species_1 == "northern_pike", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_cisco = n_distinct(ifelse(species_1 == "cisco", total_effort_ident, NA), na.rm = TRUE),
  ) %>%
  collect() %>% 
  arrange(state, -n_effort_idents) %>% 
  {. ->> sd_data_surveycounts}



# iowa 

ia_data %>% 
  group_by(state, sampling_method) %>%
  # mutate(total_effort_ident = paste(lake_name_1, lake_id, nhdhr_id, date_1, sampling_method, total_effort_1, total_effort_1_units)) %>% 
  summarise(
    n_effort_idents = n_distinct(total_effort_ident),
    n_lakes = n_distinct(paste(lake_name, county)),
    n_present_effort_walleye =     n_distinct(ifelse(species_1 == "walleye", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_yellow_perch = n_distinct(ifelse(species_1 == "yellow_perch", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_black_crappie = n_distinct(ifelse(species_1 == "black_crappie", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_bluegill = n_distinct(ifelse(species_1 == "bluegill", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_smallmouth_bass = n_distinct(ifelse(species_1 == "smallmouth_bass", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_largemouth_bass = n_distinct(ifelse(species_1 == "largemouth_bass", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_northern_pike = n_distinct(ifelse(species_1 == "northern_pike", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_cisco = n_distinct(ifelse(species_1 == "cisco", total_effort_ident, NA), na.rm = TRUE),
  ) %>%
  collect() %>% 
  arrange(state, -n_effort_idents) %>% 
  {. ->> ia_data_surveycounts}

#minnesota

mn_data %>% 
  group_by(state, sampling_method) %>%
  # mutate(total_effort_ident = paste(lake_name_1, lake_id, nhdhr_id, date_1, sampling_method, total_effort_1, total_effort_1_units)) %>% 
  summarise(
    n_effort_idents = n_distinct(total_effort_ident),
    n_lakes = n_distinct(paste(lake_name, lake_id, nhdhr_id)),
    n_present_effort_walleye =     n_distinct(ifelse(species_1 == "walleye", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_yellow_perch = n_distinct(ifelse(species_1 == "yellow_perch", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_black_crappie = n_distinct(ifelse(species_1 == "black_crappie", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_bluegill = n_distinct(ifelse(species_1 == "bluegill", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_smallmouth_bass = n_distinct(ifelse(species_1 == "smallmouth_bass", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_largemouth_bass = n_distinct(ifelse(species_1 == "largemouth_bass", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_northern_pike = n_distinct(ifelse(species_1 == "northern_pike", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_cisco = n_distinct(ifelse(species_1 == "cisco", total_effort_ident, NA), na.rm = TRUE),
  ) %>%
  collect() %>% 
  arrange(state, -n_effort_idents) %>% 
  {. ->> mn_data_surveycounts}



#wisconsin

wi_data %>% 
  group_by(state, sampling_method) %>%
  # mutate(total_effort_ident = paste(lake_name_1, lake_id, nhdhr_id, date_1, sampling_method, total_effort_1, total_effort_1_units)) %>% 
  summarise(
    n_effort_idents = n_distinct(total_effort_ident),
    n_lakes = n_distinct(paste(lake_name, county)),
    n_present_effort_walleye =     n_distinct(ifelse(species_1 == "walleye", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_yellow_perch = n_distinct(ifelse(species_1 == "yellow_perch", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_black_crappie = n_distinct(ifelse(species_1 == "black_crappie", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_bluegill = n_distinct(ifelse(species_1 == "bluegill", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_smallmouth_bass = n_distinct(ifelse(species_1 == "smallmouth_bass", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_largemouth_bass = n_distinct(ifelse(species_1 == "largemouth_bass", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_northern_pike = n_distinct(ifelse(species_1 == "northern_pike", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_cisco = n_distinct(ifelse(species_1 == "cisco", total_effort_ident, NA), na.rm = TRUE),
  ) %>%
  collect() %>% 
  arrange(state, -n_effort_idents) %>% 
  {. ->> wi_data_surveycounts}


#michigan
mi_data %>% 
  group_by(state, sampling_method) %>%
  # mutate(total_effort_ident = paste(lake_name_1, lake_id, nhdhr_id, date_1, sampling_method, total_effort_1, total_effort_1_units)) %>% 
  summarise(
    n_effort_idents = n_distinct(effort_ident),
    n_lakes = n_distinct(paste(lake_name.1, county)),
    n_present_effort_walleye =     n_distinct(ifelse(species_1 == "walleye", effort_ident, NA), na.rm = TRUE),
    n_present_effort_yellow_perch = n_distinct(ifelse(species_1 == "yellow_perch", effort_ident, NA), na.rm = TRUE),
    n_present_effort_black_crappie = n_distinct(ifelse(species_1 == "black_crappie", effort_ident, NA), na.rm = TRUE),
    n_present_effort_bluegill = n_distinct(ifelse(species_1 == "bluegill", effort_ident, NA), na.rm = TRUE),
    n_present_effort_smallmouth_bass = n_distinct(ifelse(species_1 == "smallmouth_bass", effort_ident, NA), na.rm = TRUE),
    n_present_effort_largemouth_bass = n_distinct(ifelse(species_1 == "largemouth_bass", effort_ident, NA), na.rm = TRUE),
    n_present_effort_northern_pike = n_distinct(ifelse(species_1 == "northern_pike", effort_ident, NA), na.rm = TRUE),
    n_present_effort_cisco = n_distinct(ifelse(species_1 == "cisco", effort_ident, NA), na.rm = TRUE),
  ) %>%
  collect() %>% 
  arrange(state, -n_effort_idents) %>% 
  {. ->> mi_data_surveycounts}



#illinois
il_data %>% 
  group_by(state, sampling_method) %>%
  # mutate(total_effort_ident = paste(lake_name_1, lake_id, nhdhr_id, date_1, sampling_method, total_effort_1, total_effort_1_units)) %>% 
  summarise(
    n_effort_idents = n_distinct(total_effort_ident),
    n_lakes = n_distinct(paste(lake_name, county)),
    n_present_effort_walleye =     n_distinct(ifelse(species_1 == "walleye", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_yellow_perch = n_distinct(ifelse(species_1 == "yellow_perch", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_black_crappie = n_distinct(ifelse(species_1 == "black_crappie", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_bluegill = n_distinct(ifelse(species_1 == "bluegill", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_smallmouth_bass = n_distinct(ifelse(species_1 == "smallmouth_bass", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_largemouth_bass = n_distinct(ifelse(species_1 == "largemouth_bass", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_northern_pike = n_distinct(ifelse(species_1 == "northern_pike", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_cisco = n_distinct(ifelse(species_1 == "cisco", total_effort_ident, NA), na.rm = TRUE),
  ) %>%
  collect() %>% 
  arrange(state, -n_effort_idents) %>% 
  {. ->> il_data_surveycounts}


#indiana
wi_data %>% 
  group_by(state, sampling_method, samplin) %>%
  # mutate(total_effort_ident = paste(lake_name_1, lake_id, nhdhr_id, date_1, sampling_method, total_effort_1, total_effort_1_units)) %>% 
  summarise(
    n_effort_idents = n_distinct(total_effort_ident),
    n_lakes = n_distinct(paste(lake_name, county)),
    n_present_effort_walleye =     n_distinct(ifelse(species_1 == "walleye", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_yellow_perch = n_distinct(ifelse(species_1 == "yellow_perch", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_black_crappie = n_distinct(ifelse(species_1 == "black_crappie", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_bluegill = n_distinct(ifelse(species_1 == "bluegill", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_smallmouth_bass = n_distinct(ifelse(species_1 == "smallmouth_bass", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_largemouth_bass = n_distinct(ifelse(species_1 == "largemouth_bass", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_northern_pike = n_distinct(ifelse(species_1 == "northern_pike", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_cisco = n_distinct(ifelse(species_1 == "cisco", total_effort_ident, NA), na.rm = TRUE),
  ) %>%
  collect() %>% 
  mutate(sampling_method_2 = paste(sampling_method, sampling_method_2, sep = ": ")) %>%
  select(!sampling_method) %>%
  arrange(state, -n_effort_idents) %>% 
  {. ->> in_data_surveycounts}

state_gear_surveys <- rbindlist(list(ia_data_surveycounts,
                                     il_data_surveycounts,
                                     in_data_surveycounts,
                                     mi_data_surveycounts,
                                     mn_data_surveycounts,
                                     sd_data_surveycounts,
                                     wi_data_surveycounts), fill = TRUE)
# fwrite(state_gear_sample_sizes, file = "Data_and_Scripts/Data/output/all_state_gear_ns.csv")



# cpue data review --------------------------------------------------------
 
cpue_data <- fread("Data_and_Scripts/Data/input/all_state_cpue_6Feb24.csv")

cpue_data %>% 
  group_by(state, sampling_method) %>%
  # mutate(total_effort_ident = paste(lake_name_1, lake_id, nhdhr_id, date_1, sampling_method, total_effort_1, total_effort_1_units)) %>% 
  summarise(
    n_effort_idents = n_distinct(total_effort_ident),
    n_lakes = n_distinct(paste(lake_name, lake_id, nhdhr_id)),
    
    n_sampled_effort_walleye =     n_distinct(ifelse(species_1 == "walleye", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_walleye =     n_distinct(ifelse(species_1 == "walleye" & count > 0, total_effort_ident, NA), na.rm = TRUE),
    
    n_sampled_effort_yellow_perch = n_distinct(ifelse(species_1 == "yellow_perch", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_yellow_perch = n_distinct(ifelse(species_1 == "yellow_perch" & count > 0, total_effort_ident, NA), na.rm = TRUE),
    
    n_sampled_effort_black_crappie = n_distinct(ifelse(species_1 == "black_crappie", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_black_crappie = n_distinct(ifelse(species_1 == "black_crappie" & count > 0, total_effort_ident, NA), na.rm = TRUE),
    
    n_sampled_effort_bluegill = n_distinct(ifelse(species_1 == "bluegill", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_bluegill = n_distinct(ifelse(species_1 == "bluegill" & count > 0, total_effort_ident, NA), na.rm = TRUE),
    
    n_sampled_effort_smallmouth_bass = n_distinct(ifelse(species_1 == "smallmouth_bass", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_smallmouth_bass = n_distinct(ifelse(species_1 == "smallmouth_bass" & count > 0, total_effort_ident, NA), na.rm = TRUE),
    
    n_sampled_effort_largemouth_bass = n_distinct(ifelse(species_1 == "largemouth_bass", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_largemouth_bass = n_distinct(ifelse(species_1 == "largemouth_bass" & count > 0, total_effort_ident, NA), na.rm = TRUE),
    
    n_sampled_effort_northern_pike = n_distinct(ifelse(species_1 == "northern_pike", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_northern_pike = n_distinct(ifelse(species_1 == "northern_pike" & count > 0, total_effort_ident, NA), na.rm = TRUE),
    
    n_sampled_effort_cisco = n_distinct(ifelse(species_1 == "cisco", total_effort_ident, NA), na.rm = TRUE),
    n_present_effort_cisco = n_distinct(ifelse(species_1 == "cisco" & count > 0 , total_effort_ident, NA), na.rm = TRUE)
    
  ) %>%
  arrange(state, -n_effort_idents) %>% 
  {. ->> cpe_sample_sz}

fwrite(cpe_sample_sz, file = "Data_and_Scripts/Data/output/cpe_species_summaries.csv")


