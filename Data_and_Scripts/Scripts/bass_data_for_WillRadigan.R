
library(lubridate)
library(arrow)
library(data.table)
library(tidyverse)


mn_data <- open_dataset("E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Parquet files/mn_file_arrow")
glimpse(mn_data)






# Minnesota ---------------------------------------------------------------

# see here that we have lots of irrelevant gears
  mn_data %>% 
    group_by(state, sampling_method) %>%
    count() %>% collect() %>% print(n=nrow(.))
  
#ef, but not for walleye
  
  mn_data %>% 
    filter(!str_detect(sampling_method, "walleye")) %>% 
    filter(str_detect(sampling_method, "lectro")) %>% 
    group_by(state, sampling_method) %>%
    count() %>% collect() %>% print(n=nrow(.))
  
# bass numbers with YOY marked or age zero:
  
  mn_data %>% 
    filter(!str_detect(sampling_method, "walleye")) %>% 
    filter(str_detect(sampling_method, "lectro")) %>% 
    group_by(state, sampling_method) %>%
    
    summarise(
      n_lakes = n_distinct(paste(lake_name, lake_id, nhdhr_id)),
      n_effort_idents = n_distinct(total_effort_ident),
      n_smallmouth_bass_present = n_distinct(ifelse(species_1 == "smallmouth_bass", total_effort_ident, NA), na.rm = TRUE),
      n_smbyoyORzero = n_distinct(ifelse(species_1 == "smallmouth_bass" & (age == 0 | age_class == "young_of_year"), total_effort_ident, NA), na.rm = TRUE),
      n_largemouth_bass_present = n_distinct(ifelse(species_1 == "largemouth_bass", total_effort_ident, NA), na.rm = TRUE),
      n_lmbyoyORzero_present = n_distinct(ifelse(species_1 == "largemouth_bass"& (age == 0 | age_class == "young_of_year"), total_effort_ident, NA), na.rm = TRUE)
    ) %>% 
    collect() %>% print(n=nrow(.))
  
  
  

  #suitable surveys--Something is botched here in the TEIs because I'm getting 10x duplication when I put the efforts in here
  mn_data %>%#only EF data
    filter(!str_detect(sampling_method, "walleye")) %>% 
    filter(str_detect(sampling_method, "lectro")) %>% 
    distinct(state,lake_name,lake_id, nhdhr_id,
             sampling_method, survey_type, date_total_effort_ident, total_effort_ident, total_effort_1, total_effort_1_units, total_effort_nothing_caught) %>% 
    collect() %>% 
    {. ->> suitable_surveys}
  
  #arrow handles NAs poorly, so the distinct call fails to actually execute as expected. Here I remove duplicated total_effort_ident records
  suitable_surveys %>% 
    filter(!duplicated(suitable_surveys)) %>% 
    {. ->> suitable_surveys}
  
  
  suitable_surveys %>%
    group_by(state, sampling_method) %>%
    summarise(
    n_lakes = n_distinct(paste(lake_name, lake_id, nhdhr_id)),
    n_effort_idents = n_distinct(total_effort_ident),
    )
  
  # suitable lakes
  mn_data %>%#only EF data
    filter(!str_detect(sampling_method, "walleye")) %>% 
    filter(str_detect(sampling_method, "lectro")) %>% 
    #group by lakes
    group_by(lake_name,lake_id, nhdhr_id) %>%
    summarise(n_years = n_distinct(year(date_total_effort_ident))) %>% 
    filter(n_years > 10) %>% 
    collect() %>% 
    {. ->> suitable_lakes}
  
  #Cross these
  right_join(suitable_surveys, suitable_lakes) %>% 
    {. ->> suitable_surveysxlakes}
  
  # count occurrences of all and 0/yoys within these data:
  mn_data %>% 
    filter(total_effort_ident %in% suitable_surveysxlakes$total_effort_ident) %>% 
    group_by(state, sampling_method) %>%
    summarise(
      n_lakes = n_distinct(paste(lake_name, lake_id, nhdhr_id)),
      n_effort_idents = n_distinct(total_effort_ident),
      n_smallmouth_bass_present = n_distinct(ifelse(species_1 == "smallmouth_bass", total_effort_ident, NA), na.rm = TRUE),
      n_smbyouORzero = n_distinct(ifelse(species_1 == "smallmouth_bass" & (age == 0 | age_class == "young_of_year"), total_effort_ident, NA), na.rm = TRUE),
      n_largemouth_bass_present = n_distinct(ifelse(species_1 == "largemouth_bass", total_effort_ident, NA), na.rm = TRUE),
      n_lmbyoyORzero_present = n_distinct(ifelse(species_1 == "largemouth_bass"& (age == 0 | age_class == "young_of_year"), total_effort_ident, NA), na.rm = TRUE)
    ) %>% 
    collect() %>% print(n=nrow(.))
  
  
  #export only data of interest:
  mn_data %>% 
    filter(species_1 %in% c("largemouth_bass", "smallmouth_bass") & (age == 0 | age_class == "young_of_year")) %>%
    inner_join(suitable_surveysxlakes, by = "total_effort_ident") %>% 
    collect() %>% 
    {. ->> yoy_age_zero_bass_data}
  
  
  
  
  



  
  
  
  
  
  
  
  
  
  
  