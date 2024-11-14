
library(lubridate)
library(arrow)
library(data.table)
library(tidyverse)


# We oughtta be able to get a look at recruitment in the fish data using the age-assigned data here:  
# 
# https://drive.google.com/drive/u/1/folders/1xmczFohwdlq0aE3p_E-MMsgt8j8XItQV

mn_data <- open_dataset("E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Parquet files/Age-assigned Data/mn_halk_aged_data/most_common_structures")
glimpse(mn_data)




# Minnesota ---------------------------------------------------------------

# see here that we have lots of irrelevant gears
  mn_data %>% 
    group_by(state, sampling.method) %>%
    count() %>% collect() %>% print(n=nrow(.))

#what species
  mn_data %>% 
    filter(str_detect(sampling.method, "Standard gill")) %>% 
    group_by(species) %>% 
    count() %>% collect() %>% print(n=nrow(.))

#suitable lakes will have 20+y of std gillnetting
  mn_data %>% 
    filter(str_detect(sampling.method, "Standard gill") &
             !is.na(total.effort)) %>% 
    group_by(lake.name, lake.id, nhdhr.id) %>% 
    summarise(
      nyears = n_distinct(year(date)))%>%
    filter(nyears>19) %>% 
    collect() %>% print(n=nrow(.)) %>% 
    setDT() %>% 
    {suitable_lakes <<- .}

  suitable_lakes[ lake.name == "Poplar"]
  
  
#for those lakes, how may years of data for each species?retrieve data
  mn_data %>% 
    filter(str_detect(sampling.method, "Standard gill")&
             !is.na(total.effort)) %>% 
    inner_join(. , suitable_lakes,) %>% 
    group_by(species, lake.name) %>%
    summarize(
      nyears = n_distinct(year(date))
    ) %>% filter(nyears>19) %>% 
    collect() %>% print(n=nrow(.))

#import data: all lake gillnet survey data from locs w/ 20+ years of GN survey records,
  mn_data %>% 
    filter(str_detect(sampling.method, "Standard gill") &
             !is.na(total.effort)) %>% 
    inner_join(. , suitable_lakes,) %>% 
    collect() %>% 
    setDT() %>% 
    {gn_dat_20y <<- . }

  
  
  
#for those lakes, can we see a density plot of the sizes of each aged fish?
  #drop flagged records
  gn_dat_20y[ , .N , flag ]
    gn_dat_20y <- gn_dat_20y[is.na(flag) ,  , ]
  

    
  #missing lengths
  gn_dat_20y[  , .N, length.unit      ]
  gn_dat_20y[  , .N, is.na(length)      ]
  gn_dat_20y <- gn_dat_20y[!is.na(length) ]
  
  #re check 20y of data
  
  gn_dat_20y[ , .(nyears = n_distinct(year(date)))  , lake.name][nyears>19 , lake.name ]
  
  gn_dat_20y <- gn_dat_20y[ lake.name %in% gn_dat_20y[ , .(nyears = n_distinct(year(date)))  , lake.name][nyears>19 , lake.name ]]
  
  #assume only year level is acceptable
  # gn_dat_20y[ , .N , alk ]
  # 
  # gn_dat_20y <- gn_dat_20y[alk == "year"]
  # 
  # gn_dat_20y[ , .(nyears = n_distinct(year(date)))  , lake.name][nyears>19 , lake.name ]

  #calculate an age specific cpe through time for these lakes. Here we'll use the estimated ages becasue of biases introduced in the process of subsampling for ages
  
  #dplyr style
  gn_dat_20y %>% 
    group_by(total.effort.ident, species) %>% 
    summarise(
      cpe = n()/ first(total.effort) #within a total effort ident, all records should share a total effort value
    ) %>% 
    compute()
  
  #data.table style
  gn_dat_20y[ , .(cpe = .N / first(total.effort))   ,
              .(lake.name, lake.id, nhdhr.id, 
                year(date),  species, est.age)]
  gn_cpe_20y <-     gn_dat_20y[ , .(cpe = .N / first(total.effort))   ,
                                .(lake.name, lake.id, nhdhr.id, 
                                  year(date),  species, est.age)]
  #get these ordered by date
  setorder(gn_cpe_20y, year)

# walleye -----------------------------------------------------------------

  
  
  ggplot( gn_dat_20y[species == "walleye" & !is.na(length)] ,
          aes( length, group = as.factor(age)) )+
    geom_density(aes(color = as.factor(age)))+
    facet_wrap(~lake.name, scales = "free")

#highlight ages 1,2,3  
  ggplot( gn_dat_20y[species == "walleye" & !is.na(length)] ,
          aes( length, group = as.factor(age)) )+
    geom_density(aes(color = as.factor(age)))+
    facet_wrap(~lake.name, scales = "free")+
    geom_density(data = gn_dat_20y[species == "walleye" & !is.na(length) & age == 1],
                           aes(length), fill = "red")+
    geom_density(data = gn_dat_20y[species == "walleye" & !is.na(length) & age == 2],
                 aes(length), fill = "yellow")+
    geom_density(data = gn_dat_20y[species == "walleye" & !is.na(length) & age == 3],
                 aes(length), fill = "green")
  
  # do any of these show evidence of left-truncation (aka right-skew) 
  ggplot( gn_dat_20y[species == "walleye" & !is.na(length)] ,
          aes( length))+
    geom_density()+
    facet_wrap(~age, scales = "free")
  
  # this plot shows that until about 300mm, these fish probably still have poor capture in the nets. Why? 
  # The most abundant classes should be the smallest ones, so the
  # data generating process should be creating a poisson-like shape, with huge right skew as bigger fish 
  # experience mortality and become less abundant. But becaue our gear is size-selective (especially missing little fish)
  # we see that fish under ~300 mm  have a decreasing "abundance" indicative of poor capture.     
  ggplot( gn_dat_20y[species == "walleye" & !is.na(length)] ,
          aes( length, group = year) )+
    geom_density(aes(color = year))+
    facet_wrap(~lake.name, scales = "free")

# from these plots we can see that GNs capture fish as small as 200mm. But around 300 mm looks like we've got a pretty solid capture (80+% of age 3s are in this )   
  
#usually age 3 fish are "recruits", I'd argue that these plots are showing good capture of age 1 and age 2 fish as well. 
  

  
  ggplot(gn_cpe_20y[species == "walleye" & est.age %in% c(0:3)], aes(year, cpe, group = est.age))+
    geom_path(aes(color = est.age))+
    # geom_smooth(method = "loess")+
    facet_wrap(~lake.name, scales = "free")
  
  
  
 ##time lag recruitments to their birth year
  
  gn_cpe_20y[ , birth_year := year-est.age ,]
  
  ggplot(gn_cpe_20y[species == "walleye" & est.age %in% c(1:8)], aes(birth_year, cpe, group = est.age))+
    geom_path(aes(color = est.age))+
    # geom_smooth(method = "loess")+
    facet_wrap(~lake.name, scales = "free")
  


# northern pike -----------------------------------------------------------

  
  ggplot( gn_dat_20y[species == "northern_pike" & !is.na(length)] ,
          aes( length, group = as.factor(age)) )+
    geom_density(aes(color = as.factor(age)))+
    facet_wrap(~lake.name, scales = "free")
  
  #highlight age 1,2,3  
  ggplot( gn_dat_20y[species == "northern_pike" & !is.na(length)] ,
          aes( length, group = as.factor(age)) )+
    geom_density(aes(color = as.factor(age)))+
    facet_wrap(~lake.name, scales = "free")+
    geom_density(data = gn_dat_20y[species == "northern_pike" & !is.na(length) & age == 1],
                 aes(length), fill = "red")+
    geom_density(data = gn_dat_20y[species == "northern_pike" & !is.na(length) & age == 2],
                 aes(length), fill = "yellow")+
    geom_density(data = gn_dat_20y[species == "northern_pike" & !is.na(length) & age == 3],
                 aes(length), fill = "green")
  
  
  ggplot( gn_dat_20y[species == "northern_pike" & !is.na(length)] ,
          aes( length))+
    geom_density()+
    facet_wrap(~age, scales = "free")
  
#year specific size-stock densities  
  ggplot( gn_dat_20y[species == "northern_pike" & !is.na(length)] ,
          aes( length, group = year) )+
    geom_density(aes(color = year))+
    facet_wrap(~lake.name, scales = "free")
    
# to me, it looks like age 1s are pretty well captured. However the lack of age 1 fish in pepin and the truncation of age 1 less than 250mm in St Louis river suggest using age 2s is better.  

  gn_cpe_20y 
  #get these ordered by date
  setorder(gn_cpe_20y, year)
  
  ggplot(gn_cpe_20y[species == "northern_pike" & est.age %in% c(0:3)], aes(year, cpe, group = est.age))+
    geom_path(aes(color = est.age))+
    # geom_smooth(method = "loess")+
    facet_wrap(~lake.name, scales = "free")
  
  
  
  ##time lag recruitments to their birth year

  ggplot(gn_cpe_20y[species == "northern_pike" & est.age %in% c(1:8)], aes(birth_year, cpe, group = est.age))+
    geom_path(aes(color = est.age))+
    # geom_smooth(method = "loess")+
    facet_wrap(~lake.name, scales = "free")
  

# yellow perch ------------------------------------------------------------

# is there an age that appears to recruit?
  
  ggplot( gn_dat_20y[species == "yellow_perch" & !is.na(length)] ,
          aes( length, group = as.factor(age)) )+
    geom_density(aes(color = as.factor(age)))+
    facet_wrap(~lake.name, scales = "free")
  
  #highlight age 2,3,4  
  ggplot( gn_dat_20y[species == "yellow_perch" & !is.na(length)] ,
          aes( length, group = as.factor(age)) )+
    geom_density(aes(color = as.factor(age)))+
    facet_wrap(~lake.name, scales = "free")+
    geom_density(data = gn_dat_20y[species == "yellow_perch" & !is.na(length) & age == 2],
                 aes(length), fill = "red")+
    geom_density(data = gn_dat_20y[species == "yellow_perch" & !is.na(length) & age == 3],
                 aes(length), fill = "yellow")+
    geom_density(data = gn_dat_20y[species == "yellow_perch" & !is.na(length) & age == 4],
                 aes(length), fill = "green")
  
  
  ggplot( gn_dat_20y[species == "yellow_perch" & !is.na(length)] ,
          aes( length))+
    geom_density()+
    facet_wrap(~age, scales = "free")
  
  ggplot( gn_dat_20y[species == "yellow_perch" & !is.na(length)] ,
          aes( length) )+
    geom_histogram()+
    facet_wrap(~lake.name, scales = "free")
  
  
  #looks to me that the age 2 fish are recruiting to the gear pretty well
  
  #calculate an age specific cpe through time for these lakes. Here we'll use the estimated ages becasue of biases introduced in the process of subsampling for ages
  
  #ensure these are ordered by date (fopr plotting)
  setorder(gn_cpe_20y, year)
  
  ggplot(gn_cpe_20y[species == "yellow_perch" & est.age %in% c(2,3,4)], aes(year, cpe, group = est.age))+
    geom_path(aes(color = est.age))+
    # geom_smooth(method = "loess")+
    facet_wrap(~lake.name, scales = "free")
  
  
  
  ##time lag recruitments to their birth year
  
  ggplot(gn_cpe_20y[species == "yellow_perch" & est.age %in% c(2:4)], aes(birth_year, cpe, group = est.age))+
    geom_path(aes(color = est.age))+
    # geom_smooth(method = "loess")+
    facet_wrap(~lake.name, scales = "free")  

# compare multi-species ---------------------------------------------------

  
  ggplot(gn_cpe_20y[species %in% c("northern_pike", "walleye", "black_crappie", "cisco", "largemouth_bass", "smallmouth_bass") & est.age %in% c(3)], aes(birth_year, cpe, group = species))+
    geom_path(aes(color = species))+
    # geom_smooth(method = "loess")+
    facet_wrap(~lake.name, scales = "free")
  
  #normalize the CPEs w/in lakesXspeciesXage
  
  gn_cpe_20y[ , lake_max_cpe := max(cpe)  , .(lake.name, lake.id, nhdhr.id, 
                     species, est.age) ]
  
  
  
  ggplot(gn_cpe_20y[species %in% c("northern_pike", "walleye", "black_crappie", "cisco", "yellow_perch",  "largemouth_bass", "smallmouth_bass") & est.age %in% c(3)], aes(birth_year, cpe/lake_max_cpe, group = species))+
    geom_path(aes(color = species))+
    # geom_smooth(method = "loess")+
    facet_wrap(~lake.name, scales = "free")
  
  
  

# export recruitment datasets ---------------------------------------------

  
  # # for each species 
  # fwrite(gn_cpe_20y[species == "walleye" & est.age == 1, , ],
  #        file = "Data_and_Scripts/Data/output/Recruitment_Workshop_Data/age1_walleye_GN_cpe.csv")
  # fwrite(gn_cpe_20y[species == "northern_pike" & est.age == 2, , ],
  #        file = "Data_and_Scripts/Data/output/Recruitment_Workshop_Data/age2_northernpike_GN_cpe.csv")
  # fwrite(gn_cpe_20y[species == "yellow_perch" & est.age == 3, , ],
  #        file = "Data_and_Scripts/Data/output/Recruitment_Workshop_Data/age3_yellowperch_GN_cpe.csv")
  
  
  
  
  