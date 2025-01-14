
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

#we'll use only std gillnets
mn_data %>% 
  filter(str_detect(sampling.method, "Standard gill")) %>% 
  group_by(sampling.method) %>% 
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
  
  
#for those lakes, how may years of data for each species?
  mn_data %>% 
    filter(str_detect(sampling.method, "Standard gill")&
             !is.na(total.effort)) %>% 
    inner_join(. , suitable_lakes,) %>% 
    collect() %>%
    group_by(species, lake.name) %>%
    summarize(
      nfishyears = n_distinct(year(date)),
      nyears = first(nyears)
    ) %>%
    # filter(nyears>19) %>% 
    print(n=nrow(.))

#import data: all lake gillnet survey data from lakes w/ 20+ years of GN survey records,
  mn_data %>% #all fish from:
    filter(str_detect(sampling.method, "Standard gill") & #suitable surveys
             !is.na(total.effort)) %>% 
    inner_join(. , suitable_lakes,) %>% #suitable lakes
    collect() %>% 
    setDT() %>% 
    {gn_dat_20y <<- . }

  
#for those lakes, can we see a density plot of the sizes of each aged fish?
  #drop flagged records
  gn_dat_20y[ , .N , flag ]
    gn_dat_20y <- gn_dat_20y[is.na(flag) ,  , ]
  

    
  #missing lengths
  gn_dat_20y[  , .N, length.unit      ] #missing units should mean missing lengths: 
  gn_dat_20y[  , .N, is.na(length)      ] #check for missing lengths. 
  gn_dat_20y <- gn_dat_20y[!is.na(length) ]
  
  #re check 20y of data
  gn_dat_20y[ , .(nyears = n_distinct(year(date)))  , lake.name][nyears>19 , lake.name ]
  
  gn_dat_20y <- gn_dat_20y[ lake.name %in% gn_dat_20y[ , .(nyears = n_distinct(year(date)))  , lake.name][nyears>19 , lake.name ]]
  
  #assume only year level Age-Length Keys are acceptable:
  # gn_dat_20y[ , .N , alk ]
  # 
  # gn_dat_20y <- gn_dat_20y[alk == "year"]
  # 
  # gn_dat_20y[ , .(nyears = n_distinct(year(date)))  , lake.name][nyears>19 , lake.name ]

#calculate an age specific cpe through time for these lakes. Here we'll use the estimated ages because of biases introduced in the process of subsampling for ages
# here we must be carefule about how we execute this. First make a suitable surveys set, then estimate the age specific cpe for each of those survey efforts  
  
  # effort table: 408 efforts
  gn_dat_20y %>%
    group_by(state, county, lake.name, lake.id, nhdhr.id, date, sampling.method,
             total.effort.ident, total.effort.nothing.caught, total.effort, total.effort.1.units) %>% 
    summarise(
      nspecies = length(unique(species))
    ) %>% 
    {efforts <<- .}
  
  
  
  #age specific cpes
  gn_dat_20y %>% 
    group_by(total.effort.ident, species, est.age) %>% 
    summarise(
      catch_n = n() #within a total effort ident, all records should share a total effort value
    ) %>% 
    right_join(. , efforts) %>% 
    # mutate(
    #   cpe = catch_n/total.effort
    # ) %>% 
    setDT() %>% 
    {cpes <<- .}
  
  # drop two unneeded bits
  cpes[ , .N, total.effort.nothing.caught ]
  cpes[ , c("total.effort.nothing.caught", "nspecies") := NULL , ]
  
  
  #needs an adjustment to cover all sampled years for all species within a lake across all age classes

  
  #expand this to include zeros for all age-classes
  gn_cpe_20y <- 
  melt(
    dcast(cpes, ... ~ est.age, fill = 0, value.var = "catch_n"), #expand and fill with zeros
    id.vars = c("total.effort.ident", "species", "state", "county", "lake.name", "lake.id", "nhdhr.id", "date", "sampling.method","total.effort", "total.effort.1.units"),
    variable.name = c("est.age"), value.name = c("catch_n")) #re shape to age spcific catch value
  

  
  #for each species, drop any ages exceeding max
  gn_cpe_20y[ , .N  , est.age  ]
  gn_cpe_20y[ , est.age :=  as.numeric(as.character(est.age))  ,]
  
  gn_cpe_20y[catch_n>0, max(est.age) , .(species) ]
    gn_cpe_20y[ gn_cpe_20y[catch_n>0, max(est.age) , .(species) ],
                on = .(species),
                max.age := V1 , ]
  #now execute dump of extra age class cpes  
   gn_cpe_20y <-  gn_cpe_20y[est.age<=max.age, , ]
    
   
  #get these ordered by date
  setorder(gn_cpe_20y, date)
 
  

# collapse to annual values: ----------------------------------------------

 setDT(efforts) 
  
  efforts[ , .N , total.effort.1.units ]
  
efforts %>% 
  group_by(state, county, lake.name, lake.id, nhdhr.id, year = year(date)) %>% 
  summarise(annual_effort = sum(total.effort),
            annual_effort_units = first(total.effort.1.units)) %>% 
  {ann_effort <<- .}


gn_cpe_20y %>% 
    group_by(state, county, lake.name, lake.id, nhdhr.id, year = year(date), species, est.age) %>% 
    summarise(ann_catch = sum(catch_n)
    ) %>%
    right_join(., ann_effort) %>% 
  mutate(cpe = ann_catch/annual_effort) %>% 
  setDT() %>% 
  {gn_cpe_20y_annsums <<- .}


gn_cpe_20y[ , cpe := catch_n/total.effort , ]


# zeros expansion ---------------------------------------------------------


# check for zeros that are appropriate

gn_cpe_20y[ , .("nzeros" = sum(catch_n==0)) , .(lake.name, lake.id, species, est.age, year(date), total.effort.ident )  ]
gn_cpe_20y[ , .("nzeros" = sum(catch_n==0)) , .(lake.name, lake.id, species, est.age, year(date))  ]

gn_cpe_20y_annsums[ , .("nzeros" = sum(ann_catch==0)) , .(lake.name, lake.id, species, est.age, year)  ]

#seems to jive that there are zero cpes carrying through for species-age classes in here. 

# check for species remaining in as zeros when they 

gn_cpe_20y_annsums[ , .("nspp" = length(unique(species))) , .(lake.id, year) ]
#here we see that no every species is shoiwng up in all years within a lake

#expand data to cover all species that could be sampled in each lakeXyear
gn_cpe_20y_annsums %>% 
  group_by(state, county, lake.name, lake.id, nhdhr.id) %>% 
  complete(. , year, nesting(species, est.age)) %>% #complete all missing speciesXage data for each year within each lake
  setDT() %>% 
  {gn_cpe_20y_annsums <<- .}

gn_cpe_20y_annsums[is.na(ann_catch) , ann_catch := 0 , ]

annual_efforts <- gn_cpe_20y_annsums[!is.na(annual_effort) , .N , .(state, county, lake.name, lake.id, nhdhr.id, year, annual_effort, annual_effort_units ) ]
setDT(gn_cpe_20y_annsums)

gn_cpe_20y_annsums %>% 
  select(-c(annual_effort_units,annual_effort) ) %>% 
  {gn_cpe_20y_annsums <<- .}


gn_cpe_20y_annsums[annual_efforts,
                   on = .(state, county, lake.name, lake.id, nhdhr.id, year),
                   `:=` (annual_effort = annual_effort, annual_effort_units = annual_effort_units) ]


# recalc cpes -------------------------------------------------------------

gn_cpe_20y_annsums[ , cpe := ann_catch/annual_effort , ]




# birth year --------------------------------------------------------------

##time lag recruitments to their birth year

gn_cpe_20y[ , birth_year := year(date)-est.age ,]
gn_cpe_20y_annsums[ , birth_year := year-est.age ,]  
  
# walleye -----------------------------------------------------------------

#what sizesXages are caught in nets?
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
  
  ggplot(gn_cpe_20y[species == "walleye" & est.age %in% c(0:3)], aes(year(date), cpe, group = est.age))+
    geom_path(aes(color = est.age))+
    # geom_smooth(method = "loess")+
    facet_wrap(~lake.name, scales = "free")

# Use annual sum data instead
  ggplot(gn_cpe_20y_annsums[species == "walleye" & est.age %in% c(0:3)], aes(year, cpe, group = est.age))+
    geom_path(aes(color = est.age))+
    # geom_smooth(method = "loess")+
    facet_wrap(~lake.name, scales = "free")
  

  
ggplot(gn_cpe_20y_annsums[species == "walleye" & est.age %in% c(1:8)], aes(birth_year, cpe, group = est.age))+
    geom_path(aes(color = est.age))+
    # geom_smooth(method = "loess")+
    facet_wrap(~lake.name, scales = "free")
  


# northern pike -----------------------------------------------------------

#who is getting caught?  
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
  
  ggplot(gn_cpe_20y_annsums[species == "northern_pike" & est.age %in% c(0:3)], aes(year, cpe, group = est.age))+
    geom_path(aes(color = est.age))+
    # geom_smooth(method = "loess")+
    facet_wrap(~lake.name, scales = "free")
  
  
  
  ##time lag recruitments to their birth year

  ggplot(gn_cpe_20y_annsums[species == "northern_pike" & est.age %in% c(1:8)], aes(birth_year, cpe, group = est.age))+
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
  
  ggplot( gn_dat_20y[species == "yellow_perch" & !is.na(length)] ,
          aes( length, group = year) )+
    geom_density(aes(color = year))+
    facet_wrap(~lake.name, scales = "free")
  
  #looks to me that the age 2 fish are recruiting to the gear pretty well
  
  #calculate an age specific cpe through time for these lakes. Here we'll use the estimated ages becasue of biases introduced in the process of subsampling for ages
  
  #ensure these are ordered by date (fopr plotting)
  setorder(gn_cpe_20y_annsums, year)
  
  ggplot(gn_cpe_20y_annsums[species == "yellow_perch" & est.age %in% c(2)], aes(year, cpe, group = est.age))+
    geom_path(aes(color = est.age))+
    # geom_smooth(method = "loess")+
    facet_wrap(~lake.name, scales = "free")
  
  
  
  ##time lag recruitments to their birth year
  
  ggplot(gn_cpe_20y_annsums[species == "yellow_perch" & est.age %in% c(2:4)], aes(birth_year, cpe, group = est.age))+
    geom_path(aes(color = est.age))+
    # geom_smooth(method = "loess")+
    facet_wrap(~lake.name, scales = "free")  
  
  
  

# compare multi-species ---------------------------------------------------

  #age 3 of all species through time
  ggplot(gn_cpe_20y_annsums[species %in% c("northern_pike", "walleye", "black_crappie", "cisco", "largemouth_bass", "smallmouth_bass") & est.age %in% c(3)], aes(birth_year, cpe, group = species))+
    geom_path(aes(color = species))+
    # geom_smooth(method = "loess")+
    facet_wrap(~lake.name, scales = "free")
  
  #normalize the CPEs w/in lakesXspeciesXage
  
  gn_cpe_20y_annsums[ , lake_max_cpe := max(cpe)  , .(lake.name, lake.id, nhdhr.id, 
                     species, est.age) ]
  
  
  
  ggplot(gn_cpe_20y_annsums[species %in% c("northern_pike", "walleye", "black_crappie", "cisco", "yellow_perch",  "largemouth_bass", "smallmouth_bass") & est.age %in% c(3)], aes(birth_year, cpe/lake_max_cpe, group = species))+
    geom_path(aes(color = species))+
    # geom_smooth(method = "loess")+
    facet_wrap(~lake.name, scales = "free")
  
  
  

# export recruitment datasets ---------------------------------------------

  
  # for each species
  fwrite(gn_cpe_20y_annsums[species == "walleye" & est.age == 1, , ],
         file = "Data_and_Scripts/Data/output/Recruitment_Workshop_Data/age1_walleye_GN_cpe.csv")
  fwrite(gn_cpe_20y_annsums[species == "northern_pike" & est.age == 2, , ],
         file = "Data_and_Scripts/Data/output/Recruitment_Workshop_Data/age2_northernpike_GN_cpe.csv")
  fwrite(gn_cpe_20y_annsums[species == "yellow_perch" & est.age == 3, , ],
         file = "Data_and_Scripts/Data/output/Recruitment_Workshop_Data/age3_yellowperch_GN_cpe.csv")

  
  gn_cpe_20y[lake.name == "Fox" & species == "largemouth_bass"]
  
  
  
  