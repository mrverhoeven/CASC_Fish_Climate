# take a look at Corey Geving's 9 June Data rips:


library(data.table)
library(dplyr)
library(stringr)
library(tidyr)


wae_fish <- fread("C:\\Users\\verh0064\\Desktop\\wae_raw_allregions_2019-2022_v02.txt", na.strings = "null")

wae_catch <- fread("C:\\Users\\verh0064\\Desktop\\wae_cpe_allregions_2019-2022.txt", na.strings = "null")

wae_effort <- fread("C:\\Users\\verh0064\\Desktop\\effort_allregions_2019-2022.txt", na.strings = "null")


#fish data summary:

wae_fish[ str_detect(SRVY_DT, "2019"), .N , OFF_AGE]


#clean dates
wae_fish[ , date_clean := as.IDate(SRVY_DT, format = "%m/%d/%Y") , ]

#indiv. as rows?
wae_fish[ , .N , FISH_COUNT][order(FISH_COUNT)]

#unpack multi-fish rows:
wae_fish <- uncount(wae_fish, FISH_COUNT, .remove = T , .id = "uncount_id")

#summarize by data avail:
wae_fish[ GEARTP %in% c("GN", "GSH", "GDE") , .N , .(year = year(date_clean) , catch = !is.na(CPUE), length = !is.na(LEN_MM), age = !is.na(OFF_AGE))][order(year)]

wae_fish[ , .N , year(date_clean)]

#catch data
#clean dates
wae_catch[ , date_clean := as.IDate(SURVEYDATE, format = "%m/%d/%Y") , ]

wae_catch[ GEAR %in% c("GN", "GSH","GDE"), sum(TOTAL_CATCH) , .(year(date_clean), GEAR) ][order(year)]

wae_catch[ , .N ,TOTAL_CATCH][ order(TOTAL_CATCH) , ]


#effort
colnames(wae_effort)

wae_effort[, SURVEY_ID, ] %in% wae_catch[  ,SURVEY_ID , ] 

wae_fish[ , .N , .(SRVY_DT, )]





