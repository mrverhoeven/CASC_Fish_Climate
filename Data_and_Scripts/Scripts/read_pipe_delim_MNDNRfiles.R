# open pipe delimited MN fish files, save to file as .csv

library(data.table)

#CPUE
        a <- fread("E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/MN_Data/mn_raw_disaggregated_data/GH_LAB_HIST_CPUE_SUMM_ALL.txt", 
                   na.strings= "null")
        
        #check last column for errors. This column is most likely to be blank where pipes did not correctly separate columns.
        a[ , summary(WATER_TEMP_F)]
          a[WATER_TEMP_F<30.0 | WATER_TEMP_F >90 ,WATER_TEMP_F] #there is indeed some goofy shit in here. BUT! all of the rows have a complete set of columns!
        a[ , sum(TOTAL_CATCH) , .(SURVEY_ID, COMMON_NAME)][V1==0]
        a[ , SURVEYDATE :=  as.IDate(SURVEYDATE, format = "%m/%d/%Y") , ]
        a[, sum(TOTAL_CATCH), year(SURVEYDATE)][order(year)]
        a[ , sum(TOTAL_CATCH), GEAR ]
        a[ , sum(TOTAL_CATCH),COMMON_NAME ]
        
        names(a)
        a[ , .N ,  REPRESENTATIVE_SAMPLING]
        a[ , .N, .(SURVEY_ID, GEAR, COMMON_NAME, SURVEYDATE, LKNAME, DOW, WATER_TEMP_F, REPRESENTATIVE_SAMPLING, CPUE,EFFORT , TOTAL_CATCH, SURVEYTYPE ) ]
        
        sum(duplicated(a))
        
        # fwrite(a, "E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/MN_Data/mn_raw_disaggregated_data/mn_cpue_21aug2023.csv" )


#OBSERVATIONS
        b <- fread("E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/MN_Data/mn_raw_disaggregated_data/GH_LAB_RAWFISH_W_AGES_ALL.txt", 
                   na.strings= "null")
        
        #check last column for errors. This column is most likely to be blank where pipes did not correctly separate columns.
        b[ ,summary(FISH_COUNT)]
        b[ , sum(FISH_COUNT) , .(SURVEY_ID,SP)][V1==0]
        b[!OFF_AGE=="null" , .N ]
        b[ , .N ,OFF_AGE]
        
        b[ , sum(FISH_COUNT) ,]
        
        #how many aged?
        b[ , .N , is.na(OFF_AGE)]
        b[ , .N , age]

        # fwrite(b, "E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/MN_Data/mn_raw_disaggregated_data/mn_indiv_fish_23aug2023.csv")
        

#EFFORT
        c <- fread("E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/MN_Data/mn_raw_disaggregated_data/GH_LAB_UM_EFFORT_ALL.txt", 
                   na.strings= "null")
        
        c[ ,.N , .(SURVEY_ID, SAMP_STA_TYPE_ABBREV)]#non integer numbers seem worrisome
        c[ (floor(NUMBER_OF_NETTERS) - NUMBER_OF_NETTERS) != 0 , .N ,  ]
          c[ (floor(NUMBER_OF_NETTERS) - NUMBER_OF_NETTERS) != 0 , .N , SAMP_STA_TYPE_NAME  ]
          c[ (floor(NUMBER_OF_NETTERS) - NUMBER_OF_NETTERS) != 0 , ,  ]
        #even though it is weird that there are partial "netters", the data otherwise look okay for these. 
        
        c[ , mean(EFFORT) ,  SAMP_STA_TYPE_NAME ]
        
        c[ , .N , COMPONENT_COUNT][order(COMPONENT_COUNT)]
          
        
        
        # fwrite(c, "E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/MN_Data/mn_raw_disaggregated_data/mn_effort_18aug2023.csv" )



