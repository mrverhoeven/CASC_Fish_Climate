#WI length at age and observations N observations discrepancy


#' We've noticed an oddity in the WI ages data in that there are many cases where there are more aged fish than there are fish observations (out of the same survey). This seems problematic in the sense that it is not likely that there are more sampled fish than what were caught in the survey and reported in the observations table. This script is solely going to scope out the problem and try to understand the issue. 




#required libraries
library(data.table)


#import data:
# See .Rdata export at bottom -- trying to ease the file sharing here
# laa <- fread("E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/WI_Data/wi_raw_disaggregated_data/laa.csv")
# obs <-  fread("E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/WI_Data/wi_raw_disaggregated_data/obs.csv")

load("Data_and_Scripts/Data/output/WI_DNR_diagnosticdat.RData")

colnames(obs)
colnames(laa)

#reformat dates:
laa[ , sample.date := as.IDate(sample.date) ,]

#' ## Problem Examples:
#' In the first selected example I show here, there are indeed ages for these fish (NA ages not an issue, backcalculated age not an issue).
#' 
#' There are 8 records for perch in the obs data and 23 in the laa data

  #lets have a look one sample problematic survey
  # review the data from that survey (from each source):
  obs[survey.seq.no == "651" & species == "yellow_perch", , ][order(length)]# 8 YEP in the obs file
  laa[survey.seq.no == "651"  & species == "yellow_perch", , ][order(length)]#23 YEP in teh age file
  
  
#' in another example, we can see that within a single species X gear X survey X length there's a mix of lengths with more obs in the laa file (wierd and unexpected; 334observations) and others with no laa data (160) or less laa than N in obs file (50) (these latter 2 cases are what I'd expected to see)
  a <- merge(
  obs[survey.seq.no == "281"& species == "walleye", .(obsfileN = .N) , .(survey.seq.no, gear, species, wbic, length, sample.date, age)][order(length)],
  laa[survey.seq.no == "281"& species == "walleye",  .(laafileN =  .N), .(survey.seq.no, gear, species, wbic, length, sample.date, age)][order(length)],
  by = c("survey.seq.no", "gear", "species", "wbic", "length", "sample.date", "age"),
  all = T)[ ,moreN_laa := laafileN>obsfileN]
  
  a[ , .N , moreN_laa]
  
  #maybe the gear is bad in the laa file?
  obs[survey.seq.no == "281"& species == "walleye", .(obsfileN = .N) , ] 
  laa[survey.seq.no == "281"& species == "walleye",  .(laafileN =  .N), ]
  # ignoring gear and simply counting the number of walleye in this survey-- we still get a mismatch.
  
  
#'## Overall problem scope:
#'Lets do some tiered testing of where the problem exists (or doesn't)   
  crosscheck_survey_date_gear_species <- merge( 
    obs[ , .("obsN" = .N), .(survey.seq.no, gear, species, wbic, sample.date) ],
    laa[ , .("laaN" = .N), .(survey.seq.no, gear, species, wbic, sample.date) ],
    by = c("survey.seq.no", "gear", "species", "wbic", "sample.date"), 
    all = T#full join
  )[, morelaa := laaN>obsN] #add a column indicating the relationship between N obs in each file
  
  #count prevalence (out of 25278 unique surveyXgearXspecies combinations where there's data in both datasets)
  crosscheck_survey_date_gear_species[ , .N , morelaa]#1096 cases (N of speciesXgearXsurvey) with more obs in laa data than in the ind fish dataset
  1096/24182
  
  
  #view problem cases
  crosscheck_survey_date_gear_species[morelaa == TRUE]

#' Some possible reasons for the issue:
  
  #NAs in the age file
  laa[ , .N , .(age_NA = is.na(age), length_NA = is.na(length), weight_NA = is.na(weight))] #count of unique combos of len/age/weight NAs in the data
  laa[is.na(age) & is.na(length) & is.na(weight) , .N , ] #any cases with all NA for age,len,wt?
  

#' As Paul Frater suspected, there is one of: length, weight, or age in every row of this table. So this might suggest that we could drop NAs for age then have no more issues?

  #there are fish with backcalcualted ages (not what we want[we want one fish per line], drop these)
  laa[,.N ,(back.calc)] #yes, dp, fl are all indication that the age is a backcalc age
  
  #how many ages are NAs?
  laa[, .N,is.na(age)] #443,811 fish have ages and 239,785 fish do not have ages (35% of ages are NA)
  
  # eliminate instances where 1) age is NA 2) age was backcalculated in length age file and 3) summarize by species in survey gear (try a similar test as above with NAs and backcalc lengths dropped)
  
  crosscheck_survey_date_gear_species <- merge(
    obs[                                                    , .("obsN" = .N), .(survey.seq.no, gear, species, wbic, sample.date) ],
    laa[!is.na(age) & !back.calc %in% c("dp", "yes", "fl")  , .("laaN" = .N), .(survey.seq.no, gear, species, wbic, sample.date) ], #no NAs and no backcalc ages
    by = c("survey.seq.no", "gear", "species", "wbic", "sample.date"), 
    all = T#full join
  )[, morelaa := laaN>obsN]#add a column indicating the relationship between N obs in each file
  
  
  
  #count prevalence (out of 25278 unique surveyXgearXspecies combinations where there's data in both datasets)
  crosscheck_survey_date_gear_species[ , .N , morelaa]#1096 cases (N of speciesXgearXsurvey) with more obs in laa data than in the ind fish dataset
  533/12929
  
  #view problem cases
  crosscheck_survey_date_gear_species[morelaa == TRUE]


#' Problem = not solved -- about 50% of cases fixed by doing that.
#' 
#' Is the problem alleviated by ignoring the gear from which aged fish were pulled?

  
  # eliminate survey gear (try a similar test as above with NAs and backcalc lengths dropped, now agg'd only to surveyXdate level for counts in a species)
  crosscheck_survey_date_gear_species <- merge(
    obs[                                                    , .("obsN" = .N), .(survey.seq.no, species, wbic, sample.date) ],
    laa[!is.na(age) & !back.calc %in% c("dp", "yes", "fl")  , .("laaN" = .N), .(survey.seq.no, species, wbic, sample.date) ], #no NAs and no backcalc ages
    by = c("survey.seq.no", "species", "wbic", "sample.date"), 
    all = T#full join
  )[, morelaa := laaN>obsN]#add a column indicating the relationship between N obs in each file
  
  
  
  #count prevalence 
  crosscheck_survey_date_gear_species[ , .N , morelaa]#1096 cases (N of speciesXgearXsurvey) with more obs in laa data than in the ind fish dataset
  1106/15277
  
  #view problem cases
  crosscheck_survey_date_gear_species[morelaa == TRUE]
  
#' how about ignoring the date?
  # eliminate survey gear (try a similar test as above with NAs and backcalc lengths dropped, now agg'd only to surveyXdate level for counts in a species)
  crosscheck_survey_date_gear_species <- merge(
    obs[                                                    , .("obsN" = .N), .(survey.seq.no, species, wbic) ],
    laa[!is.na(age) & !back.calc %in% c("dp", "yes", "fl")  , .("laaN" = .N), .(survey.seq.no, species, wbic) ], #no NAs and no backcalc ages
    by = c("survey.seq.no", "species", "wbic"), 
    all = T#full join
  )[, morelaa := laaN>obsN]#add a column indicating the relationship between N obs in each file
  
  
  
  #count prevalence 
  crosscheck_survey_date_gear_species[ , .N , morelaa]#1096 cases (N of speciesXgearXsurvey) with more obs in laa data than in the ind fish dataset
  330/8061
  
  #view problem cases
  crosscheck_survey_date_gear_species[morelaa == TRUE]

#' Comparing the prevalence of disgreement in N cases where laaN > obsN across my diff agreggations of these data would suggest that I've identified anything systematically wrong. 
#' 
#' Let's pull a list of all of the survey IDs in which a speciesXgear has more obs in the length age file:

  # bring back our original aggregation (N obs by surveyXdateXgearXspecies):
  crosscheck_survey_date_gear_species <- merge( 
    obs[                                                    , .("obsN" = .N), .(survey.seq.no, gear, species, wbic, sample.date) ],
    laa[!is.na(age) & !back.calc %in% c("dp", "yes", "fl")  , .("laaN" = .N), .(survey.seq.no, gear, species, wbic, sample.date) ],
    by = c("survey.seq.no", "gear", "species", "wbic", "sample.date"), 
    all = T#full join
  )[, morelaa := laaN>obsN] #add a column indicating the relationship between N obs in each file

  #print all survey IDs with this issue:
  crosscheck_survey_date_gear_species[morelaa == TRUE, unique(survey.seq.no)]
  #how much of the total aged survey data does that represent
  length(crosscheck_survey_date_gear_species[morelaa == TRUE, unique(survey.seq.no)])/length(crosscheck_survey_date_gear_species[!is.na(morelaa),unique(survey.seq.no)])

  crosscheck_survey_date_gear_species[morelaa == TRUE, .N  , .(species,survey.seq.no) ]
  
  ##save these objects
  # save(laa, obs, crosscheck_survey_date_gear_species, file = "Data_and_Scripts/Data/output/WI_DNR_diagnosticdat.RData" )
  
  
  
  
  
  
  
  
  
  
