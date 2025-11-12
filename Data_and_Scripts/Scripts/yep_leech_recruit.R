
library(lubridate)
library(arrow)
library(data.table)
library(tidyverse)


# We oughtta be able to get a look at recruitment in the fish data. 
# - 


mn_data <- open_dataset("G:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Parquet files/Age-assigned Data/mn_halk_aged_data/most_common_structures")
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
    filter(str_detect(sampling.method, "Standard gill")) %>% 
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
    filter(str_detect(sampling.method, "Standard gill")) %>% 
    inner_join(. , suitable_lakes,) %>% 
    group_by(species, lake.name) %>%
    summarize(
      nyears = n_distinct(year(date))
    ) %>% filter(nyears>19) %>% 
    collect() %>% print(n=nrow(.))

#import data: all lake gillnet survey data from locs w/ 20+ years of GN survey records,
  mn_data %>% 
    filter(str_detect(sampling.method, "Standard gill")) %>% 
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
  
  ggplot( gn_dat_20y[species == "walleye" & !is.na(length)] ,
          aes( length, group = as.factor(age)) )+
    geom_density(aes(color = as.factor(age)))+
    facet_wrap(~lake.name, scales = "free")

#highlight age 3  
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
  
  
  ggplot( gn_dat_20y[species == "walleye" & !is.na(length)] ,
          aes( length))+
    geom_density()+
    facet_wrap(~age, scales = "free")
  
  ggplot( gn_dat_20y[species == "walleye" & !is.na(length)] ,
          aes( length) )+
    geom_histogram()+
    facet_wrap(~lake.name, scales = "free")
  
  
#usually age 3 fish are "recruits", I'd argue that these plots are showing good capture of age 1 and age 2 fish as well. 
  
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
  
  ggplot(gn_cpe_20y[species == "walleye" & est.age %in% c(0:3)], aes(year, cpe, group = est.age))+
    geom_path(aes(color = est.age))+
    # geom_smooth(method = "loess")+
    facet_wrap(~lake.name, scales = "free")
  
  
  
 ##time lag recruitments to their birth year
  
  gn_cpe_20y[ , birth_year := year-est.age ,]
  
  ggplot(gn_cpe_20y[species == "walleye" & est.age %in% c(0:8)], aes(birth_year, cpe, group = est.age))+
    geom_path(aes(color = est.age))+
    # geom_smooth(method = "loess")+
    facet_wrap(~lake.name, scales = "free")
  


  ##############Leech lake perch age 2########################################
  leech_yep_est_all <- mn_data %>% 
    filter(lake.name == "Leech" & 
             species == "yellow_perch" & 
             alk %in% c("year") &
             sampling.method == "Standard gill net sets") %>% 
    collect()
  
  #exploring 
  leech_yep_est_all %>%
    filter(year == 2009) %>% 
    group_by(est.age) %>% 
    count()
  
  leech_yep_est_all %>% 
    filter(year %in% c(2008, 2009)) %>% 
    ggplot() +
    geom_density(aes(length, fill = as.factor(year)))
  
  leech_yep_est_all %>% 
    filter(year %in% c(2008, 2009)) %>% 
    ggplot() +
    geom_density(aes(length, fill = as.factor(est.age))) +
    facet_wrap(~year)
  
  leech_yep_est_all %>% 
    filter(year %in% c(2008, 2010)) %>% 
    filter(!is.na(age)) %>% 
    ggplot() +
    geom_density(aes(length, fill = as.factor(age))) +
    facet_wrap(~year)
  
  leech_yep_est_all %>% 
    filter(!is.na(age)) %>% 
    ggplot() +
    geom_density(aes(length, fill = as.factor(age))) 
  
  leech_yep_est_all %>%
    filter(year == 2008) %>% 
    group_by(est.age) %>% 
    count()
  
  leech_yep_est_all %>%
    filter(year == 2010) %>% 
    group_by(est.age) %>% 
    count()
  
  leech_yep_est_all %>% 
    filter(year == 2009) %>% 
    group_by(alk.age.str, alk, alk.n) %>% 
    count()
  
  mn_data %>% 
    filter(lake.name == "Leech" & 
             species == "yellow_perch" & 
             sampling.method == "Standard gill net sets") %>% 
    collect() %>% 
    distinct(year,
             alk) %>%  
    arrange(year) %>% 
    print(n = nrow(.))
  
  
  leech_yep_est_cpe_all <- leech_yep_est_all %>% 
    group_by(total.effort.ident, est.age) %>% 
    mutate(
      count = sum(species == "yellow_perch"),
      cpe = count / total.effort
    )  %>% 
    distinct(est.age,
             year,
             count,
             cpe) %>% 
    ungroup() %>%
    complete(
      nesting(year),
      est.age,
      fill = list(count = 0, cpe = 0)  # Fill missing combinations with zeros
    )
    
  
  leech_yep_est_cpe_all %>% 
    filter(est.age < 6) %>% 
    mutate(birth_year = year - est.age) %>% 
    ggplot() +
    geom_line(aes(year, cpe, group = est.age, color = as.factor(est.age))) +
    geom_point(aes(year, cpe, group = est.age, color = as.factor(est.age))) +
    geom_hline(yintercept = c(3), linetype = "dashed", color = "red")
  
  leech_yep_est_cpe_all %>% 
    filter(est.age < 6) %>% 
    mutate(birth_year = year - est.age) %>% 
    ggplot() +
    geom_line(aes(birth_year, cpe, group = est.age, color = as.factor(est.age))) +
    geom_point(aes(birth_year, cpe, group = est.age, color = as.factor(est.age))) +
    geom_hline(yintercept = c(3), linetype = "dashed", color = "red")
  
  leech_yep_est_cpe_all %>% 
    filter(est.age == 2) %>% 
    mutate(birth_year = year - est.age) %>% 
    ggplot() +
    geom_line(aes(year, cpe, group = est.age, color = as.factor(est.age))) +
    geom_point(aes(year, cpe, group = est.age, color = as.factor(est.age))) +
    geom_hline(yintercept = c(3, 1), linetype = "dashed", color = "red")
  
  #walleye and pike cpe
  walleye_cpe <- mn_data %>% 
    filter(lake.name == "Leech" & species %in% c("walleye", "northern_pike") & sampling.method == "Standard gill net sets") %>% 
    collect() %>% 
    group_by(total.effort.ident) %>% 
    mutate(count = sum(species == "walleye" | species == "northern_pike"),
           cpe = count/total.effort) %>% 
    distinct(year,
             count,
             cpe)
  #write_csv(walleye_cpe, "leech_wae_nop_cpe_all_ages.csv")
  #is there any pattern between the two?
  ggplot() +
    geom_line(data = leech_yep_est_cpe_all %>% 
                filter(est.age == 2), aes(year, cpe)) +
    geom_line(data = walleye_cpe %>% 
                filter(year >= 2008), aes(year, cpe))
  
  walleye_cpe <- walleye_cpe %>% 
    left_join(leech_yep_est_cpe_all %>% 
                filter(est.age == 2), by = c("year"), suffix = c("_walleye", "_yep"))
  
  walleye_cpe %>% 
  ggplot() +
    geom_point(aes(cpe_walleye, cpe_yep)) +
    geom_smooth(aes(cpe_walleye, cpe_yep), method = "lm")
  
  ####aggregation 
  
  #secchi
  secchi <- read_csv("G:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Leech Yellow Perch Recruitment/Data/dnr_wqp_secchi.csv") 
  secchi_leech <- secchi %>% 
    filter(site_id == "nhdhr_120018981") %>% 
    filter(ResultMeasureValue != 0 &
             OrganizationIdentifier == "MNPCA" &
             month(ActivityStartDate) == c(6, 7, 8)) %>% 
    group_by(MonitoringLocationIdentifier, year(ActivityStartDate)) %>% 
    summarise(mean_secchi = mean(ResultMeasureValue))  %>% 
    group_by(`year(ActivityStartDate)`) %>% 
    summarise(secchi_mean_summer = mean(mean_secchi)) %>% 
    rename(year = `year(ActivityStartDate)`)
  rm(secchi)
  
  #walleye cpe
  wae_nop_cpe <- read_csv("G:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Leech Yellow Perch Recruitment/Data/leech_wae_nop_cpe_all_ages.csv") %>% 
    rename(pred_cpe = cpe) %>% 
    select(year,
           pred_cpe)
  
  #snow depth 
  snow_depth <- read_csv("G:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Leech Yellow Perch Recruitment/Data/daily_snowdepth_temps_FederalDamMN.csv")
  snow_depth_leech <- snow_depth %>% 
    filter(year(DATE) >= 2006) %>% 
    arrange(DATE) %>% 
    fill(SNWD, .direction = "downup") 
  
  #temp, ice
  ice <- read_csv("G:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Leech Yellow Perch Recruitment/Data/leech_ann_summary_temps.csv")
  leech_ice <- ice %>% 
    select(year, 
           ice_on_date,
           ice_off_date,
           winter_dur_0_4) %>% 
    rename(ice_duration = winter_dur_0_4)
  
  #snow_depth
  snow_depth <- read_csv("G:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Leech Yellow Perch Recruitment/Data/leech_ann_snow_inchdays.csv") %>% 
    rename(year = winter_endyear)
  
  #ice out winds
  winds <- read_csv("G:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Leech Yellow Perch Recruitment/Data/leech_ann_iceoutwinds.csv") %>% 
    select(year,
           max_wind_30d_postice)
    
  
  #combining
  le_yep <- leech_yep_est_cpe_all %>% 
    filter(est.age == 2) %>% 
    select(year, cpe) %>% 
    rename(age2_yep_cpe = cpe) %>% 
    mutate(y0 = year - 2,
           y1 = year -1,
           y2 = year) %>% 
    #secchi
    left_join(secchi_leech, by = c("y0" = "year")) %>% 
    rename(y0_secchi = secchi_mean_summer) %>% 
    left_join(secchi_leech, by = c("y1" = "year")) %>% 
    rename(y1_secchi = secchi_mean_summer) %>% 
    left_join(secchi_leech, by = c("y2" = "year")) %>% 
    rename(y2_secchi = secchi_mean_summer) %>% 
    #pred data
    left_join(wae_nop_cpe, by = c("y0" = "year")) %>% 
    rename(y0_pred_cpe = pred_cpe) %>% 
    left_join(wae_nop_cpe, by = c("y1" = "year")) %>% 
    rename(y1_pred_cpe = pred_cpe) %>% 
    left_join(wae_nop_cpe, by = c("y2" = "year")) %>% 
    rename(y2_pred_cpe = pred_cpe) %>% 
    #ice duration
    left_join(leech_ice, by = c("y0" = "year")) %>% 
    rename(y0_ice_on = ice_on_date,
           y0_ice_off = ice_off_date,
           y0_ice_duration = ice_duration) %>% 
  left_join(leech_ice, by = c("y1" = "year")) %>% 
    rename(y1_ice_on = ice_on_date,
           y1_ice_off = ice_off_date,
           y1_ice_duration = ice_duration) %>% 
    left_join(leech_ice, by = c("y2" = "year")) %>% 
    rename(y2_ice_on = ice_on_date,
           y2_ice_off = ice_off_date,
           y2_ice_duration = ice_duration) %>% 
    #snow depth
    left_join(snow_depth, by = c("y0" = "year")) %>% 
    rename(y0_snow_inch_days = snow_inch_days) %>% 
    left_join(snow_depth, by = c("y1" = "year")) %>% 
    rename(y1_snow_inch_days = snow_inch_days) %>% 
    left_join(snow_depth, by = c("y2" = "year")) %>% 
    rename(y2_snow_inch_days = snow_inch_days) %>% 
    #wind stuff
    left_join(winds, by = c("y0" = "year")) %>% 
    rename(y0_max_wind_post_ice = max_wind_30d_postice) %>% 
    left_join(winds, by = c("y1" = "year")) %>% 
    rename(y1_max_wind_post_ice = max_wind_30d_postice) %>% 
    left_join(winds, by = c("y2" = "year")) %>% 
    rename(y2_max_wind_post_ice = max_wind_30d_postice) %>% 
    #renaming columns to match function
    select(-y0, -y1, -y2) %>% 
    rename(recruitment_index = age2_yep_cpe) %>% 
    mutate(good_bad = case_when(recruitment_index <= 3 ~ 0,
                                TRUE ~ 1)) %>% 
    filter(!is.na(y2_ice_off))

  
  #############plotting data##################################
  library(rpart)
  
  
  Merge_and_Analyze <- function(combined_lagged_data,cutoff.use,spp.name,loc,age_r,team_name) {
    #trouble shooting
    combined_lagged_data <- le_yep
    cutoff.use <- 1
    spp.name <- "yep"
    loc <- "leech"
    age_r <- "2"
    team_name <- "dummiez"
    
    # Set seed for reproducibility
    set.seed(123)
    
    # Drop the 'year' column
    # The 'year' column is not needed for the analysis, so we remove it
    combined_lagged_data2 <- combined_lagged_data[, !(colnames(combined_lagged_data) %in% c('year'))]
    
    # Initialize an empty list to store results for each covariate
    results_list <- list()
    
    # Calculate median recruitment index as a reference line CHANGE THIS BASED ON WHAT YOUR RECRUITMENT CUTOFF IS 
    recruitment_cutoff <- cutoff.use
    
    #set up some names
    good_bad.name <- colnames(combined_lagged_data2)[(grep("good_bad", colnames(combined_lagged_data2)))]
    r.index.name <- colnames(combined_lagged_data2)[(grep("recruitment_index", colnames(combined_lagged_data2)))]
    
    # Loop through each covariate in the dataset, excluding 'good_bad' w/ dataset that doesn't contain year
    for (covariate_name in colnames(combined_lagged_data2)[!colnames(combined_lagged_data2) %in% c(good_bad.name,r.index.name)]) {
      # troubleshoot
      # covariate_name <- colnames(combined_lagged_data2)[!colnames(combined_lagged_data2) %in% c(good_bad.name,r.index.name)][6]
      
      # Subset data to include only 'good_bad' and the covariate of interest
      analysis_data <- combined_lagged_data2[, c(good_bad.name,r.index.name, covariate_name)]
      colnames(analysis_data)<-c('good_bad',r.index.name,covariate_name) #added
      
      # Perform regression tree analysis using rpart
      # Create a regression tree model to predict good or bad recruitment using the covariate and good bad index 
      analysis_data2<-analysis_data[,c('good_bad',covariate_name)]
      model <- rpart(good_bad ~ ., data = analysis_data2, method = 'anova')
      
      # Check if model has splits
      # Ensure that the model has identified a split; if no split is found, skip to the next covariate
      if (is.null(model$splits)) {
        next
      }
      
      # Identify the environmental threshold and determine the 'bad' side
      # Extract the value of the first split from the model, which represents the threshold and store it in split_value
      split_value <- model$splits[1, 'index']
      
      # Count the number of good and bad recruitment cases on each side of the split this will tell us which side is the "bad" side, counts up good and bad recruitment on either side 
      good_left <- sum(analysis_data$good_bad[analysis_data[[covariate_name]] < split_value] == 1, na.rm = TRUE)
      good_right <- sum(analysis_data$good_bad[analysis_data[[covariate_name]] >= split_value] == 1, na.rm = TRUE)
      bad_left <- sum(analysis_data$good_bad[analysis_data[[covariate_name]] < split_value] == 0, na.rm = TRUE)
      bad_right <- sum(analysis_data$good_bad[analysis_data[[covariate_name]] >= split_value] == 0, na.rm = TRUE)
      total_left <- good_left + bad_left
      total_right <- good_right + bad_right
      
      # Calculate medians and standard deviations for each side of the split
      # These metrics help describe the distribution of the environmental variable on either side of the threshold
      med_right <- median(analysis_data[[r.index.name]][analysis_data[[covariate_name]] >= split_value], na.rm = TRUE)
      med_left <- median(analysis_data[[r.index.name]][analysis_data[[covariate_name]] < split_value], na.rm = TRUE)
      sd_right <- sd(analysis_data[[r.index.name]][analysis_data[[covariate_name]] >= split_value], na.rm = TRUE)
      sd_left <- sd(analysis_data[[r.index.name]][analysis_data[[covariate_name]] < split_value], na.rm = TRUE)
      
      # Calculate my.q2 and my.q4
      # These metrics help determine the proportion of bad and good recruitment on each side of the threshold
      #remember: robust predictor, my.q2 = 1; my.q4 = 0 
      if (good_right <= good_left) { # Right side is bad
        my.q2 <- bad_right / (good_right + bad_right) # Proportion of bad recruitment on the right side
        my.q4 <- good_right / (good_left + good_right) # Proportion of good recruitment on the right side
        bad_side <- 'right' # Indicate that the right side is the bad side
      } else { # Left side is bad
        my.q2 <- bad_left / (good_left + bad_left) # Proportion of bad recruitment on the left side
        my.q4 <- good_left / (good_left + good_right) # Proportion of good recruitment on the left side
        bad_side <- 'left' # Indicate that the left side is the bad side
      }
      
      # Store all calculated metrics, including the split value, bad side, medians, standard deviations, and proportions
      results <- data.frame(
        covariate = covariate_name, # Include the covariate name used in the analysis
        split_value = split_value,
        bad_side = bad_side,
        my.q2 = my.q2,
        my.q4 = my.q4,
        med_right = med_right,
        med_left = med_left,
        sd_right = sd_right,
        sd_left = sd_left
      )
      
      # Append the results to the results list
      results_list[[covariate_name]] <- results
      
      # Plot the data points with the threshold using ggplot2
      # Remove NA values for plotting purposes
      plot_data <- na.omit(data.frame(recruitment_index = analysis_data[[r.index.name]], covariate = analysis_data[[covariate_name]], 
                                      good_bad = as.factor(analysis_data$good_bad)))
      
      # Create a scatter plot using ggplot2 to visualize the recruitment index with the threshold
      p <- ggplot(plot_data, aes(x = covariate, y = recruitment_index, color = good_bad)) +
        geom_point() +
        geom_vline(xintercept = split_value, color = 'darkred', linetype = 'dashed') +
        geom_hline(yintercept = recruitment_cutoff, color = 'darkgreen', linetype = 'dashed') +
        labs(title = paste('Threshold Analysis for', covariate_name),
             x = covariate_name,
             y = 'Recruitment Index') +
        scale_color_manual(values = c('0' = 'red', '1' = 'blue'),
                           labels = c('0' = 'Bad Recruitment', '1' = 'Good Recruitment')) +
        theme_minimal() +
        theme(legend.position = 'right', legend.justification = c(1, 0), legend.box.margin = margin(10, 10, 10, 10))
      
      print(p)
      
      #save the plot - use spp.name for species name, loc for location
      #save the plot 
      save_plot(p, location = loc, species = spp.name, age_recruitment = age_r, 
                group_contact = team_name, plot_type = paste(covariate_name,"scatterplot_threshold",sep="_"))
      
      
      
    }
    
    # Combine all results into a single dataframe
    combined_results <- do.call(rbind, results_list)
    
    # Write the combined results to a CSV file
    write.csv(combined_results, file = paste(loc,spp.name,age_r,team_name,"combined_analysis_results.csv",sep="_"), row.names = FALSE)
    
    #save RDATA file
    
    # Return the combined results dataframe
    return(combined_results)
  }
  
  analysis_results <- Merge_and_Analyze(combined_lagged_data = le_yep,
                                        cutoff.use=3,
                                        spp.name="Yellow Perch",
                                        loc="test.loc",
                                        age_r="2yr",
                                        team_name="teamA")
analysis_results  
