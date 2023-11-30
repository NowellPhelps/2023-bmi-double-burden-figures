library(tidyverse)
library(plotly)
library(RColorBrewer)
library(rgdal)
library(gridExtra)
library(grid)
library(ggnewscale)
library(egg)
library(ggthemes)
library(scales)


source(paste0(functionsDir,"0.1 Region Superregion palette.R"))
source(paste0(functionsDir,"0.1 BMI Prevalence palette.R"))

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}


get_var_longname <- function(var){
  longname <- switch(var,
                     "mean_bmi" = "mean BMI",
                     "prev_double_burden" = "prevalence of double burden",
                     "prev_bmi_30_proportion_double_burden" = "Proportion of combined burden composed of obesity",
                     "prev_bmi_30"   = "prevalence of obesity",
                     "prev_bmi_l185" = "prevalence of underweight",
                     "prev_bmi_35"   = "prevalence of severe obesity",
                     "prev_bmi_35_proportion_obesity" = "Proportion of obesity composed of class II and class III",
                     "prev_bmi_40"  = "prevalence of morbid obesity", 
                     "prev_bmi_40_proportion_double_burden" = "proportion of  double burden composed of morbid obesity",
                     "prev_bmi_neg2sd" = "prevalence of thinness",
                     "prev_bmi_neg1sd" = "prevalence of total underweight",
                     "prev_bmi_2sd" = "prevalence of obesity",
                     "prev_bmi_30_35_proportion_obesity" = "Proportion of total obesity composed of class one obesity",
                     "prev_bmi_35_40_proportion_obesity" = "Proportion of total obesity composed of class two obesity",
                     "prev_bmi_40_proportion_obesity" =   "Proportion of total obesity composed of class three obesity",
                     "prev_bmi_2sd_proportion_double_burden" = "Proportion of combined burden composed of obesity",)
  
  return(longname)
}



read_countrylist <- function(country.list.name.tmp, type){
  countrylist.tmp <- read.csv(paste0(maindir,"Covariates/", country.list.name.tmp))
  
  if(type == "new"){
    countrylist.tmp$Superregion <- factor(countrylist.tmp$Superregion, levels = sregion_order)
  }
  
  return(countrylist.tmp)
}

countrylist    <- read_countrylist(country.list.name, type = "new")
countrylistold <- read_countrylist(country.list.old.name, type = "old")


read_source_data <- function(age_type){
  source_data <- read.csv(paste0(maindir,"Postprocessing/Data files/sources_summary_", age_type,".csv")) %>%
    mutate(sex = ifelse(sex == 1, "male",
                        ifelse(sex == 2, "female", "both")))
  return(source_data)
}


read_data_level <- function(variables, sexes, age_type, region_level, variable_type = "prev", includePP = F){
  
  data_level <- NULL
  for (my_sex in sexes){
    for (my_variable in variables){
      data_level.tmp <- read.csv(paste0(indir_est,"Model",modelnum,"_",my_sex,"_",my_variable, "_", age_type, "_",region_level,"_Means.csv")) %>%
        mutate(sex = my_sex, variable = my_variable, region_level = region_level)
      
      if(variable_type == "prev"){
        data_level.tmp <- data_level.tmp %>%
          mutate(mean = mean*100, l = l*100, u = u*100, se = se*100)
      }
      
      if(my_variable == "prev_bmi_30_proportion_double_burden" | my_variable == "prev_bmi_2sd_proportion_double_burden"){
        if(includePP == F){
          data_level.tmp <- data_level.tmp %>% select(-c(PP_obesity_larger_than_underweight))
        }
      }
      
      data_level      <- rbind(data_level, data_level.tmp)
    }
  } 
  rm(data_level.tmp)
  
  if(region_level == "Country"){
    data_level <- data_level %>% 
      dplyr::rename(Country = country) %>%
      left_join(countrylistold, by = "Country") %>%
      select(-c(Country, Region, Superregion)) %>%
      left_join(countrylist, by = "iso")
  }
  
  return(data_level)
}




read_data_timechange <- function(variables, sexes, age_groups, age_type = "ageStd", region_level = "Country", timechange_type = "absolute", variable_type = "prev"){
  
  data_timechange <- NULL
  for (my_sex in sexes){
    for (my_variable in variables){
      data_timechange.tmp <- read.csv(paste0(indir_est,"Timechanges/Model",modelnum,"_",my_sex,"_",my_variable, "_", age_type, "_",region_level,"_timechange.csv")) %>%
        mutate(sex = my_sex, variable = my_variable) %>%
        filter(age_group %in% age_groups)
      
      data_timechange      <- rbind(data_timechange, data_timechange.tmp)
    }
  } 
  rm(data_timechange.tmp)
  
  if(timechange_type == "absolute" & variable_type == "prev"){
    data_timechange <- data_timechange %>%
      mutate(mean = mean_absolute*100, l = l_absolute*100, u = u_absolute*100, se = se_absolute*100, PP.increase = PP.increase_absolute)
    
  } else if (timechange_type == "relative"){
    data_timechange <- data_timechange %>%
      mutate(mean = mean_relative*100, l = l_relative*100, u = u_relative*100, se = se_relative*100,PP.increase = PP.increase_relative)
  } else{
    message("ERROR: timechange_type not in (absolute, relative)")
  }
  
  data_timechange <- data_timechange %>%
    select(-c(mean_absolute, l_absolute, u_absolute, se_absolute, PP.increase_absolute,
              mean_relative, l_relative, u_relative, se_relative, PP.increase_relative))
  
  
  if(region_level == "Country"){
    data_timechange <- data_timechange %>% 
      dplyr::rename(Country = country) %>%
      left_join(countrylistold, by = "Country") %>%
      select(-c(Country, Region, Superregion)) %>%
      left_join(countrylist, by = "iso")
  }
  
  return(data_timechange)
}


read_data_slopes <- function(variables, sexes, age_type = "ageStd", region_level = "Country", slope_type){
  
  data_slopes <- NULL
  for (my_sex in sexes){
    for (my_variable in variables){
      data_slopes.tmp <- read.csv(paste0(indir_est,"Slopes/Model",modelnum,"_",my_sex,"_",my_variable, "_", age_type, "_",region_level,"_",slope_type,".csv")) %>%
        mutate(sex = my_sex, variable = my_variable, level = region_level)
        
      data_slopes      <- rbind(data_slopes, data_slopes.tmp)
    }
  } 
  rm(data_slopes.tmp)
  
  if(region_level == "Country"){
    data_slopes <- data_slopes %>% 
      dplyr::rename(Country = country) %>%
      left_join(countrylistold, by = "Country") %>%
      select(-c(Country, Region, Superregion)) %>%
      left_join(countrylist, by = "iso")
  }
  
  if(slope_type == "velocity"){
    data_slopes <- data_slopes %>%
      mutate(mid.year = (start.year + end.year)/2) %>%
      select(-c(start.year, end.year))
  }
  
  return(data_slopes)
}




source(paste0(functionsDir,"0.2 Arrow plot functions.R"))
source(paste0(functionsDir,"0.2 Decomposition plot functions.R"))
source(paste0(functionsDir,"0.2 Heatmap functions.R"))
source(paste0(functionsDir,"0.2 Map plotting functions.R"))
source(paste0(functionsDir,"0.2 Plotly functions.R"))
source(paste0(functionsDir,"0.2 Scatter plot functions.R"))
source(paste0(functionsDir,"0.2 Uncertainty plot functions.R"))
source(paste0(functionsDir,"0.2 Wheel plot functions.R"))
source(paste0(functionsDir,"0.2 Sankey plot functions.R"))
source(paste0(functionsDir, "0.2 Stacked region trend plots function.R"))
source(paste0(functionsDir, "0.2 Bubble plot functions.R"))
source(paste0(functionsDir, "0.2 Change dot plot function.R"))
