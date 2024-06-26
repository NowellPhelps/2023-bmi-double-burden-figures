##################### MODEL SPECIFIC QUANTITIES ####################################
modelnum_adult      <- 54
mod_dir_name_adult  <- "adult prev revision"

modelnum_ado        <- 53
mod_dir_name_ado    <- "ado prev revision"

comparisons_dir_name <- "54 53 Ado v adult revision"
maindir              <- paste0("D:/prev_model/Model comparisons/", comparisons_dir_name,"/")

scriptDir            <- paste0("S:/Projects/HeightProject/Original dataset/Anthropometrics/adult_bmi_analysis/Figures scripts/")
functionsDir         <- paste0(scriptDir, "Functions/")

parentdir                <- "D:/prev_model/Models/"
indir_est_adult          <- paste0(parentdir, "Model", modelnum_adult, ", ", mod_dir_name_adult,"/Postprocessing/Estimates/")
indir_est_ado            <- paste0(parentdir, "Model", modelnum_ado, ", ", mod_dir_name_ado,"/Postprocessing/Estimates/")

outdir                   <- paste0(maindir,"Figures/")
dir.create(outdir, showWarnings = FALSE)

plot.start.year          <- 1990
plot.end.year            <- 2022

sexes                    <- c("female", "male")
country.list.name            <- "country-list-2023-figures.csv"
country.list.old.name        <- "country-list-2023-new.csv"

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
source(paste0(functionsDir,"0.1 Figure utils.R"))

# Specific fucntions for comparisons below: 

get_age_longname_adults <- function(var){
  longname <- switch(var,
                     "ageStd" = "20+ year old",
                     "18-19" = "18-19 year old"
                     )
  
  return(longname)
}

get_age_longname_ado <- function(var){
  longname <- switch(var,
                     "ageStd" = "5-19 year old",
                     "19" = "19 year old")
  
  return(longname)
}

read_data_level <- function(variables, sexes, age_type, region_level, age_level = "adult"){
  
  if(age_level == "adult"){
    indir_est <- indir_est_adult
    modelnum  <- modelnum_adult
  } else if(age_level == "ado"){
    indir_est <- indir_est_ado
    modelnum  <- modelnum_ado
  }
  
  
  data_level <- NULL
  for (my_sex in sexes){
    for (my_variable in variables){
      data_level.tmp <- read.csv(paste0(indir_est,"Model",modelnum,"_",my_sex,"_",my_variable, "_", age_type, "_",region_level,"_Means.csv")) %>%
        mutate(sex = my_sex, variable = my_variable, region_level = region_level) %>%
        mutate(mean = mean*100, l = l*100, u = u*100, se = se*100)
      
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


read_data_timechange <- function(variables, sexes, age_groups, age_type = "ageStd", region_level = "Country", timechange_type = "absolute", age_level = "adult"){
  
  if(age_level == "adult"){
    indir_est <- indir_est_adult
    modelnum  <- modelnum_adult
  } else if(age_level == "ado"){
    indir_est <- indir_est_ado
    modelnum  <- modelnum_ado
  }
  
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
  
  if(timechange_type == "absolute"){
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
    data_level <- data_level %>% 
      dplyr::rename(Country = country) %>%
      left_join(countrylistold, by = "Country") %>%
      select(-c(Country, Region, Superregion)) %>%
      left_join(countrylist, by = "iso")
  }
  return(data_timechange)
}


