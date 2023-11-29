library(tidyverse)
library(ggnewscale)
library(reshape)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)
library(plotly)
library(htmlwidgets)
library(htmltools)

outdir_folder <- paste0(maindir,"Postprocessing/Figures/Interactive scatters/")
dir.create(outdir_folder, showWarnings = F)

if(age_type == "adult"){
  variables         <- c("prev_bmi_l185", "prev_bmi_30")
} else{
  variables         <- c("prev_bmi_neg2sd", "prev_bmi_2sd")
}

age_groups        <- c("ageStd")
names(age_groups) <- c("20+ year olds")
my_age_group      <- "ageStd"
level.year        <- plot.start.year


#### 1 LOAD DATA
data_level  <- read_data_level(variables, sexes, age_type = "ageStd", region_level = "Country") %>%
  filter(age_group %in% age_groups) %>%
  filter(year == level.year) 

data_timechange <- read_data_timechange(variables, sexes, age_groups = age_groups, age_type = "ageStd") %>%
  filter(start.year == plot.start.year & end.year == plot.end.year) 



## 2 GENERATE PLOTLY SCATTERS OF CHANGE VERSES STARTING LEVEL
plotly.ps <- list()

for (var in variables){
  for(my_sex in sexes){
    
    plotly.ps[[paste("Scatter", my_sex, var)]] <- interactive_scatter_timechange_v_level(subset_level = data_level, 
                                                                                         subset_timechange = data_timechange, 
                                                                                         level_var = var, 
                                                                                         timechange_var = var, 
                                                                                         my_sex =  my_sex, 
                                                                                         my_age_group = my_age_group, 
                                                                                         plot.start.year = plot.start.year, 
                                                                                         plot.end.year = plot.end.year, 
                                                                                         level.year = level.year,
                                                                                         corrAnnotation = T)
    
    saveWidget(plotly.ps[[paste("Scatter", my_sex, var)]], paste0(outdir_folder, "Change in ", get_var_longname(var)," ", plot.start.year, " - ", plot.end.year, " v level in ",level.year, " ", my_sex, ".html"), selfcontained = F, libdir = "lib")
  }
}

