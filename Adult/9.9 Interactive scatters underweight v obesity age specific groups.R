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
  variables <- c("prev_bmi_l185", 
                 "prev_bmi_30")
  age_groups <- c("young", "mid", "old")
} else{
  variables <- c("prev_bmi_neg2sd", 
                 "prev_bmi_2sd")
  age_groups <- c(5, 10, 15, 19)
}


#### 1 LOAD DATA
if(age_type == "adult"){
  data_level <- read_data_level(variables, sexes, age_type = "ageStd", region_level = "Country") %>%
    filter(age_group %in% age_groups) %>%
    filter(year %in% seq(plot.start.year, plot.end.year))
} else{
  data_level <- read_data_level(variables, sexes, age_type = "ageSpecific", region_level = "Country") %>%
    filter(age_group %in% age_groups) %>%
    filter(year %in% seq(plot.start.year, plot.end.year))
}
 
## 2 GENERATE PLOTLY LEVEL SCATTERS
plotly.ps <- list()


xvar <- variables[2]
yvar <- variables[1]

for(my_sex in sexes){
  for(my_age_group in age_groups){
    
    plotly.ps[[paste("Scatter", my_sex, my_age_group, xvar, yvar, "dynamic")]] <- interactive_scatter(subset = data_level %>% filter(age_group == my_age_group), xvar = xvar, yvar = yvar, size_var = xvar, my_sex = my_sex,
                                                                                                      my_age_group = my_age_group, plot.year = NULL, dynamic =T)
    
    saveWidget(plotly.ps[[paste("Scatter", my_sex, my_age_group, xvar, yvar, "dynamic")]], paste0(outdir_folder, str_to_sentence(get_var_longname(yvar))," vs ",get_var_longname(xvar), " ", my_sex, " ", my_age_group,".html"), selfcontained = F, libdir = "lib")
    
  }
}
