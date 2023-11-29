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
                 "prev_bmi_30", 
                 "prev_double_burden",
                 "prev_bmi_30_proportion_double_burden")
} else{
  variables <- c("prev_bmi_neg2sd", 
                 "prev_bmi_2sd", 
                 "prev_double_burden",
                 "prev_bmi_2sd_proportion_double_burden")
}



age_groups        <- c("ageStd")
my_age_group      <- "ageStd"



#### 1 LOAD DATA
data_level <- read_data_level(variables, sexes, age_type = "ageStd", region_level = "Country") %>%
  filter(age_group == my_age_group) %>%
  filter(year %in% seq(plot.start.year, plot.end.year))

## 2 GENERATE PLOTLY LEVEL SCATTERS
plotly.ps <- list()

for (vars in list(c(variables[4], variables[3]),
                  c(variables[2], variables[1]))){
  xvar <- vars[1]
  yvar <- vars[2]
  
  for(my_sex in sexes){
    
    plotly.ps[[paste("Scatter", my_sex, xvar, yvar, "dynamic")]] <- interactive_scatter(subset = data_level, xvar = xvar, yvar = yvar, size_var = "prev_double_burden", my_sex = my_sex,
                                                                                        my_age_group = my_age_group, plot.year = NULL, dynamic =T)
    
    saveWidget(plotly.ps[[paste("Scatter", my_sex, xvar, yvar, "dynamic")]], paste0(outdir_folder, str_to_sentence(get_var_longname(yvar))," vs ",get_var_longname(xvar), " ", my_sex, ".html"), selfcontained = F, libdir = "lib")
  }
}