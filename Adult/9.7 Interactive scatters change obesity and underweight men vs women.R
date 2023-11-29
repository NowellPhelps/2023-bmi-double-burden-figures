### SCATTERS OF PROPORTION DOUBLE BURDEN AGAINST PREVALENCE DOUBLE BURDEN
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
  variables <-  c("prev_double_burden", "prev_bmi_30", "prev_bmi_l185")
} else{
  variables <-  c("prev_double_burden","prev_bmi_2sd", "prev_bmi_neg2sd")
}

age_groups        <- c("ageStd")
my_age_group      <- "ageStd"

data_timechange <- read_data_timechange(variables, sexes, age_type = "ageStd", age_groups = age_groups, region_level = "Country") %>%
  filter(age_group == my_age_group & start.year == plot.start.year & end.year == plot.end.year)


plotly.ps <- list()

for(my_variable in variables){

  plotly.ps[[paste("Scatter", my_variable)]] <- interactive_scatter_timechange_sex_v_sex(subset = data_timechange, xsex = "male", ysex = "female", my_variable = my_variable, my_age_group = my_age_group, 
                                                                                    plot.start.year, plot.end.year)

  saveWidget(plotly.ps[[paste("Scatter", my_variable)]], paste0(outdir_folder, "Sex comparison change in ", get_var_longname(my_variable)," ", plot.start.year, "-", plot.end.year, ".html"), selfcontained = F, libdir = "lib")
}





