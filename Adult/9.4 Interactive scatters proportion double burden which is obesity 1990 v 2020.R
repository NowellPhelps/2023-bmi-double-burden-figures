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
  variables <-  c("prev_bmi_30_proportion_double_burden")
} else{
  variables <-  c("prev_bmi_2sd_proportion_double_burden")
}

age_groups        <- c("ageStd")
names(age_groups) <- c("20+ year olds")
my_age_group      <- "ageStd"

data_level <- read_data_level(variables, sexes, age_type = "ageStd", region_level = "Country") %>%
  filter(age_group == my_age_group & (year == plot.start.year |year == plot.end.year))

plotly.ps <- list()
for (var in variables){
  
  for(my_sex in sexes){

    plotly.ps[[paste("Scatter", my_sex, var)]] <- interactive_scatter_level_v_level_comparison(subset = data_level, xvar = var, yvar = var, my_sex = my_sex,
                                                                                      my_age_group = my_age_group, 
                                                                                      xyear = plot.start.year, 
                                                                                      yyear = plot.end.year)

    saveWidget(plotly.ps[[paste("Scatter", my_sex, var)]], paste0(outdir_folder, str_to_sentence(get_var_longname(var))," ", plot.start.year, " v ", plot.end.year, " ",my_sex, ".html"), selfcontained = F, libdir = "lib")
    }
}




