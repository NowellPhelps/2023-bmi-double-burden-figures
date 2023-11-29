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

outdir_folder <- paste0(maindir,"Postprocessing/Figures/Age distributions/")
dir.create(outdir_folder, showWarnings = F)

variables <-  c("prev_bmi_30_proportion_double_burden")

age_groups        <- c("ageStd", "db_pop")

names(age_groups) <- c("20+ year olds")
my_age_group      <- "ageStd"

data_level <- read_data_level(variables = variables, sexes, age_type = "ageStd", region_level = "Country") %>%
  filter(age_group %in% age_groups & (year >= plot.start.year & year <= plot.end.year)) 

plotly.ps <- list()

for(my_sex in sexes){

  plotly.ps[[paste("Scatter", my_sex)]] <- interactive_scatter_age_v_age(subset = data_level, 
                                                                         xvar = "prev_bmi_30_proportion_double_burden", 
                                                                         yvar = "prev_bmi_30_proportion_double_burden", my_sex = my_sex, 
                                                                         xage_group = "ageStd",
                                                                         yage_group = "db_pop")

  saveWidget(plotly.ps[[paste("Scatter", my_sex)]], paste0(outdir_folder, str_to_sentence(get_var_longname("prev_bmi_30_proportion_double_burden"))," comparison age standardisation ", my_sex, ".html"), selfcontained = F, libdir = "lib")
}





