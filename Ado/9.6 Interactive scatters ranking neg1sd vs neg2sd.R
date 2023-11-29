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

variables <- c("prev_bmi_neg2sd", 
               "prev_bmi_neg1sd")

age_groups        <- c("ageStd")
names(age_groups) <- c("20+ year olds")
my_age_group      <- "ageStd"


#### 1 LOAD DATA
data_level <- read_data_level(variables, sexes, age_type = "ageStd", region_level = "Country") %>%
  filter(age_group == my_age_group) %>%
  filter(year %in% seq(plot.start.year, plot.end.year)) %>%
  group_by(year, age_group,sex, variable) %>%
  mutate(ranking = rank(mean)) %>%
  ungroup()

data_timechange <- read_data_timechange(variables, sexes, age_groups) %>%
  filter(age_group == my_age_group & start.year == plot.start.year & end.year == plot.end.year) %>%
  group_by(age_group,sex, variable) %>%
  mutate(ranking = rank(mean)) %>%
  ungroup()


## 2 GENERATE PLOTLY LEVEL SCATTERS
plotly.ps <- list()

xvar <- "prev_bmi_neg2sd"
yvar <- "prev_bmi_neg1sd"

for(my_sex in sexes){
  
  plotly.ps[[paste("Scatter", my_sex, xvar, yvar, "dynamic")]] <- interactive_scatter_ranking(subset = data_level, xvar = xvar, yvar = yvar, my_sex = my_sex,
                                                                                      my_age_group = my_age_group, plot.year = NULL, dynamic =T)
  
  saveWidget(plotly.ps[[paste("Scatter", my_sex, xvar, yvar, "dynamic")]], paste0(outdir_folder, str_to_sentence(get_var_longname(yvar))," vs ",get_var_longname(xvar), " ", ifelse(my_sex == "female", "girls", "boys"), "ranking.html"), selfcontained = F, libdir = "libranking")
}
