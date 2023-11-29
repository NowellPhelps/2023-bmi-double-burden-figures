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

outdir_folder <- paste0(outdir,"Interactive scatters/")
dir.create(outdir_folder, showWarnings = F)

variables         <- c("prev_bmi_neg2sd", "prev_bmi_2sd")
age_groups        <- c("ageStd")
my_age_group      <- "ageStd"


data_timechange <- read_data_timechange(variables, sexes, age_groups) %>%
  filter(age_group == my_age_group & start.year == plot.start.year & end.year == plot.end.year)
  

## 2 GENERATE PLOTLY LEVEL SCATTERS
plotly.ps <- list()

for (vars in list(c("prev_bmi_2sd", "prev_bmi_neg2sd"))){
  
  xvar <- vars[1]
  yvar <- vars[2]

  for(my_sex in sexes){

    plotly.ps[[paste("Scatter", my_sex, xvar, yvar)]] <- interactive_scatter_timechange_v_timechange(subset = data_timechange, xvar = xvar, yvar = yvar, my_sex = my_sex,
                                                                                        my_age_group = my_age_group, plot.start.year = plot.start.year, plot.end.year = plot.end.year)

    saveWidget(plotly.ps[[paste("Scatter", my_sex, xvar, yvar)]], paste0(outdir_folder, "Change ", get_var_longname(yvar)," vs change ", get_var_longname(xvar), " ", ifelse(my_sex == "female", "girls", "boys"), ".html"), selfcontained = F, libdir = "lib")
    }
}




