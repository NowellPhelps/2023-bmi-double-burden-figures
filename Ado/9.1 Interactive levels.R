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

outdir_folder <- paste0(maindir,"Postprocessing/Figures/Interactive levels/")
dir.create(outdir_folder, showWarnings = F)

variables <- c("prev_bmi_neg2sd", 
               "prev_bmi_2sd", 
               "prev_double_burden", 
               "prev_bmi_2sd_proportion_double_burden")

age_groups        <- c("ageStd")
names(age_groups) <- c("20+ year olds")
my_age_group      <- "ageStd"

#### 1 LOAD DATA
data_level <- read_data_level(variables, sexes, age_type = "ageStd", region_level = "Country") %>%
  filter(age_group == my_age_group) %>%
  filter(year %in% seq(plot.start.year, plot.end.year))

data_timechange <- read_data_timechange(variables, sexes, age_groups) %>%
  filter(age_group == my_age_group & start.year == plot.start.year & end.year == plot.end.year)


## 2 GENERATE PLOTLY LEVEL AND TIMECHANGE ANIMATIONS
plotly.ps <- list()

for (var in variables){
  for(my_sex in sexes){
    
    plotly.ps[[paste("Level", my_sex, var, my_age_group, "level")]]   <-  interactive_level(subset =  data_level, yvar = var, my_sex = my_sex,
                                                                                                    my_age_group = my_age_group, plot.year = NULL, dynamic = TRUE, ordertype = "level", data_type = "level")

    plotly.ps[[paste("Arrow", my_sex, var, my_age_group, "level")]]   <-  interactive_timechange_arrow(subset =  data_level, yvar = var, my_sex = my_sex,
                                                                                               my_age_group = my_age_group, start.year = plot.start.year, end.year = plot.end.year, ordertype = "level", data_type = "level")
    
    plotly.ps[[paste("Timechange", my_sex, var, my_age_group, "level")]]   <-  interactive_timechange(subset =  data_timechange, 
                                                                                                     var = var, 
                                                                                                     my_sex = my_sex,
                                                                                                     my_age_group = my_age_group, 
                                                                                                     plot.start.year = plot.start.year, 
                                                                                                     plot.end.year = plot.end.year, 
                                                                                                     ordertype = "level")
    

    a <- manipulateWidget::combineWidgets(plotly.ps[[paste("Level", my_sex, var, my_age_group, "level")]],
                                          plotly.ps[[paste("Arrow", my_sex, var, my_age_group, "level")]],
                                          plotly.ps[[paste("Timechange", my_sex, var, my_age_group, "level")]],
                                          ncol = 1, height = 2100, width = 1500)

    saveWidget(a, paste0(outdir_folder, str_to_sentence(get_var_longname(var)), " level " , ifelse(my_sex == "female", "girls", "boys"), ".html"), selfcontained = F, libdir = "lib")
  }
}



