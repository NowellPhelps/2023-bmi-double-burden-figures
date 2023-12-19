# Appendix maps of level, change and PP of increase

# ADULTS
# Appendix Figure 7 - underweight - level in 1990, 2021, change from 1990 to 2021, and PP increase (8 maps)
# Appendix Figure 8 - obesity - level in 1990, 2021, change from 1990 to 2021, and PP increase (8 maps)

# ADOLESCENTS
# Appendix Figure 16 - thinness - level in 1990, 2021, change from 1990 to 2021, and PP increase (8 maps)
# Appendix Figure 17 - obesity - level in 1990, 2021, change from 1990 to 2021, and PP increase (8 maps)

library(tidyverse)
library(ggnewscale)
library(reshape)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)


figsuffix                <- ""
figsuffix                <- ifelse(figsuffix == "", "", paste0(" ", figsuffix))
outdir_folder            <- paste0(outdir,"Numbered/")


if(age_type == "adult"){
  variables    <- c("prev_bmi_l185", "prev_bmi_30")
  figNums       <- c(5, 6)
  
} else{
  variables    <- c("prev_bmi_neg2sd", "prev_bmi_2sd")
  figNums       <- c(17,18)
}

age_groups   <- c("ageStd")
my_age_group <- "ageStd"

figNum <- 10 

# LOAD DATA
data_timechange <- read_data_timechange(variables, sexes, age_groups, age_type = "ageStd", region_level = "Country", timechange_type = "absolute") %>%
  filter(start.year == plot.start.year, end.year == plot.end.year)
data_level      <- read_data_level(variables, sexes, age_type = "ageStd", region_level = "Country") %>%
  filter(age_group %in% age_groups) 

# PLOT
ps <- list()
for(my_variable in variables){
  print(my_variable)
  
  for (my_sex in sexes){
    for(my_age_group in age_groups){
  
      for(my_year in c(plot.start.year, plot.end.year)){
        subset_level <- data_level %>% 
          filter(age_group == my_age_group, variable == my_variable)
        
        ps[[paste("Map level", my_variable, my_sex, my_age_group, my_year)]] <- maps_level_plots(data = subset_level, variable = my_variable, my_sex, plot.year = my_year, appendix = T, figNum = 5)  
      }
      
      subset_timechange <- data_timechange %>% filter(age_group == my_age_group, variable == my_variable)
      
      ps[[paste("Map timechange", my_variable, my_sex, my_age_group)]] <- maps_timeChg_plots(data = subset_timechange, variable = my_variable, my_sex, my_plot.start.year = plot.start.year, my_plot.end.year = plot.end.year,appendix = T, figNum = 5)
      
      ps[[paste("Map PP", my_variable, my_sex, my_age_group)]] <- maps_timeChg_PP_plots(data = subset_timechange, my_variable, my_sex, my_age_group = "ageStd", plot.start.year, plot.end.year,appendix = T, figNum = 5)

    }
  }
}



################ PLOT MAPS LEVEL AND TIMECHANGE ################################
blank <- grid.rect(gp=gpar(col=NA, fill = NA))

for(i in 1:length(variables)){
  my_variable <- variables[i]
  figNum      <- figNums[i]
  
  cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 11, width = 21, onefile=T)
  
  grid.arrange(
    
    arrangeGrob(
      textGrob(ifelse(age_type == "adult", "Women", "Girls"),hjust=0,just = c("left"),x = unit(0.01, "npc"),gp = gpar(col = "black", fontsize = 20)),
      
      arrangeGrob(blank,
                  
                  ps[[paste("Map level", my_variable, "female", "ageStd", plot.start.year)]],
                  blank,
                  
                  ps[[paste("Map timechange", my_variable, "female", "ageStd")]],
                  blank,
                  nrow = 1, widths = c(1.5,10, 1.5, 10, 0.5)),
      
      blank,
      
      arrangeGrob(blank,
                  ps[[paste("Map level", my_variable, "female", "ageStd", plot.end.year)]],
                  blank,
                  
                  ps[[paste("Map PP", my_variable, "female", "ageStd")]],
                  blank,
                  nrow = 1, widths = c(1.5,10, 1.5, 10, 0.5)),
      
      
      ncol = 1, heights = c(3,20,1,20))
  )
  
  grid.arrange(
    
    arrangeGrob(
      textGrob(ifelse(age_type == "adult", "Men", "Boys"),hjust=0,just = c("left"),x = unit(0.01, "npc"),gp = gpar(col = "black", fontsize = 20)),
      
      arrangeGrob(blank,
                  
                  ps[[paste("Map level", my_variable, "male", "ageStd", plot.start.year)]],
                  blank,
                  
                  ps[[paste("Map timechange", my_variable, "male", "ageStd")]],
                  blank,
                  nrow = 1, widths = c(1.5,10, 1.5, 10, 0.5)),
      
      blank,
      
      arrangeGrob(blank,
                  ps[[paste("Map level", my_variable, "male", "ageStd", plot.end.year)]],
                  blank,
                  ps[[paste("Map PP", my_variable, "male", "ageStd")]],
                  blank,
                  nrow = 1, widths = c(1.5,10, 1.5, 10, 0.5)),
      
      
      ncol = 1, heights = c(3,20,1,20))
  )
  
  dev.off()
}


