library(tidyverse)
library(ggnewscale)
library(reshape)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)

if (type == "double burden"){
  variables <- c("prev_bmi_l185","prev_bmi_30")
  names(variables)<- c("Underweight","Obesity")
  fill_scale <- fill_scale_double_burden
  col_scale <- scale_colour_manual(values = c("overall" = "black"), labels = c("Double burden"))
  
  offset <- 30
} else if(type == "obesity"){
  variables <- c("prev_bmi_30_35","prev_bmi_35_40","prev_bmi_40")
  names(variables)<- c("30 to <35","35 to <40","\u2265 40")
  fill_scale <- fill_scale_all
  col_scale <- scale_colour_manual(values = c("overall" = "black"), labels = c("Total obesity"))
  
  offset <- 21
} 



# Read data
data_timechange <- read_data_timechange(variables, sexes, age_groups, age_type = "ageStd", region_level = "Country", timechange_type = "absolute") %>%
  filter(start.year == plot.start.year, end.year == plot.end.year)

overall_change <- data_timechange %>%
  group_by(start.year, end.year, age_group, Country, sex) %>%
  summarise(overall_change = sum(mean))%>%
  ungroup()

data_timechange <- left_join(data_timechange, overall_change)

data_timechange$variable <- factor(data_timechange$variable, levels = variables)

ps <- list()

for (my_sex in sexes){
    for (my_age_group in age_groups){
        for(countrycols in c(F,T)){
          
          subset <- data_timechange %>% 
            filter(sex == my_sex, age_group == my_age_group, start.year == plot.start.year, end.year == plot.end.year)
          
          
          ps[[paste(my_sex, my_age_group, "horizontal", "ascending")]] <- decomposition_plot(subset, my_sex, my_age_group, plot.start.year, plot.end.year,  type, returnTitle = T)
          
        }
 }
}


blank <- grid.rect(gp=gpar(col=NA, fill = NA))

cairo_pdf(paste0(outdir_folder, "Decomposition plot change in ", type, " by country ",plot.start.year," ",plot.end.year,".pdf"), height = 22, width = 30, onefile=T)

for (my_age_group in age_groups){
    plot.title <- switch(my_age_group, "ageStd" ="20+ year olds","young"="20-39 year olds","mid"="40-64 year olds", "old"="65+ year olds")
    
    grid.arrange(
      arrangeGrob(
        textGrob(plot.title,hjust=0,just = c("left"),x = unit(0.01, "npc"),gp = gpar(col = "black", fontsize = 18)),
        blank,
        ps[[paste("female", my_age_group, "horizontal", "ascending")]],
        blank,
        ps[[paste("male", my_age_group, "horizontal", "ascending")]],
        nrow = 5,
        ncol = 1,
        heights = c(1,.01,10,0.1,10)
      )
    )
    
}
dev.off()




