library(tidyverse)
library(ggnewscale)
library(reshape)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)

variables    <- c("prev_bmi_2sd", "prev_bmi_neg2sd", "prev_double_burden", "prev_bmi_2sd_proportion_double_burden")
age_groups   <- c("ageStd")
my_age_group <- "ageStd"
figNum <- 10 # sets size of text for maps
outdir_folder <- paste0(maindir,"Postprocessing/Figures/Maps/")
dir.create(outdir_folder, showWarnings = FALSE)

###### LEVELS AND SLOPES MAPS FOR DOUBLE BURDEN, UNDERWEIGHT & OBESITY #########

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
        
        ps[[paste("Map level", my_variable, my_sex, my_age_group, my_year)]] <- maps_level_plots(data = subset_level, variable = my_variable, my_sex, plot.year = my_year)  
      }
      
      subset_timechange <- data_timechange %>% filter(age_group == my_age_group, variable == my_variable)
      
      ps[[paste("Map timechange", my_variable, my_sex, my_age_group)]] <- maps_timeChg_plots(data = subset_timechange, variable = my_variable, my_sex, my_plot.start.year = plot.start.year, my_plot.end.year = plot.end.year)
      
    }
  }
}

##### VELOCITY LEVEL AND TIMECHANGE MAPS FOR OBESITY 
my_variable <- "prev_bmi_2sd"

data_velocity     <- read_data_slopes(c("prev_bmi_2sd"), sexes, age_type = "ageStd",region_level = "Country", slope_type = "velocity") %>%
  dplyr::rename(year = mid.year) %>%
  filter(year == plot.end.year - 0.5 | year == plot.start.year + 0.5 ) %>%
  filter(age_group %in% age_groups)

data_change_velocity <- data_velocity %>%
  select(Country, age_group, mean, sex, variable, iso, Region, Superregion, year) %>%
  mutate(year = ifelse(year ==  plot.end.year - 0.5, "end.year", "start.year")) %>%
  pivot_wider(values_from = mean, names_from = year)
data_change_velocity$mean = data_change_velocity$end.year - data_change_velocity$start.year
data_change_velocity <- data_change_velocity %>%
  mutate(mean = end.year - start.year) %>%
  mutate(start.year = plot.start.year + 0.5, end.year = plot.end.year - 0.5)
colnames(data_change_velocity) <- c("Country","age_group","sex","variable","iso","Region","Superregion","start.year","end.year","mean")  

for(my_sex in sexes){
  subset_velocity <- data_velocity %>% filter(age_group == my_age_group)
  subset_change_velocity <- data_change_velocity %>% filter(age_group == my_age_group)
  
  ps[[paste("Map velocity", my_variable, my_sex, my_age_group, plot.end.year - 0.5)]]   <- maps_velocity_level_plots(data = subset_velocity, variable = my_variable, my_sex, plot.year = plot.end.year - 0.5)  
  ps[[paste("Map velocity", my_variable, my_sex, my_age_group, plot.start.year + 0.5)]] <- maps_velocity_level_plots(data = subset_velocity, variable = my_variable, my_sex, plot.year = plot.start.year + 0.5)  
  
  ps[[paste("Map change velocity", my_variable, my_sex, my_age_group)]] <- maps_velocity_timechange_plots(data = subset_change_velocity, variable = my_variable, my_sex, my_plot.start.year = plot.start.year+0.5, my_plot.end.year = plot.end.year - 0.5)  
  
}

################ PLOT MAPS LEVEL AND TIMECHANGE ################################
blank <- grid.rect(gp=gpar(col=NA, fill = NA))

for (my_variable in variables){
  cairo_pdf(paste0(outdir_folder, "Maps of level and change in ", my_variable, ".pdf"), height = 35, width = 35, onefile=T)
  
  plot.title <- paste0("Maps of level and change in age standardised ", get_var_longname(my_variable))
  
  grid.arrange(
    arrangeGrob(
      textGrob(plot.title,hjust=0,just = c("left"),x = unit(0.01, "npc"),gp = gpar(col = "black", fontsize = 30)),
      
      arrangeGrob(arrangeGrob(textGrob("Female",hjust=0,just = c("left"),x = unit(0.01, "npc"),gp = gpar(col = "black", fontsize = 30)),
                              ps[[paste("Map level", my_variable, "female", "ageStd", plot.end.year)]],
                              blank,
                              ps[[paste("Map level", my_variable, "female", "ageStd", plot.start.year)]],
                              blank,
                              ps[[paste("Map timechange", my_variable, "female", "ageStd")]],
                              nrow = 6, heights = c(0.5,10, 0.5, 10, 0.5, 10)), 
                  blank,
                  arrangeGrob(textGrob("Male",hjust=0,just = c("left"),x = unit(0.01, "npc"),gp = gpar(col = "black", fontsize = 30)),
                              ps[[paste("Map level", my_variable, "male", "ageStd", plot.end.year)]],
                              blank,
                              ps[[paste("Map level", my_variable, "male", "ageStd", plot.start.year)]],
                              blank,
                              ps[[paste("Map timechange", my_variable, "male", "ageStd")]],
                              nrow = 6, heights = c(0.5,10, 0.5, 10, 0.5, 10)),
                  ncol = 3, widths = c(10, 0.5, 10)),
      
      nrow = 2, ncol = 1, heights = c(1, 20))
  )
  
  dev.off()
}

################ PLOT MAPS VELOCITY AND TIMECHANGE #############################
my_variable <- "prev_bmi_2sd"
my_age_group <- "ageStd"

cairo_pdf(paste0(outdir_folder, "Maps of velocity and change in velocity of ", my_variable, ".pdf"), height = 33, width = 30, onefile=T)

plot.title <- paste0("Maps of velocity and change in age standardised ", get_var_longname(my_variable))

grid.arrange(
  arrangeGrob(
    textGrob(plot.title,hjust=0,just = c("left"),x = unit(0.01, "npc"),gp = gpar(col = "black", fontsize = 30)),
    
    arrangeGrob(arrangeGrob(textGrob("Female",hjust=0,just = c("left"),x = unit(0.01, "npc"),gp = gpar(col = "black", fontsize = 30)),
                            ps[[paste("Map velocity", my_variable, "female", my_age_group, plot.start.year + 0.5)]],
                            blank,
                            ps[[paste("Map velocity", my_variable, "female", my_age_group, plot.end.year - 0.5)]],
                            blank,
                            ps[[paste("Map change velocity", my_variable, "female", my_age_group)]],
                            
                            nrow = 6, heights = c(0.5,10, 0.5, 10, 0.5, 10)),
                blank,
                arrangeGrob(textGrob("Male",hjust=0,just = c("left"),x = unit(0.01, "npc"),gp = gpar(col = "black", fontsize = 30)),
                            ps[[paste("Map velocity", my_variable, "male", my_age_group, plot.start.year + 0.5)]],
                            blank,
                            ps[[paste("Map velocity", my_variable, "male", my_age_group, plot.end.year - 0.5)]],
                            blank,
                            ps[[paste("Map change velocity", my_variable, "male", my_age_group)]],
                            
                            nrow = 6, heights = c(0.5,10, 0.5, 10, 0.5, 10)),
                ncol = 3, widths = c(10, 0.5, 10)),
    
    nrow = 2, ncol = 1, heights = c(1, 20))
)

dev.off()

