## WHEEL PLOTS - double burden, obesity and all
# Last updated 14 June 2023
library(tidyverse)
library(ggthemes)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(ggnewscale)
library(egg)
library(scales)
################## (1) SET VARIABLES AND FILLS #################################

if (type == "all"){
  variables <- c("prev_bmi_l185", "prev_bmi_185_20", "prev_bmi_20_25", "prev_bmi_25_30","prev_bmi_30_35", "prev_bmi_35_40", "prev_bmi_40")
  names(variables) <- c("<18.5","18.5 to <20","20 to <25","25 to <30","30 to <35","35 to <40","\u2265 40")
  fill_scale <- fill_scale_all
  ylims <- c(0, 100.00001)
  
} else if(type == "double burden"){
  variables <- c("prev_bmi_l185", "prev_bmi_30","prev_bmi_l185_proportion_double_burden","prev_bmi_30_proportion_double_burden")
  #variables <- c("prev_bmi_l185", "prev_bmi_30")
  
  fill_scale <- fill_scale_double_burden
  
} else if(type %in% c("obesity")){
  variables <- c("prev_bmi_30_35","prev_bmi_35_40","prev_bmi_40", "prev_bmi_30_35_proportion_obesity", "prev_bmi_35_40_proportion_obesity","prev_bmi_40_proportion_obesity")
  #variables <- c("prev_bmi_30_35","prev_bmi_35_40","prev_bmi_40")
  fill_scale <- fill_scale_all
}

##################### (2) READ AND FORMAT DATA #################################
data <- read_data_level(variables, sexes, "ageStd", region_level = "Country") %>%
  filter(year %in% c(plot.start.year, plot.end.year)) %>%
  filter(age_group %in% age_groups)

data$year     <- factor(data$year)
data$variable <- factor(data$variable, levels = variables, labels = variables)
data$age_group <- factor(data$age_group, levels = age_groups,labels = age_groups)

### (3) PLOT WHEEL PLOTS ##########
composition_types <- c("absolute", "relative")

ps <- list()

if (type == "double burden"){
  for(my_sex in sexes){
    for (variable_to_order_by in c("double_burden","prev_bmi_l185","prev_bmi_30")){
      for (order_level in c("region", "all")){
        for (plot.year in c(plot.start.year, plot.end.year)){
          for (my_composition in composition_types){
            
            variables <- c("prev_bmi_l185", "prev_bmi_30")
            
            if (my_composition == "absolute"){
              subset <- data %>% filter(variable %in% variables & sex == my_sex & age_group == "ageStd" & year == plot.year)
            }
            
            if (my_composition == "relative"){
              subset <- data %>% filter(!(variable %in% variables) & sex == my_sex & age_group == "ageStd" & year == plot.year) %>%
                mutate(variable = ifelse(variable == "prev_bmi_l185_proportion_double_burden", "prev_bmi_l185",
                                         ifelse(variable == "prev_bmi_30_proportion_double_burden","prev_bmi_30","")))
            }
            
            subset$variable <- factor(subset$variable, levels = rev(variables))
            
            print(paste(my_sex, plot.year, type, variable_to_order_by, order_level, my_composition))
            
            ps[[paste(my_sex, plot.year, type, variable_to_order_by, order_level, my_composition)]]  <- circular_bar_function(subset, my_sex, my_year = plot.year, type, showLegend = ifelse(plot.year == plot.end.year, T, F), order_level = order_level, order_by = variable_to_order_by, composition = my_composition)            
            
          }
        }
      }
    }
  }
} else if (type == "all") {
  for(my_sex in sexes) {
    for (plot.year in c(plot.start.year, plot.end.year)){
      for(my_order in c("default")){
        for(obesity_bottom in c(T,F)){
          print(paste(my_sex, plot.year, type, my_order, obesity_bottom))
          ps[[paste(my_sex, plot.year, "all", my_order, obesity_bottom)]] <- circular_bar_function_all(data %>% filter(sex == my_sex &age_group == "ageStd" & year == plot.year), my_sex = my_sex, my_year = plot.year,showLegend = ifelse(plot.year==plot.end.year,T,F), order = my_order, obesity_bottom = obesity_bottom)
        }
      }
    }
  }
} else if (type == "obesity") {
  for(my_sex in sexes) {
    for (plot.year in c(plot.start.year, plot.end.year)){
      for (variable_to_order_by in c("prev_bmi_30", "prev_bmi_35", "prev_bmi_40")){
        for (order_level in c("region", "all")){
          for(my_composition in composition_types){
            
            variables <- c("prev_bmi_30_35", "prev_bmi_35_40", "prev_bmi_40")
            
            if (my_composition == "absolute"){
              subset <- data %>% filter(variable %in% variables & sex == my_sex & age_group == "ageStd" & year == plot.year)
            }
            
            if (my_composition == "relative"){
              subset <- data %>% filter(!(variable %in% variables) & sex == my_sex & age_group == "ageStd" & year == plot.year) %>%
                mutate(variable = ifelse(variable == "prev_bmi_30_35_proportion_obesity", "prev_bmi_30_35",
                                         ifelse(variable == "prev_bmi_35_40_proportion_obesity","prev_bmi_35_40","prev_bmi_40")))
            }
            
            subset$variable <- factor(subset$variable, levels = variables)
            
            print(paste(my_sex, plot.year, type, variable_to_order_by, order_level, my_composition))
            
            ps[[paste(my_sex, plot.year, type, variable_to_order_by, order_level, my_composition)]]  <- circular_bar_function(subset, my_sex, my_year = plot.year, type, showLegend = ifelse(plot.year == plot.end.year, T, F), order_level = order_level, order_by = variable_to_order_by, composition = my_composition)
            
          }
        }
      }
    }
  }
}

blank <- grid.rect(gp=gpar(col=NA, fill = NA))

###### PLOT PDFS ###########################
if (type == "double burden"){
  cairo_pdf(paste0(outdir_folder, "Wheels double burden absolute.pdf"), height = 15, width = 25,onefile=T)
  
  grid.arrange(textGrob("Female agestd: ordered by region"),
               
               arrangeGrob(blank,ps[[paste("female", plot.start.year, "double burden", "double_burden", "region", "absolute")]],
                           blank,ps[[paste("female", plot.end.year, "double burden", "double_burden", "region", "absolute")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Male agestd: ordered by region"),
               arrangeGrob(blank,ps[[paste("male", plot.start.year, "double burden", "double_burden", "region", "absolute")]],
                           blank,ps[[paste("male", plot.end.year, "double burden", "double_burden", "region", "absolute")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Female agestd: ordered by double burden level"),
               arrangeGrob(blank,ps[[paste("female", plot.start.year, "double burden", "double_burden", "all", "absolute")]],
                           blank,ps[[paste("female", plot.end.year, "double burden", "double_burden", "all", "absolute")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Male agestd: ordered by double burden level"),
               arrangeGrob(blank,ps[[paste("male", plot.start.year, "double burden", "double_burden", "all", "absolute")]],
                           blank,ps[[paste("male", plot.end.year, "double burden", "double_burden", "all", "absolute")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Female agestd: ordered by obesity level"),
               arrangeGrob(blank,ps[[paste("female", plot.start.year, "double burden", "prev_bmi_30", "all", "absolute")]],
                           blank,ps[[paste("female", plot.end.year, "double burden", "prev_bmi_30", "all", "absolute")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Male agestd: ordered by obesity level"),
               arrangeGrob(blank,ps[[paste("male", plot.start.year, "double burden", "prev_bmi_30", "all", "absolute")]],
                           blank,ps[[paste("male", plot.end.year, "double burden", "prev_bmi_30", "all", "absolute")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Female agestd: ordered by underweight level"),
               arrangeGrob(blank,ps[[paste("female", plot.start.year, "double burden", "prev_bmi_l185", "all", "absolute")]],
                           blank,ps[[paste("female", plot.end.year, "double burden", "prev_bmi_l185", "all", "absolute")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Male agestd: ordered by underweight level"),
               arrangeGrob(blank,ps[[paste("male", plot.start.year, "double burden", "prev_bmi_l185", "all", "absolute")]],
                           blank,ps[[paste("male", plot.end.year, "double burden", "prev_bmi_l185", "all", "absolute")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  
  
  dev.off()
  
  cairo_pdf(paste0(outdir_folder, "Wheels double burden composition.pdf"), height = 15, width = 25,onefile=T)
  
  grid.arrange(textGrob("Female agestd: ordered by region"),
               
               arrangeGrob(blank,ps[[paste("female", plot.start.year, "double burden", "prev_bmi_l185", "region", "relative")]],
                           blank,ps[[paste("female", plot.end.year, "double burden", "prev_bmi_l185", "region", "relative")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Male agestd: ordered by region"),
               arrangeGrob(blank,ps[[paste("male", plot.start.year, "double burden", "prev_bmi_l185", "region", "relative")]],
                           blank,ps[[paste("male", plot.end.year, "double burden", "prev_bmi_l185", "region", "relative")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Female agestd: ordered by proportion underweight"),
               arrangeGrob(blank,ps[[paste("female", plot.start.year, "double burden", "prev_bmi_l185", "all", "relative")]],
                           blank,ps[[paste("female", plot.end.year, "double burden", "prev_bmi_l185", "all", "relative")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Male agestd: ordered by proportion underweight"),
               arrangeGrob(blank,ps[[paste("male", plot.start.year, "double burden", "prev_bmi_l185", "all", "relative")]],
                           blank,ps[[paste("male", plot.end.year, "double burden", "prev_bmi_l185", "all", "relative")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  
  
  dev.off()
  
} 

if (type == "all"){
  
  cairo_pdf(paste0(outdir_folder, "Wheels all.pdf"), height = 15, width = 25,onefile=T)
  
  grid.arrange(textGrob("Female"),
               arrangeGrob(blank,ps[[paste("female", plot.start.year, "all", "default", T)]],
                           blank,ps[[paste("female", plot.end.year, "all", "default", T)]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Male"),
               arrangeGrob(blank,ps[[paste("male", plot.start.year, "all", "default", T)]],
                           blank,ps[[paste("male", plot.end.year, "all", "default", T)]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  dev.off()
  
}
if (type == "obesity"){
  
  cairo_pdf(paste0(outdir_folder, "Wheels obesity composition absolute.pdf"), height = 15, width = 25, onefile=T)
  
  for (my_sex in sexes){
    grid.arrange(textGrob(paste0("(A) ", str_to_title(my_sex) ,   " ageStd obesity ordered by region")),
                 arrangeGrob(blank,ps[[paste(my_sex, plot.start.year, "obesity", "prev_bmi_30", "region", "absolute")]],
                             blank,ps[[paste(my_sex, plot.end.year, "obesity", "prev_bmi_30", "region", "absolute")]],
                             blank,nrow= 1, widths = c(7,50,6,50,7)),
                 blank,
                 nrow = 3, heights = c(2, 50,5))
  }
  
  for (my_sex in sexes){
    grid.arrange(textGrob(paste0("(B) ", str_to_title(my_sex) ,   " ageStd obesity ordered by absolute obesity")),
                 arrangeGrob(blank,ps[[paste(my_sex, plot.start.year, "obesity", "prev_bmi_30", "all", "absolute")]],
                             blank,ps[[paste(my_sex, plot.end.year, "obesity", "prev_bmi_30", "all", "absolute")]],
                             blank,nrow= 1, widths = c(7,50,6,50,7)),
                 blank,
                 nrow = 3, heights = c(2, 50,5))
  }
  
  for (my_sex in sexes){
    grid.arrange(textGrob(paste0("(C) ", str_to_title(my_sex) ,   " ageStd obesity ordered by absolute severe obesity")),
                 arrangeGrob(blank,ps[[paste(my_sex, plot.start.year, "obesity", "prev_bmi_35", "all", "absolute")]],
                             blank,ps[[paste(my_sex, plot.end.year, "obesity", "prev_bmi_35", "all", "absolute")]],
                             blank,nrow= 1, widths = c(7,50,6,50,7)),
                 blank,
                 nrow = 3, heights = c(2, 50,5))
  }
  
  for (my_sex in sexes){
    grid.arrange(textGrob(paste0("(C) ", str_to_title(my_sex) ,   " ageStd obesity ordered by absolute morbid obesity")),
                 arrangeGrob(blank,ps[[paste(my_sex, plot.start.year, "obesity", "prev_bmi_40", "all", "absolute")]],
                             blank,ps[[paste(my_sex, plot.end.year, "obesity", "prev_bmi_40", "all", "absolute")]],
                             blank,nrow= 1, widths = c(7,50,6,50,7)),
                 blank,
                 nrow = 3, heights = c(2, 50,5))
  }
  
  dev.off()
  
  
  
  cairo_pdf(paste0(outdir_folder, "Wheels obesity composition relative.pdf"), height = 15, width = 25, onefile=T)
  
  
  for (my_sex in sexes){
    grid.arrange(textGrob(paste0("(A) ", str_to_title(my_sex) ,   " ageStd obesity ordered by region and proportion severe")),
                 arrangeGrob(blank,ps[[paste(my_sex, plot.start.year, "obesity", "prev_bmi_35", "region", "relative")]],
                             blank,ps[[paste(my_sex, plot.end.year, "obesity", "prev_bmi_35", "region", "relative")]],
                             blank,nrow= 1, widths = c(7,50,6,50,7)),
                 blank,
                 nrow = 3, heights = c(2, 50,5))
  }
  
  for (my_sex in sexes){
    grid.arrange(textGrob(paste0("(B) ", str_to_title(my_sex) ,   " ageStd obesity ordered by region and proportion morbid")),
                 arrangeGrob(blank,ps[[paste(my_sex, plot.start.year, "obesity", "prev_bmi_40", "region", "relative")]],
                             blank,ps[[paste(my_sex, plot.end.year, "obesity", "prev_bmi_40", "region", "relative")]],
                             blank,nrow= 1, widths = c(7,50,6,50,7)),
                 blank,
                 nrow = 3, heights = c(2, 50,5))
  }
  
  for (my_sex in sexes){
    grid.arrange(textGrob(paste0("(C) ", str_to_title(my_sex) ,   " ageStd obesity ordered by proportion severe")),
                 arrangeGrob(blank,ps[[paste(my_sex, plot.start.year, "obesity", "prev_bmi_35", "all", "relative")]],
                             blank,ps[[paste(my_sex, plot.end.year, "obesity", "prev_bmi_35", "all", "relative")]],
                             blank,nrow= 1, widths = c(7,50,6,50,7)),
                 blank,
                 nrow = 3, heights = c(2, 50,5))
  }
  
  for (my_sex in sexes){
    grid.arrange(textGrob(paste0("(C) ", str_to_title(my_sex) ,   " ageStd obesity ordered by proportion morbid")),
                 arrangeGrob(blank,ps[[paste(my_sex, plot.start.year, "obesity", "prev_bmi_40", "all", "relative")]],
                             blank,ps[[paste(my_sex, plot.end.year, "obesity", "prev_bmi_40", "all", "relative")]],
                             blank,nrow= 1, widths = c(7,50,6,50,7)),
                 blank,
                 nrow = 3, heights = c(2, 50,5))
  }
  
  dev.off()
  
  
}







