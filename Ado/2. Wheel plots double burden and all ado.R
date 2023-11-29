## WHEEL PLOTS - ADOLESCENTS
# Last updated 9 May 2023
############################# (1) INITIALISATION #################
library(tidyverse)
library(ggthemes)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(ggnewscale)
library(egg)
library(scales)

age_groups        <- c("ageStd")
names(age_groups) <- c("age standardised")

errorbar_types <- c("none") # takes none, segment or composite

################## (2) READ AND FORMAT DATA ################################

if(type == "ado all"){
  variables <- c("prev_bmi_neg2sd","prev_bmi_neg2sd_neg1sd","prev_bmi_neg1sd_1sd","prev_bmi_1sd_2sd","prev_bmi_2sd")
  names(variables)<- c("BMI < -2 SD","-2 SD \u2265 BMI < -1 SD","-1 SD \u2265 BMI \u2265 1 SD","1 SD < BMI \u2265 2 SD","BMI > 2SD")
  fill_scale <- fill_scale_all_ado
  ylims <- c(0, 100.00001)
} else if (type == "ado double burden"){
  variables        <- c("prev_bmi_neg2sd", "prev_bmi_2sd", "prev_bmi_neg2sd_proportion_double_burden", "prev_bmi_2sd_proportion_double_burden")
  fill_scale      <- fill_scale_double_burden_ado
  ylims <- c(0,50)
}

data  <- read_data_level(variables, sexes, age_type = "ageStd", region_level = "Country") %>%
  filter(year == plot.start.year | year == plot.end.year)

data$variable <- factor(data$variable, levels = variables, labels = variables)


### (3) PLOT WHEEL PLOTS ##########

ps <- list()

if (type == "ado all") {
  for(my_sex in sexes) {
    for (plot.year in c(plot.start.year, plot.end.year)){
      for(my_order in c("default")){
        for(obesity_bottom in c(T,F)){
          print(paste(my_sex, plot.year, type, my_order, obesity_bottom))
          ps[[paste(my_sex, plot.year, type, my_order, obesity_bottom)]] <- circular_bar_function_all(data %>% filter(sex == my_sex &age_group == "ageStd" & year == plot.year), my_sex = my_sex, my_year = plot.year,showLegend = ifelse(plot.year==plot.end.year,T,F), order = my_order, obesity_bottom = obesity_bottom)
        }
      }
    }
  }
} 


if (type == "ado double burden"){
  for(my_sex in sexes){
    for (variable_to_order_by in c("double_burden","prev_bmi_neg2sd","prev_bmi_2sd")){
      for (order_level in c("region", "all")){
        for (plot.year in c(plot.start.year, plot.end.year)){
          for (my_composition in c("absolute", "relative")){
            
            variables <- c("prev_bmi_neg2sd", "prev_bmi_2sd")
            
            if (my_composition == "absolute"){
              subset <- data %>% filter(variable %in% variables & sex == my_sex & age_group == "ageStd" & year == plot.year)
            }
            
            if (my_composition == "relative"){
              subset <- data %>% filter(!(variable %in% variables) & sex == my_sex & age_group == "ageStd" & year == plot.year) %>%
                mutate(variable = ifelse(variable == "prev_bmi_neg2sd_proportion_double_burden", "prev_bmi_neg2sd",
                                         ifelse(variable == "prev_bmi_2sd_proportion_double_burden","prev_bmi_2sd","")))
            }
            
            subset$variable <- factor(subset$variable, levels = rev(variables))
            
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
if (type == "ado double burden"){
  cairo_pdf(paste0(outdir_folder, "Wheels double burden absolute ado.pdf"), height = 15, width = 25,onefile=T)
  
  grid.arrange(textGrob("Girls agestd: ordered by region"),
               
               arrangeGrob(blank,ps[[paste("female", plot.start.year, "ado double burden", "double_burden", "region", "absolute")]],
                           blank,ps[[paste("female", plot.end.year, "ado double burden", "double_burden", "region", "absolute")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Boys agestd: ordered by region"),
               arrangeGrob(blank,ps[[paste("male", plot.start.year, "ado double burden", "double_burden", "region", "absolute")]],
                           blank,ps[[paste("male", plot.end.year, "ado double burden", "double_burden", "region", "absolute")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Female agestd: ordered by double burden level"),
               arrangeGrob(blank,ps[[paste("female", plot.start.year, "ado double burden", "double_burden", "all", "absolute")]],
                           blank,ps[[paste("female", plot.end.year, "ado double burden", "double_burden", "all", "absolute")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Boys agestd: ordered by double burden level"),
               arrangeGrob(blank,ps[[paste("male", plot.start.year, "ado double burden", "double_burden", "all", "absolute")]],
                           blank,ps[[paste("male", plot.end.year, "ado double burden", "double_burden", "all", "absolute")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Female agestd: ordered by obesity level"),
               arrangeGrob(blank,ps[[paste("female", plot.start.year, "ado double burden", "prev_bmi_2sd", "all", "absolute")]],
                           blank,ps[[paste("female", plot.end.year, "ado double burden", "prev_bmi_2sd", "all", "absolute")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Boys agestd: ordered by obesity level"),
               arrangeGrob(blank,ps[[paste("male", plot.start.year, "ado double burden", "prev_bmi_2sd", "all", "absolute")]],
                           blank,ps[[paste("male", plot.end.year, "ado double burden", "prev_bmi_2sd", "all", "absolute")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Female agestd: ordered by underweight level"),
               arrangeGrob(blank,ps[[paste("female", plot.start.year, "ado double burden", "prev_bmi_neg2sd", "all", "absolute")]],
                           blank,ps[[paste("female", plot.end.year, "ado double burden", "prev_bmi_neg2sd", "all", "absolute")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Boys agestd: ordered by underweight level"),
               arrangeGrob(blank,ps[[paste("male", plot.start.year, "ado double burden", "prev_bmi_neg2sd", "all", "absolute")]],
                           blank,ps[[paste("male", plot.end.year, "ado double burden", "prev_bmi_neg2sd", "all", "absolute")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  
  
  dev.off()
  
  cairo_pdf(paste0(outdir_folder, "Wheels double burden composition ado.pdf"), height = 15, width = 25,onefile=T)
  
  grid.arrange(textGrob("Female agestd: ordered by region"),
               
               arrangeGrob(blank,ps[[paste("female", plot.start.year, "ado double burden", "prev_bmi_neg2sd", "region", "relative")]],
                           blank,ps[[paste("female", plot.end.year, "ado double burden", "prev_bmi_neg2sd", "region", "relative")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Boys agestd: ordered by region"),
               arrangeGrob(blank,ps[[paste("male", plot.start.year, "ado double burden", "prev_bmi_neg2sd", "region", "relative")]],
                           blank,ps[[paste("male", plot.end.year, "ado double burden", "prev_bmi_neg2sd", "region", "relative")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Female agestd: ordered by proportion underweight"),
               arrangeGrob(blank,ps[[paste("female", plot.start.year, "ado double burden", "prev_bmi_neg2sd", "all", "relative")]],
                           blank,ps[[paste("female", plot.end.year, "ado double burden", "prev_bmi_neg2sd", "all", "relative")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Boys agestd: ordered by proportion underweight"),
               arrangeGrob(blank,ps[[paste("male", plot.start.year, "ado double burden", "prev_bmi_neg2sd", "all", "relative")]],
                           blank,ps[[paste("male", plot.end.year, "ado double burden", "prev_bmi_neg2sd", "all", "relative")]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  
  
  dev.off()
  
} 

if (type == "ado all"){
  
  cairo_pdf(paste0(outdir_folder, "Wheels all ado.pdf"), height = 15, width = 25,onefile=T)
  
  grid.arrange(textGrob("Girls"),
               arrangeGrob(blank,ps[[paste("female", plot.start.year, "ado all", "default", T)]],
                           blank,ps[[paste("female", plot.end.year, "ado all", "default", T)]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  grid.arrange(textGrob("Boys"),
               arrangeGrob(blank,ps[[paste("male", plot.start.year, "ado all", "default", T)]],
                           blank,ps[[paste("male", plot.end.year, "ado all", "default", T)]],
                           blank,nrow= 1, widths = c(7,50,6,50,7)),
               blank,
               nrow = 3, heights = c(2, 50,5))
  
  dev.off()
  
}






