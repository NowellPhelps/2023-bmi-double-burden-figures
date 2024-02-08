remove(list = ls())
setwd("S:/Projects/HeightProject/Original dataset/Anthropometrics/adult_bmi_analysis/Figures scripts/Comparisons scripts")
library(ggplot2)
library(ggrepel)
library(gridtext)
source("0.0 utils light.R")
source("0.0 fact sheet functions.R")
appendix <- T

figsuffix      <- ""
figsuffix      <- ifelse(figsuffix == "", "", paste0(" ", figsuffix))
outdir_folder  <- paste0("")
figNum         <- 25

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
library(cowplot)
library(magick)

age_groups        <- c("ageStd")
names(age_groups) <- c("20+ year olds")
my_age_group      <- "ageStd"

############################# READ DATA ########################################
data_level_adult <- read_data_level(variables = c("prev_bmi_l185", "prev_bmi_30", "prev_double_burden"), sexes = c("female", "male"), age_type = "ageStd", region_level = "Country", age_level = "adult") %>%
  filter(age_group == "ageStd")

data_level <- read_data_level(variables = c("prev_bmi_neg2sd", "prev_bmi_2sd", "prev_double_burden"), sexes = c("female", "male"), age_type = "ageStd", region_level = "Country", age_level = "ado") %>%
  mutate(variable = ifelse(variable == 'prev_double_burden', 'prev_double_burden_ado', variable)) %>%
  filter(age_group == "ageStd") %>%
  rbind(., data_level_adult)

rm(data_level_adult)

data_change_adult <- read_data_timechange(variables = c("prev_bmi_l185", "prev_bmi_30", "prev_double_burden"), sexes = c("female", "male"), age_groups = c("ageStd"), region_level = "Country", age_level = "adult") %>%
   filter(age_group == "ageStd")

data_change <- read_data_timechange(variables = c("prev_bmi_neg2sd", "prev_bmi_2sd", "prev_double_burden"), sexes = c("female", "male"), age_groups = c("ageStd"), region_level = "Country", age_level = "ado") %>%
   mutate(variable = ifelse(variable == 'prev_double_burden', 'prev_double_burden_ado', variable)) %>%
   filter(age_group == "ageStd") %>%
   rbind(., data_change_adult)

rm(data_change_adult)

data_ranking_adult <- read_data_ranking(variables = c("prev_bmi_l185", "prev_bmi_30"),sexes = c("female", "male"), age_type = "ageStd", region_level = "Country", age_level = "adult") %>%
   filter(age_group == "ageStd")

data_ranking <- read_data_ranking(variables = c("prev_bmi_neg2sd", "prev_bmi_2sd"),sexes = c("female", "male"), age_type = "ageStd", region_level = "Country", age_level = "ado") %>%
   filter(age_group == "ageStd") %>%
   rbind(.,data_ranking_adult)

remove(data_ranking_adult)

data_numbers_adult <- read_data_numbers(variables = c("prev_bmi_l185", "prev_bmi_30"), sexes = c("female", "male"),region_level = "Country", age_level = "adult")
data_numbers <- read_data_numbers(variables = c("prev_bmi_neg2sd", "prev_bmi_2sd"), sexes = c("female", "male"),region_level = "Country", age_level = "ado") %>%
   rbind(., data_numbers_adult)
rm(data_numbers_adult)

data_sources_ado   <- read.csv("S:/Projects/HeightProject/Original dataset/Anthropometrics/adult_bmi_analysis/data_sources_summaries_and_overall/sources_summary_ado.csv") %>%
  select(-c(Country, Region, Superregion)) %>%
  left_join(., countrylist, by = c("iso"))
data_sources_adult <- read.csv("S:/Projects/HeightProject/Original dataset/Anthropometrics/adult_bmi_analysis/data_sources_summaries_and_overall/sources_summary_adult.csv") %>%
  select(-c(Country, Region, Superregion)) %>%
  left_join(., countrylist, by = c("iso"))

############################# GENERATE PLOTS ####################################
ps <- list()

countrylist <- countrylist[order(countrylist$Country),]

text_prevalences <- list()
text_sources     <- list()

for (my_country in c("Nigeria", "United Kingdom",  "Andorra", "Bermuda")){    
   print(my_country)
   
   text_sources[[paste(my_country)]] <- get_text_sources(my_country, data_sources_adult, data_sources_ado)
   
   
   for(age_type in c("ado", "adult")){
      
      if (age_type == "ado"){
         vars_of_interest <- c("prev_bmi_neg2sd", "prev_bmi_2sd")
      } else{
         vars_of_interest <- c("prev_bmi_l185", "prev_bmi_30")
      }
      
      
      for(my_sex in sexes){
         
         plot.title <- ifelse(my_sex == "female",
                              ifelse(age_type == "ado", "Girls", "Women"),
                              ifelse(age_type == "ado", "Boys", "Men"))
         
         ps[[paste("minitrend", my_country, my_sex, age_type)]] <- make_mini_trends(data_level, my_country, my_sex, age_type, returnLeg = F)
         
         for(my_variable in vars_of_interest){
            
            text_prevalences[[paste(my_variable, my_country, age_type, my_sex)]] <- get_text_prevalences(data_level, data_change, data_numbers, my_country, my_variable, age_type, my_sex)
            
            ps[[paste("region rank plot", my_variable, my_country, my_sex, age_type)]] <- make_region_rank_one_year(data_level, data_ranking, my_country, my_sex, my_variable, my_year = plot.end.year, age_type, returnLeg = F, rank_plot = T)
         }
      }
   }
   
   
   ##### GET LEGENDS
   ps[[paste("hist legend", my_country)]] <- make_bin_plot(data_level, my_country, my_sex, my_variable, my_year = plot.end.year, age_type, returnLeg = T)
   
}

## GET NCD-RisC logo
logo_file <- "S:/Projects/HeightProject/Original dataset/Anthropometrics/adult_bmi_analysis/Figures scripts/Comparisons scripts/NCD-RisC-Logo.png"
ps[["logo"]] <- cowplot::ggdraw() + draw_image(logo_file)

for(age_type in c("ado", "adult")){
   ps[[paste("mintrend legend", age_type)]] <- make_mini_trends(data_level, my_country, my_sex, age_type, returnLeg = T)
}

########################## PRINT PDF ###########################################
blank <- grid.rect(gp=gpar(col=NA, fill = NA))

suffix <- ""
for (my_country in c("Nigeria", "United Kingdom", "Andorra", "Bermuda")){  
   
   plot.title <- paste0("Underweight and obesity in ",my_country, ", 2022")
   
   region_length = nrow(countrylist %>% filter(Region == countrylist$Region[which(countrylist$Country == my_country)]))
   
   
   cairo_pdf(paste0("NCD-RisC country factsheet ", my_country, suffix, ".pdf"), height = 11.7, width = 8.3, onefile=T) # Dimensions are those for A4

 
   grid.arrange(
      arrangeGrob(
         textGrob(plot.title,hjust=0,just = c("left"),x = unit(0.005, "npc"),gp = gpar(col = "black", fontsize = 17)),
         ps[["logo"]],
         nrow = 1, 
         widths = c(2, 1)
      ),
      
      
      arrangeGrob(
         arrangeGrob(
            
            
            
            
            arrangeGrob(richtext_grob(paste0("**Underweight in Women**"), hjust=0, x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 10)),
                           ps[[paste("region rank plot", "prev_bmi_l185", my_country, "female", "adult")]],
                           text_prevalences[[paste("prev_bmi_l185", my_country, "adult", "female")]],
                           blank,
                           ncol = 1,
                        heights = c(0.6, 9, 1.4, 0.7)),
            
            arrangeGrob(richtext_grob(paste0("**Obesity in Women**"), hjust=0, x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 10)),
                        ps[[paste("region rank plot", "prev_bmi_30", my_country, "female", "adult")]],
                        text_prevalences[[paste("prev_bmi_30", my_country, "adult", "female")]],
                        blank,
                        ncol = 1,
                        heights = c(0.6, 9, 1.4, 0.7)),
            
            arrangeGrob(richtext_grob(paste0("**Underweight in Men**"), hjust=0, x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 10)),
                        ps[[paste("region rank plot", "prev_bmi_l185", my_country, "male", "adult")]],
                        text_prevalences[[paste("prev_bmi_l185", my_country, "adult", "male")]],
                        blank,
                        ncol = 1,
                        heights = c(0.6, 9, 1.4, 0.7)),
            
            arrangeGrob(richtext_grob(paste0("**Obesity in Men**"), hjust=0, x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 10)),
                        ps[[paste("region rank plot", "prev_bmi_30", my_country, "male", "adult")]],
                        text_prevalences[[paste("prev_bmi_30", my_country, "adult", "male")]],
                        blank,
                        ncol = 1,
                        heights = c(0.6, 9, 1.4, 0.7)),
            
            
            arrangeGrob(richtext_grob(paste0("**Thinness in Girls**"), hjust=0, x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 10)),
                        ps[[paste("region rank plot", "prev_bmi_neg2sd", my_country, "female", "ado")]],
                        text_prevalences[[paste("prev_bmi_neg2sd", my_country, "ado", "female")]],
                        blank,
                        ncol = 1,
                        heights = c(0.6, 9, 1.4, 0.7)),
            
            arrangeGrob(richtext_grob(paste0("**Obesity in Girls**"), hjust=0, x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 10)),
                        ps[[paste("region rank plot", "prev_bmi_2sd", my_country, "female", "ado")]],
                        text_prevalences[[paste("prev_bmi_2sd", my_country, "ado", "female")]],
                        blank,
                        ncol = 1,
                        heights = c(0.6, 9, 1.4, 0.7)),
            
            arrangeGrob(richtext_grob(paste0("**Thinness in Boys**"), hjust=0, x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 10)),
                        ps[[paste("region rank plot", "prev_bmi_neg2sd", my_country, "male", "ado")]],
                        text_prevalences[[paste("prev_bmi_neg2sd", my_country, "ado", "male")]],
                        blank,
                        ncol = 1,
                        heights = c(0.6, 9, 1.4, 0.7)),
            
            arrangeGrob(richtext_grob(paste0("**Obesity in Boys**"), hjust=0, x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 10)),
                        ps[[paste("region rank plot", "prev_bmi_2sd", my_country, "male", "ado")]],
                        text_prevalences[[paste("prev_bmi_2sd", my_country, "ado", "male")]],
                        blank,
                        ncol = 1,
                        heights = c(0.6, 9, 1.4, 0.7)),
            
            nrow = 4, 
            ncol = 2),
         
         arrangeGrob(
            arrangeGrob(blank,
                        ps[[paste("minitrend", my_country, "female", "adult")]],
                        ps[[paste("mintrend legend", "adult")]],
                        ps[[paste("minitrend", my_country, "male", "adult")]],
                        blank,
                        blank,
                        blank,
                        ps[[paste("minitrend", my_country, "female", "ado")]],
                        ps[[paste("mintrend legend", "ado")]],
                        ps[[paste("minitrend", my_country, "male", "ado")]],
                        blank,
                        ncol = 1, 
                        heights = c(0.5,3,2,3,.1,2,.1,3,2,3,.5)),
            blank,
            nrow = 1,
            widths = c(1,.05)),
         
         nrow = 1,
         widths = c(0.8, 0.2)),
      
      blank,
      arrangeGrob(
         textGrob("• Source: NCD Risk Factor Collaboration \"Worldwide trends in underweight and obesity from 1990 to 2022: a pooled analysis of 3663 population-representative", hjust=0, just = c("left"),x = unit(0.01, "npc"), gp = gpar(col = "black", fontsize = 8)),
         richtext_grob('<span style="color:white">ww</span>studies with 222 million children, adolescents, and adults" *The Lancet*, 2024.', hjust=0, x = unit(0, "npc"), gp = gpar(col = "black", fontsize = 8)),
         textGrob(paste0("• ",text_sources[[paste(my_country)]]), hjust=0, just = c("left"),x = unit(0.01, "npc"), gp = gpar(col = "black", fontsize = 8)),
         textGrob("• Prevalences presented here are age-standardised using the WHO Standard Population.", hjust=0, just = c("left"),x = unit(0.01, "npc"), gp = gpar(col = "black", fontsize = 8)),
         textGrob('• Complete results (for 200 countries and territories) are available at www.ncdrisc.org.', hjust=0, x = unit(0.01, "npc"),just = c("left"), gp = gpar(col = "black", fontsize = 8)),
         ncol = 1),
      
      ncol = 1,
      heights = c(0.4, 6.07, 0.05, 0.5)
   )

  dev.off()
}

