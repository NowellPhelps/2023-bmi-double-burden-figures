setwd("S:/Projects/HeightProject/Original dataset/Anthropometrics/adult_bmi_analysis/Figures scripts/Comparisons scripts")

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

data_sources_ado   <- read.csv("S:/Projects/HeightProject/Original dataset/Anthropometrics/adult_bmi_analysis/data_sources_summaries_and_overall/sources_summary_ado.csv") %>%
  select(-c(Country, Region, Superregion)) %>%
  left_join(., countrylist, by = c("iso"))
data_sources_adult <- read.csv("S:/Projects/HeightProject/Original dataset/Anthropometrics/adult_bmi_analysis/data_sources_summaries_and_overall/sources_summary_adult.csv") %>%
  select(-c(Country, Region, Superregion)) %>%
  left_join(., countrylist, by = c("iso"))

############################# GENERATE PLOTS ####################################
ps <- list()

countrylist <- countrylist[order(countrylist$Country),]

texts <- list()

for(age_type in c("ado", "adult")){
   if (age_type == "ado"){
      vars_of_interest <- c("prev_bmi_neg2sd", "prev_bmi_2sd")
      data_sources <- data_sources_ado
   } else{
      vars_of_interest <- c("prev_bmi_l185", "prev_bmi_30")
      data_sources <- data_sources_adult
   }
   

 # for (my_country in unique(countrylist$Country)){
   for (my_country in c("Nigeria")){    
   print(my_country)
    for(my_sex in sexes){
       
      n_sources_all  <- data_sources$N_all[which(data_sources$sex == ifelse(my_sex == "male", 1, 2) & data_sources$Country == my_country)]
      n_sources_natl <- data_sources$N_natl[which(data_sources$sex == ifelse(my_sex == "male", 1, 2) & data_sources$Country == my_country)]
      
      texts[[paste(my_sex, my_country, age_type)]] <- get_text_description(data_level, data_change, my_country, my_sex, age_type, n_sources_all, n_sources_natl)

      plot.title <- ifelse(my_sex == "female",
                           ifelse(age_type == "ado", "Girls", "Women"),
                           ifelse(age_type == "ado", "Boys", "Men"))
      
      ps[[paste("dotplot stacked", my_country, my_sex, age_type)]] <- make_dotplot(data_level, my_country, my_sex, plot.end.year, age_type, returnLeg = F, option = "stacked")
      ps[[paste("dotplot repel", my_country, my_sex, age_type)]]   <- make_dotplot(data_level, my_country, my_sex, plot.end.year, age_type, returnLeg = F, option = "repel")
      
      for(var in vars_of_interest){
         ps[[paste("region rank plot", var, my_country, my_sex, age_type)]] <- make_region_rank_plot(data_level, my_country, my_sex, my_variable = var, plot.start.year = plot.start.year, plot.end.year = plot.end.year, age_type, returnLeg = F, option = "stacked")
         ps[[paste("histogram country", var, my_country, my_sex, age_type)]] <- make_highlighted_histogram(data_level, my_country, my_sex, my_variable = var, my_year = plot.end.year, age_type, returnLeg = F, option = "country")
         ps[[paste("histogram region", var, my_country, my_sex, age_type)]] <- make_highlighted_histogram(data_level, my_country, my_sex, my_variable = var, my_year = plot.end.year, age_type, returnLeg = F, option = "region")
         
      }
    }
  }
   
   ps[[paste("dotplot legend", age_type)]] <- make_dotplot(data_level, my_country, my_sex, plot.end.year, age_type, returnLeg = T, option = "stacked")
}

########################## PRINT PDF ###########################################
blank <- grid.rect(gp=gpar(col=NA, fill = NA))
cairo_pdf("NCD-RisC country factsheets.pdf", height = 12, width = 12, onefile=T) # Dimensions are those for A4

#for(my_country in unique(countrylist$Country)[[1]]){
for (my_country in c("Nigeria")){   
   plot.title <- paste("NCD-RisC country factsheet:", my_country)
   
   region_length = nrow(countrylist %>% filter(Region == countrylist$Region[which(countrylist$Country == my_country)]))
   
   grid.arrange(
      
      textGrob(plot.title,hjust=0,just = c("left"),x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 18)),
      
      arrangeGrob(textGrob(expression(bold(underline("Girls (5-19 years)"))), hjust=0, just = c("left"),x = unit(0.01, "npc"), gp = gpar(col = "black", fontsize = 12)),
                  texts[[paste("female", my_country, "ado")]],
                  blank,
                  arrangeGrob(arrangeGrob(textGrob(expression(bold("Thinness")), hjust=0, just = c("left"),x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 10)),
                                          arrangeGrob(ps[[paste("region rank plot", "prev_bmi_neg2sd", my_country, "female", "ado")]],
                                                      ps[[paste("histogram country", "prev_bmi_neg2sd", my_country, "female", "ado")]],
                                                      nrow = 1, widths = c(0.6, 0.4)),
                                          ncol = 1,
                                          heights = c(1, 9)),
                              arrangeGrob(textGrob(expression(bold("Obesity")), hjust=0, just = c("left"),x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 10)),
                                          arrangeGrob(ps[[paste("region rank plot", "prev_bmi_2sd", my_country, "female", "ado")]],
                                                      ps[[paste("histogram country", "prev_bmi_2sd", my_country, "female", "ado")]],
                                                      nrow = 1, widths = c(0.6, 0.4)),
                                          ncol = 1,
                                          heights = c(1, 9)),
                     
                              arrangeGrob(ps[[paste("dotplot stacked", my_country, "female", "ado")]],
                                          ps[[paste("dotplot legend", "ado")]],
                                          nrow = 2,
                                          heights = c(0.6,.4)),
                              nrow = 1, widths = c(1, 1, 0.5)),
                  ncol = 1,
                  heights = c(1, 2,.5, 8.5)),
      blank,
      arrangeGrob(textGrob(expression(bold(underline("Boys (5-19 years)"))), hjust=0, just = c("left"),x = unit(0.01, "npc"), gp = gpar(col = "black", fontsize = 12)),
                  texts[[paste("male", my_country, "ado")]],
                  blank,
                  arrangeGrob(arrangeGrob(textGrob(expression(bold("Thinness")), hjust=0, just = c("left"),x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 10)),
                                          arrangeGrob(ps[[paste("region rank plot", "prev_bmi_neg2sd", my_country, "male", "ado")]],
                                                      ps[[paste("histogram country", "prev_bmi_neg2sd", my_country, "male", "ado")]],
                                                      nrow = 1, widths = c(0.6, 0.4)),
                                          ncol = 1,
                                          heights = c(1, 9)),
                              arrangeGrob(textGrob(expression(bold("Obesity")), hjust=0, just = c("left"),x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 10)),
                                          arrangeGrob(ps[[paste("region rank plot", "prev_bmi_2sd", my_country, "male", "ado")]],
                                                      ps[[paste("histogram country", "prev_bmi_2sd", my_country, "male", "ado")]],
                                                      nrow = 1, widths = c(0.6, 0.4)),
                                          ncol = 1,
                                          heights = c(1, 9)),
                              
                              arrangeGrob(ps[[paste("dotplot stacked", my_country, "male", "ado")]],
                                          ps[[paste("dotplot legend", "ado")]],
                                          nrow = 2,
                                          heights = c(0.6,.4)),
                              nrow = 1, widths = c(1, 1, 0.5)),
                  ncol = 1,
                  heights = c(1, 2,.5, 8.5)),
      blank,
      arrangeGrob(textGrob(expression(bold(underline("Women (20+ years)"))), hjust=0, just = c("left"),x = unit(0.01, "npc"), gp = gpar(col = "black", fontsize = 12)),
                  texts[[paste("female", my_country, "adult")]],
                  blank,
                  arrangeGrob(arrangeGrob(textGrob(expression(bold("Thinness")), hjust=0, just = c("left"),x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 10)),
                                          arrangeGrob(ps[[paste("region rank plot", "prev_bmi_l185", my_country, "female", "adult")]],
                                                      ps[[paste("histogram country", "prev_bmi_l185", my_country, "female", "adult")]],
                                                      nrow = 1, widths = c(0.6, 0.4)),
                                          ncol = 1,
                                          heights = c(1, 9)),
                              arrangeGrob(textGrob(expression(bold("Obesity")), hjust=0, just = c("left"),x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 10)),
                                          arrangeGrob(ps[[paste("region rank plot", "prev_bmi_30", my_country, "female", "adult")]],
                                                      ps[[paste("histogram country", "prev_bmi_30", my_country, "female", "adult")]],
                                                      nrow = 1, widths = c(0.6, 0.4)),
                                          ncol = 1,
                                          heights = c(1, 9)),
                              
                              arrangeGrob(ps[[paste("dotplot stacked", my_country, "female", "adult")]],
                                          ps[[paste("dotplot legend", "adult")]],
                                          nrow = 2,
                                          heights = c(0.6,.4)),
                              nrow = 1, widths = c(1, 1, 0.5)),
                  ncol = 1,
                  heights = c(1, 2,.5, 8.5)),
      blank,
      arrangeGrob(textGrob(expression(bold(underline("Men (20+ years)"))), hjust=0, just = c("left"),x = unit(0.01, "npc"), gp = gpar(col = "black", fontsize = 12)),
                  texts[[paste("male", my_country, "adult")]],
                  blank,
                  arrangeGrob(arrangeGrob(textGrob(expression(bold("Thinness")), hjust=0, just = c("left"),x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 10)),
                                          arrangeGrob(ps[[paste("region rank plot", "prev_bmi_l185", my_country, "male", "adult")]],
                                                      ps[[paste("histogram country", "prev_bmi_l185", my_country, "male", "adult")]],
                                                      nrow = 1, widths = c(0.6, 0.4)),
                                          ncol = 1,
                                          heights = c(1, 9)),
                              arrangeGrob(textGrob(expression(bold("Obesity")), hjust=0, just = c("left"),x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 10)),
                                          arrangeGrob(ps[[paste("region rank plot", "prev_bmi_30", my_country, "male", "adult")]],
                                                      ps[[paste("histogram country", "prev_bmi_30", my_country, "male", "adult")]],
                                                      nrow = 1, widths = c(0.6, 0.4)),
                                          ncol = 1,
                                          heights = c(1, 9)),
                              
                              arrangeGrob(ps[[paste("dotplot stacked", my_country, "male", "adult")]],
                                          ps[[paste("dotplot legend", "adult")]],
                                          nrow = 2,
                                          heights = c(0.6,.4)),
                              nrow = 1, widths = c(1, 1, 0.5)),
                  ncol = 1,
                  heights = c(1, 2,.5, 8.5)),
      
      blank,
      arrangeGrob(textGrob(expression(bold(underline("Notes"))), hjust=0, just = c("left"),x = unit(0.01, "npc"), gp = gpar(col = "black", fontsize = 12)),
                  textGrob("• These estimates are as reported in NCD Risk Factor Collaboration \"XXXXX\"", hjust=0, just = c("left"),x = unit(0.03, "npc"), gp = gpar(col = "black", fontsize = 9)),
                  textGrob("• See Methods and Appendix of above for detailed statistical methodology, as well as [[link to NCD-RisC methods page]].", hjust=0, just = c("left"),x = unit(0.03, "npc"), gp = gpar(col = "black", fontsize = 9)),
                  textGrob("* For a full list of data sources please see Appendix Table 1 of publication above [[or link to Zenodo]].", hjust=0, just = c("left"),x = unit(0.03, "npc"), gp = gpar(col = "black", fontsize = 9)),
                  ncol = 1),
      ncol = 1,
      heights = c(0.4,3,0.05,3,0.05,3,0.05,3,0.05, 0.6)
   )
   
}
dev.off()
