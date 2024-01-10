setwd("S:/Projects/HeightProject/Original dataset/Anthropometrics/adult_bmi_analysis/Figures scripts/Comparisons scripts")

source("0.0 utils light.R")
source("0.0 fact sheet functions.R")
appendix <- T

figsuffix                <- ""
figsuffix                <- ifelse(figsuffix == "", "", paste0(" ", figsuffix))
outdir_folder            <- paste0("")
figNum <- 25

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

data_sources_ado   <- read.csv("S:/Projects/HeightProject/Original dataset/Anthropometrics/adult_bmi_analysis/data_sources_summaries_and_overall/sources_summary_ado.csv") %>%
  select(-c(Country, Region, Superregion)) %>%
  left_join(., countrylist, by = c("iso"))
data_sources_adult <- read.csv("S:/Projects/HeightProject/Original dataset/Anthropometrics/adult_bmi_analysis/data_sources_summaries_and_overall/sources_summary_adult.csv") %>%
  select(-c(Country, Region, Superregion)) %>%
  left_join(., countrylist, by = c("iso"))

############################# GENERATE PLOTS ####################################
get_source_text<- function(N_all, N_natl){
  if(N_all == 0){
    text <- paste0("No studies")
  } else if(N_all ==  1){
    text <- paste0(N_all, " study (", N_natl, " national)")
  } else{
    text <- paste0(N_all, " studies (", N_natl, " national)")
  }
  return(text)
}

ps <- list()

countrylist <- countrylist[order(countrylist$Country),]

for(age_type in c("ado", "adult")){
   if (age_type == "ado"){
      vars_of_interest <- c("prev_bmi_neg2sd", "prev_bmi_2sd")
      data_sources <- data_sources_ado
   } else{
      vars_of_interest <- c("prev_bmi_l185", "prev_bmi_30")
      data_sources <- data_sources_adult
   }
   

  for (my_country in unique(countrylist$Country)[[1]]){
    print(my_country)
    for(my_sex in sexes){
       
      n_sources_all  <- data_sources$N_all[which(data_sources$sex == ifelse(my_sex == "male", 1, 2) & data_sources$Country == my_country)]
      n_sources_natl <- data_sources$N_natl[which(data_sources$sex == ifelse(my_sex == "male", 1, 2) & data_sources$Country == my_country)]
      text_sources[[paste(my_sex, my_country, age_type)]]   <- paste0(get_source_text(n_sources_all, n_sources_natl), "*")

      plot.title <- ifelse(my_sex == "female",
                           ifelse(age_type == "ado", "Girls", "Women"),
                           ifelse(age_type == "ado", "Boys", "Men"))
      
      ps[[paste("dotplot stacked", my_country, my_sex, age_type)]] <- make_dotplot(data_level, my_country, my_sex, plot.end.year, age_type, returnLeg = F, option = "stacked")
      ps[[paste("dotplot repel", my_country, my_sex, age_type)]]   <- make_dotplot(data_level, my_country, my_sex, plot.end.year, age_type, returnLeg = F, option = "repel")
      
      
      for(var in vars_of_interest){
         ps[[paste("region rank plot", var, my_country, my_sex, age_type)]] <- make_region_rank_plot(data_level, my_country, my_sex, my_variable = var, plot.start.year = plot.start.year, plot.end.year = plot.end.year, age_type, returnLeg = F, option = "stacked")

      }
      
    }
  }
   
   ps[[paste("dotplot legend", age_type)]] <- make_dotplot(data_level, my_country, my_sex, plot.end.year, age_type, returnLeg = T, option = "stacked")
}

########################## PRINT PDF ###########################################
blank <- grid.rect(gp=gpar(col=NA, fill = NA))
cairo_pdf("Country factsheets.pdf", height = 11.7, width = 8.3, onefile=T) # Dimensions are those for A4

for(my_country in unique(countrylist$Country)[[1]]){
   
   plot.title <- my_country
   
   region_length = nrow(countrylist %>% filter(Region == countrylist$Region[which(countrylist$Country == my_country)]))
   
   grid.arrange(
      
      textGrob(plot.title,hjust=0,just = c("left"),x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 18)),
      
      
      arrangeGrob(arrangeGrob(textGrob("Girls (5-19 years)", hjust=0, just = c("left"),x = unit(0.04, "npc"), gp = gpar(col = "black", fontsize = 12)),
                              textGrob(text_sources[[paste("female", my_country, "ado")]], hjust=0, just = c("left"),x = unit(0.04, "npc"), gp = gpar(col = "black", fontsize = 8)),
                  
                              arrangeGrob(arrangeGrob(ps[[paste("region rank plot", "prev_bmi_neg2sd", my_country, "female", "ado")]],
                                                      ps[[paste("region rank plot", "prev_bmi_2sd", my_country, "female", "ado")]],
                                                      nrow = 1),
                                          ps[[paste("dotplot stacked", my_country, "female", "ado")]],
                                          nrow = 2, heights = c(4/7*region_length, 10 - (4/7*region_length))),
                              ncol = 1,
                              heights = c(1, 1, 10)),
                  
                  arrangeGrob(textGrob("Boys (5-19 years)", hjust=0, just = c("left"),x = unit(0.04, "npc"), gp = gpar(col = "black", fontsize = 12)),
                              textGrob(text_sources[[paste("male", my_country, "ado")]], hjust=0, just = c("left"),x = unit(0.04, "npc"), gp = gpar(col = "black", fontsize = 8)),
                              
                              arrangeGrob(arrangeGrob(ps[[paste("region rank plot", "prev_bmi_neg2sd", my_country, "male", "ado")]],
                                                      ps[[paste("region rank plot", "prev_bmi_2sd", my_country, "male", "ado")]],
                                                      nrow = 1),
                                          ps[[paste("dotplot stacked", my_country, "male", "ado")]],
                                          nrow = 2, heights = c(4/7*region_length, 10 - (4/7*region_length))),
                              ncol = 1,
                              heights = c(1, 1, 10)),
                  
                  arrangeGrob(textGrob("Women (20+ years)", hjust=0, just = c("left"),x = unit(0.04, "npc"), gp = gpar(col = "black", fontsize = 12)),
                              textGrob(text_sources[[paste("female", my_country, "adult")]], hjust=0, just = c("left"),x = unit(0.04, "npc"), gp = gpar(col = "black", fontsize = 8)),
                              
                              arrangeGrob(arrangeGrob(ps[[paste("region rank plot", "prev_bmi_l185", my_country, "female", "adult")]],
                                                      ps[[paste("region rank plot", "prev_bmi_30", my_country, "female", "adult")]],
                                                      nrow = 1),
                                          ps[[paste("dotplot stacked", my_country, "female", "adult")]],
                                          nrow = 2, heights = c(4/7*region_length, 10 - (4/7*region_length))),
                              ncol = 1,
                              heights = c(1, 0.5, 10.5)),
                  
                  arrangeGrob(textGrob("Men (20+ years)", hjust=0, just = c("left"),x = unit(0.04, "npc"), gp = gpar(col = "black", fontsize = 12)),
                              textGrob(text_sources[[paste("male", my_country, "adult")]], hjust=0, just = c("left"),x = unit(0.04, "npc"), gp = gpar(col = "black", fontsize = 8)),
                              
                              arrangeGrob(arrangeGrob(ps[[paste("region rank plot", "prev_bmi_l185", my_country, "male", "adult")]],
                                                      ps[[paste("region rank plot", "prev_bmi_30", my_country, "male", "adult")]],
                                                      nrow = 1),
                                          ps[[paste("dotplot stacked", my_country, "male", "adult")]],
                                          nrow = 2, heights = c(4/7*region_length, 10 - (4/7*region_length))),
                              ncol = 1,
                              heights = c(1, 1, 10)),
                  nrow = 2,
                  ncol = 2),
      
      arrangeGrob(blank,
                  arrangeGrob(textGrob(expression(underline("Notes")), hjust=0, just = c("left"),x = unit(0.04, "npc"), gp = gpar(col = "black", fontsize = 9)),
                              textGrob("* For a full list of data sources please see Appendix Table 1 of NCD Risk Factor Collaboration \"XXXXX\"", hjust=0, just = c("left"),x = unit(0.04, "npc"), gp = gpar(col = "black", fontsize = 9)),
                              ncol = 1),
                  ncol = 1, 
                  heights = c(1, 4)), 
      
      ncol = 1,
      heights = c(0.5,10.5,0.5)
   )
   
}
dev.off()