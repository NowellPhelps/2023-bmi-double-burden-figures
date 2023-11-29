rm(list = ls())
library(tidyverse)
library(ggnewscale)
library(reshape)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)

source("0.0 BMI Prevalence palette.R")
source("0.0 Region Superregion palette.R")
source("0.6 Arrow plot function.R")

foldername      <- "Figures"
subfoldername   <- "Velocity plots"
mod_dir_name    <- "Model 41 with data update 1980"
modelnum        <- 45

plot.start.year <- 1990.5
plot.end.year   <- 2019.5

parentdir <- "D:/prev_model/Models/"
maindir  <- paste0(parentdir,"Model",modelnum,", ", mod_dir_name, "/")

dir.create(paste0(maindir, "Postprocessing/Figures/Velocity plots/"), showWarnings=FALSE)
outdir <- paste0(maindir, "Postprocessing/Figures/Velocity plots/")
indir_est  <- paste0(maindir, "Postprocessing/Estimates/")

sexes             <- c("female","male")
age_groups        <- c("ageStd")
names(age_groups) <- c("20+ year olds")

variable <- "prev_bmi_30"
xvar <- variable
yvar <- paste0(variable, "_slope")

countrylist <- read.csv(paste0(maindir,"Covariates/country-list-2020.csv"))
countrylist$Superregion <- factor(countrylist$Superregion,
                                  levels = c("High-income Asia Pacific", "East and South East Asia","Oceania",
                                             "High-income Western countries","Central and Eastern Europe","Latin America and Caribbean",
                                             "Central Asia and North Africa-Middle East",
                                             "Sub-Saharan Africa","South Asia"))

############################# LOAD AND RESHAPE DATA ############################
data_level <- NULL
for (my_sex in sexes){
  data_level.tmp <- read_csv(paste0(indir_est, "Model", modelnum, "_", my_sex, "_", variable,"_ageStd_Country_Means_SQUEEZED.csv"), show_col_types = FALSE) %>%
    filter(year %in% c(plot.start.year-.5, plot.start.year+5, plot.end.year-.5, plot.end.year+.5)) %>%
    dplyr::rename(Country = country) %>%
    mutate(sex = my_sex, 
           variable = variable,
           year_group = ifelse(year %in% c(plot.start.year-.5, plot.start.year+.5), plot.start.year, plot.end.year)) %>%
    group_by(Country, age_group,year_group, sex, variable)%>%
    summarise(mean = mean(mean)*100) %>%
    dplyr::rename(year = year_group) %>%
    ungroup()
  
  data_level <- rbind(data_level, data_level.tmp)
}
rm(data_level.tmp)


data_slope <- NULL
for (my_sex in sexes){
    data_slope.tmp <- read_csv(paste0(indir_est, "Slopes/Model", modelnum, "_", my_sex, "_",variable,"_ageStd_Slopes.csv"), show_col_types = FALSE) %>%
      mutate(year = (start.year + end.year)/2) %>%
      select(-c(start.year,end.year)) %>%
    filter(year %in% c(plot.start.year, plot.end.year)) %>%
      select(-c(l, u, se, PP.increase)) %>%
      dplyr::rename(Country = country) %>%
      mutate(sex = my_sex,
             variable = paste0(variable, "_slope"))
    
    data_slope <- rbind(data_slope, data_slope.tmp)

}
rm(data_slope.tmp)

data <- left_join(rbind(data_level, data_slope), countrylist)

## CREATE SCATTER PLOTS
ps <- list()
for (my_sex in sexes){
  for (my_age_group in age_groups){
    
    subset_level <- data %>% filter(sex == my_sex & age_group == my_age_group)
    sex.title <- str_to_title(my_sex)
    
    ps[[paste("Arrow", my_sex, my_age_group)]] <- arrow_plot(subset = subset_level, my_sex, my_age_group, 
                                                             xvar = "prev_bmi_30", yvar = "prev_bmi_30_slope", 
                                                             start.year = plot.start.year, end.year = plot.end.year, 
                                                             big_arrow_type = "unweighted_average", rescaled = F)
    
  
  }
}
  
## PLOT
cairo_pdf(paste0(outdir,"Arrows Velocity obesity level and velocity 1990 2020.pdf"), height = 16, width =30, onefile=T)

for(my_age_group in age_groups){

  age_group.name <- names(age_groups)[which(age_groups == my_age_group)]
  plot.title <- paste0("Change velocity obesity vs obesity:", age_group.name)
  
  grid.arrange(
    arrangeGrob(
      textGrob(plot.title,gp=gpar(fontsize=15, fontface = "bold"), hjust = 0, x= 0.01),
      
      arrangeGrob(
        ps[[paste("Arrow","female", my_age_group)]],
        ps[[paste("Arrow", "male", my_age_group)]],
        ncol = 2, widths = c(10, 10)
      ),
      nrow = 2, 
      heights = c(1, 20)
    )
  )
  
}

dev.off()


