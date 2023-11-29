rm(list=ls())
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

modelnum                 <- 45
mod_dir_name             <- "Model 41 with data update 1980"

parentdir                <- "D:/prev_model/Models/"
maindir                  <- paste0(parentdir, "Model", modelnum, ", ", mod_dir_name,"/")

plot.start.year          <- 1990
plot.end.year            <- 2020

sexes           <- c("female", "male")
variables       <- c("prev_bmi_l185", "prev_bmi_30", "prev_double_burden", "prev_bmi_35", "prev_bmi_40", "prev_bmi_30_proportion_double_burden", "prev_bmi_40_proportion_obesity")

names(variables)

age_groups <- c("ageStd")
names(age_groups) <- c("20+ year olds")

my_age_group  <- "ageStd"

outdir <- paste0(maindir,"Postprocessing/Figures/Interactive scatters/")
dir.create(outdir, showWarnings = F)
indir_est <- paste0(maindir,"Postprocessing/Estimates/")

source(paste0(maindir,"Postprocessing/Scripts/Figures/0.0 Region Superregion palette.R"))
source(paste0(maindir,"Postprocessing/Scripts/Figures/0.0 BMI Prevalence palette.R"))
source(paste0(maindir,"Postprocessing/Scripts/Figures/0.6 Arrow plot function.R"))
source(paste0(maindir,"Postprocessing/Scripts/Figures/0.7 plotly functions.R"))


#### 1 LOAD DATA
countrylist <- read.csv(paste0(maindir,"Covariates/country-list-2020.csv"))
countrylist$Superregion <- factor(countrylist$Superregion,
                                  levels = c("High-income Asia Pacific", "East and South East Asia","Oceania",
                                             "High-income Western countries","Central and Eastern Europe","Latin America and Caribbean",
                                             "Central Asia and North Africa-Middle East",
                                             "Sub-Saharan Africa","South Asia"))
data_level <- NULL
data_level <- NULL
for (my_sex in sexes){
  for (my_variable in variables){
    data_level.tmp <- read.csv(paste0(indir_est,"Model",modelnum,"_",my_sex,"_",my_variable,"_ageStd_Country_Means.csv")) %>%
      mutate(sex = my_sex, variable = my_variable) %>%
      select(c("country","year","age_group","mean","l","u","se","sex","variable"))

    data_level      <- rbind(data_level, data_level.tmp)
  }
}
rm(data_level.tmp)
data_level <- data_level %>%
  left_join(countrylist %>% dplyr::rename(country = Country)) %>%
  mutate(mean = mean*100, l = l*100, u = u*100, se = se*100) %>%
  filter(age_group == my_age_group & year %in% seq(plot.start.year, plot.end.year)) %>%
  dplyr::rename(Name = country) %>%
  mutate(region_level = "Country")

#
### 3 GENERATE ARROWS OF PARTI
ps <- list()
for(my_sex in sexes){
  subset_level <- data_level %>% filter(sex == my_sex, variable %in% c("prev_bmi_30_proportion_double_burden", "prev_double_burden"))
  ps[[paste("Arrow", my_sex)]] <- arrow_plot(subset_level, my_sex, my_age_group, yvar = "prev_bmi_30_proportion_double_burden", xvar =  "prev_double_burden",
                         start.year = plot.start.year, end.year = plot.end.year, big_arrow_type = "unweighted_average")

}

cairo_pdf(paste0(outdir," ageStd double burden v prop double burden obesity arrows 1990 2020.pdf"), height = 16, width =30, onefile=T)

for(my_age_group in age_groups){

  age_group.name <- names(age_groups)[which(age_groups == my_age_group)]
  plot.title <- paste0("Change double burden v prop double burden obesity:", age_group.name)

  grid.arrange(
    arrangeGrob(
      textGrob(plot.title,gp=gpar(fontsize=15, fontface = "bold"), hjust = 0, x= 0.01),

      arrangeGrob(
        ps[[paste("Arrow", "female")]],
        ps[[paste("Arrow", "male")]],
        ncol = 2, widths = c(10, 10)
      ),

      nrow = 2,
      heights = c(1, 20)
    )
  )

}

dev.off()
# 