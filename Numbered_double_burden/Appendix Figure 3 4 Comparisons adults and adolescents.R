figsuffix                <- ""
figsuffix                <- ifelse(figsuffix == "", "", paste0(" ", figsuffix))
outdir_folder            <- paste0(outdir,"Numbered/")
dir.create(outdir_folder, showWarnings = F)

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
data_level_adult <- read_data_level(variables = c("prev_bmi_l185", "prev_bmi_30"), sexes = c("female", "male"), age_type = "ageStd", region_level = "Country", age_level = "adult") %>%
  filter(year == plot.start.year | year == plot.end.year) %>%
  filter(age_group == "ageStd")

data_level <- read_data_level(variables = c("prev_bmi_neg2sd", "prev_bmi_2sd"), sexes = c("female", "male"), age_type = "ageStd", region_level = "Country", age_level = "ado") %>%
  filter(year == plot.start.year | year == plot.end.year) %>%
  filter(age_group == "ageStd") %>%
  rbind(., data_level_adult)

rm(data_level_adult)

############################# GENERATE PLOTS ####################################
ps <- list()
for (my_sex in sexes){
  for(my_year in c(plot.start.year, plot.end.year)){
    for(variables in list(c("prev_bmi_neg2sd","prev_bmi_l185"), c("prev_bmi_2sd","prev_bmi_30"))){
      xvar <- variables[1]
      yvar <- variables[2]
      
      
      subset <- data_level %>%
        filter(variable == xvar | variable == yvar)
      
      ps[[paste0(my_sex, my_year, xvar, yvar)]] <- static_scatter_level_v_level(subset, xvar, yvar, my_sex, my_age_group, plot.year = my_year)

    }
  }
}
ps[["legend"]] <- static_scatter_level_v_level(subset, xvar, yvar, my_sex, my_age_group, plot.year = my_year, returnLeg = T)



########################## PRINT PDF ###########################################
blank <- grid.rect(gp=gpar(col=NA, fill = NA))

## (A) PLOT THINNESS RESULTS - APPENDIX FIGURE 16
figNum <- 3
cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 12, width = 12, onefile=T)

for(my_age_group in age_groups){
  
  grid.arrange(
    arrangeGrob(
      
      arrangeGrob(
        blank,
        
        arrangeGrob(
          blank,
          blank,
          blank,
          
          arrangeGrob(blank,
                      ps[[paste0("female", plot.start.year, "prev_bmi_neg2sd", "prev_bmi_l185")]],
                      blank,
                      ncol = 3, 
                      widths = c(.5, 10, .5)),
          blank,
          
          arrangeGrob(blank,
                      ps[[paste0("male", plot.start.year, "prev_bmi_neg2sd", "prev_bmi_l185")]],
                      blank,
                      ncol = 3,
                      widths = c(.5, 10, .5)),
          blank,
          
          
          ncol = 1, heights = c(.3,.2,.1,5,0.3,5,0.3)
        ),
        
        blank,
        
        arrangeGrob(
          blank,
          blank,
          blank,
          
          arrangeGrob(blank,
                      ps[[paste0("female", plot.end.year, "prev_bmi_neg2sd", "prev_bmi_l185")]],
                      blank,
                      ncol = 3, 
                      widths = c(.5, 10, .5)),
          blank,
          
          arrangeGrob(blank,
                      ps[[paste0("male", plot.end.year, "prev_bmi_neg2sd", "prev_bmi_l185")]],
                      blank,
                      ncol = 3,
                      widths = c(.5, 10, .5)),
          blank,
          
          
          ncol = 1, heights = c(.3,.2,.1,5,0.3,5,0.3)
        ),
        blank,
        ncol = 5, 
        widths = c(.05,1,.03,1, .05)
      ),
      blank, 
      ps[["legend"]],
      nrow = 3, 
      heights = c(65, 1,3)
      
    )
  )
  
}

dev.off()


## (A) PLOT OBESITY RESULTS - APPENDIX FIGURE 17
figNum <- 4
cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 12, width = 12, onefile=T)

for(my_age_group in age_groups){
  
  grid.arrange(
    arrangeGrob(
      
      arrangeGrob(
        blank,
        
        arrangeGrob(
          blank,
          blank,
          blank,
          
          arrangeGrob(blank,
                      ps[[paste0("female", plot.start.year, "prev_bmi_2sd", "prev_bmi_30")]],
                      blank,
                      ncol = 3, 
                      widths = c(.5, 10, .5)),
          blank,
          
          arrangeGrob(blank,
                      ps[[paste0("male", plot.start.year, "prev_bmi_2sd", "prev_bmi_30")]],
                      blank,
                      ncol = 3,
                      widths = c(.5, 10, .5)),
          blank,
          
          
          ncol = 1, heights = c(.3,.2,.1,5,0.3,5,0.3)
        ),
        
        blank,
        
        arrangeGrob(
          blank,
          blank,
          blank,
          
          arrangeGrob(blank,
                      ps[[paste0("female", plot.end.year, "prev_bmi_2sd", "prev_bmi_30")]],
                      blank,
                      ncol = 3, 
                      widths = c(.5, 10, .5)),
          blank,
          
          arrangeGrob(blank,
                      ps[[paste0("male", plot.end.year, "prev_bmi_2sd", "prev_bmi_30")]],
                      blank,
                      ncol = 3,
                      widths = c(.5, 10, .5)),
          blank,
          
          
          ncol = 1, heights = c(.3,.2,.1,5,0.3,5,0.3)
        ),
        blank,
        ncol = 5, 
        widths = c(.05,1,.03,1, .05)
      ),
      blank, 
      ps[["legend"]],
      nrow = 3, 
      heights = c(65, 1,3)
      
    )
  )
  
}

dev.off()

