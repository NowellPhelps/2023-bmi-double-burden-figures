outdir_folder <- paste0(outdir, "Scatters/")
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
library(cowplot)

variables         <- c("prev_bmi_l185", "prev_bmi_30")
age_groups        <- c("ageStd")
names(age_groups) <- c("20+ year olds")
my_age_group      <- "ageStd"

# If needed, add in master and loop over these
colour_year       <- plot.start.year
colour_var        <- "prev_bmi_l185"

data_timechange <- read_data_timechange(variables, sexes, age_groups) %>%
  filter(start.year == plot.start.year & end.year == plot.end.year) %>%
  select(-PP.increase)

data <- read_data_level(variables, sexes, "ageStd", "Country")%>%
  filter(age_group %in% age_groups) %>%
  filter(variable == colour_var) %>%
  filter(year == colour_year) %>%
  mutate(variable = "colour_var") %>%
  mutate(start.year = plot.start.year, end.year = plot.end.year) %>%
  select(-c(year, region_level)) %>%
  rbind(data_timechange,.)

data$Superregion <- factor(data$Superregion, levels = unique(data$Superregion))

## 2 GENERATE PLOTLY LEVEL SCATTERS
ps <- list()
for(my_sex in sexes){
  ps[[paste("Scatter", my_sex, "prev_bmi_30", "prev_bmi_l185", "Superregion")]] <- static_scatter_timechange(subset = data, xvar = "prev_bmi_30", yvar = "prev_bmi_l185", my_sex = my_sex,
                                                                                 my_age_group = my_age_group, plot.start.year = plot.start.year, plot.end.year = plot.end.year, background_type = "lines",
                                                                                 facet = T,
                                                                                 colour_by = "change_double_burden")
  
  ps[[paste("Scatter", my_sex,"prev_bmi_30", "prev_bmi_l185", "Underweight level 1990")]] <- static_scatter_timechange(subset = data, xvar = "prev_bmi_30", yvar = "prev_bmi_l185", my_sex = my_sex,
                                                                                                           my_age_group = my_age_group, plot.start.year = plot.start.year, plot.end.year = plot.end.year, background_type = "shaded",
                                                                                                           facet = T,
                                                                                                           colour_by = "level", colour_var_name = colour_var, colour_var_year = colour_year)
}


ps[[paste("Legend", "Superregion")]] <- static_scatter_timechange(subset = data, xvar = "prev_bmi_30", yvar = "prev_bmi_l185", my_sex = my_sex,
                                                                  my_age_group = my_age_group, plot.start.year = plot.start.year, plot.end.year = plot.end.year, background_type = "shaded", 
                                                                  colour_by = "Superregion", returnLeg = T)
ps[[paste("Legend", "Underweight level 1990")]] <- static_scatter_timechange(subset = data, xvar = "prev_bmi_30", yvar = "prev_bmi_l185", my_sex = my_sex,
                                                                             my_age_group = my_age_group, plot.start.year = plot.start.year, plot.end.year = plot.end.year, background_type = "shaded", 
                                                                             colour_by = "level", colour_var_name = colour_var, colour_var_year = colour_year, returnLeg = T)

## Plot
blank <- grid.rect(gp=gpar(col=NA, fill = NA))

cairo_pdf(paste0(outdir_folder, "Scatters of change in underweight against obesity.pdf"), height = 15, width = 30, onefile=T)

grid.arrange(
  
  arrangeGrob(
    blank,
    
    arrangeGrob(textGrob(paste0("Female"),hjust=0,just = c("left"),x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 14)),
                ps[[paste("Scatter", "female", "prev_bmi_30", "prev_bmi_l185", "Superregion")]],
                nrow = 2, heights = c(1, 10)),
    
    blank,
    ps[[paste("Legend", "Superregion")]],
    blank,
    arrangeGrob(textGrob(paste0("Male"),hjust=0,just = c("left"),x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 14)),
                ps[[paste("Scatter", "male", "prev_bmi_30", "prev_bmi_l185", "Superregion")]],
                nrow = 2, heights = c(1, 10)),
    blank,
    
    nrow = 1, ncol = 7, widths = c(0.5,10,0.05, 2, 0.05,10,0.5)
  )
 
  
)

grid.arrange(
  
  arrangeGrob(
    blank,
    
    arrangeGrob(textGrob(paste0("Female"),hjust=0,just = c("left"),x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 14)),
                ps[[paste("Scatter", "female", "prev_bmi_30", "prev_bmi_l185", "Underweight level 1990")]],
                nrow = 2, heights = c(1, 10)),
    
    blank,
    ps[[paste("Legend", "Underweight level 1990")]],
    blank,
    arrangeGrob(textGrob(paste0("Male"),hjust=0,just = c("left"),x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 14)),
                ps[[paste("Scatter", "male", "prev_bmi_30", "prev_bmi_l185", "Underweight level 1990")]],
                nrow = 2, heights = c(1, 10)),
    blank,
    
    nrow = 1, ncol = 7, widths = c(0.5,10,0.05, 2, 0.05,10,0.5)
  )
  
  
)


dev.off()




