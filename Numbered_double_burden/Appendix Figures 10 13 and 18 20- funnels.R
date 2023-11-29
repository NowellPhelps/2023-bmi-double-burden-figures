# Appendix funnels
appendix <- T
library(tidyverse)
library(ggnewscale)
library(reshape)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)
library(htmlwidgets)
library(htmltools)
figsuffix                <- ""
figsuffix                <- ifelse(figsuffix == "", "", paste0(" ", figsuffix))
outdir_folder            <- paste0(outdir,"Numbered/")
outdir_folder_interactive <- paste0(outdir,"Interactive funnels/")
dir.create(outdir_folder_interactive, showWarnings= F)

if(age_type == "adult"){
  level_variables   <- c("prev_bmi_30_proportion_double_burden")
  change_variables  <- c("prev_bmi_l185", "prev_bmi_30", "prev_bmi_30_proportion_double_burden", "prev_double_burden")
  figNums            <- c(10, 13)
  
} else{
  level_variables   <- c("prev_bmi_2sd_proportion_double_burden")
  change_variables  <- c("prev_bmi_neg2sd", "prev_bmi_2sd", "prev_bmi_2sd_proportion_double_burden", "prev_double_burden")
  figNums            <- c(18, 20)
}

age_groups   <- c("ageStd")
my_age_group <- "ageStd"



# LOAD DATA
data_timechange <- read_data_timechange(change_variables, sexes, age_groups, age_type = "ageStd", region_level = "Country", timechange_type = "absolute") %>%
  filter(start.year == plot.start.year, end.year == plot.end.year)

data_level      <- read_data_level(level_variables, sexes, age_type = "ageStd", region_level = "Country", includePP = T) %>%
  filter(age_group %in% age_groups)  %>%
  filter(year == plot.start.year | year == plot.end.year)

# GET FUNNEL PLOTS OF CHANGE
ps <- list()

for(my_variable in change_variables){
  for (my_sex in sexes){
    ps[[paste("change", my_variable, my_sex)]] <- funnel_plots(data = data_timechange, variable = my_variable, type = "change", my_sex,  returnLeg = F)
    #ps[[paste("change interactive", my_variable, my_sex)]] <- funnel_plots_interactive(data = data_timechange,variable = my_variable, type = "change", my_sex = my_sex, returnLeg = F)
    
  }
}
ps[["legend"]] <- funnel_plots(data = data_timechange, variable = my_variable, type = "change", my_sex,  returnLeg = T)

# 
# for(my_variable in change_variables){
#   
#   a <- manipulateWidget::combineWidgets(ps[[paste("change interactive", my_variable, my_sex)]],
#                                         ncol = 1, height = 700, width = 1500)
#   
#   saveWidget(a, paste0(outdir_folder_interactive, "change in ",my_variable, " ",my_sex, ".html"), selfcontained = F, libdir = "lib")
#   
# }

for(my_variable in level_variables){
  for(plot.year in c(plot.start.year, plot.end.year)){
    for (my_sex in sexes){
      ps[[paste("level", my_variable, my_sex, plot.year)]] <- funnel_plots(data = data_level,variable = my_variable, type = "level", my_sex, plot.year = plot.year, returnLeg = F)
      #ps[[paste("level interactive", my_variable, my_sex, plot.year)]] <- funnel_plots_interactive(data = data_level,variable = my_variable, type = "level", my_sex, plot.year = plot.year, returnLeg = F)
    }
  }
}

ps[["legend"]] <- funnel_plots(data = data_level, variable, type = "level", my_sex, plot.year = plot.year, returnLeg = T)
# 
# for(my_variable in level_variables){
#   for(plot.year in c(plot.start.year, plot.end.year)){
#     for (my_sex in sexes){
#       a <- manipulateWidget::combineWidgets(ps[[paste("level interactive", my_variable, my_sex, plot.year)]],
#                                             ncol = 1, height = 700, width = 1500)
#       
#       saveWidget(a, paste0(outdir_folder_interactive, "level in ",my_variable, " ",my_sex, " ", plot.year, ".html"), selfcontained = F, libdir = "lib")
#       
#     }
#   }
# }

################ PLOT MAPS LEVEL AND TIMECHANGE ################################
blank <- grid.rect(gp=gpar(col=NA, fill = NA))

# SAVE FUNNELS OF LEVEL
cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNums[1], figsuffix,".pdf"), height = 11, width = 18, onefile=T)

grid.arrange(

  arrangeGrob(

    arrangeGrob(
      blank,
      textGrob(ifelse(age_type == "adult", "Women", "Girls"),hjust=0,just = c("left"),x = unit(0.05, "npc"),gp = gpar(col = "black", fontsize = 20)),
      arrangeGrob(blank,
                  ps[[paste("level", level_variables[1], "female", plot.start.year)]],
                  blank,
                  ps[[paste("level", level_variables[1], "female", plot.end.year)]],
                  blank,
                  nrow = 1, widths = c(.5, 10, 1, 10, .5)),
      blank,
      textGrob(ifelse(age_type == "adult", "Men", "Boys"),hjust=0,just = c("left"),x = unit(0.05, "npc"),gp = gpar(col = "black", fontsize = 20)),
      arrangeGrob(blank,
                  ps[[paste("level", level_variables[1], "male", plot.start.year)]],
                  blank,
                  ps[[paste("level", level_variables[1], "male", plot.end.year)]],
                  blank,
                  nrow = 1, widths = c(.5, 10, 1, 10, .5)),
      blank,
      ncol = 1,
      heights = c(.5,1,10,.5,1,10,.5)
    ),

    arrangeGrob(blank,
                arrangeGrob(blank,
                            ps[["legend"]],
                            blank,
                            nrow = 1,
                            widths = c(0.3, 1, 0.3)),
                blank,
                ncol = 1,
                heights = c(0.3,1,0.3)),
    nrow = 1,
    widths = c(2.5,1)

    )
)
dev.off()

## SAVE FUNNELS OF CHANGE
blank <- grid.rect(gp=gpar(col=NA, fill = NA))


cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNums[2], figsuffix,".pdf"), height = 11, width = 18, onefile=T)

grid.arrange(

  arrangeGrob(

    arrangeGrob(
      blank,
      textGrob(ifelse(age_type == "adult", "Women", "Girls"),hjust=0,just = c("left"),x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 20)),
      arrangeGrob(blank,
                  ps[[paste("change", change_variables[1], "female")]],
                  blank,
                  ps[[paste("change", change_variables[2], "female")]],
                  blank,
                  nrow = 1, widths = c(.5, 10, 1, 10, .5)),
      blank,
      blank,
      arrangeGrob(blank,
                  ps[[paste("change", change_variables[4], "female")]],
                  blank,
                  ps[[paste("change", change_variables[3], "female")]],
                  blank,
                  nrow = 1, widths = c(.5, 10, 1, 10, .5)),
      blank,
      ncol = 1,
      heights = c(.5,1,10,.5,1,10,.5)
    ),

    arrangeGrob(blank,
                arrangeGrob(blank,
                            ps[["legend"]],
                            blank,
                            nrow = 1,
                            widths = c(0.3, 1, 0.3)),
                blank,
                ncol = 1,
                heights = c(0.3,1,0.3)),
    nrow = 1,
    widths = c(2.5,1)

  )
)


grid.arrange(

  arrangeGrob(

    arrangeGrob(
      blank,
      textGrob(ifelse(age_type == "adult", "Men", "Boys"),hjust=0,just = c("left"),x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 20)),
      arrangeGrob(blank,
                  ps[[paste("change", change_variables[1], "male")]],
                  blank,
                  ps[[paste("change", change_variables[2], "male")]],
                  blank,
                  nrow = 1, widths = c(.5, 10, 1, 10, .5)),
      blank,
      blank,
      arrangeGrob(blank,
                  ps[[paste("change", change_variables[4], "male")]],
                  blank,
                  ps[[paste("change", change_variables[3], "male")]],
                  blank,
                  nrow = 1, widths = c(.5, 10, 1, 10, .5)),
      blank,
      ncol = 1,
      heights = c(.5,1,10,.5,1,10,.5)
    ),

    arrangeGrob(blank,
                arrangeGrob(blank,
                            ps[["legend"]],
                            blank,
                            nrow = 1,
                            widths = c(0.3, 1, 0.3)),
                blank,
                ncol = 1,
                heights = c(0.3,1,0.3)),
    nrow = 1,
    widths = c(2.5,1)

  )
)

dev.off()

################ PLOT MAPS LEVEL AND TIMECHANGE WITH PP ################################


## SAVE FUNNELS OF LEVEL
# cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNums[1], figsuffix,".pdf"), height = 11, width = 18, onefile=T)
# 
# grid.arrange(
#   
#   arrangeGrob(
#     
#     arrangeGrob(
#       blank,
#       textGrob(ifelse(age_type == "adult", "Women", "Girls"),hjust=0,just = c("left"),x = unit(0.05, "npc"),gp = gpar(col = "black", fontsize = 20)),
#       arrangeGrob(blank,
#                   ps[[paste("level interactive", level_variables[1], "female", plot.start.year)]],
#                   blank,
#                   ps[[paste("level interactive", level_variables[1], "female", plot.end.year)]],
#                   blank,
#                   nrow = 1, widths = c(.5, 10, 1, 10, .5)),
#       blank, 
#       textGrob(ifelse(age_type == "adult", "Men", "Boys"),hjust=0,just = c("left"),x = unit(0.05, "npc"),gp = gpar(col = "black", fontsize = 20)),
#       arrangeGrob(blank,
#                   ps[[paste("level interactive", level_variables[1], "male", plot.start.year)]],
#                   blank,
#                   ps[[paste("level interactive", level_variables[1], "male", plot.end.year)]],
#                   blank,
#                   nrow = 1, widths = c(.5, 10, 1, 10, .5)),
#       blank, 
#       ncol = 1, 
#       heights = c(.5,1,10,.5,1,10,.5)
#     ),
#     
#     arrangeGrob(blank,
#                 arrangeGrob(blank, 
#                             ps[["legend"]],
#                             blank,
#                             nrow = 1, 
#                             widths = c(0.3, 1, 0.3)),
#                 blank,
#                 ncol = 1, 
#                 heights = c(0.3,1,0.3)),
#     nrow = 1,
#     widths = c(2.5,1)
#     
#   )
# )
# dev.off()    
# 
# ## SAVE FUNNELS OF CHANGE
# cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNums[2], figsuffix,".pdf"), height = 11, width = 18, onefile=T)
# 
# grid.arrange(
#   
#   arrangeGrob(
#     
#     arrangeGrob(
#       blank,
#       textGrob(ifelse(age_type == "adult", "Women", "Girls"),hjust=0,just = c("left"),x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 20)),
#       arrangeGrob(blank,
#                   ps[[paste("change interactive", change_variables[1], "female")]],
#                   blank,
#                   ps[[paste("change interactive", change_variables[2], "female")]],
#                   blank,
#                   nrow = 1, widths = c(.5, 10, 1, 10, .5)),
#       blank, 
#       blank,
#       arrangeGrob(blank,
#                   ps[[paste("change interactive", change_variables[4], "female")]],
#                   blank,
#                   ps[[paste("change interactive", change_variables[3], "female")]],
#                   blank,
#                   nrow = 1, widths = c(.5, 10, 1, 10, .5)),
#       blank, 
#       ncol = 1, 
#       heights = c(.5,1,10,.5,1,10,.5)
#     ),
#     
#     arrangeGrob(blank,
#                 arrangeGrob(blank,
#                             ps[["legend"]],
#                             blank,
#                             nrow = 1, 
#                             widths = c(0.3, 1, 0.3)),
#                 blank,
#                 ncol = 1, 
#                 heights = c(0.3,1,0.3)),
#     nrow = 1,
#     widths = c(2.5,1)
#     
#   )
# )
# 
# 
# grid.arrange(
#   
#   arrangeGrob(
#     
#     arrangeGrob(
#       blank,
#       textGrob(ifelse(age_type == "adult", "Men", "Boys"),hjust=0,just = c("left"),x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 20)),
#       arrangeGrob(blank,
#                   ps[[paste("change interactive", change_variables[1], "male")]],
#                   blank,
#                   ps[[paste("change interactive", change_variables[2], "male")]],
#                   blank,
#                   nrow = 1, widths = c(.5, 10, 1, 10, .5)),
#       blank, 
#       blank,
#       arrangeGrob(blank,
#                   ps[[paste("change interactive", change_variables[4], "male")]],
#                   blank,
#                   ps[[paste("change interactive", change_variables[3], "male")]],
#                   blank,
#                   nrow = 1, widths = c(.5, 10, 1, 10, .5)),
#       blank, 
#       ncol = 1, 
#       heights = c(.5,1,10,.5,1,10,.5)
#     ),
#     
#     arrangeGrob(blank,
#                 arrangeGrob(blank, 
#                             ps[["legend"]],
#                             blank,
#                             nrow = 1, 
#                             widths = c(0.3, 1, 0.3)),
#                 blank,
#                 ncol = 1, 
#                 heights = c(0.3,1,0.3)),
#     nrow = 1,
#     widths = c(2.5,1)
#     
#   )
# )
# 
# dev.off()    








