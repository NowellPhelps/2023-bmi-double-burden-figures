figsuffix                <- ""
figsuffix                <- ifelse(figsuffix == "", "", paste0(" ", figsuffix))
outdir_folder            <- paste0(outdir,"Numbered/")


figNum          <- ifelse(age_type == "adult", 2, 5)
age_groups      <- c("ageStd")

if(age_type == "adult"){
  proportion_var <- "prev_bmi_30_proportion_double_burden"
} else{
  proportion_var <- "prev_bmi_2sd_proportion_double_burden"
}


##################### READ DATA FOR MAPS #####################################
data_level         <- read_data_level(variables = c(proportion_var), sexes, age_type = "ageStd", region_level = "Country") %>% 
   filter(year == plot.start.year | year == plot.end.year) %>% 
   left_join(., studies_data, by = c("sex", "iso"))


########################  GENERATE PLOTS #######################################
ps <- list()

for(my_sex in c("male","female")){
  
  for(my_age_group in age_groups){
    # Load and prepare data
    for(my_year in c(plot.start.year, plot.end.year)){
      subset_level  <- data_level %>% filter(variable == proportion_var)
      ps[[paste(my_sex,proportion_var, my_year, "map_level", my_age_group)]] <- maps_level_plots(data = subset_level, proportion_var, my_sex, my_age_group, my_year, appendix = appendix, figNum = figNum)
    }
    
  }
}

ps[[paste("legend", "maps")]] <- maps_level_plots(data = subset_level, proportion_var, my_sex, my_age_group, my_year, appendix = appendix, figNum = figNum, returnLeg = T)

blank <- grid.rect(gp=gpar(col=NA, fill = NA))

#### 3 PRINT PDF ###############################################################
cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 10, width = 17, onefile=T)

grid.arrange(
   
   
   arrangeGrob(
      arrangeGrob(
         blank,
         textGrob(ifelse(age_type == "adult", "Women", "Girls"),hjust=0,just = c("left"),x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 20)),
         arrangeGrob(blank,
                     ps[[paste("female", proportion_var, plot.start.year, "map_level", my_age_group)]],
                     blank,
                     ps[[paste("female", proportion_var, plot.end.year, "map_level", my_age_group)]],
                     blank,
                     nrow = 1, widths = c(.5, 10, .1, 10, .5)),
         blank,
         textGrob(ifelse(age_type == "adult", "Men", "Boys"),hjust=0,just = c("left"),x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 20)),
         arrangeGrob(blank,
                     ps[[paste("male", proportion_var, plot.start.year, "map_level", my_age_group)]],
                     blank,
                     ps[[paste("male", proportion_var, plot.end.year, "map_level", my_age_group)]],
                     blank,
                     nrow = 1, widths = c(.5, 10, .1, 10, .5)),
         blank,
         ncol = 1,
         heights = c(.2,0.2,10,.2,.2,10,.1)),
      
      arrangeGrob(blank,
                  ps[[paste("legend", "maps")]],
                  blank,
                  ncol = 1, heights = c(3,1,3)),
      nrow = 1, 
      widths = c(10,1))
   
 )

dev.off()
   
