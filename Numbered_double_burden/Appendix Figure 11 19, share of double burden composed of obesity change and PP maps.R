figsuffix                <- ""
figsuffix                <- ifelse(figsuffix == "", "", paste0(" ", figsuffix))
outdir_folder            <- paste0(outdir,"Numbered/")

dir.create(outdir_folder, showWarnings = F)
age_groups <- c("ageStd")

if(age_type == "adult"){
  variables    <- c("prev_bmi_30_proportion_double_burden")
  
  figNums      <- c(11)
  figNum       <- 5
  
} else{
  variables    <- c("prev_bmi_2sd_proportion_double_burden")
  figNums      <- c(19)
  figNum       <- 5
}

############################ 1  LOAD DATA ######################################

# Data for maps
data_timechange <- read_data_timechange(variables, sexes, age_groups, age_type = "ageStd", region_level = "Country", timechange_type = "absolute") %>%
  filter(start.year == plot.start.year, end.year == plot.end.year)

#### 2 PRODUCE FIGURES
ps <- list()
for(my_variable in variables){
  print(my_variable)
  
  for (my_sex in sexes){
    for(my_age_group in age_groups){
      
      subset_timechange <- data_timechange %>% filter(age_group == my_age_group, variable == my_variable)
      
      ps[[paste("Map timechange", my_variable, my_sex, my_age_group)]] <- maps_timeChg_plots(data = subset_timechange, variable = my_variable, my_sex, my_plot.start.year = plot.start.year, my_plot.end.year = plot.end.year,appendix = T, figNum = 5)
      
      ps[[paste("Map PP", my_variable, my_sex, my_age_group)]] <- maps_timeChg_PP_plots(data = subset_timechange, my_variable, my_sex, my_age_group = "ageStd", plot.start.year, plot.end.year,appendix = T, figNum = 5)
      
    }
  }
}


#### 3 PRINT PDF ON GRID
blank <- grid.rect(gp=gpar(col=NA, fill = NA))

for(i in 1:length(variables)){
  my_variable <- variables[i]
  figNum      <- figNums[i]
  
  cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 12, width = 21, onefile=T)
  
  grid.arrange(
    
    arrangeGrob(
      textGrob(paste0("  ", ifelse(age_type == "adult", "Women", "Girls")),hjust=0,just = c("left"),x = unit(0.01, "npc"),gp = gpar(col = "black", fontsize = 20)),
      
      arrangeGrob(blank,
                  
                  ps[[paste("Map timechange", my_variable, "female", my_age_group)]],
                  blank,
                  
                  ps[[paste("Map PP", my_variable, "female", my_age_group)]],
                  blank,
                  nrow = 1, widths = c(1.5,10, 1.5, 10, 0.5)),
      
      textGrob(paste0("  ", ifelse(age_type == "adult", "Men", "Boys")),hjust=0,just = c("left"),x = unit(0.01, "npc"),gp = gpar(col = "black", fontsize = 20)),
      
      arrangeGrob(blank,
                  
                  ps[[paste("Map timechange", my_variable, "male", my_age_group)]],
                  blank,
                  
                  ps[[paste("Map PP", my_variable, "male", my_age_group)]],
                  blank,
                  nrow = 1, widths = c(1.5,10, 1.5, 10, 0.5)),
      
      
      ncol = 1, heights = c(3,20,3,20))
  )
  
  dev.off()
}







