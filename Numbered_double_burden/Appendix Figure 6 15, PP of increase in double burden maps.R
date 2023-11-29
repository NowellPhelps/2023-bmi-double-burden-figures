figsuffix                <- ""
figsuffix                <- ifelse(figsuffix == "", "", paste0(" ", figsuffix))
outdir_folder            <- paste0(outdir,"Numbered/")

figNum          <- ifelse(age_type == "adult", 6, 15)
  
dir.create(outdir_folder, showWarnings = F)

############################ 1  LOAD DATA ######################################

# Data for maps
data_tmchng        <- read_data_timechange(variables = c("prev_double_burden"), sexes, age_groups = c("ageStd"), age_type = "ageStd", region_level = "Country",timechange_type = "absolute") %>% filter(start.year == plot.start.year, end.year == plot.end.year)

#### 2 PRODUCE FIGURES
ps <- list()

for (my_sex in sexes){
    
    for(my_variable in c("prev_double_burden")){
      subset_tmchng <- data_tmchng %>% filter(variable == my_variable)
      ps[[paste(my_sex, my_variable, "map_timechange_PP")]] <- maps_timeChg_PP_plots(data = subset_tmchng, my_variable, my_sex, my_age_group = "ageStd",plot.start.year, plot.end.year)
      
    }
    
   
}


#### 3 PRINT PDF ON GRID
blank <- grid.rect(gp=gpar(col=NA, fill = NA))

cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 14, width = 11, onefile=T)


grid.arrange(
  textGrob(ifelse(age_type == "ado", "Girls", "Women"),
           hjust=0,just = c("left"),x = unit(0.005, "npc"),gp = gpar(col = "black", fontsize = 20)),
  ps[[paste("female", "prev_double_burden", "map_timechange_PP")]],
  blank, 
  textGrob(ifelse(age_type == "ado", "Boys", "Men"),hjust=0,just = c("left"),x = unit(0.005, "npc"),gp = gpar(col = "black", fontsize = 20)),
  ps[[paste("male", "prev_double_burden", "map_timechange_PP")]],
  
  nrow = 5, heights = c(1, 10, .25, 1, 10)
)

dev.off()








