figsuffix                <- ""
figsuffix                <- ifelse(figsuffix == "", "", paste0(" ", figsuffix))
outdir_folder            <- paste0(outdir,"Numbered/")

if(!(appendix)){
  figNum          <- ifelse(age_type == "adult", 2, 5)
  age_groups      <- c("ageStd")
} else{
  figNum          <- 12
  age_groups      <- c("young", "mid", "old")
}

if(age_type == "adult"){
  variables <- c("prev_bmi_l185", "prev_bmi_30")
  proportion_var <- "prev_bmi_30_proportion_double_burden"
  fill_scale <- fill_scale_double_burden
} else{
  variables <- c("prev_bmi_neg2sd", "prev_bmi_2sd")
  proportion_var <- "prev_bmi_2sd_proportion_double_burden"
  fill_scale <- fill_scale_double_burden_ado
}

# Read number of sources by country
studies_data <- read_source_data(age_type) %>% select(iso, sex, N_all, N_natl)

################# READ DATA FOR TIMECHANGE #####################################
data_timechange   <- read_data_timechange(variables = c(variables, "prev_double_burden"), sexes, age_type = "ageStd", age_groups = age_groups, region_level = "Country", timechange_type = "absolute") %>% 
  filter(start.year == plot.start.year, end.year == plot.end.year)

#################### READ DATA FOR DECCOMPOSITION ##############################
data_decomposition <- read_data_timechange(variables, sexes, age_groups = age_groups, age_type = "ageStd", region_level = "Country",timechange_type = "absolute") %>% filter(start.year == plot.start.year, end.year == plot.end.year) %>% left_join(., studies_data, by = c("sex", "iso"))

overall_change <- data_decomposition %>%
  group_by(start.year, end.year, age_group, Country, sex) %>%
  summarise(overall_change = sum(mean)) %>%
  ungroup()

data_decomposition          <- left_join(data_decomposition, overall_change, by = c("Country", "age_group", "start.year", "end.year", "sex"))
data_decomposition$variable <- factor(data_decomposition$variable, levels = variables)

##################### READ DATA FOR ARROWS #####################################
data_level         <- read_data_level(variables = c("prev_double_burden", proportion_var, variables), sexes, age_type = "ageStd", region_level = "Country") %>% filter(year == plot.start.year | year == plot.end.year) %>% left_join(., studies_data, by = c("sex", "iso"))


########################  GENERATE PLOTS #######################################
ps <- list()

for(my_sex in c("male","female")){
  
  for(my_age_group in age_groups){
    # Load and prepare data
    if(age_type == "adult"){
      my_xvar <- "prev_bmi_l185"
      my_yvar <- "prev_bmi_30"
      fill_scale <- fill_scale_double_burden
    } else{
      my_xvar <- "prev_bmi_neg2sd"
      my_yvar <- "prev_bmi_2sd"
      fill_scale <- fill_scale_double_burden_ado
    }
    
    # (1) DECOMPOSITION
    col_scale <- scale_colour_manual(values = c("overall" = "black"), labels = c("Combined burden"))
    subset_decomposition <- data_decomposition %>% filter(start.year == plot.start.year & end.year == plot.end.year)
    ps[[paste(my_sex, "decomposition", "ascending", my_age_group)]] <- decomposition_plot(subset_decomposition, my_sex, my_age_group, plot.start.year, plot.end.year,"double burden", ordertype = "ascending", countrycols = F, studyNumbers = T)
    
    ### (2) COMPOSITION
    subset_composition <- data_level %>% filter(variable == proportion_var)
    ps[[paste(my_sex, "composition double burden", "none", my_age_group)]] <- timechange_arrow(subset_composition, var = proportion_var, my_sex = my_sex, my_age_group = my_age_group, start.year = plot.start.year, end.year = plot.end.year, colour_by = "highlight", studyNumbers = T)
    ps[[paste(my_sex, "composition double burden rotated", "none", my_age_group)]] <- timechange_arrow_rotated(subset_composition, var = proportion_var, my_sex = my_sex, my_age_group = my_age_group, start.year = plot.start.year, end.year = plot.end.year, colour_by = "highlight", studyNumbers = T)
    
    
    ### (3) MAPS - LEVEL OF COMPOSITION
    for(my_year in c(plot.start.year, plot.end.year)){
      subset_level  <- data_level %>% filter(variable == proportion_var)
      ps[[paste(my_sex,proportion_var, my_year, "map_level", my_age_group)]] <- maps_level_plots(data = subset_level, proportion_var, my_sex, my_age_group, my_year, appendix = appendix, figNum = figNum)
    }
    
  }
}

ps[[paste("legend", "decomposition")]]   <- decomposition_plot(subset_decomposition, my_sex, my_age_group, plot.start.year, plot.end.year,"double burden", ordertype = "ascending", countrycols = F, returnLeg = T, Legpoint = T)
ps[[paste("legend", "maps")]] <- maps_level_plots(data = subset_level, proportion_var, my_sex, my_age_group, my_year, appendix = appendix, figNum = figNum, returnLeg = T)

blank <- grid.rect(gp=gpar(col=NA, fill = NA))

#### 3 PRINT PDF ###############################################################

cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 14, width = 28, onefile=T)


for (my_sex in sexes){
  for(my_age_group in age_groups){
    
    if(age_type == "adult"){
      if(appendix){
        
        age.longname <- switch(my_age_group,
                               "young" = "20-39 years",
                               "mid" = "40-64 years",
                               "old" = "65+ years")
        
        plot.title <- paste0(ifelse(my_sex=="female", "Women", "Men"), ", ", age.longname)
      } else{
        plot.title <- ifelse(my_sex=="female", "Women", "Men")
        
      }
      
    } else if(age_type == "ado"){
      plot.title <- ifelse(my_sex=="female", "Girls", "Boys")
    }
    
    
    
    grid.arrange(
      textGrob(plot.title,hjust=0,just = c("left"),x = unit(0.005, "npc"),gp = gpar(col = "black", fontsize = 25)), 
      
      arrangeGrob(
        
        arrangeGrob(
          blank,
          arrangeGrob(arrangeGrob(textGrob(paste0("      A"),hjust=0, just = c("top"),x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 20)), blank, ncol =1, heights = c(1,10)),
                      ps[[paste(my_sex, proportion_var, plot.start.year, "map_level", my_age_group)]],
                      blank,
                      ps[[paste(my_sex, proportion_var, plot.end.year, "map_level", my_age_group)]],
                      blank,
                      arrangeGrob(ps[[paste("legend", "maps")]],
                                  blank, 
                                  nrow = 2, heights = c(2.2,1)),
                      blank,
                      ncol = 7, widths = c(1, 20, 1, 20, 1, 5, 1)), 
          blank,
          
          
          arrangeGrob(arrangeGrob(textGrob(paste0("      C"),hjust=0, just = c("top"),x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 20)), blank, ncol =1, heights = c(1,10)), 
                      ps[[paste(my_sex, "decomposition", "ascending", my_age_group)]], 
                      nrow = 1, widths = c(2, 30)),
          
          arrangeGrob(blank,ps[[paste("legend", "decomposition")]],blank, nrow = 1, widths = c(19, 10, 19)),
          blank,
          
          
          nrow = 6, heights = c(0.5,5, 0.3, 8.5, 0.3, 0.3)),
        
        arrangeGrob(textGrob("B",hjust=0,just = c("top"),x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 20)),
                    blank,
                    ncol = 1, 
                    heights = c(1, 35)),
        
        
        ps[[paste(my_sex, "composition double burden", "none", my_age_group)]],
        
        
        nrow = 1, widths = c(19.25, 0.25, 8.5)),
      
      
      
      
      nrow = 2, heights = c(1, 25)
    )
  }
}

dev.off()
