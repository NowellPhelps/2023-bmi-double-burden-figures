figsuffix                <- ""
figsuffix                <- ifelse(figsuffix == "", "", paste0(" ", figsuffix))
outdir_folder            <- paste0(outdir,"Numbered/")

if(!(appendix)){
  figNum          <- ifelse(age_type == "adult", 3, 6)
  age_groups      <- c("ageStd")
} else{
  figNum          <- 10
  age_groups      <- c("young", "mid", "old")
}

if (age_type == "adult"){
  variables    <- c("prev_bmi_l185", "prev_bmi_30")
} else if (age_type == "ado"){
  variables    <- c("prev_bmi_neg2sd", "prev_bmi_2sd")
} 

my_xvar      <- variables[1]
my_yvar      <- variables[2]

###################### READ TIMECHANGE SCATTERS DATA ###########################
data_timechange_scatters <- read_data_timechange(variables, sexes, age_groups) %>%
  filter(start.year == plot.start.year & end.year == plot.end.year) %>%
  mutate(start.year = plot.start.year, end.year = plot.end.year) %>%
  select(-PP.increase) 


###################### DECOMPOSITION PLOTS  DATA ###############################
data_decomposition <- read_data_timechange(variables, sexes, age_groups = age_groups, age_type = "ageStd", region_level = "Country",timechange_type = "absolute") %>% filter(start.year == plot.start.year, end.year == plot.end.year)

overall_change <- data_decomposition %>%
  group_by(start.year, end.year, age_group, Country, sex) %>%
  summarise(overall_change = sum(mean)) %>%
  ungroup()

data_decomposition          <- left_join(data_decomposition, overall_change, by = c("Country", "age_group", "start.year", "end.year", "sex"))
data_decomposition$variable <- factor(data_decomposition$variable, levels = variables)


###################### LEVEL VS CHANGE SCATTERS DATA ############################
data_level_scatters  <- read_data_level(variables, sexes, age_type = "ageStd", region_level = "Country") %>%
  filter(age_group %in% age_groups) %>%
  filter(year == plot.start.year) %>%
  select(-c(l,u,se))

change_scatters <- read_data_timechange(variables, sexes, age_groups = age_groups, age_type = "ageStd") %>%
  filter(start.year == plot.start.year & end.year == plot.end.year) %>%
  dplyr::rename(change = mean) %>%
  select(-c(l,u,se, PP.increase))

scatters_level_v_change_data <- left_join(data_level_scatters, change_scatters)

########################  GENERATE PLOTS #######################################
ps <- list()
for(my_sex in c("male","female")){
  
  for(my_age_group in age_groups){
    
    subset_timechange_scatters   <- data_timechange_scatters
    subset_level_v_change        <- scatters_level_v_change_data 
    
    ## (1) Scatters of change
    ps[[paste("Scatter timechange", my_sex, my_age_group)]] <- static_scatter_change_v_change(subset_timechange_scatters, xvar = my_xvar, yvar = my_yvar, my_sex = my_sex, my_age_group = my_age_group, plot.start.year = plot.start.year, plot.end.year= plot.end.year, returnLeg = F)
    
    ### (3) SCATTERS OF LEVEL V CHANGE
    for (var in variables){
      ps[[paste("Scatter change v level", var,  my_sex, my_age_group)]] <- static_scatter_timechange_v_level(subset_level_v_change, var, my_sex, my_age_group, plot.start.year, plot.end.year, level.year = plot.start.year)
      
    }
  }
  
  
}

ps[["legend"]] <- static_scatter_timechange_v_level(subset_level_v_change, var, my_sex, my_age_group, plot.start.year, plot.end.year, level.year = plot.start.year, returnLeg = T)



#### 3 PRINT PDF ###############################################################
blank <- grid.rect(gp=gpar(col=NA, fill = NA))
plot.title <- paste0("change in age-standardised obesity and underweight ", plot.start.year, "-", plot.end.year)

cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 18, width = 16, onefile=T)

for(my_age_group in age_groups){
  
  if(appendix){
    age.longname <- switch(my_age_group,
                           "young" = "20-39 years",
                           "mid" = "40-64 years",
                           "old" = "65+ years")
  } 
  
  grid.arrange(
    arrangeGrob(
      
      arrangeGrob(
        blank,
        
        arrangeGrob(
          blank,
          arrangeGrob(blank,
                      textGrob(paste0("   ", ifelse(age_type == "adult", ifelse(appendix, paste0("Women, ", age.longname),"Women"), "Girls")),hjust=0,just = c("left"), x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 18)),
                      blank,
                      nrow = 1, widths = c(.5, 10, .5)),
          blank,
          
          arrangeGrob(arrangeGrob(textGrob(paste0("A"), hjust=0, just = c("left"), x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 20)), blank, nrow = 2, heights = c(1,10)),
                      ps[[paste("Scatter change v level", my_xvar,  "female", my_age_group)]],
                      blank,
                      ncol = 3, 
                      widths = c(.5, 10, .5)),
          blank,
          
          arrangeGrob(arrangeGrob(textGrob(paste0("B"), hjust=0, just = c("left"), x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 20)), blank, nrow = 2, heights = c(1,10)),
                      ps[[paste("Scatter change v level", my_yvar, "female", my_age_group)]],
                      blank,
                      ncol = 3,
                      widths = c(.5, 10, .5)),
          blank,
          
          arrangeGrob(arrangeGrob(textGrob(paste0("C"), hjust=0, just = c("left"), x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 20)), blank, nrow = 2, heights = c(1,10)),
                      ps[[paste("Scatter timechange", "female", my_age_group)]] ,
                      blank,
                      ncol = 3, 
                      widths = c(.5, 10, .5)),
          
          nrow = 8, heights = c(.3,.2,.1,5,0.3,5,0.3,5)
        ),
        
        blank,
        
        arrangeGrob(
          blank,
          arrangeGrob(blank,
                      textGrob(paste0("   ", ifelse(age_type == "adult", ifelse(appendix, paste0("Men, ", age.longname),"Men"), "Boys")),hjust=0,just = c("left"), x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 18)),
                      blank,
                      nrow = 1, widths = c(.5, 10, .5)),   
          
          blank,
          
          arrangeGrob(blank, 
                      ps[[paste("Scatter change v level", my_xvar,  "male", my_age_group)]],
                      blank,
                      ncol = 3, 
                      widths = c(.5, 10, .5)),
          
          blank,
          
          arrangeGrob(blank, 
                      ps[[paste("Scatter change v level", my_yvar, "male", my_age_group)]],
                      blank,
                      ncol = 3, 
                      widths = c(.5, 10, .5)),
          blank,
          
          arrangeGrob(blank, 
                      ps[[paste("Scatter timechange", "male", my_age_group)]],
                      blank,
                      ncol = 3, 
                      widths = c(.5, 10, .5)),
          
          nrow = 8, heights = c(.3,.2,.1,5,0.3,5,0.3,5)
        ),
        blank,
        ncol = 5, 
        widths = c(.05,1,.03,1, .05)
      )
      ,
      blank, 
      ps[["legend"]],
      nrow = 3, 
      heights = c(100, 1,3)
      
    )
  )
  
}



dev.off()

