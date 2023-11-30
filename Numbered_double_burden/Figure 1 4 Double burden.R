figsuffix                <- ""
figsuffix                <- ifelse(figsuffix == "", "", paste0(" ", figsuffix))
outdir_folder            <- paste0(outdir,"Numbered/")

if(!(appendix)){
   figNum          <- ifelse(age_type == "adult", 1, 4)
   age_groups      <- c("ageStd")
} else{
   figNum          <- 9
   figsuffix       <- "fig1 by age group"
   age_groups      <- c("young", "mid", "old")
}

dir.create(outdir_folder, showWarnings = F)

############################ 1  LOAD DATA ######################################

# Read number of sources by country
studies_data <- read_source_data(age_type) %>% select(iso, sex, N_all, N_natl)

if(age_type == "adult"){
   variables <- c("prev_bmi_l185", "prev_bmi_30")
   proportion_var <- "prev_bmi_30_proportion_double_burden"
   fill_scale <- fill_scale_double_burden
} else{
   variables <- c("prev_bmi_neg2sd", "prev_bmi_2sd")
   proportion_var <- "prev_bmi_2sd_proportion_double_burden"
   fill_scale <- fill_scale_double_burden_ado
}

# Data for maps
data_level         <- read_data_level(variables = c("prev_double_burden", proportion_var, variables), sexes, age_type = "ageStd", region_level = "Country") %>% filter(year == plot.start.year | year == plot.end.year) %>% left_join(., studies_data, by = c("sex", "iso"))
data_tmchng        <- read_data_timechange(variables = c("prev_double_burden", proportion_var, variables), sexes, age_groups = age_groups, age_type = "ageStd", region_level = "Country",timechange_type = "absolute") %>% filter(start.year == plot.start.year, end.year == plot.end.year) %>% left_join(., studies_data,by = c("sex", "iso"))

# Data for wheels and decomposition
data_wheels           <- read_data_level(variables, sexes, age_type = "ageStd", region_level = "Country") %>% filter(year == plot.start.year | year == plot.end.year) %>% left_join(., studies_data, by = c("sex", "iso"))
data_wheels$year     <- factor(data_wheels$year)
data_wheels$variable <- factor(data_wheels$variable, levels = rev(variables), labels = rev(variables))


#################### READ DATA FOR DECCOMPOSITION ##############################
data_decomposition <- read_data_timechange(variables, sexes, age_groups = age_groups, age_type = "ageStd", region_level = "Country",timechange_type = "absolute") %>% filter(start.year == plot.start.year, end.year == plot.end.year) %>% left_join(., studies_data, by = c("sex", "iso"))

overall_change <- data_decomposition %>%
   group_by(start.year, end.year, age_group, Country, sex) %>%
   summarise(overall_change = sum(mean)) %>%
   ungroup()

data_decomposition          <- left_join(data_decomposition, overall_change, by = c("Country", "age_group", "start.year", "end.year", "sex"))
data_decomposition$variable <- factor(data_decomposition$variable, levels = variables)


#### 2 PRODUCE FIGURES
ps <- list()

for (my_sex in sexes){
   for(my_age_group in age_groups){
      
      # (1) MAPS
      for(my_variable in c("prev_double_burden")){
         
         for(my_year in c(plot.end.year)){
            subset_level  <- data_level %>% filter(variable == my_variable)
            ps[[paste(my_sex, my_variable, my_year, "map_level", my_age_group)]] <- maps_level_plots(data = subset_level, my_variable, my_sex, my_age_group, my_year)
         }
         
         subset_tmchng <- data_tmchng %>% filter(variable == my_variable)
         ps[[paste(my_sex, my_variable, "map_timechange", my_age_group)]] <- maps_timeChg_plots(data = subset_tmchng, my_variable, my_sex, my_age_group,plot.start.year, plot.end.year)
         
      }
      
      # (2) WHEELS
      subset_wheels <- data_wheels %>% filter(age_group == my_age_group)
      ps[[paste(my_sex, "wheels", plot.start.year, my_age_group)]]  <- circular_bar_function(subset_wheels, my_sex, my_year = plot.start.year, ifelse(age_type == "adult", "double burden", "ado double burden"), showLegend = F, order_level = "region", order_by = "double_burden", composition = "absolute", studyNumbers = T, appendix = appendix)            
      ps[[paste(my_sex, "wheels", plot.end.year, my_age_group)]]    <- circular_bar_function(subset_wheels, my_sex, my_year = plot.end.year,   ifelse(age_type == "adult", "double burden", "ado double burden"), showLegend = F, order_level = "region", order_by = "double_burden", composition = "absolute", studyNumbers = T, appendix = appendix)            
      
      # (3) DECOMPOSITION
      col_scale <- scale_colour_manual(values = c("overall" = "black"), labels = c("Double burden"))
      subset_decomposition <- data_decomposition %>% filter(start.year == plot.start.year & end.year == plot.end.year)
      ps[[paste(my_sex, "decomposition", "ascending", my_age_group)]] <- decomposition_plot(subset_decomposition, my_sex, my_age_group, plot.start.year, plot.end.year,"double burden", ordertype = "ascending", countrycols = F, studyNumbers = T, fontSize = 3.75)
      
   }
}

ps[[paste("legend", "decomposition")]]   <- decomposition_plot(subset_decomposition, my_sex, my_age_group, plot.start.year, plot.end.year,"double burden", ordertype = "ascending", countrycols = F, returnLeg = T)
##get legend for countries
p_leg <- ggplot(data_wheels %>% mutate(Region = factor(Region, levels = region_order)), aes(x = mean, y = l, colour = Region)) +
   geom_point(size = 2.5, shape = 15) +
   scale_colour_manual(values = region_col) +
   theme_bw() +
   theme(legend.direction = "horizontal",
         legend.text = element_text(size = 10),
         legend.title = element_blank())

ps[[paste("legend", "wheel countries")]] <- get_legend(p_leg)

blank <- grid.rect(gp=gpar(col=NA, fill = NA))

#### 3 PRINT PDF ON GRID


if (appendix){
   cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 14, width = 30, onefile=T)
   for (my_sex in sexes){
      for (my_age_group in age_groups){
         
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
         
         
         if(appendix){
            
         }
         
         
         grid.arrange(
            textGrob(plot.title,hjust=0,just = c("left"),x = unit(0.005, "npc"),gp = gpar(col = "black", fontsize = 25)),
            
            arrangeGrob(
               arrangeGrob(arrangeGrob(arrangeGrob(arrangeGrob(blank,textGrob("A",hjust=0, just = c("left"),x = unit(0.01, "npc"),gp = gpar(col = "black", fontsize = 20)), nrow =1, widths = c(.3,.7)),
                                                   blank, ncol = 1, heights = c(1, 10)),ps[[paste(my_sex, "wheels", plot.start.year, my_age_group)]],blank, ps[[paste(my_sex, "wheels", plot.end.year, my_age_group)]], nrow = 1, widths = c(0.13,1,0.12,1,0.12)),
                           arrangeGrob(blank,blank,blank, nrow = 1, widths = c(19, 2, 19)),
                           nrow = 2, heights = c(20, 1)),
               arrangeGrob(textGrob("B",hjust=0,just = c("left"),x = unit(0.01, "npc"),gp = gpar(col = "black", fontsize = 20)),
                           ps[[paste(my_sex, "prev_double_burden", plot.end.year, "map_level", my_age_group)]],
                           ps[[paste(my_sex,"prev_double_burden", "map_timechange", my_age_group)]],
                           nrow = 3, heights = c(0.1, 1,1)),
               nrow = 1, widths = c(20,8)),
            
            arrangeGrob(blank, ps[[paste("legend", "decomposition")]], blank, ps[[paste("legend", "wheel countries")]], blank,blank, 
                        nrow = 1, widths = c(2,2.5,2.5,10.5,2.5,8)),
            
            nrow = 3, heights = c(1, 25, 3))
         
      }
   }
   dev.off()
   
} else{
   
   cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 17, width = 17.2, onefile=T)
   
   grid.arrange(
      arrangeGrob(
         textGrob(paste0("A ", ifelse(age_type == "adult", "Women", "Girls")), hjust=0, vjust = 1, just = c("left", "top"),x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 30)),
         blank,
         arrangeGrob(blank,
                     ps[[paste("female", "wheels", plot.start.year, my_age_group)]],
                     blank,
                     ps[[paste("female", "wheels", plot.end.year, my_age_group)]],
                     blank,
                     nrow = 1, widths = c(.5, 10, .2, 10, .5)),
         
         blank,
         arrangeGrob(blank, ps[[paste("legend", "decomposition")]], blank, ps[[paste("legend", "wheel countries")]], blank, 
                     nrow = 1, widths = c(1,2,1,10,1)),
         blank,
         arrangeGrob(blank,
                     ps[[paste("female","prev_double_burden", "map_timechange", my_age_group)]],
                     blank,
                     ps[[paste("female", "prev_double_burden", plot.end.year, "map_level", my_age_group)]],
                     blank,
                     nrow = 1, widths = c(.5, 10, .12, 10, .5)),
         ncol = 1,
         heights = c(1,1,10,1.1,1,1,7)
      ))
   
   grid.arrange(
      arrangeGrob(
         textGrob(paste0("B ", ifelse(age_type == "adult", "Men", "Boys")), hjust=0, vjust = 1, just = c("left", "top"),x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 30)),
         blank,
         arrangeGrob(blank,
                     ps[[paste("male", "wheels", plot.start.year, my_age_group)]],
                     blank,
                     ps[[paste("male", "wheels", plot.end.year, my_age_group)]],
                     blank,
                     nrow = 1, widths = c(.5, 10, .2, 10, .5)),
         
         blank,
         arrangeGrob(blank, ps[[paste("legend", "decomposition")]], blank, ps[[paste("legend", "wheel countries")]], blank, 
                     nrow = 1, widths = c(1,2,1,10,1)),
         blank,
         arrangeGrob(blank,
                     ps[[paste("male","prev_double_burden", "map_timechange", my_age_group)]],
                     blank,
                     ps[[paste("male", "prev_double_burden", plot.end.year, "map_level", my_age_group)]],
                     blank,
                     nrow = 1, widths = c(.5, 10, .12, 10, .5)),
         ncol = 1,
         heights = c(1,1,10,1.1,1,1,7)
      ))
   
   dev.off()
}



