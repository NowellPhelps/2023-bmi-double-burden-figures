library(tidyverse)
library(ggforce)
library(egg)
library(gridExtra)
library(grid)

outdir_folder <- paste0(outdir, "Sankey plots/")
dir.create(outdir_folder, showWarnings = F)

age_group <- c("ageStd")
my_age_group <- "ageStd"

variables <- c("prev_bmi_l185", "prev_bmi_30_35", "prev_bmi_35_40", "prev_bmi_40")
data_level_global <- read_data_level(variables, sexes, age_type = "ageStd", region_level = "Global") %>% 
  dplyr::rename(Name = Global, age_group = age)
data_level_region <- read_data_level(variables, sexes, age_type = "ageStd", region_level = "Region") %>% 
  dplyr::rename(Name = Region, age_group = age)

data_level <- rbind(data_level_global, data_level_region) %>%
  filter(age_group == my_age_group) %>%
  filter(year == plot.start.year | year == plot.end.year) %>%
  select(-se, -l, -u) 

overall_double_burden <- data_level %>%
  group_by(Name, year, age_group, sex, region_level) %>% 
  summarise(overall = sum(mean)) %>%
  ungroup()

data_level <- data_level %>%
  left_join(., overall_double_burden, by = c("Name", "year", "age_group", "sex", "region_level")) %>%
  mutate(prev = mean/overall) %>%
  select(-mean) %>%
  pivot_wider(names_from = variable, values_from = prev) %>%
  mutate(cat0 = 1,
         cat1 = prev_bmi_30_35 + prev_bmi_35_40 + prev_bmi_40,
         cat2 = prev_bmi_l185,
         cat3 = prev_bmi_35_40 + prev_bmi_40,
         cat4 = prev_bmi_30_35,
         cat5 = prev_bmi_40,
         cat6 = prev_bmi_35_40) %>%
  select(-c(prev_bmi_l185, prev_bmi_30_35, prev_bmi_35_40, prev_bmi_40)) %>%
  gather(cat, prev, cat0:cat6) %>%
  mutate(prev = prev * 100)


subset_global  <- data_level %>% filter(region_level == "Global")
subset_region  <- data_level %>% filter(region_level == "Region")


ps <- list()

for(my_sex in sexes){
  for (plot.year in c(plot.start.year, plot.end.year)){
    
    ps[[paste("Global", my_sex, plot.year)]] <- plot_sankey(subset_global %>% filter(sex == my_sex, year == plot.year), gtitle = paste0("All ", ifelse(my_sex == "male", "men", "women"), " with malnutrition (World)"))
    
    for (my_region in unique(countrylist$Region)) {
      ps[[paste(my_region, my_sex, plot.year)]]   <- plot_sankey(subset_region %>% subset(Name == my_region & sex == my_sex & year == plot.year), my_region, region_level = "Region")
    }
  }
}

blank <- grid.rect(gp=gpar(col="white"))

for (my_sex in sexes){
  
  cairo_pdf(paste0(outdir_folder, " Sankey plots by region ", plot.start.year, " - ", plot.end.year, " ", my_sex, ".pdf"), height = 15, width = 15, onefile=T)
  
  for (plot.year in c(plot.start.year, plot.end.year)){
    
    plot.title <- paste(switch(my_sex, "female" = "Women", "male" = "Men"), plot.year)
    
    grid.arrange(
      arrangeGrob(
        textGrob(plot.title,hjust=0,x = 0.05, gp = gpar(col = "black", fontsize = 18)),
        arrangeGrob(
          arrangeGrob(
            ps[[paste("Central Asia", my_sex, plot.year)]],
            ps[[paste("South Asia", my_sex, plot.year)]],
            ps[[paste("South East Asia", my_sex, plot.year)]],
            ps[[paste("East Asia", my_sex, plot.year)]],
            ps[[paste("High-income Asia Pacific", my_sex, plot.year)]],
            ps[[paste("High-income English-speaking countries", my_sex, plot.year)]],
            ps[[paste("North Western Europe", my_sex, plot.year)]],
            ps[[paste("South Western Europe", my_sex, plot.year)]],
            ps[[paste("Central Europe", my_sex, plot.year)]],
            ps[[paste("Eastern Europe", my_sex, plot.year)]],
            ps[[paste("North Africa and Middle East", my_sex, plot.year)]],
            ps[[paste("West Africa", my_sex, plot.year)]],
            ps[[paste("Central Africa", my_sex, plot.year)]],
            ps[[paste("East Africa", my_sex, plot.year)]],
            ps[[paste("Southern Africa", my_sex, plot.year)]],
            ncol=5,nrow=3,
            heights=unit.c(unit((1/3),"npc"),unit((1/3),"npc"),unit((1/3),"npc")),
            widths=unit.c(unit(0.215,"npc"),rep(unit(.19625, "npc"),4))),
          
          arrangeGrob(
            arrangeGrob(
              ps[[paste("Central Latin America", my_sex, plot.year)]],
              ps[[paste("Andean Latin America", my_sex, plot.year)]],
              ps[[paste("Southern and Tropical Latin America", my_sex, plot.year)]],
              ps[[paste("Caribbean", my_sex, plot.year)]],
              ps[[paste("Melanesia", my_sex, plot.year)]],
              ps[[paste("Polynesia and Micronesia", my_sex, plot.year)]],
              ncol=3,nrow=2,
              heights=unit.c(unit(.485,"npc"),unit(.515,"npc")),
              widths=unit.c(unit(.215/.6075,"npc"),rep(unit(.19625/.6075, "npc"),2))),
            
            arrangeGrob(
              ps[[paste("Global", my_sex, plot.year)]],
              ncol=1,nrow=1),
            
            ncol=2,nrow=1,
            widths=unit.c(unit(0.6075,"npc"),unit(.3925, "npc"))),
          
          ncol=1,nrow=2,
          heights = c(.592, .408)
        ),
        nrow=2,ncol=1,
        heights = c(.025, .975))
    )
    
     
  }
  
  dev.off()
}

