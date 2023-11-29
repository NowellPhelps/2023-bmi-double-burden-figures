library(tidyverse)
library(ggnewscale)
library(reshape)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)

if(type == "all"){
  variables        <- c("prev_bmi_l185","prev_bmi_185_20","prev_bmi_20_25","prev_bmi_25_30","prev_bmi_30_35","prev_bmi_35_40","prev_bmi_40")
  names(variables) <- c("<18.5","18.5 to <20","20 to <25","25 to <30","30 to <35","35 to <40","\u2265 40")
  fill_scale       <- fill_scale_all
  ylims            <- c(0, 100.00001)
  
} else if (type == "double burden"){
  variables       <- c("prev_bmi_l185","prev_bmi_30")
  names(variables)<- c("<18.5","\u2265 30")
  fill_scale      <- fill_scale_double_burden
  ylims           <- c(0,65)
}

# Read data
data_region <- read_data_level(variables, sexes, "ageStd", region_level = "Region") %>%
  mutate(level = "Region") %>%
  dplyr::rename(name = Region)

data_global <- read_data_level(variables, sexes, "ageStd", region_level = "Global") %>%
  mutate(level = "Global") %>%
  dplyr::rename(name = Global)

data_all <- rbind(data_region, data_global) %>% 
  filter(age == "ageStd", sex == my_sex, year %in% seq(plot.start.year, plot.end.year)) %>%
  select(name,level,sex, year, age, mean, variable)

data_all$variable <- factor(data_all$variable, levels = (variables))

p <- stacked_region_trend_plots(data_all, my_sex, type)
legend <- stacked_region_trend_plots(data_all, my_sex, type, returnLeg = T)
plot.title <- switch(my_sex, "female" = "Women", "male" = "   Men   ")


cairo_pdf(paste0(outdir_folder, "Trends in ", ifelse(type=="all", "all prevalence categories", type), " by region ", my_sex, ".pdf"), height = 15, width = 15, onefile=T)

grid.arrange(
  main=textGrob(plot.title,hjust=8,gp = gpar(col = "black", fontsize = 18)),
  p,
  legend,
  nrow=3,ncol=1,
  heights=c(0.05,0.95,0.05))

dev.off()
