library(tidyverse)
library(ggnewscale)
library(reshape)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)

source(paste0(maindir_functions,"0.0 BMI Prevalence palette.R"))

indir_est <-paste0(maindir, "Postprocessing/Estimates/")
outdir    <-paste0(maindir, "Postprocessing/", foldername, "/", subfoldername,"/")

country.list <- read.csv(paste0(maindir,"Covariates/country-list-2020.csv"))

if(type == "all"){
  variables <- c("prev_bmi_l185","prev_bmi_185_20","prev_bmi_20_25","prev_bmi_25_30","prev_bmi_30_35","prev_bmi_35_40","prev_bmi_40")
  names(variables)<- c("<18.5","18.5 to <20","20 to <25","25 to <30","30 to <35","35 to <40","\u2265 40")
  fill_scale <- fill_scale_all
  ylims <- c(0, 100.00001)
  
} else if (type == "double burden"){
  variables <- c("prev_bmi_l185","prev_bmi_30")
  names(variables)<- c("<18.5","\u2265 30")
  fill_scale <- fill_scale_double_burden
  ylims <- c(0,90)
}  else if(type == "ado all"){
  variables <- c("prev_bmi_neg2sd","prev_bmi_neg2sd_neg1sd","prev_bmi_neg1sd_1sd","prev_bmi_1sd_2sd","prev_bmi_2sd")
  names(variables)<- c("BMI < -2 SD","-2 SD \u2265 BMI < -1 SD","-1 SD \u2265 BMI \u2265 1 SD","1 SD < BMI \u2265 2 SD","BMI > 2SD")
  fill_scale <- fill_scale_all_ado
  ylims <- c(0, 100.00001)
  
} else if (type == "ado double burden"){
  variables        <- c("prev_bmi_neg2sd", "prev_bmi_2sd")
  names(variables) <- c("BMI < -2 SD ", "BMI > 2SD")
  fill_scale <- fill_scale_double_burden_ado
  ylims <- c(0,50)
} 
# Read data
data_all <- NULL
for(my_variable in variables){
  data.tmp <- read.csv(paste0(indir_est,"Model", modelnum,"_",sex, "_", my_variable,"_ageStd_Country_Means.csv")) %>%
    mutate(variable = my_variable) %>% 
    dplyr::rename(age = age_group, Country = country) 
  data_all <-rbind(data_all, data.tmp)
}
remove(data.tmp)
data_all <- left_join(data_all, country.list)
data_all <- data_all %>% 
  select(year, age, Country, mean, variable, Region, Superregion) %>%
  filter(age == "ageStd") %>%
  mutate(prevalence = mean*100)

data_all$variable <- factor(data_all$variable, levels = variables)

# Read data
data_all <- NULL
for(my_variable in variables){
  data.tmp <- read.csv(paste0(indir_est,"Model", modelnum,"_",sex, "_", my_variable,"_ageStd_Region_Means.csv")) %>%
    mutate(level = "Region") %>%
    dplyr::rename(name = Region) %>%
    mutate(variable = my_variable)
  data.tmp.global <- read.csv(paste0(indir_est,"Model", modelnum,"_",sex, "_", my_variable,"_ageStd_Global_Means.csv")) %>%
    mutate(level = "Global") %>%
    dplyr::rename(name = Global) %>%
    mutate(variable = my_variable)
  data_all <-rbind(data_all, data.tmp, data.tmp.global)
}
remove(data.tmp, data.tmp.global)

data_all <- data_all %>% 
  select(name, level, year, age, mean, variable) %>%
  filter(age == "ageStd") %>%
  mutate(prevalence = mean*100)

data_all$variable <- factor(data_all$variable, levels = (variables))

# Generate plots
ps <- list()

for (my_name in c(unique(country.list$Region), "Global")){
  p <- ggplot(data_all %>% filter(name == my_name & year >= start.year.plot & year <= end.year.plot), aes(x = year, y = prevalence, fill = variable)) +
    geom_area(position='stack') +
    scale_y_continuous(expand=c(0,0), limits = ylims)+
    scale_x_continuous(expand=c(0,0), breaks = c(1990, 2000, 2010, 2020), labels = c("1990 ", "2000 ", "2010 ", "2020 ")) +
    ggtitle(ifelse(my_name == "Global", "World", my_name)) + 
    fill_scale +
    theme_bw() +
    theme(text=element_text(size=7)) +
    theme(legend.title=element_blank()) +
    theme(legend.position = "none")+
    theme(axis.title=element_blank()) + 
    theme(axis.text.x=element_blank()) +
    theme(axis.text.y=element_text(size=10.5))
  
  if (my_name %in% c("Caribbean", "Melanesia", "Polynesia and Micronesia", "Global")){
    p <- p + theme(axis.text.x=element_text(size=10.5,angle = 45,vjust=1, hjust = 1, margin = margin(0,0,0,0)))
  }
  
  ps[[my_name]] <- p
}

blank <- grid.rect(gp=gpar(col="white"))

# Recover legend
plot.legend <- ps[["Global"]] + theme(legend.position = "bottom",text=element_text(size=12)) + guides(fill = guide_legend(nrow = 1, reverse = F))
g_legend<-function(p){
  tmp <- ggplotGrob(p)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
legend <- arrangeGrob(g_legend(plot.legend), ncol=1, nrow=1)


cairo_pdf(paste0(outdir, "Trends in ", ifelse(type=="all", "all prevalence categories", type), " by region ", ifelse(sex == "female", "girls", "boys"), ".pdf"), height = 15, width = 15, onefile=T)

plot.title <- switch(sex, "female" = "Girls", "male" = "Boys")
grid.arrange(
  
  main=textGrob(plot.title,hjust=0,just = c("left"),x = unit(0.05, "npc"),gp = gpar(col = "black", fontsize = 18)),
  
  arrangeGrob(
    arrangeGrob(
      textGrob("Prevalence (%)", rot = 90, vjust = 1),
      arrangeGrob(
        arrangeGrob(
          ps[["Central Asia"]],ps[["South Asia"]],ps[["South East Asia"]],ps[["East Asia"]],ps[["High-income Asia Pacific"]],
          ps[["High-income English-speaking countries"]],ps[["North Western Europe"]],ps[["South Western Europe"]],ps[["Central Europe"]],ps[["Eastern Europe"]],
          ps[["North Africa and Middle East"]],ps[["West Africa"]],ps[["Central Africa"]],ps[["East Africa"]],ps[["Southern Africa"]],
          ncol=5,nrow=3,
          heights=unit.c(unit((1/3),"npc"),unit((1/3),"npc"),unit((1/3),"npc")),
          widths=unit.c(unit(0.215,"npc"),rep(unit(.19625, "npc"),4))),
        
        arrangeGrob(
          arrangeGrob(
            ps[["Central Latin America"]],ps[["Andean Latin America"]],ps[["Southern and Tropical Latin America"]],
            ps[["Caribbean"]],ps[["Melanesia"]],ps[["Polynesia and Micronesia"]],
            ncol=3,nrow=2,
            heights=unit.c(unit(.485,"npc"),unit(.515,"npc")),
            widths=unit.c(unit(.215/.6075,"npc"),rep(unit(.19625/.6075, "npc"),2))),
          
          arrangeGrob(
            ps[["Global"]],
            ncol=1,nrow=1),
          
          ncol=2,nrow=1,
          widths=unit.c(unit(0.6075,"npc"),unit(.3925, "npc"))),
        
        ncol=1,nrow=2,
        heights = c(.592, .408)
      ),
      ncol=2,nrow=1,
      widths = c(.025, .975)),
    
    textGrob("Year"),
    ncol=1,nrow=2,
    heights = c(.975, .025)
  ),
  legend,
  nrow=3,ncol=1,
  heights=c(0.05,0.95,0.05))

dev.off()

