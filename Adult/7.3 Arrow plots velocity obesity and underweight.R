rm(list = ls())
library(tidyverse)
library(ggnewscale)
library(reshape)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)

source("0.0 BMI Prevalence palette.R")
source("0.0 Region Superregion palette.R")

foldername      <- "Figures"
subfoldername   <- "Velocity plots"
mod_dir_name    <- "Model 41 with data update 1980"
modelnum        <- 45

plot.start.year <- 1990.5
plot.end.year   <- 2019.5

parentdir <- "D:/prev_model/Models/"
maindir  <- paste0(parentdir,"Model",modelnum,", ", mod_dir_name, "/")

dir.create(paste0(maindir, "Postprocessing/", foldername), showWarnings=FALSE)
dir.create(paste0(maindir, "Postprocessing/", foldername, "/", subfoldername), showWarnings=FALSE)

outdir <- paste0(maindir, "Postprocessing/", foldername, "/", subfoldername, "/")
indir  <- paste0(maindir, "Postprocessing/Estimates/Slopes/")

sexes <- c("female","male")
age_groups      <- c("ageStd", "young", "mid", "old")
names(age_groups) <- c("20+ year olds", "20-39 year olds", "40-59 year olds", "60+ year olds")

variables <- c("prev_bmi_30","prev_bmi_l185")
xvar <- variables[1]
yvar <- variables[2]

countrylist <- read.csv(paste0(maindir,"Covariates/country-list-2020.csv"))
countrylist$Superregion <- factor(countrylist$Superregion,
                                  levels = c("High-income Asia Pacific", "East and South East Asia","Oceania",
                                             "High-income Western countries","Central and Eastern Europe","Latin America and Caribbean",
                                             "Central Asia and North Africa-Middle East",
                                             "Sub-Saharan Africa","South Asia"))

### LOAD AND RESHAPE DATA
data <- NULL

for (my_sex in sexes){
  for (my_variable in variables){
    
    data.tmp <- read_csv(paste0(indir, "Model", modelnum, "_", my_sex, "_",my_variable,"_ageStd_Slopes.csv"), show_col_types = FALSE) %>%
      mutate(year = (start.year + end.year)/2) %>%
      select(-c(start.year,end.year))%>%
    
    filter(year %in% c(plot.start.year, plot.end.year)) %>%
      select(-c(l, u, se, PP.increase)) %>%
      dplyr::rename(Country = country) %>%
      mutate(sex = my_sex,
             variable = my_variable)
    
    data <- rbind(data, data.tmp)
  }
}

data <- left_join(data, countrylist) %>% 
  pivot_wider(id_cols = c(Country, age_group, sex, iso, Region, Superregion), names_from = c(variable, year), values_from = mean) 

names(data)[7:10] <- c("xvar_start", "xvar_end", "yvar_start", "yvar_end")


data$Superregion <- factor(data$Superregion,levels=levels(countrylist$Superregion))

data_sregion <- data %>% 
  group_by(sex, age_group, Superregion) %>%
  summarise(xvar_start = mean(xvar_start),
            xvar_end = mean(xvar_end),
            yvar_start = mean(yvar_start),
            yvar_end = mean(yvar_end))

data_sregion$level <- 'Superregion'
data_sregion$name <- data_sregion$Superregion
data$level <- "Country"
data$name <- data$Country


keep_names <- c("sex","age_group","Superregion","level", "name", "xvar_start","xvar_end","yvar_start","yvar_end")

plot_data <- rbind(data %>% select(all_of(keep_names)),
                   data_sregion %>% select(all_of(keep_names)))


## RENAME SUPERREGIONS FOR PLOTTING
plot_data$Superregion_revisedName <- as.character(plot_data$Superregion)
plot_data$Superregion_revisedName[plot_data$Superregion_revisedName=='High-income Western countries'] <- 'High-income western'
plot_data$Superregion_revisedName[plot_data$Superregion_revisedName=='Central and Eastern Europe'] <- 'Central and eastern Europe'
plot_data$Superregion_revisedName[plot_data$Superregion_revisedName=='High-income Asia Pacific'] <- 'High-income Asia Pacific'
plot_data$Superregion_revisedName[plot_data$Superregion_revisedName=='East and South East Asia'] <- 'East and southeast Asia'
plot_data$Superregion_revisedName[plot_data$Superregion_revisedName=='Central Asia and North Africa-Middle East'] <- 'Central Asia, Middle East,\nand north Africa '


sregion_col_v2 = ifelse(names(sregion_col) %in% c('South Asia',"High-income Western countries"),alpha(sregion_col,0.3),alpha(sregion_col,0.15))
names(sregion_col_v2) <- names(sregion_col)

y_max_val <- max(plot_data$yvar_start, plot_data$yvar_end)
y_min_val <- min(plot_data$yvar_start, plot_data$yvar_end)
yintv <- 2
ylims <- c(yintv*floor(y_min_val/yintv), yintv*ceiling(y_max_val/yintv))
yTicks <- seq(ylims[1],ylims[2],by=yintv)

x_max_val <- max(plot_data$xvar_start, plot_data$xvar_end)
x_min_val <- min(plot_data$xvar_start, plot_data$xvar_end)

xintv <- 2
xlims <- c(xintv*floor(x_min_val/xintv), xintv*ceiling(x_max_val/xintv))
xTicks <- seq(xlims[1],xlims[2],by=xintv)


xlab <- "Relative velocity obesity (BMI \u2265 30  kg/m\u00b2)"
ylab <- "Relative velocity underweight (BMI < 18.5  kg/m\u00b2)"

## CREATE SCATTER PLOTS
ps <- list()
for (my_sex in sexes){
  for (my_age_group in age_groups){
    
    subset <- plot_data %>% filter(sex == my_sex & age_group == my_age_group)
    sex.title <- str_to_title(my_sex)
  
    p <- ggplot(subset,aes(colour=Superregion)) + 
      facet_wrap(~Superregion,scales="fixed") +
      geom_segment(data = subset %>% filter(level == "Country"),aes(y=yvar_start,yend=yvar_end,x=xvar_start,xend=xvar_end),
                          arrow=arrow(length = unit(0.1,"cm")))+
      
      scale_colour_manual(values=sregion_col_v2) + 
      new_scale_colour() + 
      scale_colour_manual(values = sregion_col) + guides(color=guide_legend(byrow = TRUE)) +
      theme(text = element_text(family='sans')) +
    
    # ARROWS
    geom_segment(data = subset %>% filter(level=="Superregion"),
                 aes(y=yvar_start,yend=yvar_end,x=xvar_start,xend=xvar_end,color=Superregion),
                 linetype = "dashed",
                 arrow=arrow(length = unit(0.25,"cm")),cex=1) +

      geom_segment(data = subset %>% filter(level=="Superregion"),
                   aes(y=yvar_end + (yvar_start - yvar_end)/1000,yend=yvar_end,x=xvar_end + (xvar_start - xvar_end)/1000,xend=xvar_end,color=Superregion),
                   linetype = "solid",
                   arrow=arrow(length = unit(0.25,"cm")),cex=1) +

      geom_text(data = subset %>% filter(level=="Superregion"),
                aes(y=ylims[2],x=mean(xlims),label=Superregion_revisedName),hjust=0.5,vjust=1.3)+

      scale_x_continuous(breaks=xTicks,labels=xTicks,limits=xlims,expand = c(0, 0)) +
      scale_y_continuous(breaks=yTicks,labels=yTicks,limits=ylims,expand = c(0, 0)) + 
      geom_hline(yintercept =0,linetype="dashed",color="gray") +
      geom_vline(xintercept =0,linetype="dashed",color="gray") +
      theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      ggtitle(paste0(sex.title)) +
      theme(legend.position = "non", legend.title = element_blank()) +
      theme(plot.title = element_text(hjust = 0.5)) + 
      ylab(ylab) + 
      xlab(xlab) + 
      theme(strip.background = element_blank(), strip.text.x = element_blank())
    
    ps[[paste(my_sex, my_age_group)]] <- p
    
  
  }
}
  

## PLOT
cairo_pdf(paste0(outdir,"Arrows Velocity underweight and obesity 1990 2020.pdf"), height = 16, width =30, onefile=T)

for(my_age_group in age_groups){
  
  
  age_group.name <- names(age_groups)[which(age_groups == my_age_group)]
  plot.title <- paste0("Change velocity underweight vs velocity obese:", age_group.name)
  
  grid.arrange(
    arrangeGrob(
      textGrob(plot.title,gp=gpar(fontsize=15, fontface = "bold"), hjust = 0, x= 0.01),
      
      arrangeGrob(
        ps[[paste("female", my_age_group)]],
        ps[[paste("male", my_age_group)]],
        ncol = 2, widths = c(10, 10)
      ),
      nrow = 2, 
      heights = c(1, 20)
    )
  )
  
}

dev.off()


