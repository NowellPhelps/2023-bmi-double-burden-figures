remove(list = ls())
setwd("S:/Projects/HeightProject/Original dataset/Anthropometrics/adult_bmi_analysis/Figures scripts/Comparisons scripts")
library(ggplot2)
library(ggrepel)
library(gridtext)
source("0.0 utils light.R")
source("0.0 fact sheet functions.R")
appendix <- T

figsuffix      <- ""
figsuffix      <- ifelse(figsuffix == "", "", paste0(" ", figsuffix))
outdir_folder <- "S:/Projects/HeightProject/Original dataset/Anthropometrics/adult_bmi_analysis/Figures scripts/Comparisons scripts/Numbers stacked plots for press/"

library(tidyverse)
library(ggnewscale)
library(reshape)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)
library(plotly)
library(htmlwidgets)
library(htmltools)
library(cowplot)
library(magick)

age_groups        <- c("ageStd")
names(age_groups) <- c("20+ year olds")
my_age_group      <- "ageStd"

countrylist_corrected <- countrylistold %>%
   mutate(Country = ifelse(Country == "Occupied Palestinian Territory", "State of Palestine", Country))

############################# READ DATA ########################################
data_numbers_adult <- read_data_numbers(variables = c("prev_bmi_30"), sexes = c("female", "male"),region_level = "Country", age_level = "adult") %>%
   mutate(number = mean/(10^6)) %>%
   group_by(Region, year) %>%
   summarise(number = sum(number)) %>%
   mutate(Region = factor(Region, levels = rev(region_order)))

data_numbers_ado <- read_data_numbers(variables = c("prev_bmi_2sd"), sexes = c("female", "male"),region_level = "Country", age_level = "ado")%>%
   mutate(number = mean/(10^6)) %>%
   group_by(Region, year) %>%
   summarise(number = sum(number)) %>%
   mutate(Region = factor(Region, levels = rev(region_order)))

############################# GENERATE PLOTS ####################################
ps <- list()

fill_scale <- region_col

ps <- list()
for (age_group in c("ado", "adult")){
   
   if(age_group == "ado"){
      title <- "School-aged children and adolescents (5-19 years)"
      text_y <- "Number of school-aged children and adolescents\nwith obesity (millions)\n"
      
      data <- data_numbers_ado
      breaks <- c(0, 40, 80, 120, 160)
      lims_y <- c(0, 161)
   
   } else{
      title <- "Adults (20+ years)"
      text_y <- "\nNumber of adults with obesity (millions)\n"
      
      
      data <- data_numbers_adult
      breaks <- c(0,200,400,600,800)
      lims_y <- c(0, 880)
   }
   
   p <- ggplot(data = data %>% filter(year >= 1990), aes(x = year, y = number, fill = Region)) +
      geom_area(position = "stack")+
      scale_fill_manual(values = region_col) +
      theme(text=element_text(size=9)) +
      theme_classic() +
      theme(legend.title=element_text("Region", size = 12),
            axis.text = element_text(size = 11),
            axis.title  = element_text(size = 12)) +
      scale_y_continuous(expand=c(0,0),breaks = breaks, limits = lims_y)+
      scale_x_continuous(expand=c(0,0))+
      theme(panel.grid.major=element_blank(),
            panel.grid.minor = element_blank())+
      xlab("Year") +
      ylab(text_y) +
      ggtitle(title)
      
   ps[["legend"]] <- get_legend(p)
   
   p <- p + 
      theme(legend.position = "none")
      
   ps[[age_group]] <- p
   
}

blank <- grid.rect(gp=gpar(col=NA, fill = NA))

cairo_pdf(paste0(outdir_folder, "Stacked trends for press briefing bigger text.pdf"), height = 6, width = 17, onefile=T)

grid.arrange(
   arrangeGrob(
      blank,
      ps[["ado"]],
      blank, 
      ps[["adult"]],
      blank,
      ps[["legend"]],
      blank,
      nrow = 1, widths = c(.5,5.5,.5,5.5,.5,3,.5)
   )
   
)

dev.off()
