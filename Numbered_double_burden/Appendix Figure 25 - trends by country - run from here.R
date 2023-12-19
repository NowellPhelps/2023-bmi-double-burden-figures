setwd("S:/Projects/HeightProject/Original dataset/Anthropometrics/adult_bmi_analysis/Figures scripts/Comparisons scripts")

source("0.0 utils light.R")
appendix <- T

figsuffix                <- ""
figsuffix                <- ifelse(figsuffix == "", "", paste0(" ", figsuffix))
outdir_folder            <- paste0("")
figNum <- 25

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

age_groups        <- c("ageStd")
names(age_groups) <- c("20+ year olds")
my_age_group      <- "ageStd"

############################# READ DATA ########################################
data_level_adult <- read_data_level(variables = c("prev_bmi_l185", "prev_bmi_30", "prev_double_burden"), sexes = c("female", "male"), age_type = "ageStd", region_level = "Country", age_level = "adult") %>%
  filter(age_group == "ageStd")

data_level <- read_data_level(variables = c("prev_bmi_neg2sd", "prev_bmi_2sd", "prev_double_burden"), sexes = c("female", "male"), age_type = "ageStd", region_level = "Country", age_level = "ado") %>%
  mutate(variable = ifelse(variable == 'prev_double_burden', 'prev_double_burden_ado', variable)) %>%
  filter(age_group == "ageStd") %>%
  rbind(., data_level_adult)

rm(data_level_adult)

data_sources_ado   <- read.csv("S:/Projects/HeightProject/Original dataset/Anthropometrics/adult_bmi_analysis/data_sources_summaries_and_overall/sources_summary_ado.csv") %>%
  select(-c(Country, Region, Superregion)) %>%
  left_join(., countrylist, by = c("iso"))
data_sources_adult <- read.csv("S:/Projects/HeightProject/Original dataset/Anthropometrics/adult_bmi_analysis/data_sources_summaries_and_overall/sources_summary_adult.csv") %>%
  select(-c(Country, Region, Superregion)) %>%
  left_join(., countrylist, by = c("iso"))

############################# GENERATE PLOTS ####################################
get_source_text<- function(N_all, N_natl){
  if(N_all == 0){
    text <- paste0("No studies")
  } else if(N_all ==  1){
    text <- paste0(N_all, " study (", N_natl, " national)")
  } else{
    text <- paste0(N_all, " studies (", N_natl, " national)")
  }
  return(text)
}

ps <- list()

countrylist <- countrylist[order(countrylist$Country),]

for(age_type in c("ado", "adult")){

  if(age_type == "ado"){
    ylims <- c(0,56)
    breaks <- c(0,10,20,30,40,50)
    # fill_scale <- fill_scale_double_burden_ado
    subset_level <- data_level %>% filter(variable %in% c("prev_double_burden_ado","prev_bmi_2sd", "prev_bmi_neg2sd"))
    subset_level$variable <- factor(subset_level$variable, levels = c("prev_double_burden_ado","prev_bmi_neg2sd","prev_bmi_2sd"))
    levels(subset_level$variable) <- c('Combined burden','Thinness', 'Obesity')
    data_sources <- data_sources_ado
    colscale <- col_scale_double_burden_ado
    fillscale <- fill_scale_double_burden_ado
  }else{
    ylims <- c(0,90)
    breaks <- c(0,20,40,60,80)
    # fill_scale <- fill_scale_double_burden
    subset_level <- data_level %>% filter(variable %in% c("prev_double_burden","prev_bmi_l185", "prev_bmi_30"))
    subset_level$variable <- factor(subset_level$variable, levels = c("prev_double_burden","prev_bmi_l185","prev_bmi_30"))
    levels(subset_level$variable) <-c('Combined burden','Underweight', 'Obesity')
    data_sources <- data_sources_adult
    colscale <- col_scale_double_burden
    fillscale <- fill_scale_double_burden
  }

  for (my_country in unique(countrylist$Country)){
    print(my_country)
    for(my_sex in sexes){

      n_sources_all  <- data_sources$N_all[which(data_sources$sex == ifelse(my_sex == "male", 1, 2) & data_sources$Country == my_country)]
      n_sources_natl <- data_sources$N_natl[which(data_sources$sex == ifelse(my_sex == "male", 1, 2) & data_sources$Country == my_country)]
      text_sources   <- get_source_text(n_sources_all, n_sources_natl)

      plot.title <- ifelse(my_sex == "female",
                           ifelse(age_type == "ado", "Girls", "Women"),
                           ifelse(age_type == "ado", "Boys", "Men"))

      d_plot <- subset_level %>% filter(Country == my_country & sex == my_sex & year %in% seq(plot.start.year,plot.end.year)) %>% arrange(rev(desc(variable)))

      p <- ggplot(d_plot, aes(x = year, y = mean, colour = variable)) +
        geom_hline(yintercept = 0, linetype = 'dashed', colour = grey(0.5)) +
        geom_ribbon(aes(ymin = l, ymax = u, fill = variable), alpha = 0.1,  colour = NA) +
        geom_line() +
        theme_bw() +
        theme(axis.title.x = element_blank(),
              axis.text=element_text(size=10.5),
              # axis.text.x=element_text(size=10.5, angle = 45, hjust = 1),
              # legend.title=element_blank(),
              title = element_text(size = 10.5),
              legend.text = element_text(size = 10.5),
              strip.background = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_line(colour = grey(0.95)),
              panel.grid.minor.y = element_blank()) +
        scale_x_continuous(expand=c(0,0), limits = c(1989,2023), breaks = c(1990, 2000, 2010, 2020)) +
        scale_y_continuous(expand=c(0,0), limits = ylims, breaks = breaks) +
        ggtitle(plot.title,subtitle = text_sources) +
        colscale +
        fillscale

      if(my_sex == "female"){
        p <- p + ylab("Prevalence (%)") + theme(axis.title.y = element_text(size=11))
      } else{
        p <- p + ylab("")
      }

      ps[[paste(age_type, my_country, my_sex)]] <- p  + theme(legend.position = "none")

    }
  }
  
  p_leg <- ggplot(d_plot, aes(x = year, y = mean, colour = variable)) +
    geom_hline(yintercept = 0, linetype = 'dashed', colour =grey(0.5)) +
    geom_line() +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text=element_text(size=10.5),
          # axis.text.x=element_text(size=10.5, angle = 45, hjust = 1),
          # legend.title=element_blank(),
          title = element_text(size = 10.5),
          legend.text = element_text(size = 10.5),
          legend.title = element_blank(),
          strip.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(colour = grey(0.95)),
          panel.grid.minor.y = element_line(colour = grey(0.95))) +
    scale_x_continuous(expand=c(0,0), limits = c(1990,2022), breaks = c(1990, 2022)) +
    scale_y_continuous(expand=c(0,0), limits = ylims, breaks = breaks) +
    ggtitle(plot.title,subtitle = text_sources) +
    colscale +
    fillscale
  
  ps[[paste(age_type, "legend")]] <- get_legend(p_leg) 
  
}

########################## PRINT PDF ###########################################
blank <- grid.rect(gp=gpar(col=NA, fill = NA))
cairo_pdf(paste0(outdir_folder, ifelse(appendix, "Appendix Figure ", "Figure "), figNum, figsuffix,".pdf"), height = 12, width = 14, onefile=T)

for(my_country in unique(countrylist$Country)){

  plot.title <- my_country

  grid.arrange(

    arrangeGrob(

      textGrob(plot.title,hjust=0,just = c("left"),x = unit(0.04, "npc"),gp = gpar(col = "black", fontsize = 18)),

      arrangeGrob(
        blank,

        arrangeGrob(
          blank,
          textGrob("School-aged children and adolescents",hjust=0,just = c("left"),x = unit(0.05, "npc"),gp = gpar(col = "black", fontsize = 16)),
          blank,
          arrangeGrob(blank,
                      ps[[paste("ado", my_country, "female")]],
                      blank,
                      ncol = 3,
                      widths = c(0, 10, 0)),
          blank,
          textGrob("Adults",hjust=0,just = c("left"),x = unit(0.05, "npc"),gp = gpar(col = "black", fontsize = 16)),
          blank,
          arrangeGrob(blank,
                      ps[[paste("adult", my_country, "female")]],
                      blank,
                      ncol = 3,
                      widths = c(0, 10, 0)),
          blank,


          ncol = 1, heights = c(.2,.5,.1,5,0.2,.5,.1,5,0.3)
        ),
        blank,
        arrangeGrob(
          blank,
          blank,
          blank,
          
          arrangeGrob(blank,
                      ps[[paste("ado", my_country, "male")]],
                      blank,
                      ncol = 3,
                      widths = c(0, 10, 0)),
          blank,
          blank,
          blank,
          arrangeGrob(blank,
                      ps[[paste("adult", my_country, "male")]],
                      blank,
                      ncol = 3,
                      widths = c(0, 10, 0)),
          blank,
          
          
          ncol = 1, heights = c(.2,.5,.1,5,0.2,.5,.1,5,0.3)
        ),

        blank,

        arrangeGrob(
          blank,
          blank,
          blank,

          ps[[paste("ado", "legend")]],
          
          blank,
          blank,
          blank,
          ps[[paste("adult", "legend")]],
          
          blank,

          ncol = 1, heights = c(.2,.5,.1,5,0.2,.5,.1,5,0.3)
        ),
        
        blank,

        nrow = 1,
        widths = c(.05,1,.05,1,0.05, 0.25, .05)
      ),
      blank,
      blank,
      nrow = 4,
      heights = c(5, 65, 1,3)

    )
  )

}

dev.off()
