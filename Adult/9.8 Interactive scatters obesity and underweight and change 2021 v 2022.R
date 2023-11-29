### SCATTERS OF PROPORTION DOUBLE BURDEN AGAINST PREVALENCE DOUBLE BURDEN
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

outdir_folder <- paste0(maindir,"Postprocessing/Figures/Interactive scatters/")
dir.create(outdir_folder, showWarnings = F)

if(age_type == "adult"){
  variables <-  c("prev_bmi_30", "prev_bmi_l185")
} else{
  variables <-  c("prev_bmi_2sd","prev_bmi_neg2sd")
}

age_groups        <- c("ageStd")
names(age_groups) <- c("20+ year olds")
my_age_group      <- "ageStd"

data_level <- read_data_level(variables, sexes, age_type = "ageStd", region_level = "Country") %>%
  filter(age_group == my_age_group & (year == 2021 | year == 2022))

data_base <- read_data_level(variables, sexes, age_type = "ageStd", region_level = "Country") %>%
  filter(age_group == my_age_group & year == 1990)%>%
  select(-c(l,u,se, year)) %>%
  dplyr::rename(base = mean)

data_timechange <- read_data_level(variables, sexes, age_type = "ageStd", region_level = "Country") %>%
  filter(age_group == my_age_group & (year == 2021 | year == 2022)) %>%
  mutate(year = ifelse(year == 2021, "level_2021", "level_2022")) %>%
  select(-c(l,u,se)) %>%
  pivot_wider(names_from = year, values_from = mean) %>%
  left_join(., data_base) %>%
  mutate(change_2021 = level_2021-base,
         change_2022 = level_2022-base)
  
plotly.ps <- list()
for (var in variables){
  
  for(my_sex in sexes){

    #LEVEL
    plotly.ps[[paste("Scatter", my_sex, var)]] <- interactive_scatter_level_v_level_comparison(subset = data_level, xvar = var, yvar = var, my_sex = my_sex,
                                                                                      my_age_group = my_age_group, 
                                                                                      xyear = 2021, 
                                                                                      yyear = 2022)

    saveWidget(plotly.ps[[paste("Scatter", my_sex, var)]], paste0(outdir_folder, str_to_sentence(get_var_longname(var))," ", 2022, " v ", 2021, " ",my_sex, ".html"), selfcontained = F, libdir = "lib")
    
    
    # TIMECHANGE
    subset_timechange <- data_timechange %>%
      filter(variable == var, sex == my_sex)
    
    xlab     <-  paste0("change in ", get_var_longname(var), " 1990-2021")
    ylab     <-  paste0("change in ", get_var_longname(var), " 1990-2022")
    xmax <- ceiling(max(subset_timechange$change_2021)/10)*10
    ymax <- ceiling(max(subset_timechange$change_2022)/10)*10
    xmin <- floor(min(subset_timechange$change_2021)/10)*10
    ymin <- floor(min(subset_timechange$change_2022)/10)*10
    xaxislab <- str_to_sentence(paste0(xlab," (percentage points)"))
    yaxislab <- str_to_sentence(paste0(ylab, " (percentage points)"))
    
    if(age_type == "adult"){
      sex.longname <- ifelse(my_sex == "female", "women", "men")
    } else{
      sex.longname <- ifelse(my_sex == "female", "girls", "boys")
    }
    
    age_group.longname <- switch(my_age_group,
                                 "ageStd" = ifelse(age_type == "adult", "20+ years old", "5-19 years old"),
                                 "young" = "20-39 years old",
                                 "mid" = "40-64 years old",
                                 "old" = "65+ years old")
    
    plot.title <- paste0(str_to_title(sex.longname), ", ", age_group.longname)
    
     p <- plot_ly(subset_timechange,
                   type = "scatter",
                   mode = 'markers',
                   hoverinfo="text",
                   x = ~change_2021,
                   y = ~change_2022, 
                   color = ~Superregion,
                   colors = sregion_col,
                   text = ~ paste0("Country: ", Country, '<br>Region: ', Region,
                                   '<br>Superregion: ', Superregion,
                                   "<br>", str_to_sentence(xlab), ": ", round(change_2021,1),
                                   "<br>", str_to_sentence(ylab), ": ", round(change_2022,1))) %>%
        layout(title = plot.title, yaxis = list(title = yaxislab, range = list(ymin, ymax)),
               xaxis = list(title = xaxislab, range = list(xmin, xmax)),legend =list(font = list(size = 15)))%>%
       
       layout(shapes = list(list(
         type = "line",
         x0 = min(xmin, ymin),
         x1 = max(xmax, ymax),
         xref = "xvar",
         y0 = min(xmin, ymin),
         y1 = max(xmax, ymax),
         yref = "yvar",
         line = list(color = "grey50", dash = "dot")
       )))
     
      
    
     saveWidget(p, paste0(outdir_folder, str_to_sentence(get_var_longname(var))," change 1990 - ", 2022, " v change 1990 - ", 2021, " ",my_sex, ".html"), selfcontained = F, libdir = "lib")
    }
}



