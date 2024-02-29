### TEST SENSITIVITY OF RANKING
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
outdir_folder  <- paste0("S:/Projects/HeightProject/Papers/Anthropometrics/Adult & Adolescent BMI (double burden) 2023/Country factsheets/test package/")

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
   mutate(Country = ifelse(Country == "Occupied Palestinian Territory", "Palestine", Country))

############################# READ DATA ########################################
data_level_adult <- read_data_level(variables = c("prev_bmi_l185", "prev_bmi_30"), sexes = c("female", "male"), age_type = "ageStd", region_level = "Country", age_level = "adult") %>%
   filter(age_group == "ageStd")

data_level <- read_data_level(variables = c("prev_bmi_neg2sd", "prev_bmi_2sd"), sexes = c("female", "male"), age_type = "ageStd", region_level = "Country", age_level = "ado") %>%
   mutate(variable = ifelse(variable == 'prev_double_burden', 'prev_double_burden_ado', variable)) %>%
   filter(age_group == "ageStd") %>%
   rbind(., data_level_adult)

data_ranking_adult <- read_data_ranking(variables = c("prev_bmi_l185", "prev_bmi_30"),sexes = c("female", "male"), age_type = "ageStd", region_level = "Country", age_level = "adult") %>%
   filter(age_group == "ageStd")

data_ranking <- read_data_ranking(variables = c("prev_bmi_neg2sd", "prev_bmi_2sd"),sexes = c("female", "male"), age_type = "ageStd", region_level = "Country", age_level = "ado") %>%
   filter(age_group == "ageStd") %>%
   rbind(.,data_ranking_adult) %>%
   dplyr::rename(median_rank = median) %>%
   dplyr::rename(l_rank = l) %>%
   dplyr::rename(u_rank = u) %>%
   select(-mean)

data_level <- data_level %>%
   group_by(sex, age_group, variable, year) %>%
   mutate(rank_of_posterior = rank(desc(mean))) %>%
   ungroup() %>%
   select(-c(mean, u, l, se))
   
halp <- merge(data_level, data_ranking) %>%
   mutate(agreement_ind = ifelse(rank_of_posterior <= u_rank & rank_of_posterior >= l_rank, 1, 0))

p <- ggplot(data = halp %>% filter(year == 2022), aes(x = rank_of_posterior, y = median_rank, colour = agreement_ind)) +
   facet_grid(sex~variable) +
   geom_point()
   


