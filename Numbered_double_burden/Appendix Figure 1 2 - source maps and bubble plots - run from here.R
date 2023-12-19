# Appendix Figure 1 and 2 - run from S drive functions folder
# bubble plots and maps of data sources
remove(list = ls())
library(tidyverse)
library(scatterpie)
library(grid)
library(gridExtra)
library(RColorBrewer)

figsuffix <- ""
figsuffix <- ifelse(figsuffix == "", "", paste0(" ", figsuffix))
   
figNum <- 1
indir_data <- "S:/Projects/HeightProject/Original dataset/Anthropometrics/adult_bmi_analysis/data_sources_summaries_and_overall/"
data_adult.file.name <- "summary_bmi_adult18_October2023.csv"
data_ado.file.name   <- "summary_bmi_adol_5_19_October2023.csv"
outdir_folder <- "S:/Projects/HeightProject/Original dataset/Anthropometrics/adult_bmi_analysis/data_sources_summaries_and_overall/Figures/"

functionsDir <- "S:/Projects/HeightProject/Original dataset/Anthropometrics/adult_bmi_analysis/Figures scripts/Functions/"

countrylist.file.name <- "country-list-2023-figures.csv"
countrylist <- read_csv(paste0("S:/Projects/HeightProject/Original dataset/Anthropometrics/adult_bmi_analysis/Figures scripts/Covariates/",countrylist.file.name))

source(paste0(functionsDir, "0.2 Bubble plot functions.R"))
source(paste0(functionsDir, "0.2 Map plotting functions.R"))

start.year <- 1980
end.year   <- 2022

region_order <- rev(c("High-income English-speaking countries",
                      "Northwestern Europe",
                      "Southwestern Europe",
                      "Central Europe",
                      "Eastern Europe",
                      "Southern Latin America",
                      "Central Latin America",
                      "Andean Latin America",
                      "The Caribbean",
                      "East Asia and the Pacific",
                      "Southeast Asia",
                      "South Asia",
                      "Central Asia",
                      "Middle East and north Africa",
                      "Polynesia and Micronesia",
                      "Melanesia",
                      "East Africa",
                      "West Africa",
                      "Central and southern Africa",
                      "Other sub-Saharan Africa")) 

############################## READ ADULT DATA #################################
data_adult <- read.csv(paste0(indir_data,data_adult.file.name)) %>%
  mutate(check = paste(id_study, age, sex, mid_year)) %>%
  select(-c(Country, Region, Superregion)) %>%
  left_join(., countrylist, by = "iso")

data_adult$number     <- round(data_adult[,'N_bmi'])
data_adult$mid_year   <- ifelse(data_adult$mid_year >= (start.year - 3) & data_adult$mid_year < start.year & data_adult$survey_type == "National", start.year, data_adult$mid_year)
data_adult$mid_year   <- ifelse(data_adult$mid_year == (end.year + 1) & data_adult$survey_type == "National", end.year, data_adult$mid_year)

data_adult            <- subset(data_adult, mid_year>=start.year & mid_year<=end.year & age >= 18 & age < 200)
data_adult            <- subset(data_adult, number >1)

# remove two studies with missing mean BMI and all prevalence categories
data_adult    <- data_adult %>% filter(!(id_study%in% c("PAK_1996_4586040","PER_2005_11604002")))
data_adult     <- data_adult[,c("id_study","mid_year","iso","survey_type","urban_rural","sex","age","number","mean_bmi", "se_bmi", "prev_bmi_l185", "prev_bmi_185_20", "prev_bmi_20_25", "prev_bmi_25_30", "prev_bmi_30_35", "prev_bmi_35_40", "prev_bmi_40","check")]

############################## READ ADO DATA ###################################
data_ado   <- read.csv(paste0(indir_data,data_ado.file.name)) %>%
  mutate(check = paste(id_study, age, sex, mid_year)) %>%
  select(-c(Country, Region, Superregion)) %>%
  left_join(., countrylist, by = "iso")

data_ado$number             <- round(data_ado[,'N_bmi'])
data_ado$mid_year           <- ifelse(data_ado$mid_year >= (start.year - 3) & data_ado$mid_year < start.year & 
                                        data_ado$survey_type == "National", start.year, data_ado$mid_year)          # Include national studies from up to 3 years before the minimum data_ado year
data_ado$mid_year           <- ifelse(data_ado$mid_year == (end.year +1) & data_ado$survey_type == "National", end.year, data_ado$mid_year)# Include national studies from up to 3 years before the minimum data_ado year


data_ado     <- data_ado[,c("id_study","mid_year","iso","survey_type","urban_rural","sex","age","number","mean_bmi", "se_bmi", "prev_bmi_neg2sd", "prev_bmi_neg2sd_neg1sd", "prev_bmi_neg1sd_1sd", "prev_bmi_1sd_2sd", "prev_bmi_2sd", "check")]
data_ado     <- subset(data_ado, mid_year >= start.year & mid_year <= end.year & age >= 5 & age < 20)                                 # Subset to ages within range
data_ado     <- subset(data_ado, number >1) 

data_adult <- left_join(data_adult, countrylist, by = c("iso")) %>% 
  select(id_study, Country, Region, Superregion, survey_type, mid_year, sex) %>%
  unique() %>%
  mutate(Region = factor(Region,
                         levels = region_order),
         survey_type = factor(survey_type,
                              levels = c("Community", "Subnational","National")),
         sex = ifelse(sex == 1, 'male', 'female'))

data_ado <- left_join(data_ado, countrylist, by = c("iso")) %>% 
  select(id_study, Country, Region, Superregion, survey_type, mid_year, sex) %>%
  unique() %>%
  mutate(Region = factor(Region,
                         levels = region_order),
         survey_type = factor(survey_type,
                              levels = c("Community", "Subnational","National")),
         sex = ifelse(sex == 1, 'male', 'female'))



############################## GENERATE BUBBLE PLOTS AND MAPS FILL##################################
plt_width <- 16.5
cols      <- c("National" = "#0085FF", "Subnational" = "#FFD600", "Community" = "#FF0099")

## legend color
my_fill <- scale_fill_gradientn(
  colours = c("White",brewer.pal(9,"YlGnBu")[1:4],brewer.pal(9,"BuPu")[6:9]),
  name="Data sources",
  na.value="darkgrey",
  lim=c(0,152),
  values=c(0,0.00000000000001,0.05,0.125,0.1575,0.19845,0.25,0.315,0.3969,0.5,0.63,0.7936,1.0),
  breaks=seq(0,150,25)
)

ps <- list()
for(my_sex in c("both")){
  for(age_type in c("ado", "adult")){
    
    if(age_type == "ado"){
      data <- data_ado 
    } else if (age_type == "adult"){
      data <- data_adult 
    }
    
    if (my_sex == 'both'){
      d <- data[which(data$mid_year >= start.year),] %>% select(-sex) %>% unique()
    } else{
      d <- data[which(data$mid_year >= start.year),] %>% filter(sex == my_sex) %>% select(-sex) %>% unique()
    }
    
    # Bubble plot
    ps[[paste0(my_sex, age_type, "bubble")]] <- bubble_plot_function(data = d, age_type, my_sex = "both", plot.start.year = start.year, plot.end.year = end.year)
    
    # Maps
    d_map <- d %>% left_join(., countrylist) %>% group_by(Country, iso) %>% summarise(n_source = n()) %>% ungroup() %>% left_join(., countrylist)
    no_data <- data.frame(iso = setdiff(countrylist$iso, d_map$iso), "n_source" = 0)
    no_data <- left_join(no_data,countrylist,by="iso")
    d_map <- rbind(d_map, no_data)
    print(max(d_map$n_source))
    ps[[paste0(my_sex, age_type, "map")]] <- maps_data_sources_plots(data = d_map)
  }
}

ps[["legend_bubble"]] <- bubble_plot_function(data = d, age_type, my_sex = "both", plot.start.year = start.year, plot.end.year = end.year, returnLeg = T)


blank <- grid.rect(gp=gpar(col=NA, fill = NA))

### PLOT MAPS OF DATA SOURCES ##################################################
figNum <- 1
cairo_pdf(paste0(outdir_folder, "Appendix Figure ", figNum, figsuffix,".pdf"), height = 15, width = 12,onefile=T)

grid.arrange(textGrob("School-aged children and adolescents",hjust=0, just = c("left"),x = unit(0.01, "npc"),gp = gpar(col = "black", fontsize = 20)),
             ps[[paste0("both", "ado", "map")]],
             blank,
             textGrob("Adults",hjust=0, just = c("left"),x = unit(0.01, "npc"),gp = gpar(col = "black", fontsize = 20)),
             ps[[paste0("both", "adult", "map")]],
             blank,
             nrow = 6, heights = c(1, 10, 1, 1, 10, 1))


dev.off()


###### PLOT BUBBLE PLOTS #######################################################
figNum <- 2
cairo_pdf(paste0(outdir_folder, "Appendix Figure ", figNum, figsuffix,".pdf"), height = 15, width = 18,onefile=T)

grid.arrange(textGrob("School-aged children and adolescents",hjust=0, just = c("left"),x = unit(0.01, "npc"),gp = gpar(col = "black", fontsize = 20)),
             ps[[paste0("both", "ado", "bubble")]],
             blank,
             textGrob("Adults",hjust=0, just = c("left"),x = unit(0.01, "npc"),gp = gpar(col = "black", fontsize = 20)),
             ps[[paste0("both", "adult", "bubble")]],
             ps[["legend_bubble"]],
             nrow = 6, heights = c(1, 10, 1, 1, 10, 1))


dev.off()
