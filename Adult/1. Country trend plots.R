## NP country trend plots for BMI prevalence model (squeezed categories)
## New plots for countries separately including original data 

library(tidyverse)
library(ggnewscale)
filename    <- paste(paste0("Model",modelnum),sex,variable,sep = "_")
results_dir <- paste0(maindir, "/Results/")

### Plot with data points ###
if (variable %in% c("prev_bmi_30", "prev_bmi_35")){
  
  a <- readRDS(paste0(results_dir,"prev_bmi_l185","_",sex,"/",paste(paste0("Model",modelnum),sex,"prev_bmi_l185",sep = "_"),"_Combined.RDS"))
  attach(a)
  
  data <- read.csv(paste0(maindir, "Run Scripts/", data.file.name))
  
  data$number             <- round(data[,'N_bmi'])
  
  if(variable == "prev_bmi_30"){
    data <- data %>% mutate(prev = prev_bmi_30_35 + prev_bmi_35_40 + prev_bmi_40)
  } else if (variable == "prev_bmi_35"){
    data <- data %>% mutate(prev = prev_bmi_35_40 + prev_bmi_40)
  }
  
  data$id_study           <- as.character(data$id_study)                                                          
  data$mid_year           <- ifelse(data$mid_year >= (start.year - 3) & data$mid_year < start.year & 
                              data$survey_type == "National", start.year, data$mid_year)              
  data$mid_year           <- ifelse(data$mid_year >= (end.year + 1) & data$survey_type == "National", 
                                    end.year, data$mid_year)
  # Columns used in analysis
  colnames(data)[colnames(data)=="mid_year"] <- "data_year"
  
  data     <- subset(data, data_year>=start.year & data_year<=end.year &  
                       age >= minimum.age & age < maximum.age)
  data     <- subset(data, number >1 & !is.na(prev))
  
  subset   <- subset(data, sex==sex.val) 
  #subset   <- merge(subset,covar,by=c("data_year","iso"),all.x=TRUE)   
  subset$age_original <- subset$age
  subset$coverage     <- subset$survey_type
  subset$scope        <- subset$urban_rural
} else {
  a <- readRDS(paste0(results_dir,variable,"_",sex,"/",paste(paste0("Model",modelnum),sex,variable,sep = "_"),"_Combined.RDS"))
  attach(a)
  attach(a$subset)
  attach(a$covar)
  subset <- a$subset
  subset$age_original <- a$subset$age
  subset$Region <- subset$region
  subset$Superregion <- subset$sregion
  subset$Country <- subset$country
}

subset$age=cut(subset$age_original, c(17.5, seq(20,85, by=5), 200), right=FALSE)
levels(subset$age) <- c(18, seq(20, 85, by = 5))

cols <- c(rainbow(50)[30], rainbow(50)[8],rainbow(50)[46])
names(cols) <- c("National","Subnational","Community")
colScale <- scale_colour_manual(values = cols, guide = NULL)
shapes <- c(16, 15, 17)
names(shapes) <- c("both","urban","rural")
shpScale <- scale_shape_manual(values = shapes, guide = NULL)

subset$year    <- subset$data_year
subset <- subset %>% 
  mutate(se = sqrt((prev * (1-prev))/number)) %>%
  mutate(l = pmax(prev - 1.96*se,0)) %>%
  mutate(u = pmin(prev + 1.96*se,1))


#load results
res <- read.csv(paste0(parentdir, "Model", modelnum, ", ", mod_dir_name,"/Postprocessing/Estimates/",filename,"_agespecific_Country_Means.csv"))
res$model <- paste0("Model",modelnum)
res$age <- as.numeric(substr(res$age_group,1,2))
results <- left_join(res,a$country.names,by=c("country"="Country")) %>%
  rename("Country"="country")
axis_range <- c(0,1)

#labels
age_groups <- c("18"="18-19","20"="20-25","25"="25-30","30"="30-35","35"="35-40","40"="40-45","45"="45-50",
                "50"="50-55","55"="50-60","60"="60-65","65"="65-70","70"="70-75","75"="75-80","80"="80-85","85"="85+")

country.names <- a$country.names
country.names <- country.names[order(country.names$Superregion,country.names$Region,country.names$Country),]
cols_mod <- c("black")
names(cols_mod) <- paste0("Model", modelnum)
colScale_mod <- scale_colour_manual(values = cols_mod)


pdf(paste0(outdir_folder,filename, "_country_trends_by_age_new.pdf"), height=8, width=14)

for (sr in split(1:nrow(country.names), ceiling(1:nrow(country.names) / 5))) {
  countrylist <- country.names$Country[sr]
  p <- ggplot(subset[subset$Country %in% countrylist & is.finite(subset$prev),], aes(x = year)) + 
    facet_grid(Superregion + Region + Country~age, labeller = labeller(age = age_groups)) + theme_bw() 
  
  p <- p + geom_ribbon(data = results %>% subset(Country %in% countrylist), aes(ymin = l, ymax = u), fill = grey(0.92))
  p <- p + geom_line(data = results %>% subset(Country %in% countrylist), aes(y = mean),color="black", linewidth = 0.2)
  p <- p + colScale_mod
  p <- p + new_scale_colour()
  p <- p + geom_errorbar(aes(ymin = l, ymax = u, colour = coverage), width = 0) + geom_point(aes(y = prev, shape=scope, colour=coverage))
  p <- p + colScale + shpScale + theme(legend.position = "bottom", legend.title = element_blank(), axis.title = element_blank(), panel.grid = element_blank())
  p <- p + coord_cartesian(ylim = axis_range) +  scale_x_continuous(breaks=c(1985,2000,2020))
  p <- p + theme(axis.text.x = element_text(angle = 45, vjust = 0.75))
  
  plot(p)
}

dev.off()


