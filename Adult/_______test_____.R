### COUNTRIES WITH MAX EXCESS OBESITY AND UDNERWEIGHT #######################
outdir_folder <- paste0(outdir, "Scatters/")

data_level <- read_data_level(variables = c("prev_bmi_30", "prev_bmi_l185"),sexes = sexes,age_type = "ageStd",region_level = "Country") %>% 
  filter(age_group == "ageStd") %>%
  filter(year == plot.start.year | year == plot.end.year)

data_level_underweight <- data_level %>%
  select(-c(l,u,se)) %>%
  pivot_wider(names_from = variable, values_from = mean) %>%
  mutate(excess_underweight = prev_bmi_l185-prev_bmi_30) %>%
  group_by(sex,year) %>%
  filter(excess_underweight == max(excess_underweight)) %>%
  ungroup()

data_level_obese <- data_level %>%
  select(-c(l,u,se)) %>%
  pivot_wider(names_from = variable, values_from = mean) %>%
  mutate(excess_obese = prev_bmi_30-prev_bmi_l185) %>%
  group_by(sex,year) %>%
  filter(excess_obese == max(excess_obese)) %>%
  ungroup()

######################### LOOKING AT COUNTRIES WITH NO SIGNIFICANT CHANGE IN UNDERWIEGHT #######
data_level <- read_data_level(variables = c("prev_bmi_l185"),sexes = sexes,age_type = "ageStd",region_level = "Country") %>% 
  filter(age_group == "ageStd") %>%
  select(-c(l,u,se)) %>%
  filter(year == plot.start.year) %>%
  rename(start_level = mean)

data_timechange <- read_data_timechange(variables = c("prev_bmi_l185"),sexes = sexes,age_groups = c("ageStd"),age_type = "ageStd",region_level = "Country") %>% 
  filter(age_group == "ageStd") %>%
  select(-c(l,u,se)) %>%
  filter(start.year == plot.start.year & end.year == plot.end.year) %>%
  rename(mean_timechange = mean) %>%
  left_join(.,data_level)

my_sex <- "male"
subset <- data_timechange %>%
  filter(sex == my_sex) %>%
  filter(PP.increase >= 0.2 & PP.increase <= 0.8)

######################### LOOKING AT COUNTRIES WHERE DOUBLE BURDEN COMPOSITION SHIFTED ##############

data_level <- read_data_level(variables = c("prev_bmi_30_proportion_double_burden"),sexes = sexes,age_type = "ageStd",region_level = "Country") %>% 
  filter(age_group == "ageStd") %>%
  select(-c(l,u,se, PP_obesity_larger_than_underweight)) %>%
  filter(year == plot.start.year | year == plot.end.year) %>%
  mutate(year = ifelse(year == plot.start.year, "start", "end")) %>%
  pivot_wider(values_from = mean, names_from = year) %>%
  group_by(sex) %>%
  summarise(n = length(which(start < 50 & end >50))) %>%
  ungroup()

######################### LOOKING AT COUNTRIES PREVALENCE OF SEVERE OBESITY 2020 v PREV OBESITY 1990 ##############

data_level <- read_data_level(variables = c("prev_bmi_30", "prev_bmi_35"),sexes = sexes,age_type = "ageStd",region_level = "Country") %>% 
  filter(age_group == "ageStd") %>%
  select(-c(l,u,se)) %>%
  filter(year == plot.start.year & variable == "prev_bmi_30" | year == plot.end.year & variable == "prev_bmi_35") %>%
  select(-year) %>%
  mutate(variable = ifelse(variable == "prev_bmi_30", "prev_bmi_30_start", "prev_bmi_35_end")) %>%
  pivot_wider(names_from = variable, values_from = mean) %>%
  mutate(diff = prev_bmi_35_end - prev_bmi_30_start) 

data_level_f <- data_level %>% filter(sex == "female")
data_level_m <- data_level %>% filter(sex == "male")


#### REGRESSIONS OF CHANGE V LEVEL AND CHANGE V CHANGE

## UNDERWEIGHT CHANGE V LEVEL
data_level <- read_data_level(variables = c("prev_bmi_l185", "prev_bmi_30"),sexes = sexes,age_type = "ageStd",region_level = "Country") %>% 
  filter(age_group == "ageStd") %>%
  select(-c(l,u,se)) %>%
  filter(year == plot.start.year) %>%
  rename(start_level = mean)

data_timechange <- read_data_timechange(variables = c("prev_bmi_l185", "prev_bmi_30"),sexes = sexes,age_groups = c("ageStd"),age_type = "ageStd",region_level = "Country") %>% 
  filter(age_group == "ageStd") %>%
  select(-c(l,u,se)) %>%
  filter(start.year == plot.start.year & end.year == plot.end.year) %>%
  rename(mean_timechange = mean) %>%
  left_join(.,data_level)

data_timechange_v_timechange <- read_data_timechange(variables = c("prev_bmi_l185", "prev_bmi_30"),sexes = sexes,age_groups = c("ageStd"),age_type = "ageStd",region_level = "Country") %>% 
  filter(age_group == "ageStd") %>%
  select(-c(l,u,se, PP.increase)) %>%
  filter(start.year == plot.start.year & end.year == plot.end.year) %>%
  pivot_wider(names_from = variable, values_from = mean)

## find correlation coefficients
ps <- list()
for (my_sex in sexes){
  for(my_variable in c("prev_bmi_30", "prev_bmi_l185")){
    xpoz <- ifelse(my_variable == "prev_bmi_l185", 30,  40) 
    ypoz <- ifelse(my_variable == "prev_bmi_l185", 2, 28) 
    
    ps[[paste(my_variable, my_sex, "change v level")]] <- ggscatter(data_timechange %>% filter(sex == my_sex & variable == my_variable), 
                                                  x = "start_level", y = "mean_timechange",
                                                  add = "reg.line",
                                                  add.params = list(color = "blue", fill = "lightgray"),
                                                  conf.int = TRUE) +
      stat_cor(method = "pearson", label.x = xpoz, label.y = ypoz) +
      geom_hline(yintercept = 0, colour = "grey50") +
      geom_vline(xintercept = 0, colour = "grey50") +
      ggtitle(paste0("Change vs level ", ifelse(my_variable == "prev_bmi_l185", "underweight", "obesity")," in ", ifelse(my_sex == "female", "women", "men")))+
      xlab(paste0("Level of ", ifelse(my_variable == "prev_bmi_l185", "underweight", "obesity"), " in 1990 (%)")) +
      ylab(paste0("Change ", ifelse(my_variable == "prev_bmi_l185", "underweight", "obesity"), " 1990-2020 (percentage points)"))  
  }
  
  my_variable <- "prev_bmi_30"
  xpoz <- ifelse(my_variable == "prev_bmi_l185", 30,  40) 
  ypoz <- ifelse(my_variable == "prev_bmi_l185", 2, 28)
  # ruling out points with bmi <5%
  ps[[paste("prev_bmi_30", my_sex, "change v level", "test 5%")]] <- ggscatter(data_timechange %>% filter(sex == my_sex & variable == "prev_bmi_30" & start_level >=5), 
                                                                  x = "start_level", y = "mean_timechange",
                                                                  add = "reg.line",
                                                                  add.params = list(color = "blue", fill = "lightgray"),
                                                                  conf.int = TRUE) +
    xlab("Level obesity 1990 (%)") +
    ylab("Absolute change in obesity 1990-2020 (percentage points)") +
    stat_cor(method = "pearson", label.x = xpoz, label.y = ypoz) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_vline(xintercept = 0, colour = "grey50") +
    ggtitle(paste0("Change vs level obesity in ", ifelse(my_sex == "female", "women", "men"), ", dropping those with <5% obesity in 1990")) +
    xlab(paste0("Level of ", ifelse(my_variable == "prev_bmi_l185", "underweight", "obesity"), " in 1990 (%)")) +
    ylab(paste0("Change ", ifelse(my_variable == "prev_bmi_l185", "underweight", "obesity"), " 1990-2020 (percentage points)"))  
  
  # timechnage v timechange correlation
  ps[[paste(my_sex, "timechange obesity vs timechange underweight")]] <- ggscatter(data_timechange_v_timechange %>% filter(sex == my_sex), 
                                                                  x = "prev_bmi_30", y = "prev_bmi_l185",
                                                                  add = "reg.line",
                                                                  add.params = list(color = "blue", fill = "lightgray"),
                                                                  conf.int = TRUE) +
    xlab("Change obesity 1990-2020 (percentage points)") +
    ylab("Change underweight 1990-2020 (percentage points)") +
    stat_cor(method = "pearson", label.x = 10, label.y = 5) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_vline(xintercept = 0, colour = "grey50") +
    ggtitle(paste0("Change underweight vs change obesity in ",ifelse(my_sex == "female", "women", "men")))
  
}

blank <- grid.rect(gp=gpar(col=NA, fill = NA))

cairo_pdf(paste0(outdir_folder, "correlations change v level.pdf"), height = 15, width = 15, onefile = T)

grid.arrange(
  arrangeGrob(
    ps[[paste("prev_bmi_l185", "female","change v level")]],
    ps[[paste("prev_bmi_l185", "male","change v level")]],
    ps[[paste("prev_bmi_30", "female","change v level")]],
    ps[[paste("prev_bmi_30", "male","change v level")]]
  )
)

grid.arrange(
  arrangeGrob(
    ps[[paste("prev_bmi_30", "female", "change v level", "test 5%")]],
    ps[[paste("prev_bmi_30", "male", "change v level", "test 5%")]],
    ps[[paste("female", "timechange obesity vs timechange underweight")]],
    ps[[paste("male", "timechange obesity vs timechange underweight")]]
  )
)


dev.off()





