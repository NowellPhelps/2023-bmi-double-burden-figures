### COUNTRIES WITH MAX EXCESS OBESITY AND UDNERWEIGHT #######################
outdir_folder <- paste0(outdir, "Scatters/")
dir.create(outdir_folder, showWarnings = F)

data_level <- read_data_level(variables = c("prev_bmi_2sd", "prev_bmi_neg2sd"),sexes = sexes,age_type = "ageStd",region_level = "Country") %>% 
  filter(age_group == "ageStd") %>%
  filter(year == plot.start.year | year == plot.end.year)

data_level_thinness <- data_level %>%
  select(-c(l,u,se)) %>%
  pivot_wider(names_from = variable, values_from = mean) %>%
  mutate(excess_thinness = prev_bmi_neg2sd-prev_bmi_2sd) %>%
  group_by(sex,year) %>%
  filter(excess_thinness == max(excess_thinness)) %>%
  ungroup()

data_level_obese <- data_level %>%
  select(-c(l,u,se)) %>%
  pivot_wider(names_from = variable, values_from = mean) %>%
  mutate(excess_obese = prev_bmi_2sd-prev_bmi_neg2sd) %>%
  group_by(sex,year) %>%
  filter(excess_obese == max(excess_obese)) %>%
  ungroup()

######################### LOOKING AT COUNTRIES WITH NO SIGNIFICANT CHANGE IN UNDERWIEGHT #######
data_level <- read_data_level(variables = c("prev_bmi_neg2sd"),sexes = sexes,age_type = "ageStd",region_level = "Country") %>% 
  filter(age_group == "ageStd") %>%
  select(-c(l,u,se)) %>%
  filter(year == plot.start.year) %>%
  dplyr::rename(start_level = mean)

data_timechange <- read_data_timechange(variables = c("prev_bmi_neg2sd"),sexes = sexes,age_groups = c("ageStd"),age_type = "ageStd",region_level = "Country") %>% 
  filter(age_group == "ageStd") %>%
  select(-c(l,u,se)) %>%
  filter(start.year == plot.start.year & end.year == plot.end.year) %>%
  dplyr::rename(mean_timechange = mean) %>%
  left_join(.,data_level)

my_sex <- "male"
subset <- data_timechange %>%
  filter(sex == my_sex) %>%
  filter(PP.increase >= 0.2 & PP.increase <= 0.8)

######################### LOOKING AT COUNTRIES WHERE DOUBLE BURDEN COMPOSITION SHIFTED ##############

data_level <- read_data_level(variables = c("prev_bmi_2sd_proportion_double_burden"),sexes = sexes,age_type = "ageStd",region_level = "Country") %>% 
  filter(age_group == "ageStd") %>%
  select(-c(l,u,se)) %>%
  filter(year == plot.start.year | year == plot.end.year) %>%
  mutate(year = ifelse(year == plot.start.year, "start", "end")) %>%
  pivot_wider(values_from = mean, names_from = year) %>%
  group_by(sex) %>%
  summarise(n = length(which(start < 50 & end >50))) %>%
  ungroup()


#### REGRESSIONS OF CHANGE V LEVEL AND CHANGE V CHANGE

## thinness CHANGE V LEVEL
data_level <- read_data_level(variables = c("prev_bmi_neg2sd", "prev_bmi_2sd"),sexes = sexes,age_type = "ageStd",region_level = "Country") %>% 
  filter(age_group == "ageStd") %>%
  select(-c(l,u,se)) %>%
  filter(year == plot.start.year) %>%
  dplyr::rename(start_level = mean)

data_timechange <- read_data_timechange(variables = c("prev_bmi_neg2sd", "prev_bmi_2sd"),sexes = sexes,age_groups = c("ageStd"),age_type = "ageStd",region_level = "Country") %>% 
  filter(age_group == "ageStd") %>%
  select(-c(l,u,se)) %>%
  filter(start.year == plot.start.year & end.year == plot.end.year) %>%
  dplyr::rename(mean_timechange = mean) %>%
  left_join(.,data_level)

data_timechange_v_timechange <- read_data_timechange(variables = c("prev_bmi_neg2sd", "prev_bmi_2sd"),sexes = sexes,age_groups = c("ageStd"),age_type = "ageStd",region_level = "Country") %>% 
  filter(age_group == "ageStd") %>%
  select(-c(l,u,se, PP.increase)) %>%
  filter(start.year == plot.start.year & end.year == plot.end.year) %>%
  pivot_wider(names_from = variable, values_from = mean)

## find correlation coefficients
ps <- list()
for (my_sex in sexes){
  for(my_variable in c("prev_bmi_2sd", "prev_bmi_neg2sd")){
    xpoz <- ifelse(my_variable == "prev_bmi_neg2sd", 20, 7) 
    ypoz <- ifelse(my_variable == "prev_bmi_neg2sd", 2, -2) 
    
    ps[[paste(my_variable, my_sex, "change v level")]] <- ggscatter(data_timechange %>% filter(sex == my_sex & variable == my_variable), 
                                                                    x = "start_level", y = "mean_timechange",
                                                                    add = "reg.line",
                                                                    add.params = list(color = "blue", fill = "lightgray"),
                                                                    conf.int = TRUE) +
      stat_cor(method = "pearson", label.x = xpoz, label.y = ypoz) +
      geom_hline(yintercept = 0, colour = "grey50") +
      geom_vline(xintercept = 0, colour = "grey50") +
      ggtitle(paste0("Change vs level ", ifelse(my_variable == "prev_bmi_neg2sd", "thinness", "obesity")," in ", ifelse(my_sex == "female", "girls", "boys")))+
      xlab(paste0("Level of ", ifelse(my_variable == "prev_bmi_neg2sd", "thinness", "obesity"), " in ", plot.start.year, "(%)")) +
      ylab(paste0("Change ", ifelse(my_variable == "prev_bmi_neg2sd", "thinness", "obesity"), " ",plot.start.year, "-",plot.end.year," (percentage points)"))  
  }
  
  # timechange v timechange correlation
  ps[[paste(my_sex, "timechange obesity vs timechange thinness")]] <- ggscatter(data_timechange_v_timechange %>% filter(sex == my_sex), 
                                                                                   y = "prev_bmi_2sd", x = "prev_bmi_neg2sd",
                                                                                   add = "reg.line",
                                                                                   add.params = list(color = "blue", fill = "lightgray"),
                                                                                   conf.int = TRUE) +
    ylab(paste0("Change obesity ",plot.start.year, "-",plot.end.year," (percentage points)")) +
    xlab(paste0("Change thinness ",plot.start.year, "-",plot.end.year," (percentage points)")) +
    stat_cor(method = "pearson", label.y = 25, label.x = -10) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_vline(xintercept = 0, colour = "grey50") +
    ggtitle(paste0("Change thinness vs change obesity in ",ifelse(my_sex == "female", "girls", "boys")))
  
}

blank <- grid.rect(gp=gpar(col=NA, fill = NA))

cairo_pdf(paste0(outdir_folder, "correlations change v level and change v change.pdf"), height = 15, width = 15, onefile = T)

grid.arrange(
  arrangeGrob(
    ps[[paste("prev_bmi_neg2sd", "female","change v level")]],
    ps[[paste("prev_bmi_neg2sd", "male","change v level")]],
    ps[[paste("prev_bmi_2sd", "female","change v level")]],
    ps[[paste("prev_bmi_2sd", "male","change v level")]]
  )
)

grid.arrange(
  arrangeGrob(
    ps[[paste("female", "timechange obesity vs timechange thinness")]],
    ps[[paste("male", "timechange obesity vs timechange thinness")]],
    blank,
    blank
  )
)


dev.off()
