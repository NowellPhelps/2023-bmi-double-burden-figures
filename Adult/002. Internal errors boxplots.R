library(tidyverse)
scriptname      <- "002. Internal errors boxplots.R"
outdir_folder <- paste0(outdir, "Age trends/")
dir.create(outdir_folder, showWarnings = FALSE)

sexes <- c("female","male")

variables <- c("prev_bmi_l185",
               "prev_bmi_185_20",
               "prev_bmi_20_25",
               "prev_bmi_25_30",
               "prev_bmi_30_35",
               "prev_bmi_35_40",
               "prev_bmi_40")

script <- paste0(scriptDir, "Adult/", scriptname)


print('job started')
library(tidyverse)
library(grid)
library(gridExtra)
outdir_folder    <- paste0(outdir, "Internal errors/")
dir.create(outdir_folder, showWarnings = F)

res <- NULL
for (my_sex in sexes) {
  for (my_variable in variables) {
    res.tmp <- read.csv(paste0(indir_est, "Raw/Model", modelnum, "_", my_sex, "_", my_variable, 
                               "_internal_errors_posterior mean with study random effects CRUDE.csv")) %>%
      mutate(variable = my_variable, sex = my_sex)
    res <- rbind(res, res.tmp)
  }
}

data_final <- res %>%
  mutate(year_group = cut(data_year, breaks = c(1979,1989,1999, 2009,2022), labels = c("1980-89","1990-99","2000-2009","2010-2022"))) %>%
  mutate(error = pmean - observed)

data_final$age_original <- data_final$age
data_final$age <-  cut(data_final$age_original, c(17.5, seq(20, 80, by = 10), 200), right=FALSE)
levels(data_final$age) <- c(18, seq(20, 80, by = 10))

levels(data_final$age) <- c("18"="18-19","20"="20-29","30"="30-39","40"="40-49",
                            "50"="50-59","60"="60-69","70"="70-79","80"="80+")

ylims <- c(floor(min(data_final$error)/0.05)*0.05, ceiling(max(data_final$error)/0.05)*0.05) *100


ps <- list()
for (my_sex in sexes){
  for(my_variable in variables){
    # ps[[paste(my_sex, my_variable, "region")]] <- ggplot(data_final %>% filter(variable == my_variable, sex == my_sex), aes(y = error, x = age)) +
    #   facet_wrap(~Superregion) +
    #   ylim(ylims) +
    #   geom_boxplot() + 
    #   theme_bw()
    # 
    ps[[paste(my_sex, my_variable, "all", "violin")]] <- ggplot(data_final %>% filter(variable == my_variable, sex == my_sex), aes(y = error*100, x = age)) +
      geom_violin() + 
      ylim(ylims) +
      theme_bw() +
      ggtitle(paste(my_sex, my_variable)) +
      ylab("Residual (percentage points)") +
      xlab("Age group")
    
    ps[[paste(my_sex, my_variable, "all", "boxplot")]] <- ggplot(data_final %>% filter(variable == my_variable, sex == my_sex), aes(y = error*100, x = age)) +
      geom_boxplot(outlier.size=0.3) + 
      ylim(ylims) +
      theme_bw() +
      ggtitle(paste(my_sex, my_variable)) +
      ylab("Residual (percentage points)") +
      xlab("Age group")
  }
}


blank <- grid.rect(gp=gpar(col=NA, fill = NA))
# 
# cairo_pdf(paste0(outdir_folder, "Internal errors by age for ", age_type, " by region.pdf"), height = 15, width = 20, onefile=T)
# 
# for (sex in sexes){
#   for(variable in variables){
#     
#     grid.arrange(
#       main=textGrob(paste(sex, variable),hjust=0,just = c("left"),x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 25)),
#       
#       ps[[paste(sex, variable, "region")]],
#       nrow = 2, 
#       heights = c(1, 20)
#     )
#   }
# }
# 
# dev.off()


cairo_pdf(paste0(outdir_folder, "Internal errors by age for ", age_type, " all violin.pdf"), height = 5, width = 15, onefile=T)

for(variable in variables){
  grid.arrange(
    
    arrangeGrob(ps[[paste("female", variable, "all", "violin")]],
                ps[[paste("male", variable, "all", "violin")]],
                nrow = 1)
  )
}
dev.off()

cairo_pdf(paste0(outdir_folder, "Internal errors by age for ", age_type, " all boxplot.pdf"), height = 5, width = 15, onefile=T)

for(variable in variables){
  grid.arrange(
    
    arrangeGrob(ps[[paste("female", variable, "all", "boxplot")]],
                ps[[paste("male", variable, "all",  "boxplot")]],
                nrow = 1)
  )
}


dev.off()


summary <- data_final %>%
  group_by(age, sex, variable) %>%
  summarise(median = median(error)*100) %>%
  ungroup()

max(summary$median)
min(summary$median)

