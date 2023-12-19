## APPENDIX FIGURE 3 - WHEEL PLOTS ALL

figNum <- 4
outdir_folder <- paste0(outdir, "Numbered/")
dir.create(outdir_folder, showWarnings = FALSE)

age_groups      <- c("ageStd")

errorbar_types  <- c("none") 
type            <- "all"

library(tidyverse)
library(ggthemes)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(ggnewscale)
library(egg)
library(scales)

################## (1) SET VARIABLES AND FILLS #################################
variables <- c("prev_bmi_l185", "prev_bmi_185_20", "prev_bmi_20_25", "prev_bmi_25_30","prev_bmi_30_35", "prev_bmi_35_40", "prev_bmi_40")
names(variables) <- c("<18.5","18.5 to <20","20 to <25","25 to <30","30 to <35","35 to <40","\u2265 40")
fill_scale <- fill_scale_all
ylims <- c(0, 100.00001)


##################### (2) READ AND FORMAT DATA #################################
studies_data <- read_source_data(age_type) %>% select(iso, sex, N_all, N_natl)

data <- read_data_level(variables, sexes, "ageStd", region_level = "Country") %>%
  filter(year %in% c(plot.start.year, plot.end.year)) %>%
  filter(age_group %in% age_groups)%>% 
  left_join(studies_data, by = c("sex", "iso"))

data$year     <- factor(data$year)
data$variable <- factor(data$variable, levels = variables, labels = variables)
data$age_group <- factor(data$age_group, levels = age_groups,labels = age_groups)

### (3) PLOT WHEEL PLOTS ##########
composition_types <- c("absolute")

ps <- list()
for(my_sex in sexes) {
  for (plot.year in c(plot.start.year, plot.end.year)){
    for(my_order in c("default")){
      for(obesity_bottom in c(T,F)){
        print(paste(my_sex, plot.year, type, my_order, obesity_bottom))
        ps[[paste(my_sex, plot.year, "all", my_order, obesity_bottom)]] <- circular_bar_function_all(data %>% filter(sex == my_sex &age_group == "ageStd" & year == plot.year), my_sex = my_sex, my_year = plot.year,showLegend = ifelse(plot.year==plot.end.year,T,F), order = my_order, obesity_bottom = obesity_bottom, studyNumbers = T)
      }
    }
  }
}

p_leg <- ggplot(data %>% mutate(Region = factor(Region, levels = region_order)), aes(x = mean, y = l, colour = Region)) +
  geom_point(size = 2.5, shape = 15) +
  scale_colour_manual(values = region_col) +
  theme_bw() +
  theme(legend.direction = "horizontal",
        legend.text = element_text(size = 12),
        legend.title = element_blank())

ps[[paste("legend", "wheel countries")]] <- get_legend(p_leg)

blank <- grid.rect(gp=gpar(col=NA, fill = NA))

###### PLOT PDFS ###########################
cairo_pdf(paste0(outdir_folder, "Appendix Figure ",figNum, ".pdf"), height = 18, width = 25,onefile=T)

grid.arrange(textGrob("Women",hjust=0, just = c("left"),x = unit(0.01, "npc"),gp = gpar(col = "black", fontsize = 20)),
             arrangeGrob(blank,ps[[paste("female", plot.start.year, "all", "default", T)]],
                         blank,ps[[paste("female", plot.end.year, "all", "default", T)]],
                         blank,nrow= 1, widths = c(7,50,6,50,7)),
             blank,
             ps[[paste("legend", "wheel countries")]],
             blank,
             ncol = 1, heights = c(2, 50,2,5,2))

grid.arrange(textGrob("Men",hjust=0, just = c("left"),x = unit(0.01, "npc"),gp = gpar(col = "black", fontsize = 20)),
             arrangeGrob(blank,ps[[paste("male", plot.start.year, "all", "default", T)]],
                         blank,ps[[paste("male", plot.end.year, "all", "default", T)]],
                         blank,nrow= 1, widths = c(7,50,6,50,7)),
             blank,
             ps[[paste("legend", "wheel countries")]],
             blank,
             ncol = 1, heights = c(2, 50,2,5,2))

dev.off()











