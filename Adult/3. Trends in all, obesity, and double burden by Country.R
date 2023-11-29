library(tidyverse)
library(ggnewscale)
library(reshape)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)


if(type == "all"){
  variables <- c("prev_bmi_l185","prev_bmi_185_20","prev_bmi_20_25","prev_bmi_25_30","prev_bmi_30_35","prev_bmi_35_40","prev_bmi_40")
  names(variables)<- c("<18.5","18.5 to <20","20 to <25","25 to <30","30 to <35","35 to <40","\u2265 40")
  fill_scale <- fill_scale_all
  ylims <- c(0, 100.00001)
  
} else if (type == "double burden"){
  variables <- c("prev_bmi_l185","prev_bmi_30")
  names(variables)<- c("<18.5","\u2265 30")
  fill_scale <- fill_scale_double_burden
  ylims <- c(0,90)
} 

# Read data
data_level <- read_data_level(variables, sexes, "ageStd", region_level = "Country")

data_level <- data_level %>% 
  filter(sex == my_sex) %>%
  filter(age_group == "ageStd") %>%
  select(year, age_group, Country, mean, variable, Region, Superregion)
  
  
  
data_level$variable <- factor(data_level$variable, levels = variables)

# Generate plots
ps <- list()
palette(rev(c(brewer.pal(11,"Spectral"))[c(4,11)]))

for (my_country in unique(countrylist$Country)){
  p <- ggplot(data_level %>% filter(Country == my_country & year >= plot.start.year & year <= plot.end.year), aes(x = year, y = mean, fill = variable)) +
    geom_area(position='stack') +
    scale_y_continuous(expand=c(0,0), limits = ylims)+
    scale_x_continuous(expand=c(0,0), breaks = c(1990, 2000, 2010, 2020), labels = c("1990 ", "2000 ", "2010 ", "2020 ")) +
    ggtitle(my_country) + 
    fill_scale +
    theme_bw() +
    theme(text=element_text(size=7)) +
    theme(legend.title=element_blank()) +
    theme(legend.position = "none")+
    theme(axis.title=element_blank()) +
    theme(axis.text.y=element_text(size=10.5)) +
    theme(axis.text.x=element_blank())

  ps[[my_country]] <- p
}

blank <- ggplot()+ theme(panel.background = element_rect(fill='transparent'), #transparent panel bg
                                             plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
                                             panel.grid.major = element_blank(), #remove major gridlines
                                             panel.grid.minor = element_blank(), #remove minor gridlines
                                             legend.background = element_rect(fill='transparent'), #transparent legend bg
                                             legend.box.background = element_rect(fill='transparent') #transparent legend panel
)

ps[["blank"]] <- blank

# Recover legend
plot.legend <- ps[["Afghanistan"]] + theme(legend.position = "bottom",text=element_text(size=12)) + guides(fill = guide_legend(nrow = 1, reverse = F))
get_region_trend_plot_legend<-function(p){
  tmp <- ggplotGrob(p)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
legend <- arrangeGrob(get_region_trend_plot_legend(plot.legend), ncol=1, nrow=1)

country.names <- countrylist[order(countrylist$Superregion,countrylist$Region,countrylist$Country),]
region_order <- unique(country.names$Region)

find_countries_to_give_x_labels <- function(my_list, num_in_row){
  num_countries <- length(my_list)
  remainder <- ifelse(num_countries%%num_in_row == 0, num_in_row, num_countries%%num_in_row)
  countries_to_label <- my_list[(num_countries - remainder +1):num_countries]
}



cairo_pdf(paste0(outdir_folder, "Trends in ", ifelse(type=="all", "all prevalence categories", type), " by country ", my_sex,".pdf"), height = 15, width = 15, onefile=T)

for (my_region in region_order){
  my_sregion <- unique(country.names$Superregion[which(country.names$Region == my_region)])
  my_countries <- c(country.names$Country[which(country.names$Region == my_region)], rep("blank",20-length(which(country.names$Region == my_region))))
  countries_to_give_x_labels <- find_countries_to_give_x_labels(country.names$Country[which(country.names$Region == my_region)], 5)
  heights <- rep(.485/1.97,4)
  heights[ceiling(min(which(my_countries %in% countries_to_give_x_labels))/5)] <- .515/1.97
  for (my_country in countries_to_give_x_labels){
    ps[[my_country]] <- ps[[my_country]] + theme(axis.text.x=element_text(size=10.5,angle = 45,vjust=1, hjust = 1, margin = margin(0,0,0,0)))
  }
  plot.title <- paste0("Sex: ", switch(my_sex, "female" = "Female", "male" = "Male"), "\nSuperregion: ", my_sregion, "\nRegion: ", my_region)
  
  grid.arrange(
    main=textGrob(plot.title,hjust=0,just = c("left"),x = unit(0.05, "npc"),gp = gpar(col = "black", fontsize = 18)),
    
    arrangeGrob(
      arrangeGrob(
        textGrob("Prevalence (%)", rot = 90, vjust = 1),
        
          arrangeGrob(
            ps[[my_countries[1]]],ps[[my_countries[2]]],ps[[my_countries[3]]],ps[[my_countries[4]]],ps[[my_countries[5]]],
            ps[[my_countries[6]]],ps[[my_countries[7]]],ps[[my_countries[8]]],ps[[my_countries[9]]],ps[[my_countries[10]]],
            ps[[my_countries[11]]],ps[[my_countries[12]]],ps[[my_countries[13]]],ps[[my_countries[14]]],ps[[my_countries[15]]],
            ps[[my_countries[16]]],ps[[my_countries[17]]],ps[[my_countries[18]]],ps[[my_countries[19]]],ps[[my_countries[20]]],
            ncol=5,nrow=4,
            heights=unit.c(unit(heights[1],"npc"),unit(heights[2],"npc"),unit(heights[3],"npc"),unit(heights[4],"npc")),
            widths=unit.c(unit(0.215,"npc"),rep(unit(.19625, "npc"),4))),
      
        ncol=2,nrow=1,
        widths = c(.025, .975)),
      
      textGrob("Year"),
      ncol=1,nrow=2,
      heights = c(.975, .025)
    ),
    
    legend,
    nrow=3,ncol=1,
    heights=c(0.3,0.95,0.05))

}

dev.off()
