library(tidyverse)
library(ggnewscale)
library(reshape)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)

if (variable_type == "obesity"){
  variables <- c("prev_bmi_30")
  names(variables)<- c("\u2265 30")
  col_scale<- col_scale_obesity
  fill_scale <- fill_scale_obesity
  
}else if (variable_type == "severe obesity"){
  variables <- c("prev_bmi_35")
  names(variables)<- c("\u2265 35")
  col_scale <- col_scale_obesity
  fill_scale <- fill_scale_obesity
  
}else if (variable_type == "underweight"){
  variables <- c("prev_bmi_l185")
  names(variables) <- c("<18.5")
  col_scale<- col_scale_all
  fill_scale <- fill_scale_all
}


# Read data
data_slopes <- read_data_slopes(variables, sexes, age_type = "ageStd", region_level = "Country", slope_type = slope_type) %>% 
  filter(sex == my_sex, age_group == "ageStd") %>%
  select(mid.year,Country, mean, l,u, variable, Region, Superregion) 
  
data_slopes$variable <- factor(data_slopes$variable, levels = variables)

# Generate plots
ps <- list()

for (my_country in unique(countrylist$Country)){
  
  my_subset <- data_slopes %>% filter(Country == my_country & mid.year > plot.start.year & mid.year < plot.end.year)
  
  p <- ggplot(my_subset, aes(x = mid.year, group = variable)) + 
    geom_line(mapping=aes(y=mean, color = variable)) +
    geom_ribbon(mapping=aes(ymin = l, ymax = u, fill = variable), alpha= 0.2) +
    col_scale +
    fill_scale +
    scale_x_continuous(expand = c(0,0), limits = c(plot.start.year, plot.end.year))+
    xlab("")+
    theme_bw() +
    ggtitle(my_country)+
    theme(text=element_text(size=7)) +
    theme(legend.title=element_blank()) +
    theme(legend.position = "none")+
    theme(axis.title=element_blank()) + 
    theme(axis.text.x=element_blank()) +
    theme(axis.text.y=element_text(size=10.5)) +
    geom_hline(yintercept = 0)
  
  ps[[my_country]] <- p
}

blank <- ggplot()+ theme(panel.background = element_rect(fill='transparent'), #transparent panel bg
                                             plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
                                             panel.grid.major = element_blank(), #remove major gridlines
                                             panel.grid.minor = element_blank(), #remove minor gridlines
                                             legend.background = element_rect(fill='transparent'), #transparent legend bg
                                             legend.box.background = element_rect(fill='transparent'))

ps[["blank"]] <- blank

# Recover legend
plot.legend <- ps[["Afghanistan"]] + theme(legend.position = "bottom",text=element_text(size=12)) + guides(fill = guide_legend(nrow = 1, reverse = F))
g_legend<-function(p){
  tmp <- ggplotGrob(p)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
legend <- arrangeGrob(g_legend(plot.legend), ncol=1, nrow=1)

country.names <- countrylist[order(countrylist$Superregion,countrylist$Region,countrylist$Country),]
region_order <- unique(country.names$Region)

find_countries_to_give_x_labels <- function(my_list, num_in_row){
  num_countries <- length(my_list)
  remainder <- ifelse(num_countries%%num_in_row == 0, num_in_row, num_countries%%num_in_row)
  countries_to_label <- my_list[(num_countries - remainder +1):num_countries]
}

pdf_title <- paste0(outdir_folder, slope_type, " in ", ifelse(variable_type == "all", "all prevalence categories", variable_type)," by country ", my_sex, ".pdf")


cairo_pdf(pdf_title, height = 15, width = 15, onefile=T)


for (my_region in region_order){
  my_sregion <- unique(country.names$Superregion[which(country.names$Region == my_region)])
  my_countries <- c(country.names$Country[which(country.names$Region == my_region)], rep("blank",20-length(which(country.names$Region == my_region))))
  countries_to_give_x_labels <- find_countries_to_give_x_labels(country.names$Country[which(country.names$Region == my_region)], 5)
  heights <- rep(.485/1.97,4)
  heights[ceiling(min(which(my_countries %in% countries_to_give_x_labels))/5)] <- .515/1.97
  
  my_subset <- data_slopes %>% filter(Country %in% my_countries & mid.year > plot.start.year & mid.year < plot.end.year)
  ylims <- c(min(my_subset$l) -1, max(my_subset$u) + 1)
  
  for (my_country in my_countries[1:length(country.names$Country[which(country.names$Region == my_region)])]){
    ps[[my_country]] <- ps[[my_country]] + scale_y_continuous(expand = c(0,0), limits = ylims)
    if(my_country %in% countries_to_give_x_labels){
      ps[[my_country]] <- ps[[my_country]] + theme(axis.text.x=element_text(size=10.5,angle = 45,vjust=1, hjust = 1, margin = margin(0,0,0,0)))
    }
  }
  
  
  plot.title <- paste0("Sex: ", switch(my_sex, "female" = "Female", "male" = "Male"), "\nSuperregion: ", my_sregion, "\nRegion: ", my_region)
  
  grid.arrange(
    main=textGrob(plot.title,hjust=0,just = c("left"),x = unit(0.05, "npc"),gp = gpar(col = "black", fontsize = 18)),
    
    arrangeGrob(
      arrangeGrob(
        textGrob("Annual relative change (%)", rot = 90, vjust = 1),
        
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

