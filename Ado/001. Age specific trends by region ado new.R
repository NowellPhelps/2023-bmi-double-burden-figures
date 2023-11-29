## Spaghetti plots of sorts

print('job started')
library(tidyverse)
library(grid)
library(gridExtra)

outdir_folder    <- paste0(outdir, "Age trends/")
dir.create(outdir_folder, showWarnings = F)
res <- read_data_level(variables = c(variable), sexes = c(sex), region_level = "Country",age_type = "ageSpecific") %>%
  filter(year %in% c(plot.start.year, plot.end.year))

ymax <- ceiling(max(res$mean)/5)*5
ymin <- 0

ps <- list()
for(sregion in sregion_order){
  for (plot.year in c(plot.start.year, plot.end.year)){
    p <- ggplot(res %>% filter(Superregion == sregion, year == plot.year), aes(x = age_group, y = mean)) +
      geom_line(aes(group = Country), colour = sregion_col[sregion]) +
      theme_bw() +
      ggtitle(sregion) +
      xlab("Age (years)") +
      scale_x_continuous(breaks = seq(min(res$age_group), max(res$age_group)))+
      scale_y_continuous(limits = c(ymin,ymax)) +
      theme(panel.grid = element_blank(), 
            axis.title = element_text(size = 15), 
            axis.text = element_text(size = 10)) 
    
    if(sregion == "High-income western" | sregion == "South Asia"){
      p <- p + ylab(paste0(str_to_sentence(get_var_longname(variable)), "%"))
    } else{
      p <- p + ylab("")
    }
    
    if(sregion %in% sregion_order[1:4]){
      p <- p + theme(axis.text.x = element_blank()) + xlab("")
    }
    
    ps[[paste(plot.year, sregion)]] <- p
  }
}

blank <- grid.rect(gp=gpar(col=NA, fill = NA))


plot.title <- paste0("Age trends in ", get_var_longname(variable)," for ", ifelse(sex == "male","boys", "girls"))
cairo_pdf(paste0(outdir_folder, paste0("Age trends in ", variable," ", sex), ".pdf"), height = 15, width = 20, onefile=T)


grid.arrange(
  main=textGrob(plot.title,hjust=0,just = c("left"),x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 25)),
  
  textGrob(plot.start.year,hjust=0,just = c("left"),x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 20)),
  arrangeGrob(ps[[paste(plot.start.year, "High-income western")]],
              ps[[paste(plot.start.year, "Central and eastern Europe")]],
              ps[[paste(plot.start.year, "Latin America and the Caribbean")]],
              ps[[paste(plot.start.year, "East and southeast Asia and the Pacific" )]],
              ps[[paste(plot.start.year, "South Asia")]],
              ps[[paste(plot.start.year, "Central Asia, Middle East and north Africa")]],
              ps[[paste(plot.start.year, "Oceania")]],
              ps[[paste(plot.start.year, "Sub-Saharan Africa")]],
              nrow = 2, ncol = 4),
  blank,
  
  textGrob(plot.end.year,hjust=0,just = c("left"),x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 20)),
  arrangeGrob(ps[[paste(plot.end.year, "High-income western")]],
              ps[[paste(plot.end.year, "Central and eastern Europe")]],
              ps[[paste(plot.end.year, "Latin America and the Caribbean")]],
              ps[[paste(plot.end.year, "East and southeast Asia and the Pacific" )]],
              ps[[paste(plot.end.year, "South Asia")]],
              ps[[paste(plot.end.year, "Central Asia, Middle East and north Africa")]],
              ps[[paste(plot.end.year, "Oceania")]],
              ps[[paste(plot.end.year, "Sub-Saharan Africa")]],
              nrow = 2, ncol = 4),
  blank,
  nrow = 7, 
  heights = c(2, 1, 10, 1, 1, 10, 1)
)
dev.off()

  

