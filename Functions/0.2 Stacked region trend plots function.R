get_region_trend_plot_legend<-function(p){
  tmp <- ggplotGrob(p)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

stacked_region_trend_plots <- function(data_all, my_sex, type, returnLeg = F){
  
  if(type == "double burden"){
    fill_scale      <- fill_scale_double_burden
    ylims           <- c(0,65)
    
  } else if(type == "all"){
    fill_scale       <- fill_scale_all
    ylims            <- c(0, 100.00001)
    
  }else if (type == "ado double burden"){
    fill_scale <- fill_scale_double_burden_ado
    ylims <- c(0,50)
  } 
  
  data_all <- data_all %>% filter(sex == my_sex)
  
  ps <- list()
  
  for (my_name in c(unique(countrylist$Region), "Global")){
    p <- ggplot(data_all %>% filter(name == my_name & year >= plot.start.year & year <= plot.end.year), aes(x = year, y = mean, fill = variable)) +
      geom_area(position='stack') +
      scale_y_continuous(expand=c(0,0), limits = ylims)+
      scale_x_continuous(expand=c(0,0), breaks = c(1990, 2000, 2010, 2020), labels = c("1990 ", "2000 ", "2010 ", "2020 ")) +
      ggtitle(ifelse(my_name == "Global", "World", my_name)) + 
      fill_scale +
      theme_bw() +
      theme(text=element_text(size=7)) +
      theme(legend.title=element_blank()) +
      theme(legend.position = "none")+
      theme(axis.title=element_blank()) + 
      theme(axis.text.x=element_blank()) +
      theme(axis.text.y=element_text(size=10.5))
    
    if (my_name %in% c("Caribbean", "Melanesia", "Polynesia and Micronesia", "Global")){
      p <- p + theme(axis.text.x=element_text(size=10.5,angle = 45,vjust=1, hjust = 1, margin = margin(0,0,0,0)))
    }
    
    ps[[my_name]] <- p
  }
  
  blank <- grid.rect(gp=gpar(col="white"))
  
  # Recover legend
  plot.legend <- ps[["Global"]] + theme(legend.position = "bottom",text=element_text(size=12)) + guides(fill = guide_legend(nrow = 1, reverse = F))
  
  legend <- arrangeGrob(get_region_trend_plot_legend(plot.legend), ncol=1, nrow=1)
  
  plot.title <- switch(my_sex, "female" = "Women", "male" = "   Men   ")
  
  p <- arrangeGrob(
          arrangeGrob(
            textGrob("Prevalence (%)", rot = 90, vjust = 1),
            arrangeGrob(
              arrangeGrob(
                ps[["Central Asia"]],ps[["South Asia"]],ps[["South East Asia"]],ps[["East Asia"]],ps[["High-income Asia Pacific"]],
                ps[["High-income English-speaking countries"]],ps[["North Western Europe"]],ps[["South Western Europe"]],ps[["Central Europe"]],ps[["Eastern Europe"]],
                ps[["North Africa and Middle East"]],ps[["West Africa"]],ps[["Central Africa"]],ps[["East Africa"]],ps[["Southern Africa"]],
                ncol=5,nrow=3,
                heights=unit.c(unit((1/3),"npc"),unit((1/3),"npc"),unit((1/3),"npc")),
                widths=unit.c(unit(0.215,"npc"),rep(unit(.19625, "npc"),4))),
              
              arrangeGrob(
                arrangeGrob(
                  ps[["Central Latin America"]],ps[["Andean Latin America"]],ps[["Southern and Tropical Latin America"]],
                  ps[["Caribbean"]],ps[["Melanesia"]],ps[["Polynesia and Micronesia"]],
                  ncol=3,nrow=2,
                  heights=unit.c(unit(.485,"npc"),unit(.515,"npc")),
                  widths=unit.c(unit(.215/.6075,"npc"),rep(unit(.19625/.6075, "npc"),2))),
                
                arrangeGrob(
                  ps[["Global"]],
                  ncol=1,nrow=1),
                
                ncol=2,nrow=1,
                widths=unit.c(unit(0.6075,"npc"),unit(.3925, "npc"))),
              
              ncol=1,nrow=2,
              heights = c(.592, .408)
            ),
            ncol=2,nrow=1,
            widths = c(.025, .975)),
          
          textGrob("Year"),
          ncol=1,nrow=2,
          heights = c(.975, .025)
        )
  
  if(returnLeg){
    return(legend)
  } else{
    return(p)
  }
  
  return(p)
}
