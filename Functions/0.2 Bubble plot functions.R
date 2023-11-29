bubble_plot_function <- function(data, age_type, my_sex, plot.start.year, plot.end.year, plotTitle = F, returnLeg = F){
  
  met <- d %>%
    group_by(Region, mid_year, survey_type) %>%
    summarise(n = n()) %>%
    spread(survey_type, n) %>%
    mutate_at(vars(c('National','Subnational','Community')), function(x) ifelse(is.na(x), 0, x)) %>%
    mutate(group = 1:n(),
           total = National + Subnational + Community)
  
  
  p <- ggplot() +
    theme_bw() +
    scale_y_continuous(breaks = 1:length(levels(met$Region)),
                       expand = c(0,0),
                       labels = levels(met$Region), name = NULL,
                       minor_breaks = c()) +
    
    scale_x_continuous(minor_breaks = seq(plot.start.year,plot.end.year,1),
                       expand = c(0,0)) +
    
    theme(axis.title.x = element_blank(), text = element_text(size = 25),
          legend.position = 'none', legend.title = element_blank(),
          panel.grid = element_line(colour = grey(0.9), linewidth = .5)) +
    
    coord_equal(clip = 'off',
                xlim = c(start.year - .5, plot.end.year + .5),
                ylim = c(0.5, 21.5)) +
    
    geom_scatterpie(data = met,
                    aes(x = mid_year, y = as.numeric(Region), group = group, r = sqrt(total/100)),
                    cols = c('National','Subnational','Community'), colour = NA) +
    
    scale_colour_manual(values = cols,
                        guide = guide_legend(override.aes = list(size = 4))) +
    
    scale_fill_manual(values = cols)
  
  if(plotTitle){
    if (age_type == "ado"){
      p <- p +
        annotate(geom = 'text', label = ifelse(my_sex == 'female', 'Girls', ifelse(my_sex == 'male','Boys','School-aged children and adolescents')),
                 x = start.year - 13, y = 21, hjust = 0,
                 size = 8)
    } else if (age_type == "adult") {
      p <- p +
        annotate(geom = 'text', label = ifelse(my_sex == 'female', 'Girls', ifelse(my_sex == 'male','Boys','Adults')),
                 x = start.year - 13, y = 21, hjust = 0,
                 size = 8)
    }
  }
  
  
  if(returnLeg){
    d$size <- sample(c(1,10,20), nrow(d), replace = TRUE)
    p_leg1 <- ggplot(data = d) +
      geom_point(aes(x = Region, y = mid_year, colour = survey_type, size = size), stat = 'identity') +
      scale_colour_manual(values = cols) +
      scale_size_area(breaks = c(1,10,20), max_size = 4) +
      # scale_size_manual(values = sizes) +
      guides(colour = guide_legend(override.aes = list(size = 3))) +
      theme(legend.key = element_rect(fill = NA), legend.position = 'bottom', legend.title = element_blank(),
            text = element_text(size = 20))
    
    p <- ggpubr::get_legend(p_leg1)
  }
  
  return(p)
}