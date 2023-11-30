decomposition_plot <- function(subset, my_sex, my_age_group, plot.start.year, plot.end.year, type, ordertype = "ascending", countrycols = F, vertical = F, returnLeg = F, Legpoint = F, returnTitle = F, offset = 32, studyNumbers = F, fontSize = 3.2){
  
  # Rename countries and regions for plotting
  subset$Country[subset$Country == "Czech Republic"]   <- "Czechia"
  subset$Country[subset$Country == "Swaziland"]        <- "Eswatini"
  subset$Country[subset$Country == "Macedonia (TFYR)"] <- "North Macedonia"
  subset$Region[subset$Region == "North Africa and Middle East"]        <- "Middle East and North Africa"
  subset$Region[subset$Region == "Southern and Tropical Latin America"] <- "Southern and Latin America"
  
  plot_data <- subset %>% 
    filter(start.year == plot.start.year & end.year == plot.end.year) 

  if(my_age_group == "ageStd"){
    plot_data <- plot_data %>% filter(age_group == "ageStd")
  }
  
 # plot.title <- str_to_title(paste0(ifelse(my_sex == "female", "Female ", "Male "),"decomposed change in ", type, " ", plot.start.year, "-", plot.end.year))
                       
  ylims <- c(min(c(plot_data$mean, plot_data$overall_change)), max(c(plot_data$mean, plot_data$overall_change)))
  ytick <- 10
  yticklims <- c(ceiling(ylims[1]/ytick)*ytick, floor(ylims[2]/ytick)*ytick)
  
  ylims[1] <- ylims[1] - offset
  ylims[2] <- ylims[2] + offset
  
  ## subset by sex and order
  plot_data <- plot_data %>% filter(sex == my_sex) %>% filter(age_group == my_age_group)
  
  if(ordertype == "ascending"){
    order <- plot_data %>% 
      filter(variable == variables[1]) %>%
      arrange(overall_change)
  } else if (ordertype == "region"){
    order <- plot_data %>% 
      filter(variable == variables[1]) %>%
      arrange(match(Region, region_order), -overall_change)
  } 
  
  plot_data <- plot_data %>% mutate(pos = match(Country, order$Country))
  text_data <- plot_data %>% filter(variable == variables[1])
  
  limits_data <- plot_data %>%
    group_by(iso) %>%
    summarise(min = min(mean), max = max(mean)) %>%
    ungroup()
  
  text_data <- left_join(text_data, limits_data, by = "iso") %>%
    mutate(min = ifelse(min >0 & overall_change >0, 0, ifelse(min < overall_change, min, overall_change)),
           max = ifelse(max <0 & overall_change <0, 0, ifelse(max < overall_change, max, overall_change)),
           ypos_text = ifelse(pos%%2 == 0,max, min),
           y = ifelse(pos %%2 == 0,ylims[2] - offset  + 2,ylims[1] + offset  - 2)) %>%
    arrange(pos)
  
  if (countrycols){
    text_data <- text_data %>% mutate(colour = region_col[Region])
  } else{
    text_data <- text_data %>% mutate(colour = "grey50")
  }
  
  # if(studyNumbers){
  #   text_data <- text_data %>% 
  #     mutate(Country = ifelse(pos%%2 == 0,
  #            paste0(Country," ", N_all, " (", N_natl, ")"),
  #            paste0(N_all, " (", N_natl, ") ", Country)))
  # }
  if(studyNumbers){
      text_data <- text_data %>%
        mutate(Country = paste0(Country," (", N_all, ",", N_natl, ")"))
    }
  
  
  p <- ggplot(plot_data, aes(y = mean, x = pos)) +
    geom_segment(data = text_data, aes(x = pos, y =y , xend = pos, yend = ypos_text), inherit.aes = F, colour = grey(0.85),alpha = 1,linewidth = 0.001) +
    geom_bar(position = "stack", stat = "identity", width = 0.6, aes(fill=variable)) +
    fill_scale +
#    ggtitle(plot.title) +
    geom_point(data = plot_data %>% filter(variable == variables[1]) %>% mutate(variable = "overall"), aes(y = overall_change, colour = variable), alpha = 0.6, shape = 16, size = 2, show.legend = F)+
    col_scale +
    ylab(paste0("Change ",plot.start.year, "-", plot.end.year, " (percentage points)")) +
    xlab("Country") +
    geom_hline(yintercept = 0)+
    new_scale_color() +
    
    geom_text(data = text_data %>% filter(pos%%2 == 1), aes(x = pos, y = y -1, label = Country),
              size =  fontSize,
              colour = (text_data %>% filter(pos%%2 == 1))$colour,
              angle = -45,
              vjust = 0.5,
              hjust = 0)  +
    
    geom_text(data = text_data %>% filter(pos%%2 == 0), aes(x = pos, y = y + 1, label = Country),
              size =  fontSize,
              colour = (text_data %>% filter(pos%%2 == 0))$colour,
              angle = -45,
              vjust = 0.5,
              hjust = 1)  +
    
    
    scale_y_continuous(breaks = seq(yticklims[1], yticklims[2], ytick), limits = ylims) +
    
    theme_bw()+
    scale_x_continuous(limits = c(0, 201), expand = expansion(add =ifelse(studyNumbers,10,3)))+
    theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(), axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.y = element_text(size = 12)) +
    
    theme(legend.title = element_blank())
    
  if(returnTitle){
    p <- p + ggtitle(str_to_title(my_sex))
  }
 
  if(returnLeg){
    
    
    p <- p + theme(legend.position = "left", 
                   legend.direction = "horizontal", 
                   legend.text = element_text(size = 10))
    
    
    p2 <- ggplot(plot_data, aes(y = mean, x = pos)) +
      geom_point(data = plot_data %>% filter(variable == variables[1]) %>% mutate(variable = "overall"), aes(y = overall_change, colour = variable), alpha = 0.6, shape = 16, size = 2, show.legend = T)+
      col_scale +
      theme_bw() +
      theme(legend.position = "left", 
            legend.direction = "horizontal", 
            legend.text = element_text(size = 10), 
            legend.title = element_blank())
    
    if(Legpoint){
      p <- arrangeGrob(get_legend(p), get_legend(p2), nrow = 1, widths = c(1,1))
    } else{
      p <- get_legend(p)
    }
    
     
    
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
}

decomposition_plot_rotated <- function(subset, my_sex, my_age_group, plot.start.year, plot.end.year, type, ordertype = "ascending", countrycols = F, vertical = F, returnLeg = F, Legpoint = F, returnTitle = F, offset = 32, studyNumbers = F, fontSize = 3){
   
   # Rename countries and regions for plotting
   subset$Country[subset$Country == "Czech Republic"]   <- "Czechia"
   subset$Country[subset$Country == "Swaziland"]        <- "Eswatini"
   subset$Country[subset$Country == "Macedonia (TFYR)"] <- "North Macedonia"
   subset$Region[subset$Region == "North Africa and Middle East"]        <- "Middle East and North Africa"
   subset$Region[subset$Region == "Southern and Tropical Latin America"] <- "Southern and Latin America"
   
   plot_data <- subset %>% 
      filter(start.year == plot.start.year & end.year == plot.end.year) 
   
   if(my_age_group == "ageStd"){
      plot_data <- plot_data %>% filter(age_group == "ageStd")
   }
   
   # plot.title <- str_to_title(paste0(ifelse(my_sex == "female", "Female ", "Male "),"decomposed change in ", type, " ", plot.start.year, "-", plot.end.year))
   
   ylims <- c(min(c(plot_data$mean, plot_data$overall_change)), max(c(plot_data$mean, plot_data$overall_change)))
   ytick <- 10
   yticklims <- c(ceiling(ylims[1]/ytick)*ytick, floor(ylims[2]/ytick)*ytick)
   
   ylims[1] <- ylims[1] - offset
   ylims[2] <- ylims[2] + offset
   
   ## subset by sex and order
   plot_data <- plot_data %>% filter(sex == my_sex) %>% filter(age_group == my_age_group)
   
   if(ordertype == "ascending"){
      order <- plot_data %>% 
         filter(variable == variables[1]) %>%
         arrange(overall_change)
   } else if (ordertype == "region"){
      order <- plot_data %>% 
         filter(variable == variables[1]) %>%
         arrange(match(Region, region_order), -overall_change)
   } 
   
   plot_data <- plot_data %>% mutate(pos = match(Country, order$Country))
   text_data <- plot_data %>% filter(variable == variables[1])
   
   limits_data <- plot_data %>%
      group_by(iso) %>%
      summarise(min = min(mean), max = max(mean)) %>%
      ungroup()
   
   text_data <- left_join(text_data, limits_data, by = "iso") %>%
      mutate(min = ifelse(min >0 & overall_change >0, 0, ifelse(min < overall_change, min, overall_change)),
             max = ifelse(max <0 & overall_change <0, 0, ifelse(max < overall_change, max, overall_change)),
             ypos_text = ifelse(pos%%2 == 0,max, min),
             y = ifelse(pos %%2 == 0,ylims[2] - offset  + 2,ylims[1] + offset  - 2)) %>%
      arrange(pos)
   
   if (countrycols){
      text_data <- text_data %>% mutate(colour = region_col[Region])
   } else{
      text_data <- text_data %>% mutate(colour = "grey50")
   }
   
   # if(studyNumbers){
   #   text_data <- text_data %>% 
   #     mutate(Country = ifelse(pos%%2 == 0,
   #            paste0(Country," ", N_all, " (", N_natl, ")"),
   #            paste0(N_all, " (", N_natl, ") ", Country)))
   # }
   if(studyNumbers){
      text_data <- text_data %>%
         mutate(Country = paste0(Country," (", N_all, ",", N_natl, ")"))
   }
   
   
   p <- ggplot(plot_data, aes(y = mean, x = pos)) +
      geom_segment(data = text_data, aes(x = pos, y =y , xend = pos, yend = ypos_text), inherit.aes = F, colour = grey(0.85),alpha = 1,linewidth = 0.001) +
      geom_bar(position = "stack", stat = "identity", width = 0.6, aes(fill=variable)) +
      fill_scale +
      #    ggtitle(plot.title) +
      geom_point(data = plot_data %>% filter(variable == variables[1]) %>% mutate(variable = "overall"), aes(y = overall_change, colour = variable), alpha = 0.6, shape = 16, size = 2, show.legend = F)+
      col_scale +
      ylab(paste0("Change ",plot.start.year, "-", plot.end.year, " (percentage points)")) +
      xlab("Country") +
      geom_hline(yintercept = 0)+
      new_scale_color() +
      
      geom_text(data = text_data %>% filter(pos%%2 == 1), aes(x = pos, y = y -1, label = Country),
                size =  fontSize,
                colour = (text_data %>% filter(pos%%2 == 1))$colour,
                angle = 0,
                vjust = 0,
                hjust = 1)  +
      
      geom_text(data = text_data %>% filter(pos%%2 == 0), aes(x = pos, y = y + 1, label = Country),
                size =  fontSize,
                colour = (text_data %>% filter(pos%%2 == 0))$colour,
                angle = 0,
                vjust = 0,
                hjust = 0)  +
      
      
      scale_y_continuous(breaks = seq(yticklims[1], yticklims[2], ytick), limits = ylims) +
      
      theme_bw()+
      scale_x_continuous(limits = c(0, 201), expand = expansion(add =1))+
      theme(axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.title.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_blank(),
            axis.title.x = element_text(size = 10),
            axis.text.x = element_text(size = 10)) +
      
      theme(legend.title = element_blank()) +
      coord_flip() +
   
   if(returnTitle){
      p <- p + ggtitle(str_to_title(my_sex))
   }
   
   if(returnLeg){
      
      
      p <- p + theme(legend.position = "left", 
                     legend.direction = "horizontal", 
                     legend.text = element_text(size = 10))
      
      
      p2 <- ggplot(plot_data, aes(y = mean, x = pos)) +
         geom_point(data = plot_data %>% filter(variable == variables[1]) %>% mutate(variable = "overall"), aes(y = overall_change, colour = variable), alpha = 0.6, shape = 16, size = 2, show.legend = T)+
         col_scale +
         theme_bw() +
         theme(legend.position = "left", 
               legend.direction = "horizontal", 
               legend.text = element_text(size = 10), 
               legend.title = element_blank())
      
      if(Legpoint){
         p <- arrangeGrob(get_legend(p), get_legend(p2), nrow = 1, widths = c(1,1))
      } else{
         p <- get_legend(p)
      }
      
      
      
   } else {
      p <- p + theme(legend.position = "none")
   }
   
   return(p)
}



decomposition_plot_region <- function(subset, my_sex, my_age_group, plot.start.year, plot.end.year, type, ordertype = "ascending", returnLeg = F, returnTitle = F, offset = 32){
  
  plot_data <- subset %>% 
    filter(start.year == plot.start.year & end.year == plot.end.year) 
  
  if(my_age_group == "ageStd"){
    plot_data <- plot_data %>% filter(age_group == "ageStd")
  }
  
  # plot.title <- str_to_title(paste0(ifelse(my_sex == "female", "Female ", "Male "),"decomposed change in ", type, " ", plot.start.year, "-", plot.end.year))
  
  ylims <- c(min(c(plot_data$mean, plot_data$overall_change)), max(c(plot_data$mean, plot_data$overall_change)))
  ytick <- 10
  yticklims <- c(ceiling(ylims[1]/ytick)*ytick, floor(ylims[2]/ytick)*ytick)
  
  ylims[1] <- ylims[1] - offset
  ylims[2] <- ylims[2] + offset
  
  ## subset by sex and order
  plot_data <- plot_data %>% filter(sex == my_sex) %>% filter(age_group == my_age_group)
  
  order <- plot_data %>% 
    filter(variable == variables[1]) %>%
    arrange(overall_change)
 
  plot_data <- plot_data %>% mutate(pos = match(Region, order$Region))
  text_data <- plot_data %>% filter(variable == variables[1])
  
  limits_data <- plot_data %>%
    group_by(Region) %>%
    summarise(min = min(mean), max = max(mean)) %>%
    ungroup()
  
  text_data <- left_join(text_data, limits_data, by = "Region") %>%
    mutate(min = ifelse(min >0 & overall_change >0, 0, ifelse(min < overall_change, min, overall_change)),
           max = ifelse(max <0 & overall_change <0, 0, ifelse(max < overall_change, max, overall_change)),
           ypos_text = max,
           y = ylims[1] + offset  - 2) %>%
    arrange(pos)
  
  
  text_data <- text_data %>% mutate(colour = region_col[Region])
  
  p <- ggplot(plot_data, aes(y = mean, x = pos)) +
    geom_segment(data = text_data, aes(x = pos, y =y , xend = pos, yend = ypos_text), inherit.aes = F, colour = grey(0.85), alpha = 1,linewidth = 0.001) +
    geom_bar(position = "stack", stat = "identity", aes(fill=variable)) +
    fill_scale +
    
    geom_point(data = plot_data %>% filter(variable == variables[1]) %>% mutate(variable = "overall"), aes(y = overall_change, colour = variable), alpha = 0.6, shape = 16, size = 3, show.legend = F)+
    col_scale +
    ylab(paste0("Change ",plot.start.year, "-", plot.end.year, " (percentage points)")) +
    xlab("Country") +
    geom_hline(yintercept = 0)+
    new_scale_color() +
    geom_text(data = text_data, aes(y = y-1, label = Region),
              size = 3.6,
              colour = region_col[order(match(names(region_col), text_data$Region))],
              angle = -40,
              hjust = 0) +
    
    scale_y_continuous(breaks = seq(yticklims[1], yticklims[2], ytick), limits = c(-60, 40)) +
    scale_x_continuous(limits = c(0.5,22), expand = c(0,0)) +
    theme_bw() +
    
    theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(), axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.y = element_text(size = 12)) +
    
    theme(legend.title = element_blank())
  
  if(returnTitle){
    p <- p + ggtitle(str_to_title(my_sex))
  }
  
  if(returnLeg){
    
    p <- p + theme(legend.position = "right", legend.direction = "horizontal", legend.text = element_text(size = 12))
    p <- get_legend(p)
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
}
