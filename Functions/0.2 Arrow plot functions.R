
timechange_arrow <- function(subset, subset_order = NULL, var, my_sex, my_age_group, start.year, end.year, ordertype = "level", data_type = "level", colour_by = "Region", offset = 60, studyNumbers = F){
  # gives interactive time change arrow
  # order takes country_order (ordered by country, coloured by region), or level (ordered by level 1990, colored by magnitude of change)
  
  plot_data <- subset %>% 
    filter(age_group == my_age_group) %>%
    filter(variable == var) %>%
    filter(year == start.year | year == end.year) %>%
    mutate(year = ifelse(year ==start.year, "start", "end")) %>%
    select(-c(l,u,se))
  
  
  xmax <- ceiling(max(plot_data$mean/10))*10
  if(data_type == "level"){
    xmin <- 0
  } else if (data_type == "velocity"){
    xmin <- floor(min(plot_data$mean/10))*10
  }
  
  xlims <- c(xmin - offset, xmax + offset)
  xtick <- 25
  xticklims <- c(max(ceiling(xmin/xtick)*xtick, 0), min(100, floor(xmax/xtick)*xtick))
  
  plot_data <- plot_data %>%
    filter(sex == my_sex) %>%
    pivot_wider(names_from = year, values_from = mean)
  
  if (ordertype == "country") {
    country.list.arranged <- countrylist
    country.list.arranged$Region[country.list.arranged$Region == "North Africa and Middle East"]        <- "Middle East and North Africa"
    country.list.arranged$Region[country.list.arranged$Region == "Southern and Tropical Latin America"] <- "Southern and Latin America"
    country.list.arranged <- country.list.arranged %>%
      arrange(match(Region, region_order), Country) %>%
      mutate(pos = seq(1:length(Country)))
    
    plot_data <- left_join(plot_data, country.list.arranged %>% select(Country,pos))
    
  } else if (ordertype == "level") {
    d_order <- plot_data %>%
      arrange(end)%>%
      mutate(pos = 1:length(Country)) %>%
      ungroup()
    
    plot_data <- left_join(plot_data, d_order)
  } else if (ordertype == "absolute_change"){
    order <- subset_order %>% 
      filter(age_group == my_age_group & start.year == plot.start.year & end.year == plot.end.year & sex == my_sex) %>%
      filter(variable == "prev_bmi_l185") %>%
      arrange(overall_change)
    
    plot_data <- plot_data %>% mutate(pos = match(Country, order$Country))
  }
  
  xlab <- get_var_longname(var)
  if(data_type == "level"){
    xaxislab <- paste0(xlab, " (%)")
  } else if (data_type == "velocity"){
    xaxislab <- str_to_sentence(paste0("Velocity of ", xlab, " (%)"))
  }
  
  year_longname <-  paste0(plot.start.year, "-",end.year)
  
  plot.title <- paste0(str_to_sentence(my_age_group), " ", my_sex, " ", year_longname)
  
  arrow_size <- (xmax-xmin)/200
  
  text_data <- plot_data %>%
    mutate(min = xmin,
           max = xmax,
           xpos_text = ifelse(pos%%2 == 0, ifelse(end > start, end + 1, start + 1), ifelse(start< end, start-1, end-1)),
           x = ifelse(pos %%2 == 0, xlims[2] - offset  + 2, xlims[1] + offset  - 2),
           colour = "grey50") %>%
    arrange(pos)
  
  # Add number of data points to country
  # if(studyNumbers){
  #   text_data <- text_data %>%  mutate(Country = ifelse(pos%%2 == 1, paste0(Country," ", N_all, " (", N_natl, ")"),
  #                                                       paste0(N_all, " (", N_natl, ") ", Country)))
  #  }
  if(studyNumbers){
      text_data <- text_data %>%  mutate(Country = paste0(Country," (", N_all, ",", N_natl, ")"))
    }

  colour_data <- data.frame(x = seq(0, 99.9, 0.1),
                            xend = seq(0.1, 100, 0.2),
                            y = rep(-2, 1000), 
                            yend = rep(-1, 1000)) %>%
    mutate(mid_x = (x + xend)/2)
  
  if ((var == "prev_bmi_30_proportion_double_burden" | var == "prev_bmi_2sd_proportion_double_burden") & colour_by == "highlight"){
    plot_data <- plot_data %>% mutate(color_var = ifelse(end > 50 & start < 50, "highlight", "no_highlight"))
    # print(nrow(plot_data))
    # print(length(which(plot_data$color_var == "highlight")))
  } 
  
  if (colour_by == "Region"){
    p <- ggplot(plot_data, aes(y = pos, x = start, yend = pos, xend = end, color = Region)) +
      geom_vline(xintercept = 50, colour = "grey") +
      geom_segment(linetype = "solid", arrow=arrow(length = unit(0.2,"cm")),linewidth=0.9, alpha = 0.9) +
      scale_colour_manual(values = region_col)
    
  } else if (colour_by == "level"){
    p <- ggplot(plot_data, aes(y = pos, x = start, yend = pos, xend = end, color = end)) +
      geom_vline(xintercept = 50, colour = "grey") +
      geom_segment(linetype = "solid", arrow=arrow(length = unit(0.18,"cm")),linewidth=0.9, alpha = 0.9) +
      col_scale_proportion_double_burden
  } else if (colour_by == "none"){
  p <- ggplot(plot_data, aes(y = pos, x = start, yend = pos, xend = end)) 
  
  if(var == "prev_bmi_30_proportion_double_burden" | var == "prev_bmi_2sd_proportion_double_burden"){
    p <- p + geom_vline(xintercept = 50, colour = "grey")
  }
  p <- p + geom_segment(linetype = "solid", arrow=arrow(length = unit(0.18,"cm")),linewidth=0.9,  alpha = 0.9, colour = grey(0.4)) 
  
  }else if (colour_by == "highlight"){
    p <- ggplot(plot_data, aes(y = pos, x = start, yend = pos, xend = end, color = color_var))
  
    if(var == "prev_bmi_30_proportion_double_burden" | var == "prev_bmi_2sd_proportion_double_burden"){
      p <- p + geom_vline(xintercept = 50, colour = "grey")
    }
    p <- p+ geom_segment(linetype = "solid", arrow=arrow(length = unit(0.18,"cm")),linewidth=0.9, alpha = 0.9) +
  scale_colour_manual(values = c("no_highlight" = grey(0.8), "highlight" = grey(0.4)))
  }
   
  p <- p +
    xlim(xlims) +
    xlab(xaxislab) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_line(linewidth =0.5,colour = "grey"),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank()) +
  
    scale_x_continuous(breaks = seq(xticklims[1], xticklims[2], xtick), limits = xlims, expand = c(0,0)) +
    scale_y_continuous(limits = c(0,201),expand = c(0,0)) +
    
    geom_text(data = text_data %>% filter(pos%%2 == 1), aes(y = pos, x = x, label = Country),
              size =  3.2,
              colour = (text_data %>% filter(pos%%2 == 1))$colour,
              angle = 0,
              vjust = 0.5,
              hjust = 1)  +
    
    geom_text(data = text_data %>% filter(pos%%2 == 0), aes(y = pos, x = x, label = Country),
              size =  3.2,
              colour = (text_data %>% filter(pos%%2 == 0))$colour,
              angle = 0,
              vjust = 0.5,
              hjust = 0)  +
    
    geom_segment(data = text_data, aes(y = pos, x = x , yend = pos, xend = xpos_text), inherit.aes = F, colour = grey(0.85),alpha = 1,linewidth = 0.001)+
    theme(legend.position = "none")
    
  if (colour_by == "level"){
    p <- p + 
    geom_rect(data = colour_data, aes(xmin = x, xmax = xend, ymin = y, ymax = yend, fill = mid_x), inherit.aes = F) 
    fill_col_scale_proportion_double_burden 
  }
  
  if (var == "prev_bmi_35" & colour_by == "highlight"){
    p <- p + 
      geom_point(data = plot_data, aes(y = pos, x = start_obesity),  alpha = 0.6, shape = 16, size = 3) 
  }
  
  return(p)
}



timechange_arrow_rotated <- function(subset, subset_order = NULL, var, my_sex, my_age_group, start.year, end.year, ordertype = "level", data_type = "level", colour_by = "Region", offset = 32, studyNumbers = F){
  # gives interactive time change arrow
  # order takes country_order (ordered by country, coloured by region), or level (ordered by level 1990, colored by magnitude of change)
  
  plot_data <- subset %>% 
    filter(age_group == my_age_group) %>%
    filter(variable == var) %>%
    filter(year == start.year | year == end.year) %>%
    mutate(year = ifelse(year ==start.year, "start", "end")) %>%
    select(-c(l,u,se))
  
  
  ymax <- ceiling(max(plot_data$mean/10))*10
  if(data_type == "level"){
    ymin <- 0
  } else if (data_type == "velocity"){
    ymin <- floor(min(plot_data$mean/10))*10
  }
  
  ylims <- c(ymin - offset, ymax + offset)
  ytick <- 25
  yticklims <- c(max(ceiling(ymin/ytick)*ytick, 0), min(100, floor(ymax/ytick)*ytick))
  
  plot_data <- plot_data %>%
    filter(sex == my_sex) %>%
    pivot_wider(names_from = year, values_from = mean)
  
  if (ordertype == "country") {
    country.list.arranged <- countrylist
    country.list.arranged$Region[country.list.arranged$Region == "North Africa and Middle East"]        <- "Middle East and North Africa"
    country.list.arranged$Region[country.list.arranged$Region == "Southern and Tropical Latin America"] <- "Southern and Latin America"
    country.list.arranged <- country.list.arranged %>%
      arrange(match(Region, region_order), Country) %>%
      mutate(pos = seq(1:length(Country)))
    
    plot_data <- left_join(plot_data, country.list.arranged %>% select(Country,pos))
    
  } else if (ordertype == "level") {
    d_order <- plot_data %>%
      arrange(end)%>%
      mutate(pos = 1:length(Country)) %>%
      ungroup()
    
    plot_data <- left_join(plot_data, d_order)
  } else if (ordertype == "absolute_change"){
    order <- subset_order %>% 
      filter(age_group == my_age_group & start.year == plot.start.year & end.year == plot.end.year & sex == my_sex) %>%
      filter(variable == "prev_bmi_l185") %>%
      arrange(overall_change)
    
    plot_data <- plot_data %>% mutate(pos = match(Country, order$Country))
  }
  
  ylab <- get_var_longname(var)
  if(data_type == "level"){
    yaxislab <- paste0(ylab, " (%)")
  } else if (data_type == "velocity"){
    yaxislab <- str_to_sentence(paste0("Velocity of ", ylab, " (%)"))
  }
  
  year_longname <-  paste0(plot.start.year, "-",end.year)
  
  plot.title <- paste0(str_to_sentence(my_age_group), " ", my_sex, " ", year_longname)
  
  arrow_size <- (ymax-ymin)/200
  
  text_data <- plot_data %>%
    mutate(min = ymin,
           max = ymax,
           ypos_text = ifelse(pos%%2 == 0, ifelse(end > start, end + 1, start + 1), ifelse(start< end, start-1, end-1)),
           y = ifelse(pos %%2 == 0, ylims[2] - offset  + 2, ylims[1] + offset  - 2),
           colour = "grey50") %>%
    arrange(pos)
  
  if(studyNumbers){
    text_data <- text_data %>%  mutate(Country = paste0(Country," (", N_all, ",", N_natl, ")"))
  }
  
  colour_data <- data.frame(y = seq(0, 99.9, 0.1),
                            yend = seq(0.1, 100, 0.2),
                            x = rep(-2, 1000), 
                            xend = rep(-1, 1000)) %>%
    mutate(mid_y = (y + yend)/2)
  
  if ((var == "prev_bmi_30_proportion_double_burden" | var == "prev_bmi_2sd_proportion_double_burden") & colour_by == "highlight"){
    plot_data <- plot_data %>% mutate(color_var = ifelse(end > 50 & start < 50, "highlight", "no_highlight"))
    # print(nrow(plot_data))
    # print(length(which(plot_data$color_var == "highlight")))
  } 
  
  if (colour_by == "Region"){
    p <- ggplot(plot_data, aes(x = pos, y = start, xend = pos, yend = end, color = Region)) +
      geom_vline(yintercept = 50, colour = "grey") +
      geom_segment(linetype = "solid", arrow=arrow(length = unit(0.2,"cm")),linewidth=0.9, alpha = 0.9) +
      scale_colour_manual(values = region_col)
    
  } else if (colour_by == "level"){
    p <- ggplot(plot_data, aes(x = pos, y = start, xend = pos, yend = end, color = end)) +
      geom_vline(yintercept = 50, colour = "grey") +
      geom_segment(linetype = "solid", arrow=arrow(length = unit(0.18,"cm")),linewidth=0.9, alpha = 0.9) +
      col_scale_proportion_double_burden
  } else if (colour_by == "none"){
    p <- ggplot(plot_data, aes(x = pos, y = start, xend = pos, yend = end)) 
    
    if(var == "prev_bmi_30_proportion_double_burden" | var == "prev_bmi_2sd_proportion_double_burden"){
      p <- p + geom_vline(xintercept = 50, colour = "grey")
    }
    p <- p + geom_segment(linetype = "solid", arrow=arrow(length = unit(0.18,"cm")),linewidth=0.9,  alpha = 0.9, colour = grey(0.4)) 
    
  }else if (colour_by == "highlight"){
    p <- ggplot(plot_data, aes(x = pos, y = start, xend = pos, yend = end, color = color_var))
    
    if(var == "prev_bmi_30_proportion_double_burden" | var == "prev_bmi_2sd_proportion_double_burden"){
      p <- p + geom_hline(yintercept = 50, colour = "grey")
    }
    p <- p+ geom_segment(linetype = "solid", arrow=arrow(length = unit(0.18,"cm")),linewidth=0.9, alpha = 0.9) +
      scale_colour_manual(values = c("no_highlight" = grey(0.8), "highlight" = grey(0.4)))
  }
  
  p <- p +
    ylim(ylims) +
    ylab(yaxislab) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.y = element_line(linewidth =0.5,colour = "grey"),
          axis.ticks.x = element_blank(),
          panel.grid = element_blank()) +
    
    scale_y_continuous(breaks = seq(yticklims[1], yticklims[2], ytick), limits = ylims, expand = c(0,0)) +
    scale_x_continuous(limits = c(0,201),expand = expansion(add = 10)) +
    
    geom_text(data = text_data %>% filter(pos%%2 == 1), aes(x = pos, y = y, label = Country),
              size =  3.2,
              colour = (text_data %>% filter(pos%%2 == 1))$colour,
              angle = -45,
              vjust = 0.5,
              hjust = 0)  +
    
    geom_text(data = text_data %>% filter(pos%%2 == 0), aes(x = pos, y = y, label = Country),
              size =  3.2,
              colour = (text_data %>% filter(pos%%2 == 0))$colour,
              angle = -45,
              vjust = 0.5,
              hjust = 1)  +
    
    geom_segment(data = text_data, aes(x = pos, y = y , xend = pos, yend = ypos_text), inherit.aes = F, colour = grey(0.85),alpha = 1,linewidth = 0.001)+
    theme(legend.position = "none")
  
  if (colour_by == "level"){
    p <- p + 
      geom_rect(data = colour_data, aes(ymin = y, ymax = yend, xmin = x, xmax = xend, fill = mid_y), inherit.aes = F) 
    fill_col_scale_proportion_double_burden 
  }
  
  if (var == "prev_bmi_35" & colour_by == "highlight"){
    p <- p + 
      geom_point(data = plot_data, aes(x = pos, y = start_obesity),  alpha = 0.6, shape = 16, size = 3) 
  }
  
  return(p)
}
