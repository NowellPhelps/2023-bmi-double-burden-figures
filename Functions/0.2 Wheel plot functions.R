### Adapted for double burden of obese and obesity by NP November 2022 
circular_bar_function_all <- function(subset, my_sex, my_year, showLegend = FALSE, order = "default", obesity_bottom = T, studyNumbers = F) {
  ######### SUBSET AND RESHAPE DATA FOR PLOTTING ########
  plot_data <- subset %>% filter(year == my_year & sex == my_sex)
  
  if(order == "default"){
    d_order <- plot_data %>%
      group_by(year) %>%
      subset(variable == variables[1]) %>%
      arrange(match(Region, region_order), Country)
  } else if (order == "obesity"){
    obesity <- plot_data %>%
      filter(variable %in% c("prev_bmi_30_35", "prev_bmi_35_40","prev_bmi_40")) %>%
      group_by(Country, year, age_group, sex) %>%
      summarise(obesity = sum(mean)) %>%
      ungroup()
    
    d_order <- left_join(plot_data, obesity) %>%
      group_by(year) %>%
      subset(variable == variables[1]) %>%
      arrange(Superregion, Region, desc(obesity))
  } else if (order == "double burden"){
    double_burden <- plot_data %>%
      filter(variable %in% c("prev_bmi_30_35", "prev_bmi_35_40","prev_bmi_40", "prev_bmi_l185")) %>%
      group_by(Country, year, age_group, sex) %>%
      summarise(double_burden = sum(mean)) %>%
      ungroup()
    
    d_order <- left_join(plot_data, double_burden) %>%
      group_by(year) %>%
      subset(variable == variables[1]) %>%
      arrange(Superregion, Region, desc(double_burden))
  }
  
  if (!(obesity_bottom)){
    plot_data$variable <- factor(plot_data$variable, levels = rev(levels(plot_data$variable)))
  }

  
  country_order <- d_order$Country
  country_cols  <- region_col[match(d_order$Region, names(region_col))]
  
  plot_data$pos <- match(plot_data$Country, country_order)
  
  if(studyNumbers){
    text_data <- data.frame(Country = d_order$Country, N_natl = d_order$N_natl, N_all = d_order$N_all, pos = 1:200) %>%
      mutate(Country = paste0(Country, " (", N_all, ", ", N_natl,")"))
    
  } else{
    text_data <- data.frame(Country = country_order, pos = 1:200)
  }
  
  rotate_angles = c(seq(90, -90, length = 103)[-c(1,2,102,103)], seq(90, -90, length = 103)[-c(2,103)])
  hjusts = c(rep(0, 99),rep(1,101))
  
  xintv <- 20
  
  
  max_measure <- 100.1
  xlims       <- c(0, max_measure)
  xTicks      <- seq(0,max_measure,xintv)
  
  # generate indicator for drawing a coloured ring as legends
  Region_breaks <- which(d_order$Region[-1] != d_order$Region[-nrow(d_order)])
  
  region_ring <- data.frame(start = c(1, Region_breaks+1), end = c(Region_breaks, 200), Region = unique(d_order$Region), pos = 0, mean = 0)
  region_ring$leg_pos1 <- max_measure
  region_ring$leg_pos2 <- max_measure * 1.08
  region_col2 <- region_col
  names(region_col2) <- names(region_col)
  region_col2 <- region_col2[match(unique(d_order$Region),names(region_col2))]
  
  horizontal_lines <- data.frame(mean = xTicks[xTicks < max_measure]) %>%
    mutate(label = paste0(mean)) %>%
    rbind(., data.frame(mean = ceiling(max_measure/10)*10,
                        label = "(%)"))
  
  vlines <- data.frame(loc = 1:200, yend = max_measure)
  legInd <- str_to_title(my_year)
  
  text_data$text_pos   <- max_measure * 1.01
  text_data$text_pos2  <- max_measure * 1.01
  text_data$low_pos  <- 0
  
  p <- ggplot(plot_data, aes(pos, y = mean)) +
    theme_tufte() + coord_polar(clip = "off") +
    geom_segment(data = vlines, aes(x = loc, xend = loc, y = 0, yend = yend), colour=grey(0.95), linewidth=0.05) +
    fill_scale +
    geom_bar(aes(fill = variable), colour = grey(1), linewidth = 0.1, stat = "identity", position = "stack", width = 1) +
    
    geom_blank(data = text_data[1,], aes(y = -text_pos2*0.5)) +
    
    geom_text(data = text_data, aes(y = text_pos, label = Country),
              size = 3.6,
              colour = country_cols,
              angle = rotate_angles,
              hjust = hjusts) +
    
    geom_blank(data = text_data[1,], aes(y = text_pos2)) +
    
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          text = element_text(family = "sans"),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) +
    
    xlim(c(-1.5, 202.5)) + ggtitle(label=legInd) +  theme(plot.title = element_text(hjust = 0.5,face='bold')) +
    
    geom_text(data = horizontal_lines %>% subset(mean!=0 ), aes(x = -1.5, y =mean, label = label, hjust = 0.5)) +
    
    geom_segment(data = horizontal_lines %>% subset(mean < max_measure), aes(x = 0.5, xend = 200, y = mean, yend = mean),
                 colour = grey(0.85), linewidth = 0.05) 
    
  if (showLegend == FALSE){
    p <- p + theme(legend.position = "None")
  } else{
      p <- p + guides(fill=guide_legend(title="BMI category \n(kg/m\u00B2)")) +
        theme(legend.position = c(0,.9))
  }
  return(p)
}


circular_bar_function <- function(subset, my_sex, my_year, type, showLegend = FALSE, order_level = "region", order_by = "prev_bmi_30", composition = "absolute", studyNumbers = F, appendix = F) {
  # composition takes "absolute" or "relative"
  # order_level takes "region" or "all"
  # order_by takes "alphabetic" or "prev_bmi_30" (works with "absolute" composition only) or "prev_bmi_35" or "prev_bmi_40"
  
  
  ######### SUBSET AND RESHAPE DATA FOR PLOTTING ########
  plot_data <- subset %>% filter(year == my_year & sex == my_sex)
  
  if(my_age_group == "ageStd"){
    plot_data <- plot_data %>% filter(age_group == "ageStd")
  }
  
  if(type == "obesity"){
    text_size <- 3.2
  } else{
    text_size <- 3.6
  }
  
  ### DERIVE COUNTRY ORDER
  
  d_order <- plot_data %>% select(-c(l,u,se)) %>% pivot_wider(names_from = variable, values_from = mean)
  
  if (order_by == "prev_bmi_30" & type == "double burden"){
    d_order <- d_order %>% mutate(order_var = prev_bmi_30)
    
  } else if (order_by == "prev_bmi_30" & type == "obesity"){
    d_order <- d_order %>% mutate(order_var = prev_bmi_30_35 + prev_bmi_35_40 + prev_bmi_40)
    
  } else if (order_by == "prev_bmi_35"){
    d_order <- d_order %>% mutate(order_var = prev_bmi_35_40 + prev_bmi_40)
    
  } else if (order_by == "prev_bmi_40"){
    d_order <- d_order %>% mutate(order_var = prev_bmi_40)
    
  } else if (order_by == "double_burden" & type == "double burden"){
    d_order <- d_order %>% mutate(order_var = prev_bmi_l185 + prev_bmi_30)
    
  } else if (order_by == "double_burden" & type == "ado double burden"){
    d_order <- d_order %>% mutate(order_var = prev_bmi_neg2sd + prev_bmi_2sd)
    
  } else if (order_by == "prev_bmi_l185"){
    d_order <- d_order %>% mutate(order_var = prev_bmi_l185)
    
  } else if (order_by == "prev_bmi_neg2sd"){
    d_order <- d_order %>% mutate(order_var = prev_bmi_neg2sd)
    
  } else if (order_by == "prev_bmi_2sd"){
    d_order <- d_order %>% mutate(order_var = prev_bmi_2sd)
  }
  
  if(order_level == "all"){
    d_order <- d_order %>%
      arrange(desc(order_var))
  } else if(order_level == "region"){
    d_order <- d_order %>%
      arrange(match(Region, region_order), desc(order_var))
  }
  
  country_order <- d_order$Country
  country_cols  <- region_col[match(d_order$Region, names(region_col))]
  
  plot_data$pos <- match(plot_data$Country, country_order)
  
  if(studyNumbers){
      text_data <- data.frame(Country = d_order$Country, N_natl = d_order$N_natl, N_all = d_order$N_all, pos = 1:200) %>%
        mutate(Country = paste0(Country, " (", N_all, ", ", N_natl,")"))

    } else{
    text_data <- data.frame(Country = country_order, pos = 1:200)
  }
  
  rotate_angles = c(seq(90, -90, length = 103)[-c(1,2,102,103)], seq(90, -90, length = 103)[-c(2,103)])
  hjusts = c(rep(0, 99),rep(1,101))
  xintv <- 0.05
  
  if (composition == "absolute" & (type == "double burden" | type == "obesity")){
    if(appendix){
      max_measure <- 87
    } else{
      max_measure <- 87
    }
    
    
  } else if (composition == "absolute" & (type == "ado double burden")){
    max_measure <- 51
    
  } else if (composition == "relative"){
    max_measure <- 100.001
  }
  
  xlims       <- c(0,   max_measure)
  xTicks      <- seq(0, max_measure, 10)
  
  # generate indicator for drawing a coloured ring as legends
  Region_breaks <- which(d_order$Region[-1] != d_order$Region[-nrow(d_order)])
  
  
  if (order_level == "region"){
    region_ring <- data.frame(start = c(1, Region_breaks+1), end = c(Region_breaks +1, 200), Region = unique(d_order$Region), pos = 0, mean = 0)
    region_ring$leg_pos1 <- max_measure
    region_ring$leg_pos2 <- max_measure * 1.08
    
    region_col2 <- region_col
    names(region_col2) <- names(region_col)
    region_col2 <- region_col2[match(unique(d_order$Region),names(region_col2))]
    
  }
  
  horizontal_lines <- data.frame(mean = xTicks[xTicks < max_measure]) %>%
    mutate(label = paste0(mean)) %>%
    rbind(., data.frame(mean = ceiling(max_measure/10)*10,
                        label = "(%)"))
  
  
  vlines <- data.frame(loc = 1:200, yend = max_measure)
  
  title <- ""
  legInd <- str_to_title(my_year)
  
  text_data$text_pos   <- max_measure * 1.08
  text_data$text_pos2  <- max_measure * 1.08
  text_data$low_pos  <- 0
  
  p <- ggplot(plot_data, aes(x = pos, y = mean)) +
    theme_tufte() + coord_polar(clip = "off") +
    geom_segment(data= vlines, aes(x = loc, xend = loc, y = 0, yend = yend), colour=grey(0.95), linewidth=0.05) +
    fill_scale +
    geom_bar(aes(fill = variable), colour = grey(1), linewidth = 0.1, stat = "identity", position = "stack", width = 1) +
    geom_blank(data = text_data[1,], aes(y = -text_pos2*0.5)) +
    geom_text(data = text_data, aes(y = text_pos, label = Country),
              size = text_size,
              colour = country_cols,
              angle = rotate_angles,
              hjust = hjusts) +
    geom_blank(data= text_data[1,], aes(y = text_pos2)) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          text = element_text(family = "sans"),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) +
    xlim(c(-1.5, 202.5)) + ggtitle(label=legInd) +  theme(plot.title = element_text(hjust = 0.5,face='bold')) +
    geom_text(data = horizontal_lines %>% subset(mean!=0), aes(x = -1.5, y = mean, label = label, hjust = 0.5), size = 3.5) +
    geom_segment(data = horizontal_lines %>% subset(mean!=0 & mean < max_measure), aes(x = 0.5, xend = 200, y = mean, yend = mean),
                 colour = grey(0.85), linewidth = 0.05) 
  
  if (order_level == "region"){
    p <- p +  
      geom_rect(data = region_ring, aes(xmin = start- 0.5, xmax = end + 0.5, ymin = leg_pos2, ymax = leg_pos1, fill = Region), colour = "white", fill = region_col2, linewidth = 0.5)
      #geom_segment(data= vlines %>% filter(loc %in% Region_breaks), aes(x = loc + 0.5, xend = loc+ 0.5, y = 0, yend = max_measure * 1.08), colour=grey(0), linewidth=0.5)
  }
  
  if (showLegend == FALSE){
    p <- p + theme(legend.position = "None")
  } else{
    p <- p + theme(legend.position = c(0,.9))
    
    if (type == "obesity"){
      p <- p + guides(fill=guide_legend(title="BMI (kg/m\u00B2)")) +
        theme(legend.position = c(0,.9))
    }else{
      p <- p + theme(legend.title = element_blank())
    }
  }
  return(p)
}

bar_function_region <- function(subset, my_sex, my_year, type, showLegend = FALSE, composition = "absolute") {
  
  ######### SUBSET AND RESHAPE DATA FOR PLOTTING ########
  plot_data <- subset %>% filter(sex == my_sex) %>% filter(year== my_year) 
  
  if(my_age_group == "ageStd"){
    plot_data <- plot_data %>% filter(age_group == "ageStd")
  }
  
  ### DERIVE COUNTRY ORDER
  plot_data$pos <- match(plot_data$Region, region_order)
  
  text_data <- data.frame(Region = region_order, pos = 1:20)
  
  rotate_angles = rep(seq(90, -90, length = 10), 2)
  hjusts = c(rep(0, 10),rep(1,10))
  xintv <- 0.05
  
  if (composition == "absolute" & (type == "double burden" | type == "obesity")){
    max_measure <- 62
    
  } else if (composition == "absolute" & (type == "ado double burden")){
    max_measure <- 51
    
  } else if (composition == "relative"){
    max_measure <- 100.001
  }
  
  xlims       <- c(0,   max_measure)
  xTicks      <- seq(0, max_measure, 10)
  
  horizontal_lines <- data.frame(mean = xTicks[xTicks < max_measure]) %>%
    mutate(label = paste0(mean)) %>%
    rbind(., data.frame(mean = ceiling(max_measure/10)*10,
                        label = "(%)"))
  
  vlines <- data.frame(loc = 1:20, yend = max_measure)
  
  title <- str(my_year)
  # 
  text_data$text_pos   <- max_measure * 1.08
  text_data$text_pos2  <- max_measure * 1.08
  text_data$low_pos    <- 0
  
  if(age_type == "adult"){
    ybreaks <- c(0, 20, 40, 60)
    ylims <- c(-45, 60)
  } else{
    ybreaks <- c(0, 10, 20, 30, 40, 50)
    ylims <- c(-20, 50)
  }
  

  p <- ggplot(plot_data, aes(x = pos, y = mean)) +
    theme_tufte() +
    geom_segment(data= vlines, aes(x = loc, xend = loc, y = 0, yend = yend), colour=grey(0.95), linewidth=0.05) +
    geom_segment(data = horizontal_lines %>% subset(mean!=0 & mean < max_measure), aes(x = 0.5, xend = 20.5, y = mean, yend = mean),
                 colour = grey(0.85), linewidth = 0.05) +
    fill_scale +
    geom_bar(aes(fill = variable), colour = grey(1), linewidth = 0.1, stat = "identity", position = "stack", width = 1) +
    geom_blank(data = text_data[1,], aes(y = -text_pos2*0.5)) +
    geom_text(data = text_data, aes(y = -1, label = Region),
              size = 3.6,
              colour = region_col[order(match(names(region_col),region_order))],
              angle = -40,
              hjust = 0) +
    geom_blank(data= text_data[1,], aes(y = text_pos2)) +
    
    scale_y_continuous(breaks = ybreaks, limits = ylims) +
    scale_x_continuous(limits = c(0.5,22), expand = c(0,0)) +
    ylab("Prevalence (%)")+
   theme_bw() +
    
    theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(), axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.y = element_text(size = 12)) +
    
    theme(legend.title = element_blank())
  

  if (showLegend == FALSE){
    p <- p + theme(legend.position = "None")
  } else{
    p <- p + theme(legend.position = c(0,.9))
    
  }
  return(p)
}
