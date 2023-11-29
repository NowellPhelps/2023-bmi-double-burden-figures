
static_scatter_change_v_change <- function(subset, xvar, yvar, my_sex, my_age_group, plot.start.year, plot.end.year, xintv = 10, yintv = 10, returnLeg = F){
  # static scatter of timechange in two variables against each other
  # coloured by Superregion
  
  plot_data <- subset %>%
    filter(start.year == plot.start.year & end.year == plot.end.year) 
  
  if(my_age_group == "ageStd"){
    plot_data <- plot_data %>% filter(age_group == "ageStd")
  }
  
  ## Find max and minimum values for axes
  
  #xmax <- ceiling(max(plot_data$mean[which(plot_data$variable == xvar)])/xintv)*xintv
  #xmin <- floor(min(plot_data$mean[which(plot_data$variable == xvar)])/xintv)*xintv
  
  if(!(appendix)){
    xmin <- min(c(-50, floor(min(plot_data$mean[which(plot_data$variable == xvar)])/xintv)*xintv))
    xmax <- max(c(20, ceiling(max(plot_data$mean[which(plot_data$variable == xvar)])/xintv)*xintv))
  } else{
    xmin <- min(c(-50, floor(min(plot_data$mean[which(plot_data$variable == xvar)])/xintv)*xintv))
    xmax <- max(c(30, ceiling(max(plot_data$mean[which(plot_data$variable == xvar)])/xintv)*xintv))
  }
  
  

  ymin <- floor(min(plot_data$mean[which(plot_data$variable == yvar)])/yintv)*yintv
  ymax <- max(c(40, ceiling(max(plot_data$mean[which(plot_data$variable == yvar)])/yintv)*yintv))
  
  xticks <- seq(xmin, xmax, by = xintv)
  yticks <- seq(ymin, ymax, by = yintv)
  
  plot_data <- plot_data %>%
    subset(sex == my_sex) %>%
    filter(age_group == my_age_group) %>%
    select(c(Country, Superregion, age_group, start.year, end.year, variable, iso, Region, mean, l , u)) %>%
    mutate(variable = if_else(variable == xvar, "xvar", if_else(variable == yvar, "yvar", variable))) %>%
    pivot_wider(names_from = variable, values_from = c(mean, l, u))
  
  ########## PLOT  #############################################################
  xlab     <-  paste0("Change in ", get_var_longname(xvar)," ", plot.start.year, " - ", plot.end.year, "\n(percentage points)")
  ylab     <-  paste0("Change in ", get_var_longname(yvar)," ", plot.start.year, " - ", plot.end.year, "\n(percentage points)")
  
  xaxislab <- str_to_sentence(paste0(xlab, " ", plot.start.year, "-", plot.end.year, " (%)"))
  yaxislab <- str_to_sentence(paste0(ylab, " ", plot.start.year, "-", plot.end.year, " (%)"))
  
  plot.title <- paste0(str_to_sentence(my_age_group), " ", my_sex, " ",plot.start.year, "-", plot.end.year)
  linecol <- "grey50"
  
  if (age_type == "adult"){
    arrow_data <- data.frame(x = c(-32.5,-31.5), xend = c(-33.5, -30.5), y = c(31.5, 32.5), yend = c(30.5, 33.5))
    text_data  <- data.frame(label = c("Decrease in\ncombined prevalence", "Increase in\ncombined prevalence"),
                             x = c(-35, -29),
                             y = c(29, 35))
  } else {
    arrow_data <- data.frame(x = c(-32.5,-31.5), xend = c(-33.5, -30.5), y = c(31.5, 32.5), yend = c(30.5, 33.5))
    text_data  <- data.frame(label = c("Decrease in\ncombined prevalence", "Increase in\ncombined prevalence"),
                             x = c(-35, -29),
                             y = c(29, 35))
  }
  
  
  p <- ggplot(plot_data, aes(x = mean_xvar, y = mean_yvar)) +
    geom_hline(yintercept = 0, color = linecol, linetype = "solid") +
    geom_vline(xintercept = 0,color =  linecol, linetype = "solid") + 
    geom_abline(slope= -1, intercept = 0, color = linecol, linetype = "solid") + 
    geom_abline(slope= -1, intercept = 10,  color  = linecol, linetype = "dashed")  +
    geom_abline(slope= -1, intercept = 20,  color  = linecol, linetype = "dashed")  +
    geom_abline(slope= -1, intercept = 30,  color  = linecol, linetype = "dashed")  +
    geom_abline(slope= -1, intercept = 40, color = linecol, linetype = "dashed") +
    geom_abline(slope= -1, intercept = 50, color = linecol, linetype = "dashed") +
    geom_abline(slope= -1, intercept = -10, color = linecol, linetype = "dashed") +
    geom_abline(slope= -1, intercept = -20, color = linecol, linetype = "dashed") +
    geom_abline(slope= -1, intercept = -30, color = linecol, linetype = "dashed") +
    geom_abline(slope= -1, intercept = -40, color = linecol, linetype = "dashed") +
    geom_point(data = plot_data, aes(colour = Superregion), alpha = 0.8, size = 2) +
    scale_colour_manual(values = sregion_col)+
    xlab(xlab) + 
    ylab(ylab) +
    theme(panel.grid = element_blank())+
    theme_classic() + 
    scale_x_continuous(limits = c(xmin, xmax), breaks = xticks) +
    scale_y_continuous(limits = c(ymin, ymax), breaks = yticks) +
    theme(axis.title = element_text(size = 14)) +
    coord_equal()
    
    
  if(my_sex == "male"){
    p <- p + ylab("") 
  } else {
    p <- p +
      geom_segment(data = arrow_data, aes(x = x, y = y, xend = xend, yend = yend),
                   linetype = "solid",
                   colour = linecol,
                   linewidth = 1,
                   arrow=arrow(length = unit(0.2,"cm"))) +
      geom_text(data = text_data, mapping = aes(x = x, y = y, label = label,), 
                color='darkgray',size=ifelse(appendix,3,4),hjust=0.5, vjust=0.5, angle = -45, lineheight = 1)
  }

  if(returnLeg){
    p <- p + theme(legend.box = "horizontal")
    p <- get_legend(p)
  } else {
    p <- p + theme(legend.position = "none")
  }

  return(p)
  
}


static_scatter_timechange_v_level <- function(subset, var, my_sex, my_age_group, plot.start.year, plot.end.year, level.year, returnLeg = F){
  # static scatter of timechange from plot.start.year to plot.end.year against level in level.year 
  
  
  
  plot_data <- subset %>%
    filter(variable == var) %>%
    filter(start.year == plot.start.year & end.year == plot.end.year) %>%
    filter(year == level.year)
  
  if(my_age_group == "ageStd"){
    plot_data <- plot_data %>% filter(age_group == "ageStd")
  }
  
  
  xmax <- ceiling(max(plot_data$mean)/10)*10
  xmin <- floor(min(plot_data$mean)/10)*10
  ymax <- ceiling(max(plot_data$change)/10)*10
  ymin <- floor(min(plot_data$change)/10)*10
  
  if(var == "prev_bmi_l185"){
    xlims <- c(xmin,xmax)
    ylims <- c(min(c(ymin,-xmax)),ymax)
    
  } else{
    xlims <- c(xmin, xmax)
    ylims <- c(ymin, ymax)
  }
  
  plot_data <- plot_data %>% filter(sex == my_sex, age_group == my_age_group)
  
  xlab     <-  paste0(str_to_sentence(get_var_longname(var)), " in ", level.year)
  ylab     <-  paste0("Change in ", get_var_longname(var))
  
  xaxislab <- xlab
  yaxislab <- paste0(ylab, " ", plot.start.year, "-", plot.end.year, "\n(percentage points)")

  
  
  
  plot.title <- ""
  
  if(var == "prev_bmi_l185"){
    dat_text <- data.frame(label = c("Increase in\nunderweight", "Decrease in\nunderweight"),
                           x = c(xlims[2], xlims[2]),
                           y = c(ylims[2]*0.9, -ylims[2]*0.9))
    
  } else if(var == "prev_bmi_30"){
    dat_text <- data.frame(label = c("Obesity more than\ndoubled", "Obesity increased but\ndid not double", "Decrease in\nobesity"),
                           x = c(15,     xlims[2], xlims[2]),
                           y = c(ylims[2], ylims[2], -5))
  } else if(var == "prev_bmi_neg2sd"){
    dat_text <- data.frame(label = c("Increase in\nthinness", "Decrease in\nthinness"),
                           x = c(xlims[2], xlims[2]),
                           y = c(ylims[2]*0.9, -ylims[2]*0.9))
  } else if(var == "prev_bmi_2sd"){
    dat_text <- data.frame(label = c("Obesity more than\ndoubled", "Obesity increased but\ndid not double",  "Decrease in\nobesity"),
                           x = c(xlims[2], xlims[2], xlims[2]),
                           y = c(ylims[2]*0.9, 10, -5))
  }

  p <- ggplot(plot_data, aes(x = mean, y = change, colour= Superregion)) +
    geom_point(alpha = 0.8, size = 2) +
    scale_colour_manual(values = sregion_col) +
    ggtitle(plot.title) +
    ylab(yaxislab) +
    xlab(xaxislab) +
    scale_x_continuous(limits = xlims) +
    scale_y_continuous(limits = ylims) +
    geom_hline(yintercept = 0, linetype = "solid", colour= "grey50") +
    geom_vline(xintercept = 0, linetype = "solid", colour= "grey50") + 
    theme_classic() +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(limits = c(xmin, xmax), labels = scales::percent_format(scale = 1)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    theme(axis.title = element_text(size = 14),
          legend.text = element_text(size = 12), 
          legend.title = element_text(size = 14)) + 
    new_scale_color() +
    geom_segment(aes(x = l_xvar, xend = u_xvar, y = mean_yvar, yend = mean_yvar, colour = Superregion), linewidth = 3, alpha = 0.3) +
    geom_segment(aes(y = l_yvar, yend = u_yvar, x = mean_xvar, xend = mean_xvar, colour = Superregion), linewidth = 3, alpha = 0.3) +
    scale_colour_manual(values = sregion_col)
    
  
  if(var == "prev_bmi_l185" | var == "prev_bmi_neg2sd"){
    p <- p +
      geom_abline(intercept = 0, slope = -1, linetype = "dashed", colour= "grey50")
  } else if (var == "prev_bmi_30"| var == "prev_bmi_2sd"){
    p <- p +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour= "grey50")
  }
  
  if(my_sex == "male"){
    p <- p + ylab("")
  } else{
    # add text
    if(var == "prev_bmi_l185" | var == "prev_bmi_neg2sd"){
      p <- p + 
        geom_text(data = dat_text[c(1),], mapping = aes(x = x, y = y, label = label,),color='darkgray',size=4,hjust=1, vjust=1) +
        geom_text(data = dat_text[c(2),], mapping = aes(x = x, y = y, label = label,),color='darkgray',size=4,hjust=1, vjust=0)
      
    } else if(var == "prev_bmi_30"){
      p <- p + 
        geom_text(data = dat_text[c(1),], mapping = aes(x = x, y = y, label = label,),color='darkgray',size=4, hjust=0, vjust=1) +
        geom_text(data = dat_text[c(2),], mapping = aes(x = x, y = y, label = label,),color='darkgray',size=4, hjust=1, vjust=1) +
        geom_text(data = dat_text[c(3),], mapping = aes(x = x, y = y, label = label,),color='darkgray',size=4, hjust=1, vjust=0.5) 
    } else if (var == "prev_bmi_2sd"){
      p <- p + 
        geom_text(data = dat_text[c(1),], mapping = aes(x = x, y = y, label = label,),color='darkgray',size=4, hjust=1, vjust=1) +
        geom_text(data = dat_text[c(2),], mapping = aes(x = x, y = y, label = label,),color='darkgray',size=4, hjust=1, vjust=1) +
        geom_text(data = dat_text[c(3),], mapping = aes(x = x, y = y, label = label,),color='darkgray',size=4, hjust=1, vjust=0.5) 
    }
  }
  
  if(returnLeg){
    p <- p + theme(legend.direction = "horizontal") + theme(legend.title = element_blank())
    
    p <- get_legend(p)
  } else{
    p <- p + theme(legend.position = "none")
  }
  
  
  return(p)
}

static_scatter_level_v_level <- function(subset, xvar, yvar, my_sex, my_age_group, plot.year,xintv = 10, yintv = 10, returnLeg = F){
  # static scatter of level in two variables against each other
  # coloured by Superregion
  
  plot_data <- subset %>%
    filter(variable == xvar | variable == yvar) 
  
  if(my_age_group == "ageStd"){
    plot_data <- plot_data %>% filter(age_group == "ageStd")
  }
  
  ## Find max and minimum values for axes
  xmin <- floor(min(plot_data$mean[which(plot_data$variable == xvar)])/xintv)*xintv
  xmax <- ceiling(max(plot_data$mean[which(plot_data$variable == xvar)])/xintv)*xintv
  xticks <- seq(xmin, xmax, by = xintv)
  
  ymin <- floor(min(plot_data$mean[which(plot_data$variable == yvar)])/yintv)*yintv
  ymax <- ceiling(max(plot_data$mean[which(plot_data$variable == yvar)])/yintv)*yintv
  yticks <- seq(ymin, ymax, by = yintv)
  
  plot_data <- plot_data %>%
    subset(sex == my_sex) %>%
    filter(year == plot.year) %>%
    filter(age_group == my_age_group) %>%
    select(c(Country, Superregion, age_group, year, variable, iso, Region, mean, l , u)) %>%
    mutate(variable = if_else(variable == xvar, "xvar", if_else(variable == yvar, "yvar", variable))) %>%
    pivot_wider(names_from = variable, values_from = c(mean, l, u))
  
  ########## PLOT  #############################################################
  xlab     <-  paste0("Age-standardised ", get_var_longname(xvar), " in\nchildren and adolescents")
  ylab     <-  paste0("Age-standardised ", get_var_longname(yvar), " in adults")
  
  xaxislab <- paste0(xlab, " (%)")
  yaxislab <- paste0(ylab, " (%)")
  
  plot.title <- paste0(ifelse(my_sex == "male", "Male", "Female"), ", ", plot.year)
  linecol <- "grey50"
  
  
  p <- ggplot(plot_data, aes(x = mean_xvar, y = mean_yvar)) +
    geom_point(size = 1.3, aes(color = Superregion)) +
    geom_hline(yintercept = 0, color = linecol, linetype = "solid") +
    geom_vline(xintercept = 0,color =  linecol, linetype = "solid") + 
    scale_colour_manual(values = sregion_col)+
    xlab(xaxislab) + 
    ylab(yaxislab) +
    ggtitle(plot.title) +
    theme(panel.grid = element_blank())+
    theme_classic() + 
    scale_x_continuous(limits = c(xmin, xmax), breaks = xticks) +
    scale_y_continuous(limits = c(ymin, ymax), breaks = yticks) +
    theme(axis.title = element_text(size = 11),
          legend.text = element_text(size = 10),
          title = element_text(size = 12, hjust = 0.5)) + 
    geom_smooth(method='lm', formula= y~x, se = FALSE,linetype="dashed", colour = "grey",fullrange=TRUE)
  
  
  if(returnLeg){
    p <- p + theme(legend.direction = "horizontal") + theme(legend.title = element_blank())
    p <- get_legend(p)
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
  
}

