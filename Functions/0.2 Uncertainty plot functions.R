library(tidyverse)
library(grid)
library(gridExtra)
library(tidyverse)
library(egg)
library(ggthemes)
library(scales)
library(ggpubr)


perviz_plot <- function(subset, my_sex, my_age_group, variable, plot.start.year, plot.end.year, change_type, returnLeg = F, yintv = 10){
  
  
  plot_data <- subset %>% 
    filter(start.year == plot.start.year & end.year == plot.end.year) 
  
  if(my_age_group == "ageStd"){
    plot_data <- plot_data %>% filter(age_group == "ageStd")
  }
  
  ymin   <- floor(min(plot_data$l)/yintv)   * yintv
  ymax   <- ceiling(max(plot_data$u)/yintv) * yintv
  ylims  <- c(ymin, ymax)
  yTicks <- seq(ymin, ymax, by = yintv)

  plot_data <- plot_data %>% 
    filter(sex == my_sex) %>%
    filter(age_group == my_age_group)
  
  
  if(variable == "prev_bmi_30" | variable == "prev_bmi_2sd"){
    var.longname <- "Obesity"
  } else if (variable == "prev_bmi_l185"){
    var.longname <- "Underweight"
  } else if (variable == "prev_bmi_neg2sd"){
    var.longname <- "Thinness"
  } else{
    var.longname <- get_var_longname(variable)
  }
  
  
  ylab         <- ifelse(change_type == "relative", 
                         paste0("Relative change ", plot.start.year, "-", plot.end.year, " (%)"), 
                         paste0("Change ", plot.start.year, "-", plot.end.year, " (percentage points)"))
  
  plot_data <- plot_data %>%
    arrange(match(Superregion, sregion_order), desc(mean)) %>%
    mutate(pos = match(Country, Country))
  
  p <- ggplot(plot_data, aes(x = pos, colour = Superregion))+
    geom_point(aes(y = mean), size = .1)+
    geom_linerange(aes(ymin = l, ymax = u), alpha = 0.6, linewidth = .1)+
    scale_colour_manual(values = sregion_col)+
    ggtitle(str_to_sentence(var.longname)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_bw() +
    scale_y_continuous(limits = ylims, breaks = yTicks) +
    geom_hline(yintercept = 0,linetype = "dashed")+
    ylab(ylab)+
    theme(axis.text.x = element_blank(), 
          axis.title.x = element_blank(),
          panel.grid = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.y = element_text(size = 12))
  
  if (returnLeg){
    p <- ggplot(plot_data, aes(x = pos, colour = Superregion)) + 
      geom_point(aes(y = mean), size = 3) +
      scale_colour_manual(values = sregion_col) +
      theme_bw() +
      theme(legend.position = "right",
            legend.title = element_blank(),
            legend.text = element_text(size = 10))
    p <- get_legend(p)
  } else{
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
}


funnel_plots <- function(data, variable, type = 'change', my_sex, plot.year = NULL, returnLeg = F, leg_direction = "vertical") {
  # type takes 'level' or 'change'
  
  sizes <- rep(1.5, 9)
  names(sizes) <- c(unique(data$Superregion))
  sizScale  <- scale_size_manual(name="", values=sizes)
  colScale  <- scale_colour_manual(name="", values=sregion_col) #values=c("World" = "black", sregion_col))
  fillScale  <- scale_fill_manual(name="", values= c(sregion_col, "hollow" = "white"))
  
  plot_data <- data 
  
  if(type == "level"){
    xlims <- c(0,100)
    xbreaks <- c(0, 25, 50, 75, 100)
    ylims <- c(max(plot_data$se)* 1.05, 0)
    
  } else{
    if(my_variable == "prev_bmi_30_proportion_double_burden" | my_variable == "prev_bmi_2sd_proportion_double_burden"){
      plot_data <- plot_data %>% filter(variable == my_variable)
      xtick <- 25
    } else{
      plot_data <- plot_data %>% filter(!(variable %in% c("prev_bmi_30_proportion_double_burden", "prev_bmi_2sd_proportion_double_burden")))
      xtick <- 20
    }
    
    plot.max.val <- ceiling(max(abs(plot_data$mean))/(xtick/2))*(xtick/2)
    xlims        <- c(-plot.max.val, plot.max.val)
    xbreaks      <- seq(-ceiling(plot.max.val/xtick)*xtick + xtick, ceiling(plot.max.val/xtick)*xtick - xtick, by = xtick)
  }

  if(type == "change"){
    ylims <- c(max(plot_data$se)* 1.05, 0)
    plot_data <- plot_data %>% filter(start.year == plot.start.year & end.year == plot.end.year)
    xlab <- paste0("Change ", plot.start.year, "-", plot.end.year, " (percentage points)")
    ylab <- paste0("Uncertainty (posterior SD) of the estimate \n(percentage points)")
  } else if (type == "level"){
    plot_data <- plot_data %>% filter(year == plot.year)
    
    xlab <- paste0("Proportion of combined burden\ncomposed of obesity (%)")
    ylab <- paste0("Uncertainty (posterior SD) of the estimate (%)")
  }
  
  plot_data <- plot_data  %>% 
    filter(sex == my_sex & variable == my_variable)
  
  
  if(type == "level"){
    plot_data <- plot_data %>% filter(year == plot.year)%>% mutate(PP = PP_obesity_larger_than_underweight)
  } else{
    plot_data <- plot_data %>% mutate(PP = PP.increase)
  }
  
  plot_data <- plot_data %>%
    mutate(SregionFill = ifelse(0.2 < PP & PP < 0.8, "hollow", as.character(Superregion)))
  
  
  ribbon_dat <- plot_data %>%
    summarise(se = ylims[1]) %>%
    mutate(y0min = qnorm(0.01) * se,
           y0max = qnorm(0.99) * se,
           y1min = qnorm(0.025) * se,
           y1max = qnorm(0.975) * se,
           y2min = qnorm(0.05) * se,
           y2max = qnorm(0.95) * se,
           y3min = qnorm(0.2) * se,
           y3max = qnorm(0.8) * se) %>%
    bind_rows(data.frame(se = 0, y0min = 0, y0max = 0, y1min = 0, y1max = 0, y2min = 0, y2max = 0, y3min = 0, y3max = 0))
  
  if(type == "level"){
    ribbon_dat <- plot_data %>%
      summarise(se = ylims[1]) %>%
      mutate(y0min = (qnorm(0.01) * se) + 50,
             y0max = (qnorm(0.99) * se) + 50,
             y1min = (qnorm(0.025) * se)+ 50,
             y1max = (qnorm(0.975) * se)+ 50,
             y2min = (qnorm(0.05) * se)+ 50,
             y2max = (qnorm(0.95) * se)+ 50,
             y3min = (qnorm(0.2) * se)+ 50,
             y3max = (qnorm(0.8) * se)+ 50) %>%
      bind_rows(data.frame(se = 0, y0min = 50, y0max = 50, y1min = 50, y1max = 50, y2min = 50, y2max = 50, y3min = 50, y3max = 50))
    
  }
  
  d_legend <- data.frame(x = 0, y = 1, col = c("PP \u22650.99", "0.975\u2264 PP <0.99", "0.95\u2264 PP <0.975", "0.8\u2264 PP <0.95", "PP <0.8"))
  d_legend$col <- factor(d_legend$col, levels = unique(d_legend$col))
  cols <- c("white", "#F5F7F7", "#EBEDED", "#DDE0E4", "#C4CAD0")
  names(cols) <- unique(d_legend$col)
  p_legend <- ggplot(d_legend, aes(x, y, fill = col)) + geom_col() + scale_fill_manual(values = cols, name = "") + theme(legend.position = "right", legend.justification= c("left", "top"), text = element_text(size=14)) + guides(fill = guide_legend(reverse = FALSE))
  leg <- get_legend(p_legend)
  
  p_leg2 <- ggplot(plot_data %>% filter(sex == my_sex), aes(se, mean, colour = Superregion, size = Superregion)) + geom_point() + colScale + sizScale + theme(legend.position = "right", legend.key = element_rect(fill = NA), legend.justification= c("left", "top"), text = element_text(size = 14)) + guides(size = 'none', colour = guide_legend(reverse = FALSE))
  leg_reg <- get_legend(p_leg2)
  
  
  p_leg3 <-ggplot(plot_data %>% filter(sex == my_sex) %>% mutate(SregionFill = ifelse(SregionFill == "hollow", "Posterior probability < 0.8", "Posterior probability \u2265 0.8")), aes(se, mean, fill = SregionFill)) + 
    scale_fill_manual(values = c("Posterior probability < 0.8" = "white", "Posterior probability \u2265 0.8" = "black"), name = "") + 
    geom_point(shape = 21, colour = "black") +
    theme(legend.position = "right", legend.key = element_rect(fill = NA), legend.justification= c("left", "top"), text = element_text(size = 14)) + 
    guides(size = 'none', colour = guide_legend(reverse = FALSE))
  
  
  leg_reg_fill <- get_legend(p_leg3)
  
  blank <- grid.rect(gp=gpar(col="white"))
  
  
  p <- ggplot(plot_data, aes(x = se)) +
    # geom_ribbon(data = ribbon_dat, aes(ymin = y0min, ymax = y0max), fill = "#F5F7F7") +
    # geom_ribbon(data = ribbon_dat, aes(ymin = y1min, ymax = y1max), fill = "#EBEDED") +
    # geom_ribbon(data = ribbon_dat, aes(ymin = y2min, ymax = y2max), fill = "#DDE0E4") +
    # geom_ribbon(data = ribbon_dat, aes(ymin = y3min, ymax = y3max), fill = "#C4CAD0") +
    geom_point(aes(y = mean, fill = SregionFill, colour = Superregion, size = Superregion, shape = 21)) +
    geom_blank(data = data.frame(se = 0, mean = 0)) +
    geom_blank(data =plot_data, aes(x = se, y = mean)) +
    fillScale + sizScale + colScale +
    scale_shape_identity() +
    theme_bw() +
    theme(panel.grid = element_blank(), legend.position = "none", strip.background = element_blank(), text = element_text(size = 12)) +
    scale_x_reverse(expand = expansion(mult = 0, add = 0), limits = ylims) +
    scale_y_continuous(expand = expansion(mult = 0, add = 0),limits = xlims, breaks = xbreaks) +
    coord_flip()
  
  if(type == "level"){
    p <- p + 
      geom_hline(yintercept = 50) +
      ggtitle(plot.year) 
    
    if(plot.year == plot.start.year){
      p <- p + xlab(ylab)
    } else{
      p <- p + xlab(" ")
    }
    
    
    p <- p + ylab(xlab)
    
  
  } else if(type == "change"){
    
    
    if(my_variable == "prev_bmi_30" | my_variable == "prev_bmi_2sd"){
      plot.title <- "Obesity"
    } else if(my_variable == "prev_bmi_30_proportion_double_burden" | my_variable == "prev_bmi_2sd_proportion_double_burden"){
      plot.title <- "Proportion of combined burden composed of obesity"
    }else if(my_variable == "prev_bmi_l185"){
      plot.title <- "Underweight"
    }else if(my_variable == "prev_bmi_neg2sd"){
      plot.title <- "Thinness"
    }else if(my_variable == "prev_double_burden"){
      plot.title <- "Combined burden"
    }
    p <- p + 
      geom_hline(yintercept = 0) +
      ggtitle(plot.title)
    
    if(my_variable == change_variables[1] | my_variable == change_variables[3] | my_variable == change_variables[4]){
      p <- p + xlab(ylab)
    } else{
      p <- p + xlab("")
    }
    
    p <- p + ylab(xlab)
    
  }
  
  if(leg_direction == "vertical"){
    ps <- arrangeGrob(leg_reg, leg_reg_fill, ncol = 1, heights = c(1, 1))
  } else{
    ps <- arrangeGrob(leg_reg, leg_reg_fill, nrow = 1, widths = c(3, 2))
  }
  
  if(returnLeg){
    
    return(ps)
  } else{
    return(p)
  }
}

# 

#data = data_timechange;variable = my_variable; type = "change"; my_sex = my_sex; returnLeg = F
# 
# funnel_plots_interactive <- function(data, variable, type = 'change', my_sex, plot.year = NULL, returnLeg = F) {
#   # type takes 'level' or 'change'
# 
#   sizes <- rep(1.5, 9)
#   names(sizes) <- c(unique(data$Superregion))
#   sizScale  <- scale_size_manual(name="", values=sizes)
#   colScale  <- scale_colour_manual(name="", values=sregion_col) #values=c("World" = "black", sregion_col))
# 
#   plot_data <- data %>% filter(sex == my_sex & variable == my_variable)
# 
#   if(type == "level"){
#     plot_data <- plot_data %>% mutate(mean = mean - 50)
#   }
# 
# 
# 
#   if(type == "change"){
#     plot_data <- plot_data %>% filter(start.year == plot.start.year & end.year == plot.end.year)
#     xlab <- paste0("Change in ", get_var_longname(variable), " ", plot.start.year, "-", plot.end.year, "\n(percentage points)")
#     ylab <- paste0("Uncertainty (posterior SD) of the estimate (percentage points)")
#   } else if (type == "level"){
#     plot_data <- plot_data %>% filter(year == plot.year)
#     xlab <- paste0(get_var_longname(variable), " (%)")
#     ylab <- paste0("Uncertainty (posterior SD) of the estimate (%)")
#   }
# 
#   ribbon_dat <- plot_data %>%
#     summarise(se = max(se) * 1.05) %>%
#     mutate(y0min = qnorm(0.01) * se,
#            y0max = qnorm(0.99) * se,
#            y2min = qnorm(0.05) * se,
#            y2max = qnorm(0.95) * se,
#            y3min = qnorm(0.2) * se,
#            y3max = qnorm(0.8) * se) %>%
#     bind_rows(data.frame(se = 0, y0min = 0, y0max = 0, y1min = 0, y1max = 0, y2min = 0, y2max = 0, y3min = 0, y3max = 0))
# 
#   d_legend <- data.frame(x = 0, y = 1, col = c("PP \u22650.99", "0.95\u2264 PP <0.99", "0.8\u2264 PP <0.95", "PP <0.8"))
#   d_legend$col <- factor(d_legend$col, levels = unique(d_legend$col))
#   cols <- c("white", "#EBEDED", "#DDE0E4", "#C4CAD0")
#   names(cols) <- unique(d_legend$col)
#   p_legend <- ggplot(d_legend, aes(x, y, fill = col)) + geom_col() + scale_fill_manual(values = cols, name = "") + theme(legend.position = "right", legend.justification= c("left", "top"), text = element_text(size=14)) + guides(fill = guide_legend(reverse = FALSE))
#   leg <- get_legend(p_legend)
# 
#   p_leg2 <- ggplot(plot_data %>% filter(sex == my_sex), aes(se, mean, colour = Superregion, size = Superregion)) + geom_point() + colScale + sizScale + theme(legend.position = "right", legend.key = element_rect(fill = NA), legend.justification= c("left", "top"), text = element_text(size = 14)) + guides(size = 'none', colour = guide_legend(reverse = FALSE))
#   leg_reg <- get_legend(p_leg2)
# 
#   blank <- grid.rect(gp=gpar(col="white"))
#   
#   if(type == "level"){
#     plot_data <- plot_data %>% filter(year == plot.year)%>% mutate(PP = PP_obesity_larger_than_underweight)
#   } else{
#     plot_data <- plot_data %>% mutate(PP = PP.increase)
#   }
#   #Calculate if PP approximation is correct
#   plot_data <- plot_data %>% 
#     mutate(classified_approx_PP = ifelse(mean < qnorm(0.01)*se, "l0.01",
#                                          ifelse(mean <qnorm(0.05)*se, "0.01_0.05",
#                                                 ifelse(mean <qnorm(0.2)*se, "0.05_0.2",
#                                                        ifelse(mean <qnorm(0.8)*se, "0.2_0.8",
#                                                               ifelse(mean <qnorm(0.95)*se, "0.8_0.95",
#                                                                             ifelse(mean <qnorm(0.99)*se, "0.95_0.99", "g0.99"))))))) %>%
#     mutate(classified_PP = ifelse(PP < 0.01, "l0.01",
#                                          ifelse(PP <0.05, "0.01_0.05",
#                                                 ifelse(PP <0.2, "0.05_0.2",
#                                                        ifelse(PP <0.8, "0.2_0.8",
#                                                               ifelse(PP <0.95, "0.8_0.95",
#                                                                      ifelse(PP <0.99, "0.95_0.99", "g0.99"))))))) %>%
#     mutate(PP_match_ind = ifelse(classified_approx_PP == classified_PP, 1, 0)) %>%
#     mutate(PP_match_ind = factor(PP_match_ind, levels = c(0,1)))
#     
#   
#   p <- ggplot(plot_data, aes(x = se)) +
#     # geom_ribbon(data = ribbon_dat, aes(ymin = y0min, ymax = y0max), fill = "#F5F7F7") +
#     # geom_ribbon(data = ribbon_dat, aes(ymin = y2min, ymax = y2max), fill = "#DDE0E4") +
#     # geom_ribbon(data = ribbon_dat, aes(ymin = y3min, ymax = y3max), fill = "#C4CAD0") +
#     geom_point(aes(y = mean, colour = PP_match_ind, size = Superregion)) +
#     geom_blank(data = data.frame(se = 0, mean = 0)) +
#     geom_blank(data = plot_data, aes(x = se, y = mean)) +
#     scale_color_manual(values = c("1" = "black", "0" = "red"))+
#     sizScale +
#     geom_hline(yintercept = 0) +
#     theme_bw() +
#     theme(panel.grid = element_blank(), legend.position = "none", strip.background = element_blank(), text = element_text(size = 12)) +
#     scale_x_reverse(expand = expansion(mult = 0, add = 0), name = ylab) +
#     scale_y_continuous(name = xlab) +
#     coord_flip()
# 
#   if(type == "level"){
#     p <- p + ggtitle(plot.year)
#   }
# 
#   return(p)
# 
#   ps <- arrangeGrob(leg_reg, leg, ncol = 2, widths = c(4, 3))
# 
#   if(returnLeg){
# 
#     return(ps)
#   } else{
#     return(p)
#   }
# }
