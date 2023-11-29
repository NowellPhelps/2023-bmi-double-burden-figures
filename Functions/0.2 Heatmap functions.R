colour_vals <- c("prev_bmi_l185"    = "#00CED1", 
                  "prev_bmi_185_20" = "#3288BD", 
                  "prev_bmi_20_25"  = "#66C2A5",
                  "prev_bmi_25_30"  = "#FFFFBF",
                  "prev_bmi_30_35"  = "#FDAE61",
                  "prev_bmi_35_40"  = "#D53E4F",
                  "prev_bmi_40"     = "#9E0142")

plot_heatmap <- function(subset, my_sex, my_age_group, plot.year){
  
  plot_data <- subset %>% filter(sex == my_sex, age_group == my_age_group, year == plot.year)
  
  order <- plot_data %>% 
    filter(variable == variables[1]) %>%
    arrange(match(Region, region_order), Country)
  
  plot_data <- plot_data %>% 
    mutate(pos = match(Country, order$Country)) %>%
    group_by(variable) %>%
    ungroup()
  
  p <- ggplot(plot_data, aes(x = variable, y = pos, fill = variable)) +
    geom_tile(data = plot_data, aes(x = variable, y = pos, fill = variable, alpha = mean)) +
    fill_scale +
    theme_bw() +
    scale_x_discrete(labels = names(variables))+
    xlab("") +
    ylab("") +
    scale_y_discrete(label = list(order$Country)) +
    theme(legend.position = "none",
          axis.ticks.x  = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1), 
          panel.grid = element_blank())
  
  p2 <- 
  
  return(p)
}
