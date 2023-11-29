# Change dot plot function
letters_extended <- paste0(rep(letters, 2), rep(c("A", "B"), each = length(letters)))

change_dot_plot_function <- function(subset, my_sex, my_variable, plot.start.year, plot.end.year, plot.gap.year, returnLeg = F, ytick = 5, my_change_type = "absolute"){
  
  # Match names to year groups
  years <- data.frame(year = seq(plot.start.year, plot.end.year)) %>%
    filter((year - plot.end.year)%%plot.gap.year == 0) %>%
    mutate(name = letters_extended[1:nrow(.)])
  
  # Format data as needed
  plot_data <- subset %>%
    filter(variable == my_variable) %>%
    group_by(sex) %>% 
    filter(year >= plot.start.year & year <= plot.end.year)
  
  if(plot.gap.year != 1){
    plot_data <- plot_data %>% filter((year - plot.end.year)%%plot.gap.year == 0)
  }
  plot_data <- plot_data %>%
    select(-c(l, u, se)) %>%
    pivot_wider(names_from = year, values_from = mean) %>%
    rename_with(~years$name[which(years$year == .x)], matches("^\\d{4}$")) %>%
    mutate(across(years$name[which(!(years$name == "aA"))], .fns = ~ . - get(letters_extended[which(letters_extended == cur_column())-1]), .names = "change_absolute_{.col}")) %>%
    mutate(across(years$name[which(!(years$name == "aA"))], .fns = ~ (./get(letters_extended[which(letters_extended == cur_column())-1]) - 1)*100, .names = "change_relative_{.col}")) %>%
    select(-years$name) %>%
    pivot_longer(cols = paste0(rep(c("change_absolute_", "change_relative_"), each = nrow(years)-1), rep(years$name[which(!(years$name == "aA"))],2)), names_to = "change_years", values_to = "mean") %>% 
    mutate(change_type = ifelse(grepl("absolute", change_years), "absolute", "relative")) %>%
    mutate(change_years = gsub("change_absolute_", "", change_years),
           change_years = gsub("change_relative_", "", change_years)) %>%
    mutate(change_years = lapply(change_years, FUN = function(X) paste0(years$year[which(years$name == X) - 1], "-", years$year[which(years$name == X)]))) %>%
    ungroup() %>%
    mutate(change_years = as.character(change_years))
  
  plot_data$change_years = factor(plot_data$change_years, labels = unique(plot_data$change_years))
  
  plot_data <- plot_data %>%
    filter(change_type == my_change_type)
  
  if(my_change_type == "absolute"){
    if(variable_type == "prev"){
      yunits <- "(percentage points)"
      ytick <- ifelse(plot.gap.year == 5,2.5,
                      ifelse(plot.gap.year == 10, 5, 
                             ifelse(plot.gap.year == 1, 0.2, 2.5)))
    } else{
      yunits <- "(kg/m\u00B2)"
      ytick <- ifelse(plot.gap.year == 5,0.2,
                      ifelse(plot.gap.year == 10, 0.5, 
                             ifelse(plot.gap.year == 1, 0.1, .5)))
    }
    
     
    
  } else{
    if(variable_type == "prev"){
      yunits <- "(%)"
      ytick <- ifelse(plot.gap.year == 5,25,
                      ifelse(plot.gap.year == 10, 50, 
                             ifelse(plot.gap.year == 1, 5, 20)))
    } else{
      yunits <- "(%)"
      ytick <- ifelse(plot.gap.year == 5,10,
                      ifelse(plot.gap.year == 10, 20, 
                             ifelse(plot.gap.year == 1, 5, 20)))
    }
   
    
  }
  
  ymin <- floor(min(plot_data$mean)/ytick) * ytick
  ymax <- ceiling(max(plot_data$mean)/ytick) * ytick
  ybreaks <- seq(ymin, ymax, ytick)
  yticks <- seq(ymin + ytick, ymax - ytick, ytick)
  
  plot_data <- plot_data %>%
    filter(sex == my_sex)
  
  p <- ggplot(plot_data, aes(y = mean, colour = Superregion, x = change_years)) +
    geom_jitter(aes(group = Country), position = position_jitterdodge(jitter.width = .5), size = 0.8)+
    scale_colour_manual(values = sregion_col) +
    theme_bw() + 
    ylab(paste0("Change in ", get_var_longname(my_variable), " during \nperiod ",yunits)) +
    ggtitle(paste(ifelse(my_sex == "female", "Women", "Men"), gsub("prevalence of ", "",get_var_longname(my_variable)), my_change_type, "change")) +
    theme(panel.grid = element_blank()) + 
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
    scale_y_continuous(breaks = ybreaks, limits = c(ymin, ymax))
  
  if(plot.gap.year <= 2){
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  if(my_sex == "male"){
    p <- p + xlab("Time period")
  } else{
    p <- p + xlab("")
  }
    
  if(returnLeg){
    p <- get_legend(p)
  } else{
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
}
  

