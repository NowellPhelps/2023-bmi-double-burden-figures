# Functions generate maps including color settings and density plots insets    
# Adapted from JB Code 2016 and AM code 2022 for adult BMI analysis by NP 2023

world_map <- readRDS(paste0(functionsDir,"World_map.RDS"))
# Define caribbean and inset countries
caribbean <- c("ATG","BHS","BRB","CUB","DMA","DOM","GRD","HTI","JAM","KNA","LCA","PRI","TTO","VCT")
insets    <- c("ASM","BHR","BMU","BRN","COK","COM","CPV","FJI","FSM","KIR","MDV","MHL","MNE","MUS","NIU","NRU","PLW","PYF","SLB","STP","SYC","TKL","TON","TUV","VUT","WSM")
inset_countries <- countrylist$Country[match(insets,countrylist$iso)]
inset_countries[which(inset_countries=="Cabo Verde")] <- "Cape Verde"
#inset_countries[grep("Micronesia", inset_countries)] <- "Micronesia, Federated States of"


get_change_fill_scale <- function(plot.min, plot.max, legend.title, var_intv, scale_colour_type = "green"){
  
  n      <- 200
  breaks <- seq(ceiling(plot.min/var_intv)*var_intv, floor(plot.max/var_intv)*var_intv, var_intv)
  
  if(scale_colour_type == "green"){
    # pt_col <- colorRampPalette(c("darkgreen","green4","green3","darkolivegreen1","lightgoldenrodyellow","#FED976","#FEB24C","#FC4E2A", "#BD0026"))
    # vals <- c(-0.49, -0.25, -0.15,-0.07,0, 0.04,0.07, 0.15, 0.23)
    
    pt_col <- colorRampPalette(c("darkgreen","green4","green3","darkolivegreen1","lightgoldenrodyellow","#FBF2A3", "#FED976","#FEB24C","#FC4E2A", "#CE1527"))
    vals <- c(-0.5, -0.3, -0.15,-0.07,0, 0.05, 0.1, 0.2, 0.4, 0.5)
    
    vals[1:5] <- vals[1:5]*abs(plot.min)/(0.5)
    vals[6:length(vals)] <- vals[6:length(vals)]*(plot.max/0.5)
    
  }else if(scale_colour_type == "green_prev_bmi_2sd"){
    # pt_col <- colorRampPalette(c("darkgreen","green4","green3","darkolivegreen1","lightgoldenrodyellow","#FED976","#FEB24C","#FC4E2A", "#BD0026"))
    # vals <- c(-0.49, -0.25, -0.15,-0.07,0, 0.04,0.07, 0.15, 0.23)
    
    pt_col <- colorRampPalette(c("darkolivegreen1","#e2ffb2","lightgoldenrodyellow","#FBF2A3", "#FED976","#FEB24C","#FC4E2A", "#CE1527"))
    vals <- c(-0.5,  -0.2,0, 0.05, 0.1, 0.15, 0.25, 0.5)
    
    vals[1:3] <- vals[1:3]*abs(plot.min)/(0.5)
    vals[4:length(vals)] <- vals[4:length(vals)]*(plot.max/0.5)
    
  } else if(scale_colour_type == "green_prev_bmi_30"){
    # pt_col <- colorRampPalette(c("darkgreen","green4","green3","darkolivegreen1","lightgoldenrodyellow","#FED976","#FEB24C","#FC4E2A", "#BD0026"))
    # vals <- c(-0.49, -0.25, -0.15,-0.07,0, 0.04,0.07, 0.15, 0.23)
    
    pt_col <- colorRampPalette(c("darkolivegreen1","#e2ffb2","lightgoldenrodyellow","#FBF2A3", "#FED976","#FEB24C","#FC4E2A", "#CE1527"))
    vals <- c(-0.5, -0.2,0, 0.05, 0.1, 0.2, 0.4, 0.5)
    
    vals[1:3] <- vals[1:3]*abs(plot.min)/(0.5)
    vals[4:length(vals)] <- vals[4:length(vals)]*(plot.max/0.5)
    
  }else if(scale_colour_type == "green_prev_bmi_l185"){
    # pt_col <- colorRampPalette(c("darkgreen","green4","green3","darkolivegreen1","lightgoldenrodyellow","#FED976","#FEB24C","#FC4E2A", "#BD0026"))
    # vals <- c(-0.49, -0.25, -0.15,-0.07,0, 0.04,0.07, 0.15, 0.23)
    
    pt_col <- colorRampPalette(c("darkgreen","green4","green3","darkolivegreen1","lightgoldenrodyellow","#FBF2A3", "#FED976","#fec06c"))
    vals <- c(-0.5, -0.3, -0.15,-0.07,0, 0.1, 0.25, 0.4)
    
    vals[1:5] <- vals[1:5]*abs(plot.min)/(0.5)
    vals[6:length(vals)] <- vals[6:length(vals)]*(plot.max/0.4)
    
  } else if(scale_colour_type =="blue_adult"){
    pt_col <- colorRampPalette(c("cornflowerblue","cornflowerblue","#96C7E8","#96C7E8","powderblue", "#fffdf5", "#FBF2A3","gold", "orange", "orangered","red4"))
    pt_col <- colorRampPalette(c("#d6e5f4", "#fff7f3", "#fcc5c0", "#f768a1", "#dd3497", "#ae017e","#7a0177"))
    
    vals <- c(-0.5, -0.15, 0.25, 0.75, 1.2, 1.75, 2.5,3.2,3.2)
    
    vals[1:2] <- vals[1:2]*abs(plot.min)/(0.5)
    vals[3:length(vals)] <- vals[3:length(vals)]*(plot.max/3.2)
    
  }else if(scale_colour_type =="blue_ado"){
    pt_col <- colorRampPalette(c("cornflowerblue","cornflowerblue","#96C7E8","#96C7E8","powderblue", "#fffdf5", "#FBF2A3","gold", "orange", "orangered","red4"))
    pt_col <- colorRampPalette(c("#d6e5f4", "#fff7f3", "#fcc5c0", "#f768a1", "#dd3497", "#ae017e","#7a0177"))
    
    vals <- c(-0.5, -0.05, 0.25, 0.75, 1.2, 1.75, 2.5,3.2,3.2)
    
    vals[1:2] <- vals[1:2]*abs(plot.min)/(0.5)
    vals[3:length(vals)] <- vals[3:length(vals)]*(plot.max/3.2)
    
  }
  # "#fff7f3"
  # "#fde0dd"
  # "#fcc5c0"
  # "#fa9fb5"
  # "#f768a1"
  # "#dd3497"
  # "#ae017e"
  # "#7a0177"
  # "#49006a"
  
  my_fill <- scale_fill_gradientn(colours = pt_col(n-1),
                                  values = scales::rescale(vals),
                                  name=legend.title,
                                  na.value = "grey50", limits=c(plot.min, plot.max), breaks=breaks)
  
  return(my_fill)
}





map_function <- function(data_map, fill_name, map = world_map, not_inset = TRUE, legend = FALSE, plot_title = NA, leg_fill = my_fill, fig.num = figNum, type = "level") {
  # Main map utility function to generate base maps for plotting. Arguments are :
  # data: dataframe containing estimated variable for each country/sex/variable/ind
  # fill_name: variable in dataset to be plotted
  # map: shape file for map saved as RDS (see World_map.RDS)
  # not_inset: controls if specified inset countries should be plotted, takes "TRUE" or "FALSE"
  # legend: controls if legend should be plotted, takes "TRUE" or "FALSE"
  # plot_title: character string to be used for plotting title
  # leg_fill: color called used for plotting (see "Map color scale functions.R" for some built in options)
  # fig.num: takes either 1, 2, 4 or 8 to refer to the extended figures this function is used with, controls font sizes.
  
  title_font_size <- 14
  inset_text_size <- 3.7
  leg_title_size  <- 8
  leg_text_size   <- 12
  leg_key_size    <- 0.7
  
  leg_pos <- c(0.06, 0.185)
  
  if(figNum == 4 & type == "level"){
    leg_pos <- c(0.06, 0.25)
  } else if  (figNum == 5 & type == "level"){
    leg_pos <- c(0.06, 0.25)
  }
  
  data_map[["mean"]] <- data_map[[fill_name]]
  
  if (not_inset) {
    # plot main map and Caribbean map
    dataset <- merge(map, data_map, all = TRUE, by = c("iso")) %>% arrange(order)
    p <- ggplot(dataset, aes(x = long, y = lat, group = group, fill = mean)) +
      geom_polygon() +
      geom_path(linewidth=0.1, colour="black") +
      coord_equal() + theme_void() +  leg_fill
    
    if(!(is.na(plot_title))){
        p <- p + ggtitle(plot_title)
    }
    
    if (legend) {
      
      p <- p +theme(legend.position = leg_pos,
                    legend.justification = c("left", "bottom")) + 
        geom_rect(xmin=-202+360*leg_pos[1],xmax=-202+360*leg_pos[1]+33,
                  ymin=-65+143*leg_pos[2], ymax=-65+143*leg_pos[2]+70, fill="white")
      
      
    } else {
      p <- p + theme(legend.position = "none")
    } 
    
  } else {
    # plot inset countries as squares
    ncols = 4
    nrows = ceiling(length(inset_countries)/ ncols)
    dx = 11
    alpha = 0.6
    marg = 0.2
    data.inset <- data.frame(iso = insets, country = inset_countries) %>% arrange(country) %>% mutate(i = 1:length(inset_countries) - 1)
    data.inset$i <- ifelse(data.inset$i >= 20, data.inset$i+1, data.inset$i)
    data.inset$x <- floor(data.inset$i / nrows) * dx
    data.inset$y <- nrows - data.inset$i %% nrows
    data.inset$colour <- data_map$mean[match(data.inset$iso, data_map$iso)]
    p <- ggplot(data.inset, aes(fill = colour)) + 
      geom_rect(aes(xmin=x, xmax=x+alpha, ymin=y, ymax=y+alpha), colour="black", linewidth=0.2) + 
      geom_text(aes(x=x+alpha+marg, y=y+0.5*alpha, label=country), hjust=0, size = inset_text_size) +
      xlim(c(0, max(data.inset$x)+dx*0.6)) + leg_fill +
      coord_equal() + 
      theme_void() + theme(legend.position = "none")
  }
  return(p)
}

maps_level_plots <- function(data, variable, my_sex, my_age_group = "ageStd", plot.year, appendix = F, figNum = 1, returnLeg = F) {
  
  
  var.longname <- get_var_longname(variable)
  plot.title   <- paste0(plot.year)
  
  if(my_age_group == "ageStd"){
    data <- data %>% filter(age_group == "ageStd")
  }
  
  plot.min <- floor(min(data$mean))
  plot.max <- ceiling(max(data$mean))
  
  if(variable == "prev_double_burden"){
    data <- data %>% filter(year == plot.year)
    plot.min <- floor(min(data$mean))
    legend.title <- paste0("(%)")
    
    if(age_type == "ado"){
      var_intv <- 10
    } else{
      var_intv <- 15
    }
    
  }else if(variable == "prev_bmi_30"){
    legend.title <- paste0("(%)")
    var_intv <- 15
    
  }else if(variable == "prev_bmi_35"){
    legend.title <- paste0("(%)")
    var_intv <- 10
    
  }else if(variable == "prev_bmi_2sd"){
    var_intv <- 10
    legend.title <- "(%)"
    
  } else if(variable == "prev_bmi_l185"){
    var_intv <- 10
    legend.title <- "(%)"
  } else if(variable == "prev_bmi_neg2sd"){
    var_intv <- 10
    legend.title <- "(%)"
    
  } else if(variable == "prev_bmi_30_proportion_double_burden"){
    var_intv <- 50
    legend.title <- "(%)"
  } else if(variable == "prev_bmi_2sd_proportion_double_burden"){
    var_intv <- 50
    legend.title <- "(%)"
  }
  
  breaks=seq(ceiling(plot.min/var_intv)*var_intv, floor(plot.max/var_intv)*var_intv, var_intv)
  
  
  if(variable == "prev_bmi_30_proportion_double_burden"){
    my_fill <- fill_scale_proportion_double_burden
    
  } else if(variable == "prev_bmi_2sd_proportion_double_burden"){
    my_fill <- fill_scale_proportion_double_burden_ado
  } else {
    vals <- c(0, 0.05, 0.1,0.2,0.3, 0.4, 0.53, 0.70, 0.82, 1)
    my_fill <- scale_fill_gradientn(#colours = brewer.pal(9,"YlOrRd"), # "#FFFFCC" "#FFEDA0" "#FED976" "#FEB24C" "#FD8D3C" "#FC4E2A" "#E31A1C" "#BD0026" "#800026"
      colours = c("#ffffe5","#FFFFCC","#FFEDA0","#FED976","#FEB24C","#FD8D3C","#FC4E2A","#E31A1C","#BD0026","#800026"),                              
      values = scales::rescale(vals),
                                    name=legend.title,
                                    na.value="grey50",
                                    limits=c(plot.min, plot.max),
                                    breaks= breaks) 
  }
  
  # Filter data to sex - needed here rather than earlier so bounds consistent across sexes
  data <- data %>% filter(sex == my_sex) %>% filter(year == plot.year) %>% filter(age_group == my_age_group)
  
  # Generate main map, Caribbean map and inset boxes 
  main_map      <- map_function(data, "mean", legend = ifelse(variable %in% c("prev_bmi_30_proportion_double_burden", "prev_bmi_2sd_proportion_double_burden"), FALSE, TRUE), plot_title = paste(c(rep(" ", 10), plot.title), collapse = ""), leg_fill = my_fill, type = "level")
  caribbean_map <- map_function(data %>% filter(iso %in% caribbean), "mean",world_map %>% subset(iso %in% caribbean),leg_fill = my_fill, type = "level")
  inset_map     <- map_function(data %>% filter(iso %in% insets), "mean",map = NULL, not_inset = FALSE,leg_fill = my_fill, type = "level")
  
  # Generate density plot
  density <- ggplot(data, aes(x = mean, y = 1, fill = after_stat(x))) +
    ggridges::geom_density_ridges_gradient(colour="black", size=0.1) +
    my_fill + theme_minimal() +
    scale_x_continuous(expand = c(0, 0), breaks = breaks, limits = c(plot.min, plot.max)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(legend.position="none", legend.title=element_blank(),
          panel.grid = element_blank(),
          axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks.x = element_line())
  
  # Arrange and plot
  if((appendix & (figNum %in% c(5,6,7,8,14,15,16))) | (!(appendix) & figNum %in% c(2,6))){
    main_map <- main_map + annotation_custom(ggplotGrob(density), xmin=-150,xmax=-80,ymin=-60,ymax=-15)
  } else{
   main_map <- main_map + annotation_custom(ggplotGrob(density), xmin=-160,xmax=-80,ymin=-60,ymax=-10)
  }
  
  p <- arrangeGrob(main_map,
                   arrangeGrob(caribbean_map, inset_map, nrow = 1, widths = c(1,5)),
                   ncol = 1, heights = c(5,2))
  
  if(returnLeg){
    p <- get_legend(map_function(data, "mean", legend = T, plot_title = paste(c(rep(" ", 10), plot.title), collapse = ""), leg_fill = my_fill, type = "level"))
  }
  
  return(p)
}


maps_timeChg_plots <- function(data, variable, my_sex, my_age_group = "ageStd", my_plot.start.year, my_plot.end.year, ado_version = F, scale_colour_type= "green", appendix = F, figNum = 1) {
  
  data <- data %>% 
    filter(start.year == my_plot.start.year, end.year == my_plot.end.year)
  
  if(my_age_group == "ageStd"){
    data <- data %>% filter(age_group == "ageStd")
  }
  
  var.longname <- get_var_longname(variable)
  plot.title   <- paste0("Change from ", my_plot.start.year," to ", my_plot.end.year)
  
  # if(figNum %in% c(1,2)){
  #   plot.title <- ""
  # }
  
  # DEFINE LIMITS, BREAKS, AND FILL SCALES
  plot.min <- floor(min(data$mean))
  plot.max <- ceiling(max(data$mean))
  
  if(variable == "prev_double_burden"){
    var_intv <- 10
    legend.title <- "(Percentage\npoints)"
    scale_colour_type <- "green"
  } else if(variable == "prev_bmi_30"){
    var_intv <- 10
    legend.title <- "(Percentage\npoints)"
    scale_colour_type <- "green_prev_bmi_30"
    
  } else if(variable == "prev_bmi_35"){
      var_intv <- 5
      legend.title <- "(Percentage\npoints)"
      scale_colour_type <- "green"
      
  }else if(variable == "prev_bmi_l185"){
    var_intv <- 10
    legend.title <- "(Percentage\npoints)" 
    scale_colour_type <- "green_prev_bmi_l185"
  }else if(variable == "prev_bmi_l185"){
    var_intv <- 5
    legend.title <- "(Percentage\npoints)"
    scale_colour_type <- "green"
  }else if(variable == "prev_bmi_30_proportion_double_burden"){
    var_intv <- 15
    legend.title <- "(Percentage\npoints)" 
    scale_colour_type <- "blue_adult"
  }else if(variable == "prev_bmi_2sd"){
    var_intv <- 5
    legend.title <- "(Percentage\npoints)"
    scale_colour_type <- "green_prev_bmi_2sd"
    
  }else if(variable == "prev_bmi_neg2sd"){
    var_intv <- 5
    legend.title <- "(Percentage\npoints)"
    scale_colour_type <- "green_prev_bmi_l185"
    
  }else if(variable == "prev_bmi_2sd_proportion_double_burden"){
    var_intv <- 15
    legend.title <- "(Percentage\npoints)"
    scale_colour_type <- "blue_ado"
    
  }
  
  breaks = seq(ceiling(plot.min/var_intv)*var_intv, floor(plot.max/var_intv)*var_intv, var_intv)
  
  
  my_fill <- get_change_fill_scale(plot.min, plot.max, legend.title, var_intv, scale_colour_type)
  
  
  # Filter data to sex - needed here rather than earlier so bounds consistent across sexes
  data <- data %>% filter(sex == my_sex) %>% filter(age_group == my_age_group)
  
  
  # Generate main map, caribbean map and inset boxes 
  main_map      <- map_function(data, "mean", legend = TRUE, plot_title = paste(c(rep(" ", 10), plot.title), collapse = ""), leg_fill = my_fill, type = "timechange")
  caribbean_map <- map_function(data %>% filter(iso %in% caribbean), "mean", world_map %>% subset(iso %in% caribbean),leg_fill = my_fill, type = "timechange")
  inset_map     <- map_function(data %>% filter(iso %in% insets), "mean", map = NULL, not_inset = FALSE, leg_fill = my_fill, type = "timechange")
  
  # Generate density plot
  density <- ggplot(data, aes(x = mean, y = 1, fill = after_stat(x))) +
    ggridges::geom_density_ridges_gradient(colour="black", size=0.1) +
    my_fill + theme_minimal() +
    scale_x_continuous(expand = c(0, 0), breaks = breaks, limits = c(plot.min, plot.max)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(legend.position="none", legend.title=element_blank(),
          panel.grid = element_blank(),
          axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks.x = element_line())
  
  # Arrange and plot
  if((appendix & (figNum %in% c(5,6,7,8,14,15,16))) | (!(appendix) & figNum == 2)){
    main_map <- main_map + annotation_custom(ggplotGrob(density), xmin=-150,xmax=-80,ymin=-60,ymax=-15)
  } else {
    main_map <- main_map + annotation_custom(ggplotGrob(density), xmin=-160,xmax=-80,ymin=-60,ymax=-10)
  }
  
  p <- arrangeGrob(main_map,
                   arrangeGrob(caribbean_map, inset_map, nrow = 1, widths = c(1,5)),
                   ncol = 1, heights = c(5,2))
  
  return(p)
}



maps_timeChg_PP_plots <- function(data, variable, my_sex, my_age_group = "ageStd", my_plot.start.year, my_plot.end.year, ado_version = F, scale_colour_type= "green", appendix = F, figNum = 1) {
  
  data <- data %>% 
    filter(start.year == my_plot.start.year, end.year == my_plot.end.year)
  
  if(my_age_group == "ageStd"){
    data <- data %>% filter(age_group == "ageStd")
  }
  
  var.longname <- get_var_longname(variable)
  plot.title   <- paste0("Posterior probability of an increase from ", my_plot.start.year," to ", my_plot.end.year)
  
  
  var_intv <- 0.25
  legend.title <- "Posterior\nprobability"
  
  minqt <- plot.min <- 0; maxqt <- plot.max <- 1
  vals <- c(minqt,0.025, 0.05,0.1,0.15, 0.5, 0.85, 0.9, 0.95,0.975, maxqt) ## needs to be odd
  vals <- c(minqt,0.05, 0.1,0.2,0.25, 0.5, 0.75, 0.8, 0.9,0.95, maxqt) ## needs to be odd
  breaks <- seq(0,1,0.25)
  pt_col <- c('#796409','#D6B732','#F7DE70','#FAEEB7','#FAF2D7',  #needs to be same length as vals
              '#FCF8F3',
              '#E3F0F1', '#C2E6F0','#96D0E1','#70A1B0','#456A76')
  
  pt_col <- rev(c('#796409','#D6B732','#F7DE70','#FAEEB7','#FAF2D7',  #needs to be same length as vals
              '#FCF8F3',
              '#E3F0F1', '#C2E6F0','#96D0E1','#70A1B0','#456A76'))
  
  my_fill <- scale_fill_gradientn(colours = pt_col,
                                  values = scales::rescale(vals),
                                  name= legend.title,
                                  na.value = "grey50", limits=c(minqt, maxqt),breaks=breaks)
  
  
  
  # Filter data to sex - needed here rather than earlier so bounds consistent across sexes
  data <- data %>% filter(sex == my_sex) %>% filter(age_group == my_age_group)
  
  
  # Generate main map, caribbean map and inset boxes 
  main_map      <- map_function(data, "PP.increase", legend = TRUE, plot_title = paste(c(rep(" ", 10), plot.title), collapse = ""), leg_fill = my_fill, type = "timechange")
  caribbean_map <- map_function(data %>% filter(iso %in% caribbean), "PP.increase", world_map %>% subset(iso %in% caribbean),leg_fill = my_fill, type = "timechange")
  inset_map     <- map_function(data %>% filter(iso %in% insets), "PP.increase", map = NULL, not_inset = FALSE, leg_fill = my_fill, type = "timechange")
  
  # Generate density plot
  density <- ggplot(data, aes(x = PP.increase, y = 1, fill = after_stat(x))) +
    ggridges::geom_density_ridges_gradient(colour="black", size=0.1) +
    my_fill + theme_minimal() +
    scale_x_continuous(expand = c(0, 0), breaks = breaks, limits = c(plot.min, plot.max)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(legend.position="none", legend.title=element_blank(),
          panel.grid = element_blank(),
          axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks.x = element_line())
  
  # Arrange and plot
  if(appendix & (figNum %in% c(5,6,7,8,14,15,16))){
    main_map <- main_map + annotation_custom(ggplotGrob(density), xmin=-150,xmax=-80,ymin=-60,ymax=-15)
  } else{
    main_map <- main_map + annotation_custom(ggplotGrob(density), xmin=-160,xmax=-80,ymin=-60,ymax=-10)
  }
  
  p <- arrangeGrob(main_map,
                   arrangeGrob(caribbean_map, inset_map, nrow = 1, widths = c(1,5)),
                   ncol = 1, heights = c(5,2))
  
  return(p)
}



maps_data_sources_plots <- function(data){

  main_map      <- map_function(data, "n_source", legend = TRUE, plot_title = "")
  caribbean_map <- map_function(data %>% subset(iso %in% caribbean), "n_source",
                                world_map %>% subset(iso %in% caribbean))
  inset_map     <- map_function(data %>% subset(iso %in% insets), "n_source",
                                map = NULL, not_inset = FALSE)
  maps_source <- arrangeGrob(main_map, 
                             arrangeGrob(caribbean_map, inset_map, nrow = 1, widths = c(1,5)),
                             ncol = 1, heights = c(5,2))
  return(maps_source)
}


maps_velocity_level_plots <- function(data, variable, my_sex, plot.year, ado = F, figNum = 1) {
  
  var.longname <- get_var_longname(variable)
  plot.title   <- paste0("Velocity in age-standardised ", var.longname, " (%) in ", plot.year)
  
  
  if(variable == "prev_bmi_30"){
    legend.title <- "Velocity\nobesity (%)"
    var_intv <- 2
  } else if(variable == "prev_bmi_35"){
    legend.title <- "Velocity\nsevere obesity (%)"
    var_intv <- 2
  } else if (variable == "prev_bmi_2sd"){
      legend.title <- "Velocity\nobesity (%)"
      var_intv <- 2
  }else if(variable == "mean_bmi"){
    var_intv <- 0.1
    legend.title <- "Velocity\nmean BMI (kg/m\u00B2)"
  }
  
  plot.min <- floor(min(data$mean)/var_intv)*var_intv
  plot.max <- ceiling(max(data$mean)/var_intv)*var_intv
  
  breaks = seq(plot.min, plot.max, var_intv)
  n     <- 200
  # vals  <- c(-0.8, -0.78, -0.76, -0.75, -0.7, -0.5, -0.25, 0, 0.25, 0.75, 1.2, 1.75, 2.5,3.2,3.2)
  # 
  # vals[1:8] <- vals[1:8]*abs(plot.min)/(0.8)
  # vals[9:length(vals)] <- vals[9:length(vals)]*(plot.max/3.2)
  # 
  # pt_col <- colorRampPalette(c("cornflowerblue","cornflowerblue","#96C7E8","#96C7E8","powderblue", "#fffdf5", "#FBF2A3","gold", "orange", "orangered","red4"))
  
  vals  <- c(-0.8, -0.78, -0.76, -0.75, -0.7, -0.5, -0.25, 0, 0.25, 0.75, 1.2, 1.75, 2.5,3.2,3.2)
  
  vals[1:8] <- vals[1:8]*abs(plot.min)/(0.8)
  vals[9:length(vals)] <- vals[9:length(vals)]*(plot.max/3.2)
  
  pt_col <- colorRampPalette(c("cornflowerblue","cornflowerblue","#96C7E8","#96C7E8","powderblue", "#fffdf5", "#FBF2A3","gold", "orange", "orangered","red4"))
  
  my_fill <- scale_fill_gradientn(colours = pt_col(n-1),
                                  values = scales::rescale(vals),
                                  name=legend.title,
                                  na.value = "grey50", limits=c(plot.min, plot.max), breaks=breaks)
  
  # Filter data to sex and year - needed here rather than earlier so bounds consistent across sexes and years
  data <- data %>% filter(sex == my_sex, year == plot.year)
  
  # Generate main map, Caribbean map and inset boxes 
  main_map      <- map_function(data, "mean", legend = TRUE, plot_title = paste(c(rep(" ", 10), plot.title), collapse = ""), leg_fill = my_fill)
  caribbean_map <- map_function(data %>% filter(iso %in% caribbean), "mean",world_map %>% subset(iso %in% caribbean),leg_fill = my_fill)
  inset_map     <- map_function(data %>% filter(iso %in% insets), "mean",map = NULL, not_inset = FALSE,leg_fill = my_fill)
  
  # Generate density plot
  density <- ggplot(data, aes(x = mean, y = 1, fill = after_stat(x))) +
    ggridges::geom_density_ridges_gradient(colour="black", size=0.1) +
    my_fill + theme_minimal() +
    scale_x_continuous(expand = c(0, 0), breaks = breaks, limits = c(plot.min, plot.max)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(legend.position="none", legend.title=element_blank(),
          panel.grid = element_blank(),
          axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks.x = element_line())
  
  # Arrange and plot
  main_map <- main_map + annotation_custom(ggplotGrob(density), xmin=-160,xmax=-80,ymin=-60,ymax=-10)
  
  p <- arrangeGrob(main_map,
                   arrangeGrob(caribbean_map, inset_map, nrow = 1, widths = c(1,5)),
                   ncol = 1, heights = c(5,2))
  
  return(p)
}

maps_velocity_timechange_plots <- function(data, variable, my_sex, my_plot.start.year, my_plot.end.year, ado_version = F, figNum = 1) {
  
  data <- data %>% 
    filter(start.year == my_plot.start.year, end.year == my_plot.end.year)
  
  var.longname <- get_var_longname(variable)
  
  if(variable == "mean_bmi"){
    plot.title   <- paste0("Change in velocity ", var.longname, " ", my_plot.start.year,"-", my_plot.end.year)
  } else{
    plot.title   <- paste0("Change in relative velocity ", var.longname, " ", my_plot.start.year,"-", my_plot.end.year)
  }
  
  # DEFINE LIMITS, BREAKS, AND FILL SCALES
  
  
  if(variable == "prev_bmi_30"){
    var_intv <- 2
    legend.title <- "Change in\nvelocity obesity (%)"
  } else if (variable == "prev_bmi_2sd"){
    var_intv <- 2
    legend.title <- "Change in\nvelocity obesity (%)"
  }else if (variable == "prev_bmi_35"){
    var_intv <- 2
    legend.title <- "Change in velocity\nsevere obesity (%)"
  }else if(variable == "mean_bmi"){
    var_intv <- 0.1
    legend.title <- "Change in\nvelocity mean BMI (kg/m\u00B2)"
  }
  
  plot.min <- floor(min(data$mean)/var_intv)*var_intv
  plot.max <- ceiling(max(data$mean)/var_intv)*var_intv
  
  breaks = seq(plot.min, plot.max, var_intv)
  
  n    <- 200
  vals <- c(-0.8, -0.78, -0.76, -0.75, -0.7, -0.5, -0.25, 0, 0.25, 0.75, 1.2, 1.75, 2.5,3.2,3.2)
  
  vals[1:8] <- vals[1:8]*abs(plot.min)/(0.8)
  vals[9:length(vals)] <- vals[9:length(vals)]*(plot.max/3.2)
  
  pt_col <- colorRampPalette(c("cornflowerblue","cornflowerblue","#96C7E8","#96C7E8","powderblue", "#fffdf5", "#FBF2A3","gold", "orange", "orangered","red4"))
  
  my_fill <- scale_fill_gradientn(colours = pt_col(n-1),
                                  values = scales::rescale(vals),
                                  name=legend.title,
                                  na.value = "grey50", limits=c(plot.min, plot.max), breaks=breaks)
  
  
  # Filter data to sex - needed here rather than earlier so bounds consistent across sexes
  data <- data %>% filter(sex == my_sex)
  
  # Generate main map, caribbean map and inset boxes 
  main_map      <- map_function(data, "mean", legend = TRUE, plot_title = paste(c(rep(" ", 10), plot.title), collapse = ""), leg_fill = my_fill)
  caribbean_map <- map_function(data %>% filter(iso %in% caribbean), "mean", world_map %>% subset(iso %in% caribbean),leg_fill = my_fill)
  inset_map     <- map_function(data %>% filter(iso %in% insets), "mean", map = NULL, not_inset = FALSE, leg_fill = my_fill)
  
  # Generate density plot
  density <- ggplot(data, aes(x = mean, y = 1, fill = after_stat(x))) +
    ggridges::geom_density_ridges_gradient(colour="black", size=0.1) +
    my_fill + theme_minimal() +
    scale_x_continuous(expand = c(0, 0), breaks = breaks, limits = c(plot.min, plot.max)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(legend.position="none", legend.title=element_blank(),
          panel.grid = element_blank(),
          axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks.x = element_line())
  
  # Arrange and plot
  main_map <- main_map + annotation_custom(ggplotGrob(density), xmin=-160,xmax=-80,ymin=-60,ymax=-10)
  
  p <- arrangeGrob(main_map,
                   arrangeGrob(caribbean_map, inset_map, nrow = 1, widths = c(1,5)),
                   ncol = 1, heights = c(5,2))
  
  return(p)
}





maps_slope_type_plots <- function(data, variable, plot_sex, slope_type, plot.years = F) {
  # Function to generate maps of category of change in variable over time
  # Takes classification variable in data and returns map with colour by 'always increase', 'always decrease', 'both' or neither'
  # Data: Dataframe giving classification and minimum years of increase and decrease
  # Variable: takes prevalence category (or mean BMI)
  # my_sex: takes "male" or "female"
  # Subset data
  
  dd <- data %>% filter(type == slope_type)
  # dd$classification <- lapply(dd$classification, FUN = function(X) switch(X, "always_increase" = "Always increasing",
  #                                                                         "always_decrease" = "Always decreasing",
  #                                                                         "increase_to_decrease" = "Increasing then decreasing",
  #                                                                         "decrease_to_increase" = "Decreasing then increasing"))
                              
  variable.legendname <- switch(variable, "prev_bmi_30" = "obesity")
  
  # Define fill scales, limits, and breaks, colour palettes, and plot titles
  plot.title <- paste0("Age-standardised ", plot_sex, " ", variable.legendname)
  
  if(slope_type == "change"){
    legend.name = paste0(variable.legendname, " is:")
  } else{
    legend.name = paste0("Change in ",variable.legendname," is:")
  }
  
                       
  if (!(plot.years)){
    myPalette <- rainbow(4)
    names(myPalette) <- c("Always increasing", "Always decreasing","Increasing then decreasing","Decreasing then increasing")
    my_fill <- scale_fill_manual(values = myPalette,
                                 #labels = c("always_increase", "always_decrease","increase_to_decrease","decrease_to_increase"),
                                 name = legend.name,
                                 na.value = "grey50") 
  } else{
    # To be completed
  }
  
#  "#FF0000" "#80FF00" "#00FFFF" "#8000FF"
  
  # Generate main map, caribbean map and inset boxes 
  main_map      <- map_function(dd, "classification", legend = TRUE, plot_title = paste(c(rep(" ", 10), plot.title), collapse = ""), leg_fill = my_fill, fig.num = 1)
  caribbean_map <- map_function(dd %>% filter(iso %in% caribbean), "classification",world_map %>% subset(iso %in% caribbean),leg_fill = my_fill, fig.num = 1)
  inset_map     <- map_function(dd %>% filter(iso %in% insets), "classification",map = NULL, not_inset = FALSE,leg_fill = my_fill, fig.num = 1)
  
  # Arrange and plot
  p <- arrangeGrob(main_map,
                   arrangeGrob(caribbean_map, inset_map, nrow = 1, widths = c(1,5)),
                   ncol = 1, heights = c(5,2))
  
}
