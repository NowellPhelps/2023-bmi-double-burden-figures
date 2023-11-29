global_text_size <- 4
region_text_size <- 4

fill_scale_sankey <- scale_fill_manual(values = c(
  "cat0"   = "Purple",      # Malnutrition (100%)
  "cat1"   = "#EB0358",     # Total obesity
  "cat1up" = scales::alpha("#EB0358",alpha = 0.6),    # Connector to obesity
  "cat2"   = "#00CED1",     # Underweight
  "cat3"   = "#C80815",     # Severe Obesity (prev_bmi_35)
  "cat3up" = scales::alpha("#C80815", alpha = 0.6),         # Connector to severe obesity (prev_bmi_35)
  "cat4"   = "#FDAE61",     # Class 1 obesity (prev_bmi_30_35)
  "cat5"   = "#9E0142",     # Class 3 Obesity  (prev_bmi_40)
  "cat5up" = scales::alpha("#C80815",alpha = 0.6),         # Connector to class 3 obesity
  "cat6"   = "#D53E4F",     # Class 2 Obesity (prev_bmi_30_35)
  "cat7"   = "#ffffff",
  "cat8"   = "#ffffff",
  "cat9"   = "#ffffff",
  "underweight"  = scales::alpha("#00CED1", 0.6),  # connector to Underweight
  "classone"    = scales::alpha("#FDAE61", alpha = 0.6),  # connector to Class 1 
  "classtwo" = scales::alpha("#D53E4F", alpha = 0.6))) # connector to Class 2



plot_sankey <- function(plot_data, gtitle = NA, region_level = "Global") {
  col_width = switch(region_level, "Global" = 0.06, "Region" = 0.12)
  cat_sep   = 0.07
  text_marg_side = switch(region_level, "Global" = 0.01, "Region" = 0.15)
  text_marg_top  = switch(region_level, "Global" = 0.02, "Region" = 0.03)
  text_size <- switch(region_level, "Global" = global_text_size, "Region" = region_text_size)
  font_face <- switch(region_level, "Global" = "plain", "Region" = "plain")
  v_just = switch(region_level, "Global" = 0, "Region" = 0.5)
  h_just = switch(region_level, "Global" = 0.5, "Region" = 0.5)
  
  plot_data <- plot_data %>%
    mutate(loc = recode(cat, cat0 = 1, cat1 = 2, cat2 = 4, cat3 = 3, cat4 = 4, cat5 = 4, cat6 = 4)) %>%
    bind_rows(
      data.frame(cat = c("cat7", "cat8", "cat9"), prev = cat_sep * 100, loc = 4)
    )
  plot_data$cat <- factor(plot_data$cat, levels = c("cat0", "cat1", "cat2", "cat7", "cat3", "cat4", "cat8", "cat6", "cat9", "cat5"))
  
  connectors <- data.frame(
    loc = c(3 + col_width/2, 4 - col_width/2, 4 - col_width/2, 3 + col_width/2,
            2 + col_width/2, 4 - col_width/2, 4 - col_width/2, 2 + col_width/2,
            1 + col_width/2, 4 - col_width/2, 4 - col_width/2, 1 + col_width/2
    ),
    prev = c(plot_data$prev[plot_data$cat=="cat5"], sum(plot_data$prev[plot_data$cat%in%c("cat5")]) + 1*cat_sep*100, 
             plot_data$prev[plot_data$cat=="cat3"] + 1*cat_sep*100, plot_data$prev[plot_data$cat=="cat3"],
             plot_data$prev[plot_data$cat=="cat3"], sum(plot_data$prev[plot_data$cat%in%c("cat3")]) + 2*cat_sep*100, 
             plot_data$prev[plot_data$cat=="cat1"] + 2*cat_sep*100, plot_data$prev[plot_data$cat=="cat1"],
             plot_data$prev[plot_data$cat=="cat1"], sum(plot_data$prev[plot_data$cat%in%c("cat1")]) + 3*cat_sep*100, 
             plot_data$prev[plot_data$cat=="cat0"] + 3*cat_sep*100, plot_data$prev[plot_data$cat=="cat0"]
    ),
    cat = c(rep("classtwo", 4), rep("classone", 4), rep("underweight", 4))
  )
  
  texts1 <- data.frame(
    loc = switch(region_level,"Global" = 2:4 - col_width - text_marg_side, "Region" = 2:4 - 0.5),
    prev = text_marg_top * 100,
    text = paste0(round(plot_data$prev[match(c("cat1", "cat3", "cat5"), plot_data$cat)],0))
  )
  if (region_level == "Region") texts1$loc[3] <- 4 - col_width - text_marg_side
  
  texts2 <- data.frame(
    loc = 4 - col_width - text_marg_side,
    prev = plot_data$prev[match(c("cat5", "cat3", "cat1"), plot_data$cat)] + cat_sep * c(1,2,3) * 100 + 
      switch(region_level,
             "Global" = plot_data$prev[match(c("cat6", "cat4", "cat2"), plot_data$cat)]/2, # - c(12, 9.5, 13) * 1,
             "Region" = plot_data$prev[match(c("cat6", "cat4", "cat2"), plot_data$cat)]/2
      ),
    text = paste0(round(plot_data$prev[match(c("cat6", "cat4", "cat2"), plot_data$cat)],0))
  )
  
  if (region_level == "Global") {
    texts1$text <- paste0(texts1$text, "%", c(" total obesity", " severe ", "\nmorbid\n\n"))
    texts2$text <- paste0(texts2$text, "%", c("\nClass 2", "\nClass 1 ", " underweight\n"))
  }
  
  p <- ggplot(plot_data, aes(loc, -prev)) + 
    geom_col(aes(fill = cat), width = col_width) + 
    geom_col(data = plot_data %>% subset(cat %in% c("cat1", "cat3", "cat5")) %>% mutate(cat = paste0(cat,"up")),
             aes(x = loc-0.5, fill = cat), width = 1- col_width) +
    geom_diagonal_wide(data = connectors, aes(group = cat, fill = cat), strength = 0.5) +
    geom_text(data = texts1, aes(label = text), hjust = 1, vjust = v_just, size = text_size, fontface = font_face, lineheight = 1) +
    geom_text(data = texts2, aes(label = text), hjust = h_just, vjust = v_just, size = text_size, fontface = font_face, lineheight = 1) +
    scale_y_continuous(expand = expansion(mult = 0, add = 0)) +
    scale_x_reverse(expand = expansion(mult = 0, add = 0)) +
    fill_scale_sankey +
    theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank()) +
    theme(panel.background = element_blank(), panel.grid = element_blank()) +
    theme(legend.position = "none") +
    coord_flip()
  if (!is.na(gtitle)) {
    p <- p + ggtitle(gtitle)
    if (region_level == "Global")     p <- p + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 22))
    if (region_level == "Region")     p <- p + theme(plot.title = element_text(hjust = 0.5, size = 12))
  }
  return(p)
}

