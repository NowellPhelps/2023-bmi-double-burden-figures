# plot_data <- data_level 
# my_sex <- "female"
# my_year <- 2022
# age_group <- "adult"
# option <-  "stacked"

make_dotplot <- function(plot_data, my_country, my_sex, my_year, age_type, returnLeg = F, option = "stacked"){
   # option takes "stacked" or "repel"
   # stacked gives underweight under obesity
   # repel gives underweight on left and obesity on right
   
   plot_data <- plot_data %>% filter(Country == my_country, sex == my_sex, year == my_year)
   
   data_dots <- data.frame(x = rep(c(1:20), each = 5),
                           y = rep(c(1:5), 20),
                           id = rep(1:100),
                           fill = rep("white", 100))
   
   if (age_type == "ado"){
      val_underweight <- round(plot_data$mean[which(plot_data$variable == "prev_bmi_neg2sd")])
      val_obesity     <- round(plot_data$mean[which(plot_data$variable == "prev_bmi_2sd")])
      val_db          <- round(plot_data$mean[which(plot_data$variable == "prev_bmi_neg2sd")] + plot_data$mean[which(plot_data$variable == "prev_bmi_2sd")])
      
   } else{
      val_underweight <- round(plot_data$mean[which(plot_data$variable == "prev_bmi_l185")])
      val_obesity     <- round(plot_data$mean[which(plot_data$variable == "prev_bmi_30")])
      val_db          <- round(plot_data$mean[which(plot_data$variable == "prev_bmi_l185")] + plot_data$mean[which(plot_data$variable == "prev_bmi_30")])
   }
   
   if (option == "stacked"){
      
      data_dots <- data_dots %>%
         mutate(fill = ifelse(id <= val_underweight, "uw",
                              ifelse(id <= val_underweight + val_obesity, "ob", 
                                     "non")))
      
      
   } else if (option == "repel"){
      
      data_dots <- data_dots %>%
         mutate(fill = ifelse(id <= val_underweight, "uw",
                              ifelse(id >= 100-val_obesity, "ob", 
                                     "non")))
      
      
   } else{
      message("invalid value for argument 'option' supplied")
   }
   
   if (age_type == "adult"){
      fill_scale <- scale_fill_manual(values = c("non" = "white", "uw"="#00CED1","ob"= "#EB0358"), 
                                      labels = c("non" = "Neither", "uw" = "Underweight","ob"= "Obesity"))
   } else{
      fill_scale <- scale_fill_manual(values = c("non" = "white", "uw"="#00CED1","ob"= "#EB0358"), 
                                      labels = c("non" = "Neither", "uw" = "Thinness","ob"= "Obesity"))
   }
   
   p <- ggplot(data = data_dots, aes(x,y, fill = fill)) +
      geom_point(size = 3, shape = 21) +
      fill_scale +
      ylab("") +
      xlab("") +
      theme_classic() +
      theme(panel.grid = element_blank(),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(size = 8, face = "bold")) +
      ggtitle(my_year)
   
   if(returnLeg){
      p <- get_legend(p + theme(legend.title = element_blank()))
   } else{
      p <- p + theme(legend.position = "none")
   }

   return(p)
}

make_region_rank_plot <- function(plot_data, my_country, my_sex, my_variable, plot.start.year = plot.start.year, plot.end.year = plot.end.year, age_type, returnLeg = F, option = "stacked"){
   # option takes "stacked" or "repel"
   # stacked gives underweight under obesity
   # repel gives underweight on left and obesity on right
   
   plot_region <- plot_data$Region[which(plot_data$Country == my_country)][[1]]
   
   plot_data <- plot_data %>% 
      filter(sex == my_sex, variable == my_variable, year %in% c(plot.start.year, plot.end.year)) %>%
      filter(Region == plot_region) %>%
      group_by(year) %>%
      mutate(ranking = dense_rank(desc(mean))) %>%
      ungroup() %>%
      mutate(y = ranking) %>%
      mutate(x = ifelse(year == plot.start.year, 0, 1)) %>%
      mutate(col_ind = ifelse(Country == my_country, "1"," 0"))
   
   arrow_data <- plot_data %>%
      mutate(year = ifelse(year == plot.start.year, "start", "end")) %>%
      select(year, Country, col_ind, y) %>%
      pivot_wider(names_from = c(year), values_from = y)
   
   col_scale <- scale_color_manual(values = c("1" = "black", "0" = "grey50"))
   
   plot.title <- switch(my_variable, 
                        "prev_bmi_neg2sd" = "Thinness",
                        "prev_bmi_2sd" = "Obesity",
                        "prev_bmi_l185" = "Underweight",
                        "prev_bmi_30" = "Obesity")
   
   p <- ggplot(plot_data, aes(x,y, colour = col_ind)) +
      geom_text(data = plot_data %>% filter(x == 0), aes(label = Country), hjust = 1, size = 2.5) +
      geom_text(data = plot_data %>% filter(x == 1), aes(label = Country), hjust = 0, size = 2.5) +
      ylab("") +
      xlab("") +
      col_scale +
      theme_classic() +
      theme(panel.grid = element_blank(),
            axis.line = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(size = 8, face = "bold"))+
      
      scale_x_continuous(breaks = c(0,1), labels = (c("1990","2022")), limits = c(-1,2), position = "top") +
      geom_segment(data = arrow_data, aes(y = start, yend = end, color = col_ind), x = 0.1, xend = 0.9, linewidth = 0.4, alpha = 0.7,
                   arrow = arrow(length = unit(0.05, "cm"))) +
      theme(legend.position = "none") +
      ggtitle(plot.title)
   
   if(returnLeg){
      p <- get_legend(p + theme(legend.title = element_blank()))
   } else{
      p <- p + theme(legend.position = "none")
   }
   
   return(p)
}

plot_data <- data_level

make_highligted_histogram <- function(plot_data, my_country, my_sex, my_variable, my_year, age_type, returnLeg = F, option = "country"){
   #  option takes "country" or "region"
   # "country" highlights country only
   # "region" highlights country and other countries in region
   
   my_region <- unique(plot_data$Region[which(plot_data$Country == my_country)])[[1]]
   
   plot_data <- plot_data %>% filter(sex == my_sex, year == my_year, variable == my_variable) %>%
      mutate(rank = rank(mean)) %>%
      mutate(col_ind = ifelse(Country == my_country, "country", "non"))
   
   plot.title <- switch(my_variable, 
                        "prev_bmi_neg2sd" = "Thinness",
                        "prev_bmi_2sd" = "Obesity",
                        "prev_bmi_l185" = "Underweight",
                        "prev_bmi_30" = "Obesity")
   
   if(option == "region"){
      plot_data <- plot_data %>%
         mutate(col_ind = ifelse(!(Country == my_country) & Region == my_region, "region", col_ind))
   }
   
   p <- ggplot(plot_data, aes(x = rank, y = mean, fill = col_ind)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("country" = "black", "region" = "grey30", "non" = "grey60")) +
      theme_classic() +
      theme(panel.grid = element_blank(),
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(size = 8, face = "bold")) +
      ylab("(%)")+
      ggtitle(plot.title)
   
   if(returnLeg){
      p <- get_legend(p + theme(legend.title = element_blank()))
   } else{
      p <- p + theme(legend.position = "none")
   }
   
   return(p)
}

