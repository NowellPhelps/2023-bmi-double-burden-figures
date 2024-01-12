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
      ggtitle(paste0("Prevalence in ", my_year)) +
      scale_x_continuous(expand = c(0,0), limits = c(0.5, 20.5)) +
      scale_y_continuous(expand = c(0,0), limits = c(0.5, 5.5))
   
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
      geom_text(data = plot_data %>% filter(x == 0), aes(label = Country), hjust = 1, size = 2) +
      geom_text(data = plot_data %>% filter(x == 1), aes(label = Country), hjust = 0, size = 2) +
      ylab("") +
      xlab("") +
      col_scale +
      theme_classic() +
      theme(panel.grid = element_blank(),
            axis.line = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_blank()) +
      
      scale_x_continuous(breaks = c(0,1), labels = (c("1990","2022")), limits = c(-1,2), position = "top") +
      scale_y_continuous(limits = c(0.5, 19.5), expand = c(0,0)) +
      geom_segment(data = arrow_data, aes(y = start, yend = end, color = col_ind), x = 0.1, xend = 0.9, linewidth = 0.4, alpha = 0.7,
                   arrow = arrow(length = unit(0.05, "cm"))) +
      theme(legend.position = "none")
   
   if(returnLeg){
      p <- get_legend(p + theme(legend.title = element_blank()))
   } else{
      p <- p + theme(legend.position = "none")
   }
   
   return(p)
}


make_highlighted_histogram <- function(plot_data, my_country, my_sex, my_variable, my_year, age_type, returnLeg = F, option = "country", order = "decreasing"){
   #  option takes "country" or "region"
   # "country" highlights country only
   # "region" highlights country and other countries in region
   
   my_region <- unique(plot_data$Region[which(plot_data$Country == my_country)])[[1]]
   
   plot_data <- plot_data %>% filter(sex == my_sex, year == my_year, variable == my_variable) %>%
      mutate(col_ind = ifelse(Country == my_country, "country", "non"))
   
   if (order == "increasing"){
      plot_data <- plot_data %>%
         mutate(rank = rank(mean))
   } else{
      plot_data <- plot_data %>%
         mutate(rank = rank(desc(mean)))
   }
   
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
      scale_fill_manual(values = c("country" = "red", "region" = "grey30", "non" = "grey70")) +
      theme_classic() +
      theme(panel.grid.major.x = element_blank(),
            axis.line = element_blank(),
            axis.text.x = element_text(size = 6),
            axis.text.y = element_text(size = 6),
            axis.title = element_text(size = 7),
            plot.title = element_text(size = 8, face = "bold")) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      scale_x_continuous(breaks = c(1,50, 100, 150, 200), expand = c(0,0)) +
      ggtitle(plot.end.year)+
      xlab("Country ranking")+
      ylab("Prevalence (%)")
   
   if(returnLeg){
      p <- get_legend(p + theme(legend.title = element_blank()))
   } else{
      p <- p + theme(legend.position = "none")
   }
   
   return(p)
}

get_source_text<- function(N_all, N_natl, my_country, my_sex, age_type){
   
   if(age_type == "ado"){
      if(my_sex == "female"){
         group_name <- "girls"
      } else{
         group_name <- "boys"
      }
   } else{
      if(my_sex == "female"){
         group_name <- "women"
      } else{
         group_name <- "men"
      }
   }
   
   if(N_all == 0){
      text <- paste0("No studies")
   } else if(N_all ==  1){
      text <- paste0("• ", N_all, " study (", N_natl, " ", ifelse(N_natl == 1, "is", "are")," nationally representative)*")
   } else{
      text <- paste0("• ", N_all, " studies (", N_natl, " ",ifelse(N_natl == 1, "is", "are")," nationally representative)*")
   }
   return(text)
}

get_change_text <- function(pp){
   
   if(pp < 0.2){
      change_text <- "a decrease"
   } else if(pp <= 0.8){
      change_text <- "with no observable change"
   }else {
      change_text <- "an increase"
   }
   
   return(change_text)
}

get_text_description <- function(data_level, data_change, my_country, my_sex, age_type, n_sources_all, n_sources_natl){
   
   # get text about source numbers
   text_sources <- get_source_text(n_sources_all, n_sources_natl, my_country, my_sex, age_type)
   
   my_region <- unique(data_level$Region[which(data_level$Country == my_country)])[[1]]
   
   data_country <- data_level %>% filter(Country == my_country, sex == my_sex, year == plot.end.year)
   
   ranking_data <- data_level %>% filter(sex == my_sex, year == plot.end.year) %>%
      group_by(variable) %>%
      mutate(ranking_world = rank(-mean)) %>%
      ungroup() %>%
      group_by(Region, variable) %>%
      mutate(ranking_region = rank(-mean)) %>%
      ungroup()
   
   data_change_country <-  data_change %>% filter(Country == my_country, sex == my_sex, start.year == plot.start.year, end.year == plot.end.year)
   
   if (age_type == "adult"){
      prev_underweight <- data_country$mean[which(data_country$variable == "prev_bmi_l185")]
      prev_obesity <- data_country$mean[which(data_country$variable == "prev_bmi_30")]
      
      underweight.pp.increase <-  data_change_country$PP.increase[which(data_country$variable == "prev_bmi_l185")]
      obesity.pp.increase <-  data_change_country$PP.increase[which(data_country$variable == "prev_bmi_30")]
      
      rank_world_underweight <- ranking_data$ranking_world[which(ranking_data$Country == my_country & ranking_data$variable == "prev_bmi_l185")]
      rank_world_obesity <- ranking_data$ranking_world[which(ranking_data$Country == my_country & ranking_data$variable == "prev_bmi_30")]
      
      rank_region_underweight <- ranking_data$ranking_region[which(ranking_data$Country == my_country & ranking_data$variable == "prev_bmi_l185")]
      rank_region_obesity <- ranking_data$ranking_region[which(ranking_data$Country == my_country & ranking_data$variable == "prev_bmi_30")]
      
      underweight_name <- "underweight"
   
   } else{
      prev_underweight <- data_country$mean[which(data_country$variable == "prev_bmi_neg2sd")]
      prev_obesity <- data_country$mean[which(data_country$variable == "prev_bmi_2sd")]
      
      underweight.pp.increase <-  data_change_country$PP.increase[which(data_country$variable == "prev_bmi_neg2sd")]
      obesity.pp.increase <-  data_change_country$PP.increase[which(data_country$variable == "prev_bmi_2sd")]
      
      rank_world_underweight <- ranking_data$ranking_world[which(ranking_data$Country == my_country & ranking_data$variable == "prev_bmi_neg2sd")]
      rank_world_obesity <- ranking_data$ranking_world[which(ranking_data$Country == my_country & ranking_data$variable == "prev_bmi_2sd")]
      
      rank_region_underweight <- ranking_data$ranking_region[which(ranking_data$Country == my_country & ranking_data$variable == "prev_bmi_neg2sd")]
      rank_region_obesity <- ranking_data$ranking_region[which(ranking_data$Country == my_country & ranking_data$variable == "prev_bmi_2sd")]
      
      underweight_name <- "thinness"
   }
   
   text1 <- paste0("• Age standardised prevalence of ", underweight_name, " in ", my_country, " was ", round(prev_underweight),"%", " in 2022, ",get_change_text(underweight.pp.increase)," since 1990. It was ranked ", rank_world_underweight, " in the world (", rank_region_underweight, " in ", my_region,")")
   text2 <- paste0("• Age standardised prevalence of obesity was ", round(prev_obesity),"%", " in 2022, ", get_change_text(obesity.pp.increase), " since 1990. It was ranked ", rank_world_obesity, " in the world (", rank_region_obesity, ") in ", my_region,")")
   
   texttext <- arrangeGrob(textGrob(text_sources, hjust=0, just = c("left"),x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 9)),
                           textGrob(text1, hjust=0, just = c("left"),x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 9)),
                           textGrob(text2, hjust=0, just = c("left"),x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 9)),
                           ncol = 1)
   
   return(texttext)
}

