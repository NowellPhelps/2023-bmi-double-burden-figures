
make_bin_plot <- function(plot_data, my_country, my_sex, my_variable, my_year, age_type, returnLeg = F){
   #  Option takes "country" or "region"
   # "country" highlights country only
   # "region" highlights country and other countries in region
   
   if(my_variable == "prev_bmi_30"){
      bin.width <- 5
      centre <- 2.5
   } else {
      bin.width <- 2
      centre <- 1
   }
   
   my_region <- unique(plot_data$Region[which(plot_data$Country == my_country)])[[1]]
   
   plot_data <- plot_data %>% filter(year == my_year, variable == my_variable) %>%
      mutate(col_ind = ifelse(Country == my_country, "country", "non"))
   
   ylims <- c(0, ceiling(max(plot_data$mean)/bin.width)*bin.width)
   
   plot_data <- plot_data %>% filter(sex == my_sex)
   
   plot.title <- switch(my_variable,
                        "prev_bmi_neg2sd" = "Thinness",
                        "prev_bmi_2sd" = "Obesity",
                        "prev_bmi_l185" = "Underweight",
                        "prev_bmi_30" = "Obesity")
   
   
   plot_data <- plot_data %>%
      mutate(col_ind = ifelse(!(Country == my_country) & Region == my_region, "region", col_ind))
   
   plot_data$col_ind <- factor(plot_data$col_ind,
                               levels = c("country", "region", "non"))
   
   
   
   p <- ggplot(plot_data, aes(y = mean, fill = col_ind)) +
      geom_histogram(binwidth = bin.width, center = centre) +
      scale_fill_manual(values = c("country" = "#F39200", "region" = "grey30", "non" = "#666666"),
                        labels = c(my_country, paste0("Other countries in\n", my_region), "Other"),
                        limits = c("country", "region")) +
      theme_classic() +
      theme(panel.grid.major.x = element_blank(),
            axis.line = element_blank(),
            axis.text = element_text(size = 6),
            axis.title = element_text(size = 7),
            plot.title = element_text(size = 8, face = "bold")) +
      scale_y_continuous(limits = ylims, expand = c(0,0)) +
      scale_x_continuous(expand = c(0,0)) +
      ggtitle(paste0("World")) +
      xlab("Number of Countries") +
      ylab("Prevalence (%)") +
      theme(legend.position = "none")
   
   
   if(returnLeg){
      # Assume option = Region not coded other possibility
      p <- p +
         guides(fill=guide_legend(title="Legend")) +
         theme(legend.background = element_rect(linetype = 2, linewidth = 0.5, colour = "black"),
               legend.position = "right",
               legend.text=element_text(size= 8),
               legend.title=element_text(size= 9),
               legend.key.size = unit(6, "pt"))
      
      p <- cowplot::get_legend(p)
   } else{
      p <- p + theme(legend.position = "none")
   }
   
   return(p)
}

make_mini_trends <- function(plot_data, my_country, my_sex,  age_type, returnLeg = F, year_overtaking = T){
   #  Option takes "country" or "region"
   # "country" highlights country only
   # "region" highlights country and other countries in region
   
   if(age_type == "ado"){
      ylims <- c(0,56)
      breaks <- c(0,10,20,30,40,50)
      plot_data <- plot_data %>% filter(Country == my_country & sex == my_sex  & year %in% seq(plot.start.year,plot.end.year)) %>%
         arrange(rev(desc(variable))) %>%
         filter(variable %in% c("prev_double_burden_ado","prev_bmi_2sd", "prev_bmi_neg2sd"))
      
      plot_data$variable <- factor(plot_data$variable, levels = c("prev_double_burden_ado","prev_bmi_neg2sd","prev_bmi_2sd"))
      levels(plot_data$variable) <- c('Double burden','Thinness', 'Obesity')
      colscale <- col_scale_double_burden_ado
      fillscale <- fill_scale_double_burden_ado
      
   } else {
      ylims <- c(0,90)
      breaks <- c(0,20,40,60,80)
      plot_data <- plot_data %>% filter(Country == my_country & sex == my_sex  & year %in% seq(plot.start.year,plot.end.year)) %>%
         arrange(rev(desc(variable))) %>% filter(variable %in% c("prev_double_burden","prev_bmi_l185", "prev_bmi_30"))
      plot_data$variable <- factor(plot_data$variable, levels = c("prev_double_burden","prev_bmi_l185","prev_bmi_30"))
      levels(plot_data$variable) <-c('Double burden','Underweight', 'Obesity')
      colscale <- col_scale_double_burden
      fillscale <- fill_scale_double_burden
   }
   
   ### New code for year overtaking
   
   plot.title <- ifelse(age_type == "ado",
                        ifelse(my_sex == "female", "Girls", "Boys"),
                        ifelse(my_sex == "female", "Women", "Men"))
   
   p <- ggplot(plot_data, aes(x = year, y = mean, colour = variable)) +
      geom_hline(yintercept = 0, linetype = 'dashed', colour = grey(0.5)) +
      geom_ribbon(aes(ymin = l, ymax = u, fill = variable), alpha = 0.1,  colour = NA) +
      geom_line() +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_text(size = 8),
            axis.text.y=element_text(size=8),
            axis.text.x=element_text(size=8, angle = 45, hjust = 1),
            title = element_text(size = 10.5),
            strip.background = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = grey(0.95)),
            panel.grid.minor.y = element_blank()) +
      scale_x_continuous(expand=c(0,0), limits = c(1989,2023), breaks = c(1990, 2020)) +
      scale_y_continuous(expand=c(0,0), limits = ylims, breaks = breaks) +
      ggtitle(plot.title) +
      ylab("Prevalence (%)")+
      colscale +
      fillscale
   
   if(returnLeg){
      p <- p +
         theme(legend.title = element_blank(),
               legend.key = unit(1,"cm"),
               legend.text = element_text(size = 5))
      p <- get_legend(p)
   } else{
      p <- p + theme(legend.position = "none")
   }
   
   return(p)
}

# plot_data <- data_level
# rank_data = data_ranking
# my_year = plot.end.year
# returnLeg = F
# rank_plot = T

make_region_rank_one_year <- function(plot_data, rank_data = NULL, my_country, my_sex, my_variable, my_year = plot.end.year, age_type, returnLeg = F, rank_plot = F){
   
   plot_region <- plot_data$Region[which(plot_data$Country == my_country)][[1]]
   
   plot_data <- plot_data %>%
      filter(sex == my_sex, variable == my_variable, year == my_year) %>%
      mutate(ranking_global = rank(desc(mean))) %>%
      filter(Region == plot_region) %>%
      mutate(ranking = rank(desc(mean))) %>%
      mutate(y = nrow(.) + 1 - ranking) %>%
      mutate(x = 0.9) %>%
      mutate(col_ind = ifelse(Country == my_country, "country","region"))
   
   n_names <- nrow(plot_data)
   
   if(n_names >= 12){
      text_size = 2
   } else {
      text_size = 2.25
   }
   
   plot_data_ranking <- data_ranking %>%
      filter(sex == my_sex, variable == my_variable, year == my_year) %>%
      filter(Region == plot_region) %>%
      dplyr::rename(mean_ranking = mean, l_ranking = l, u_ranking = u, median_ranking = median)
   
   plot_data <- merge(plot_data, plot_data_ranking)  %>%
      rowwise() %>%
      #     mutate(country_label = paste0(ifelse(ranking_global < 10, "  ", ifelse(ranking_global < 100, " ", "")), ranking_global, ". ", Country, " (", round(mean), "%)"))
      mutate(country_label = paste0(Country, " (", round(mean), "%; ", ordinal_suffix(ranking_global),")"))
   
   col_scale <-  scale_colour_manual(values = c("country" = "#F39200", "region" =  "grey30", "non" = "#666666"),
                                     labels = c("country" = "halp", "region" = plot_region, "non" = "Other countries"))
   
   text_data <- data.frame(y = nrow(plot_data) + 1, x = 0.9, label = "Country (prevalence; global ranking)", col_ind = 0)
   
   p <- ggplot(plot_data, aes(x,y, colour = col_ind)) +
      geom_text(data = plot_data %>% filter(!(Country == my_country)), aes(label = country_label), hjust = 0, size = text_size) +
      geom_text(data = plot_data %>% filter(Country == my_country), aes(label = country_label), hjust = 0, size = text_size, fontface="bold") +
      ylab("") +
      xlab("") +
      col_scale +
      coord_cartesian(clip = 'off') +
      theme_classic() +
      theme(panel.grid = element_blank(),
            axis.line = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.length = unit(0, "pt"),
            plot.title = element_text(size = 8, face = "bold")) +
      theme(legend.position = "none",
            title = element_blank())
   
   
   
   # set right margin of ranking plots according to region
   # so that spacing is minimised yet no texts are cut
   right_margin <- switch(plot_region,
                          `The Caribbean` = 2,
                          `South Asia` = 0,
                          `Southwestern Europe` = 0,
                          `Southern Latin America` = 0,
                          `Central Latin America` = 0,
                          `High-income English-speaking countries` = 0,
                          `Central Asia` = 0,
                          `Eastern Europe` = 0,
                          `Andean Latin America` = 0,
                          `Central and southern Africa` = 0,
                          `East Asia and the Pacific` = 0,
                          `Melanesia` = 0,
                          `Other sub-Saharan Africa` = 0,
                          -15
   )
   
   region_lab <- paste0("\n", plot_region, strrep(" ", max(27 - floor(right_margin/2 + nchar(plot_region)/2), 0)),"\n")
   
   if(plot_region == "High-income English-speaking countries"){
      region_lab<- paste0("High-income",strrep(" ", 27 - floor(right_margin/2 + nchar("High-income")/2)),
                          "\nEnglish-speaking countries", strrep(" ", 27 - floor(right_margin/2)- floor(nchar("English-speaking countries")/2)),"\n")
   }
   
   if(plot_region == "Other sub-Saharan Africa"){
      region_lab <- "  "
   }
   
   
   if(rank_plot){
      p <- p +
         geom_vline(xintercept = 1/(200/.8), colour = "grey80", linetype = "dashed", linewidth = 0.2) +
         geom_vline(xintercept = 100/(200/.8), colour = "grey80", linetype = "dashed", linewidth = 0.2) +
         geom_vline(xintercept = 200/(200/.8), colour = "grey80", linetype = "dashed", linewidth = 0.2) +
         geom_point(data = plot_data,
                    inherit.aes = F,
                    aes(y = y, x = ranking_global/(200/.8), colour = col_ind),
                    size = 0.8) +
         geom_segment(data = plot_data,
                      inherit.aes = F,
                      aes(y = y, yend = y, x = l_ranking/(200/.8), xend = u_ranking/(200/.8), colour = col_ind),
                      linewidth = 0.3) +
         theme(plot.margin = margin(r = right_margin, t = 5, b = 5)) +
         theme(axis.text.x = element_text(size = text_size*2.85, hjust = 1),
               axis.title.x = element_blank()) +
         #               axis.title.x = element_text(size = 7, hjust = 0)) +
         #         xlab(paste0("Global ranking                               ", plot_region)) +
         scale_x_continuous(limits = c(-0.2,2.1), breaks = c(0.005*.8, 0.5*.8, 1*.8, 2.1), labels = c("(highest\nprevalence)\n1","\n100", "(lowest\nprevalence)\n200", region_lab), position = "top") +
         scale_y_continuous(limits = c(0.5, nrow(plot_data) + 1), expand = expansion(mult = c(0, 0))) +
         geom_text(data = text_data, inherit.aes = F, aes(x=x, y = y, label = label), hjust = 0, size = text_size, fontface = "bold")
      
   } else{
      p <- p + theme(axis.text.x = element_blank(),
                     axis.title.x = element_blank())
   }
   
   if(returnLeg){
      p <- get_legend(p + theme(legend.title = element_blank()))
   } else{
      p <- p + theme(legend.position = "none")
      
      
      blank <- grid.rect(gp=gpar(col=NA, fill = NA))
      
   }
   
   return(p)
}



make_mini_trends <- function(plot_data, my_country, my_sex,  age_type, returnLeg = F){
   #  Option takes "country" or "region"
   # "country" highlights country only
   # "region" highlights country and other countries in region
   
   if(age_type == "ado"){
      ylims <- c(0,56)
      breaks <- c(0,10,20,30,40,50)
      plot_data <- plot_data %>% filter(Country == my_country & sex == my_sex  & year %in% seq(plot.start.year,plot.end.year)) %>%
         arrange(rev(desc(variable))) %>%
         filter(variable %in% c("prev_double_burden_ado","prev_bmi_2sd", "prev_bmi_neg2sd"))
      
      plot_data$variable <- factor(plot_data$variable,
                                   levels = c("prev_double_burden_ado","prev_bmi_neg2sd","prev_bmi_2sd"),
                                   labels = c('Double burden','Thinness', 'Obesity'))
      levels(plot_data$variable) <- c('Double burden','Thinness', 'Obesity')
      colscale <- col_scale_double_burden_ado
      fillscale <- fill_scale_double_burden_ado
      
   } else{
      ylims <- c(0,90)
      breaks <- c(0,20,40,60,80)
      plot_data <- plot_data %>% filter(Country == my_country & sex == my_sex  & year %in% seq(plot.start.year,plot.end.year)) %>%
         arrange(rev(desc(variable))) %>% filter(variable %in% c("prev_double_burden","prev_bmi_l185", "prev_bmi_30"))
      plot_data$variable <- factor(plot_data$variable,
                                   levels = c("prev_double_burden","prev_bmi_l185","prev_bmi_30"),
                                   labels = c('Double burden','Underweight', 'Obesity'))
      levels(plot_data$variable) <- c('Double burden','Underweight', 'Obesity')
      colscale <- col_scale_double_burden
      fillscale <- fill_scale_double_burden
   }
   
   plot.title <- ifelse(age_type == "ado",
                        ifelse(my_sex == "female", "Girls", "Boys"),
                        ifelse(my_sex == "female", "Women", "Men"))
   
   p <- ggplot(plot_data, aes(x = year, y = mean, colour = variable)) +
      geom_hline(yintercept = 0, linetype = 'dashed', colour = grey(0.5)) +
      geom_ribbon(aes(ymin = l, ymax = u, fill = variable), alpha = 0.1,  colour = NA) +
      geom_line() +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_text(size = 8),
            axis.text.y=element_text(size=8),
            axis.text.x=element_text(size=8, angle = 45, hjust = 1),
            title = element_text(size = 10.5),
            legend.text = element_text(size = 8),
            strip.background = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = grey(0.95)),
            panel.grid.minor.y = element_blank()) +
      scale_x_continuous(expand=c(0,0), limits = c(1989,2023), breaks = c(1990, 2020)) +
      scale_y_continuous(expand=c(0,0), limits = ylims, breaks = breaks) +
      ggtitle(plot.title) +
      ylab("Prevalence (%)")+
      colscale +
      fillscale
   
   if(returnLeg){
      p <- p + theme(legend.title = element_blank())
      p <- get_legend(p)
   } else{
      p <- p + theme(legend.position = "none")
   }
   
   return(p)
}

get_text_sources <- function(my_country, data_sources_adult, data_sources_ado){
   
   n_sources_all_women  <- data_sources_adult$N_all[which(data_sources_adult$sex == 2  & data_sources_adult$Country == my_country)]
   n_sources_all_men  <- data_sources_adult$N_all[which(data_sources_adult$sex == 1  & data_sources_adult$Country == my_country)]
   
   n_sources_all_girls  <- data_sources_ado$N_all[which(data_sources_ado$sex == 2  & data_sources_ado$Country == my_country)]
   n_sources_all_boys  <- data_sources_ado$N_all[which(data_sources_ado$sex == 1  & data_sources_ado$Country == my_country)]
   
   text <- paste0(my_country, " had ", n_sources_all_women, " ",ifelse(n_sources_all_women == 1, "study", "studies")," for women, ", n_sources_all_men, " for men, ", n_sources_all_girls, " for girls, and ", n_sources_all_boys, " for boys.")
   
   if (min(c(n_sources_all_women,n_sources_all_men,n_sources_all_girls,n_sources_all_boys)) < 1){
      text2 <- paste0("For age-sex groups with no data, results are inferred from overall global data based on proximity.")
      text  <- paste(text, text2)
   }
   
   return(text)
}

ordinal_suffix <- function(n) {
   if (n %% 100 %in% c(11, 12, 13)) {
      suffix <- "th"
   } else {
      if(n %%10 == 0){
         suffix <- "th"
      } else{
         suffix <- switch(n %% 10, "st", "nd", "rd", "th", "th","th","th","th","th","th")
      }
      
   }
   
   return(paste0(n, suffix))
}

get_text_prevalences <- function(data_level, data_change, data_numbers, my_country, my_variable, age_type, my_sex){
   
   my_region <- unique(data_level$Region[which(data_level$Country == my_country)])[[1]]
   
   data_country <- data_level %>% filter(Country == my_country, variable == my_variable, sex == my_sex, year == plot.end.year)
   
   data_change_country <-  data_change %>%
      filter(Country == my_country, variable == my_variable, sex == my_sex, start.year == plot.start.year, end.year == plot.end.year)
   
   ranking_data <- data_level %>% filter(variable == my_variable,sex == my_sex, year == plot.end.year) %>%
      mutate(ranking_world = rank(-mean)) %>%
      group_by(Region) %>%
      mutate(ranking_region = rank(-mean)) %>%
      ungroup()
   
   group_name <- ifelse(age_type == "adult",
                        ifelse(my_sex == "male", "men", "women"),
                        ifelse(my_sex == "male", "boys", "girls"))
   
   
   var.name<- switch(my_variable,
                     "prev_bmi_neg2sd" = "thinness",
                     "prev_bmi_2sd" = "obesity",
                     "prev_bmi_l185" = "underweight",
                     "prev_bmi_30" = "obesity")
   
   ## get offset number of sf rounding:
   prev_val   <- format(round(data_country$mean,1), nsmall = 1)
   change_val <- format(round(abs(data_change_country$mean),1), nsmall = 1)
   
   if(data_change_country$PP.increase < 0.2){
      #change_text <- paste0('<span style="color:darkgreen">a decrease</span> of ', change_val, ' percentage points')
      change_text <- paste0('a decrease of ', change_val, ' percentage points')
   } else if(data_change_country$PP.increase <= 0.8){
      change_text <- "with no detectable change"
      
   } else {
      #change_text <- paste0('<span style="color:red">an increase</span> of ', change_val,  ' percentage points')
      change_text <- paste0('an increase of ', change_val, ' percentage points')
   }
   
   text1 <- paste0(
      "• ",
      prev_val,
      "% prevalence, ",
      change_text)
   
   text2 <- "   from 1990."
   
   numbers_exist <- !(my_country %in% c("Greenland","Bermuda","American Samoa","Tokelau"))
   
   if(numbers_exist){
      
      number_country <- data_numbers %>%
         filter(Country == my_country, variable == my_variable, sex == my_sex, year == plot.end.year)
      number_country <- number_country$mean
      
      if(number_country > 10^6){
         number_country <- paste0(signif(number_country/10^6, 2), " million")
      } else if (number_country >= 100) {
         number_country <- format(signif(number_country, 2), big.mark = ",", trim = TRUE, scientific = F)
      } else{
         number_country <- "Less than 100"
      }
      
      text3 <- paste0("• ", number_country, " ", group_name, " with ", var.name, ".")
   } else{
      text3 <- " "
   }
   
   if(numbers_exist){
      texttext <- arrangeGrob(textGrob(text3, hjust=0, x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 8)),
                              textGrob(text1, hjust=0, x = unit(0.02, "npc"),gp = gpar(col = "black", fontsize = 8)),
                              textGrob(text2, hjust=0, x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 8)),
                              
                              ncol = 1)
   } else{
      texttext <- arrangeGrob(textGrob(text1, hjust=0, x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 8)),
                              textGrob(text2, hjust=0, x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 8)),
                              textGrob(text3, hjust=0, x = unit(0.02, "npc"), gp = gpar(col = "black", fontsize = 8)),
                              ncol = 1)
   }
   
   
   return(texttext)
}
