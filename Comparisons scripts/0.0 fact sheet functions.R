library(ggbeeswarm)

make_region_rank_one_year <- function(plot_data, my_country, my_sex, my_variable, my_year = plot.end.year, age_type, returnLeg = F, uncertainty = F){
   
   plot_region <- plot_data$Region[which(plot_data$Country == my_country)][[1]]
   
   plot_data <- plot_data %>% 
      filter(sex == my_sex, variable == my_variable, year == my_year) %>%
      filter(Region == plot_region) %>%
      mutate(ranking = dense_rank(desc(mean))) %>%
      mutate(y = nrow(.) + 1 - ranking) %>%
      mutate(x = 1) %>%
      mutate(col_ind = ifelse(Country == my_country, "1"," 0"))
   
   if(uncertainty){
      plot_data <- plot_data %>% 
         mutate(country_label = paste0(ifelse(ranking < 10, " ", ""), ranking, ". ", Country, " (", round(mean), "%, ", round(l), "-",round(u),")"))
   } else{
      plot_data <- plot_data %>% 
         mutate(country_label = paste0(ifelse(ranking < 10, " ", ""), ranking, ". ", Country, " (", round(mean), "%)"))
   }
      
   
   col_scale <- scale_color_manual(values = c("1" = "blue", "0" = "black"))
   
   p <- ggplot(plot_data, aes(x,y, colour = col_ind)) +
      geom_text(data = plot_data %>% filter(x == 1), aes(label = country_label), hjust = 0, size = 2) +
      ylab("") +
      xlab("") +
      col_scale +
      theme_classic() +
      theme(panel.grid = element_blank(),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.length = unit(0, "pt"),
            plot.title = element_text(size = 8, face = "bold")) +
      ggtitle(plot_region) +
      scale_x_continuous(breaks = c(0,1), limits = c(1,2), position = "top") +
      scale_y_continuous(limits = c(0.5, nrow(plot_data) + 0.5), expand = expansion(mult = c(0, 0)))+ 
      theme(legend.position = "none")
   
   
   if(returnLeg){
      p <- get_legend(p + theme(legend.title = element_blank())) 
   } else{
      p <- p + theme(legend.position = "none")
      
      n_names <- nrow(plot_data)
      blank <- grid.rect(gp=gpar(col=NA, fill = NA))
      
      p <- arrangeGrob(p, blank, ncol = 1, heights = c(3/20 + (n_names/20), (22-n_names)/20))
   }
   
   return(p)
}



make_highlighted_histogram <- function(plot_data, my_country, my_sex, my_variable, my_year, age_type, plotLeg = F, option = "region", order = "decreasing", orientation = "up"){
   #  option takes "country" or "region"
   # "country" highlights country only
   # "region" highlights country and other countries in region
   
   my_region <- unique(plot_data$Region[which(plot_data$Country == my_country)])[[1]]
   
   plot_data <- plot_data %>% filter(year == my_year, variable == my_variable) %>%
      mutate(col_ind = ifelse(Country == my_country, "country", "non"))
   
   ylims <- c(0, ceiling(max(plot_data$mean)))
   
   plot_data <- plot_data %>% filter(sex == my_sex)
   
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
      scale_fill_manual(values = c("country" = "blue", "region" = "grey30", "non" = "grey70"),
                        labels = c("country" = "halp", "region" = my_region, "non" = "Other"),
                        limits = c("region", "non")) +
      new_scale_fill() +
      geom_bar(plot_data %>% filter(Country == my_country),
               mapping = aes(x = rank, y = mean),
               inherit.aes = F,
               stat = "identity",
               fill = "royalblue") +
      theme_classic() +
      theme(panel.grid.major.x = element_blank(),
            axis.line = element_blank(),
            axis.text.x = element_text(size = 6),
            axis.text.y = element_text(size = 6),
            axis.title = element_text(size = 7),
            plot.title = element_text(size = 8, face = "bold")) +
      scale_y_continuous(limits = ylims, expand = c(0,0)) +
      scale_x_continuous(breaks = c(1, 50, 100, 150, 200),  limits = c(0, 201), expand = c(0,0)) +
      ggtitle(paste0("World")) +
      xlab("Country ranking")+
      ylab("Prevalence (%)")
   
   if(option == "region"){
      subdata <- plot_data %>% filter(Country == my_country)
      
      if(my_sex == "female" & age_type == "adult" & my_variable == "prev_bmi_l185"){
         if(subdata$rank >= 100){
            nudge.x <- 150 - subdata$rank
         } else{
            nudge.x <- 50 - subdata$rank
         }
         
      } else{
         nudge.x <- 100 - subdata$rank
      }
      
      p <- p + 
         coord_cartesian(clip = "off") +
         geom_text_repel(
            data = subdata,
            size = 6/.pt,
            aes(label = Country),
            force_pull   = 2, # do not pull toward data points
            nudge_y  = max(plot_data$mean)/2,
            nudge_x = nudge.x,
            color = "blue", # Needed for Geom_text not label
            direction    = "both",
            segment.curvature = ifelse(subdata$rank >= 100, -1e-20,1e-20),
            segment.angle = ifelse(subdata$rank >= 100,-20,20),
            segment.ncp = 3,
            hjust        = 0,
            segment.size = 0.2,
            max.iter = 1e7, max.time = 1)
   }
   
   
   if(plotLeg){
      # Assume option = Region not coded other possibility
      if(subdata$rank <= 100){
         legend.y <- 0.5
         legend.x <- 0.8
      } else{
         legend.y <- 0.8
         legend.x <- 0.05
      }
      
      p <- p + 
         theme(legend.title = element_blank(),
               legend.position = c(legend.x, legend.y),
               legend.text=element_text(size= 6),
               legend.key.size = unit(6, "pt"))
         
   }else{
      p <- p + theme(legend.position = "none")
   }
   
   if(orientation == "up"){
      p <- p + coord_flip() + scale_x_reverse()
   }
   
   return(p)
}


make_beeswarm_plot <- function(plot_data, my_country, my_sex, my_variable, my_year, age_type, plotLeg = F){
   #  Option takes "country" or "region"
   # "country" highlights country only
   # "region" highlights country and other countries in region
   
   my_region <- unique(plot_data$Region[which(plot_data$Country == my_country)])[[1]]
   
   plot_data <- plot_data %>% filter(year == my_year, variable == my_variable) %>%
      mutate(col_ind = ifelse(Country == my_country, "country", "non"))
   
   ylims <- c(-1, ceiling(max(plot_data$mean)+1))
   
   plot_data <- plot_data %>% filter(sex == my_sex)
   
   plot.title <- switch(my_variable, 
                        "prev_bmi_neg2sd" = "Thinness",
                        "prev_bmi_2sd" = "Obesity",
                        "prev_bmi_l185" = "Underweight",
                        "prev_bmi_30" = "Obesity")
   
   
   plot_data <- plot_data %>%
      mutate(col_ind = ifelse(!(Country == my_country) & Region == my_region, "region", col_ind))
   
   
   p <- ggplot(plot_data, aes(y = mean,  x = 1, colour = col_ind, alpha = 0.9), size = 0.1) +
      geom_beeswarm(data = plot_data %>% filter(col_ind == "non")) +
      geom_beeswarm(data = plot_data %>% filter(col_ind == "region")) +                
      scale_colour_manual(values = c("country" = "blue", "region" = "grey30", "non" = "grey70"),
                        labels = c("country" = "halp", "region" = my_region, "non" = "Other"),
                        limits = c("region", "non")) +
    new_scale_fill() +
    geom_beeswarm(data = plot_data %>% filter(Country == my_country),
            mapping = aes(y = mean, x = 1),
            size = 1,
            inherit.aes = F,
            stat = "identity",
            colour = "royalblue") +
      
   theme_classic() +
   theme(panel.grid.major.x = element_blank(),
         axis.line = element_blank(),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.text.y = element_text(size = 6),
         axis.title = element_text(size = 7),
         plot.title = element_text(size = 8, face = "bold")) +
   scale_y_continuous(limits = ylims, expand = c(0,0)) +
   scale_x_continuous(expand = c(0,0)) +
   ggtitle(paste0("All countries")) +
   xlab("") +
   ylab("Prevalence (%)") +
   theme(legend.title = element_blank())
   
   
   if(plotLeg){
      # Assume option = Region not coded other possibility
      if(subdata$rank <= 100){
         legend.y <- 0.5
         legend.x <- 0.8
      } else{
         legend.y <- 0.8
         legend.x <- 0.05
      }
      
      p <- p + 
         theme(legend.title = element_blank(),
               legend.position = c(legend.x, legend.y),
               legend.text=element_text(size= 6),
               legend.key.size = unit(6, "pt"))
      
   }else{
      p <- p + theme(legend.position = "none")
   }
   
   return(p)
}



make_mini_trends <- function(plot_data, my_country, my_sex,  age_type, plotLeg = F){
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
      levels(plot_data$variable) <- c('Combined burden','Thinness', 'Obesity')
      colscale <- col_scale_double_burden_ado
      fillscale <- fill_scale_double_burden_ado
      
   }else{
      ylims <- c(0,90)
      breaks <- c(0,20,40,60,80)
      plot_data <- plot_data %>% filter(Country == my_country & sex == my_sex  & year %in% seq(plot.start.year,plot.end.year)) %>% 
         arrange(rev(desc(variable))) %>% filter(variable %in% c("prev_double_burden","prev_bmi_l185", "prev_bmi_30"))
      plot_data$variable <- factor(plot_data$variable, levels = c("prev_double_burden","prev_bmi_l185","prev_bmi_30"))
      levels(plot_data$variable) <-c('Combined burden','Underweight', 'Obesity')
      colscale <- col_scale_double_burden
      fillscale <- fill_scale_double_burden
   }
   
   
   p <- ggplot(plot_data, aes(x = year, y = mean, colour = variable)) +
      geom_hline(yintercept = 0, linetype = 'dashed', colour = grey(0.5)) +
      geom_ribbon(aes(ymin = l, ymax = u, fill = variable), alpha = 0.1,  colour = NA) +
      geom_line() +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.text=element_text(size=10.5),
            # axis.text.x=element_text(size=10.5, angle = 45, hjust = 1),
            # legend.title=element_blank(),
            title = element_text(size = 10.5),
            legend.text = element_text(size = 10.5),
            strip.background = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour = grey(0.95)),
            panel.grid.minor.y = element_blank()) +
      scale_x_continuous(expand=c(0,0), limits = c(1989,2023), breaks = c(1990, 2000, 2010, 2020)) +
      scale_y_continuous(expand=c(0,0), limits = ylims, breaks = breaks) +
      ggtitle(plot.title) +
      ylab("Prevalence (%)")+
      colscale +
      fillscale
   
   return(p)
}

get_text_sources <- function(my_country, age_type, data_sources){
   
   n_sources_all_f  <- data_sources$N_all[which(data_sources$sex == 2  & data_sources$Country == my_country)]
   n_sources_natl_f <- data_sources$N_natl[which(data_sources$sex == 2 & data_sources$Country == my_country)]
   
   n_sources_all_m  <- data_sources$N_all[which(data_sources$sex == 1  & data_sources$Country == my_country)]
   n_sources_natl_m <- data_sources$N_natl[which(data_sources$sex == 1 & data_sources$Country == my_country)]
   
   if(age_type == "ado"){
      name.f <- "girls"
      name.m <- "boys"
   }else{
      name.f <- "women"
      name.m <- "men"
   }
   
   text <- paste0("Number of studies: ", n_sources_all_f, " (", n_sources_natl_f, " nationally representative) for ",name.f,", ", n_sources_all_m, " (", n_sources_natl_f, " nationally representative) for ", name.m, "*")
   
   return(text)
}

get_change_text <- function(pp, change, l_change, u_change, uncertainty = T){
   
   if(pp < 0.2){
      
      if (uncertainty){
         if (round(u_change) > 0){
            change_text <- paste0('<span style="color:darkgreen">a decrease</span> of ', round(abs(change)), ' (', -round(u_change),' to ',round(abs(l_change)),') percentage points')
            
         } else{
            change_text <- paste0('<span style="color:darkgreen">a decrease</span> of ', round(abs(change)), ' (', round(abs(u_change)),'-',round(abs(l_change)),') percentage points')
            
         }
      } else{
         change_text <- paste0('<span style="color:darkgreen">a decrease</span> of ', round(abs(change)), ' percentage points')
      }
      
      
      
      
   } else if(pp <= 0.8){
      change_text <- "with no distinguishable change"
   }else {
      
      if(uncertainty){
         if (round(l_change) < 0){
            change_text <- paste0('<span style="color:red">an increase</span> of ', round(change), ' (',round(l_change),' to ',round(u_change),') percentage points')
         } else{
            change_text <- paste0('<span style="color:red">an increase</span> of ', round(change), ' (',round(l_change),'-',round(u_change),') percentage points ')
            
         }
      } else{
         change_text <- paste0('<span style="color:red">an increase</span> of ', round(change), ' percentage points')
      }
      
      
      
   }
   
   return(change_text)
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

data_level
data_change
my_country
my_variable
age_type
my_sex


get_text_prevalences <- function(data_level, data_change, my_country, my_variable, age_type, my_sex){

   my_region <- unique(data_level$Region[which(data_level$Country == my_country)])[[1]]
   
   data_country <- data_level %>% filter(Country == my_country, variable == my_variable, sex == my_sex, year == plot.end.year)
   
   data_change_country <-  data_change %>% 
      filter(Country == my_country, variable == my_variable, sex == my_sex, start.year == plot.start.year, end.year == plot.end.year)
   
   ranking_data <- data_level %>% filter(variable == my_variable,sex == my_sex, year == plot.end.year) %>%
      mutate(ranking_world = rank(-mean)) %>%
      group_by(Region) %>%
      mutate(ranking_region = rank(-mean)) %>%
      ungroup()
   
   
   text1 <- paste0(
      "• ",
      round(data_country$mean),
      "% in 2022, ",
      get_change_text(data_change_country$PP.increase, data_change_country$mean, data_change_country$l, data_change_country$u, uncertainty = F),
      " from 1990. ")
   
   text2 <- paste0(
      "• Ranked ",
      ranking_data$ranking_region[which(ranking_data$Country == my_country)], 
      "/",
      max(ranking_data$ranking_region[which(ranking_data$Region == my_region)]),
      " countries in ",
      my_region, 
      " and " ,
      ranking_data$ranking_world[which(ranking_data$Country == my_country)], 
      "/200 countries in the world."
   )
   
   texttext <- arrangeGrob(gridtext::richtext_grob(text1, hjust=0, x = unit(0.02, "npc"), halign = 0,gp = gpar(col = "black", fontsize = 8)),
                           gridtext::richtext_grob(text2, hjust=0, x = unit(0.02, "npc"), halign = 0,gp = gpar(col = "black", fontsize = 8)),
                           ncol = 1)
   
   return(texttext)
}

