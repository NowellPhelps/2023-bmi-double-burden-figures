library(htmlwidgets)
library(htmltools)
outdir_folder <- paste0(outdir, "Velocity plots/")
dir.create(outdir_folder, showWarnings = F)
outdir_folder_int <- paste0(outdir_folder, "interactive/")
dir.create(outdir_folder_int, showWarnings = F)

if (variable_type == "prev"){
  variables <- c("prev_bmi_30", "prev_bmi_35")
  change_types <- c("absolute", "relative")
}else{
  variables <- c("mean_bmi")
  change_types <- c("absolute")
}

sexes <- c("female", "male")

data <- read_data_level(variables, sexes, age_type = "ageStd", region_level = "Country", variable_type)

subset <- data %>% 
  filter(age_group == "ageStd")

ps <- list()
plotly.ps <- list()
for(my_sex in sexes){
  for(my_variable in variables){
   for(my_change_type in change_types){
     
     for(plot.gap.year in c(1, 5, 10)){
       ps[[paste(my_variable, my_sex, my_change_type, plot.gap.year)]] <- change_dot_plot_function(subset, my_sex, my_variable, plot.start.year, plot.end.year, plot.gap.year = plot.gap.year, my_change_type = my_change_type)
      
       plotly.plot <- ggplotly(ps[[paste(my_variable, my_sex, my_change_type, plot.gap.year)]])
       
       a <- manipulateWidget::combineWidgets(plotly.plot, ncol = 1, nrow = 1, height = 700, width = 1500)
       saveWidget(a, paste0(outdir_folder_int, paste(gsub("prevalence of ", "",get_var_longname(my_variable)),my_sex, my_change_type, plot.gap.year),".html"), selfcontained = F, libdir = "lib")
       
       }
     }
    }
}

ps[["legend"]] <- change_dot_plot_function(subset, my_sex, my_variable, plot.start.year, plot.end.year, plot.gap.year = 5, returnLeg = T)
ps[[paste(my_variable, my_sex)]]

blank <- grid.rect(gp=gpar(col=NA, fill = NA))

a <- ggplotly(ps[[paste(my_variable, my_sex, my_change_type, plot.gap.year)]])

for(my_variable in variables){
  
  cairo_pdf(paste0(outdir_folder, "change ", my_variable, " over 5 year periods.pdf"), width = 15, height = 10, onefile = T)
  
  for(my_change_type in change_types){
    grid.arrange(
      arrangeGrob(arrangeGrob(ps[[paste(my_variable, "female", my_change_type, 5)]], 
                              ps[[paste(my_variable, "male", my_change_type, 5)]],
                              ncol = 1, heights = c(1,1)), 
                  blank, 
                  ps[["legend"]], 
                  blank,
                  nrow = 1, widths = c(12, .5, 3, .5))
      
    )
  }

  dev.off()
  
  cairo_pdf(paste0(outdir_folder, "change ", my_variable, " over 10 year periods.pdf"), width = 13, height = 10, onefile = T)
  for(my_change_type in change_types){
    grid.arrange(
      arrangeGrob(arrangeGrob(ps[[paste(my_variable, "female", my_change_type, 10)]], 
                              ps[[paste(my_variable, "male", my_change_type, 10)]],
                              ncol = 1, heights = c(1,1)), 
                  blank, 
                  ps[["legend"]], 
                  blank,
                  nrow = 1, widths = c(8, .5, 3, .5))
      
    )
  }

dev.off()

  cairo_pdf(paste0(outdir_folder, "change ", my_variable, " over 1 year periods.pdf"), width = 20, height = 10, onefile = T)
  for(my_change_type in change_types){
    grid.arrange(
      arrangeGrob(arrangeGrob(ps[[paste(my_variable, "female", my_change_type, 1)]], 
                              ps[[paste(my_variable, "male", my_change_type, 1)]],
                              ncol = 1, heights = c(1,1)), 
                  blank, 
                  ps[["legend"]], 
                  blank,
                  nrow = 1, widths = c(12, .5, 2, .5))
      
    )
  }
}
dev.off()
