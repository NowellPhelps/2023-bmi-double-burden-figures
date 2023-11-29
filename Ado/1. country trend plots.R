## NP country trend plots for ado BMI

library(tidyverse)
library(ggnewscale)

resultsDir <- paste0(parentdir, "Model", modelnum, ", ", mod_dir_name, "/Results/", variable, "_",sex,"/")
outdir_folder <- paste0(outdir, "Country trend plots/")
covardir <- paste0(parentdir, "Model", modelnum, ", ", mod_dir_name, "/Covariates/")

filename <- paste(paste0("Model",modelnum),sex,variable,sep = "_")

#load data
a <- readRDS(paste0(resultsDir,filename, "_Combined.RDS"))
subset <- a$subset


#load results
res <- read.csv(paste0(indir_est,filename,"_Agespecific_Country_Means.csv"))
res$model <- paste0("Model",modelnum)
results <- left_join(res, a$country.names, by=c("country"="Country"), all = T) %>%
  rename("Country"="country")
axis_range <- c(0,1)

### Plot with data points ###
ages <- seq(5, 19)

results <- results %>% filter(age_group %in% ages)
subset <- subset %>% filter(age %in% ages)

subset$age_group  <-  factor(subset$age, levels = ages)
results$age_group <-  factor(results$age_group, levels = ages)

subset <- subset %>% 
  mutate(se = sqrt((prev * (1-prev))/number)) %>%
  mutate(l = pmax(prev - 1.96*se,0)) %>%
  mutate(u = pmin(prev + 1.96*se,1))

cols <- c(rainbow(50)[30], rainbow(50)[8],rainbow(50)[46], "green")
names(cols) <- c("National","Subnational","Community", "")
colScale <- scale_colour_manual(values = cols, guide = NULL)
shapes <- c(16, 15, 17)
names(shapes) <- c("both","urban","rural")
shpScale <- scale_shape_manual(values = shapes, guide = NULL)

subset$Region      <- subset$region
subset$Superregion <- subset$sregion
subset$Country     <- subset$country
subset$year        <- subset$data_year

#labels

countrylist.order <- countrylistold[order(countrylistold$Superregion,countrylistold$Region,countrylistold$Country),]

pdf(paste0(outdir_folder, filename, "_Country_trends.pdf"), height=8, width=14)

cols_mod <- c("black")
names(cols_mod) <- paste0("Model", modelnum)
colScale_mod <- scale_colour_manual(values = cols_mod)
for (sr in split(1:nrow(countrylist.order), ceiling(1:nrow(countrylist.order) / 5))) {
  countrylist.tmp <- countrylist.order$Country[sr]
  p <- ggplot(subset[subset$Country %in% countrylist.tmp,], aes(x = year)) +
    facet_grid(Superregion + Region + Country ~ age_group) + theme_bw() 
  p <- p + geom_ribbon(data = results %>% subset(Country %in% countrylist.tmp), aes(ymin = l, ymax = u), fill = grey(0.92))
  p <- p + geom_line(data = results %>% subset(Country %in% countrylist.tmp), aes(y = mean),color="black", linewidth = 0.2)
  #p <- p + colScale_mod
  p <- p + new_scale_colour()
  p <- p + geom_errorbar(aes(ymin = l, ymax = u, colour = coverage), width = 0) + 
    geom_point(aes(y = prev, shape=scope, colour=coverage))
  p <- p + colScale + shpScale + theme(legend.position = "bottom", legend.title = element_blank(), axis.title = element_blank(), panel.grid = element_blank())
  p <- p + scale_y_continuous(expand = expansion(mult = c(0.05,0.05))) + coord_cartesian(ylim = axis_range) +  scale_x_continuous(breaks=c(1980,2000,2020))
  p <- p + theme(axis.text.x = element_text(angle = 45, vjust = 0.75))
  
  plot(p)
}

dev.off()

print('job complete')
