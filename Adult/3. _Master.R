############## 3 MASTER FOR STACKED TREND PLOTS
# Source from 0.0 MASTER ALL FIGURES.R
outdir_folder <- paste0(outdir, "Stacked trends/")
dir.create(outdir_folder, showWarnings = FALSE)

types <- c("all", "double burden")
region_levels <- c("Country", "Region")
region_levels <- c("Country")

for (region_level in region_levels){
  for (my_sex in sexes) {
    for (type in types) {
      
      scriptname      <- paste0("3. Trends in all, obesity, and double burden by ", region_level, ".R")
      script          <- paste0(functionsDir, scriptname)
      
      rstudioapi::jobRunScript(script, 
                               name = paste(region_level, my_sex, type),
                               workingDir = paste0(maindir,"Postprocessing/Scripts/Figures"),
                               importEnv = TRUE)
      Sys.sleep(10)
    }
  }
}
