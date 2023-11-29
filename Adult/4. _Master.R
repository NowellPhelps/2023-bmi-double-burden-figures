############## 4 MASTER FOR STACKED TREND PLOTS
# Source from 0.0 MASTER ALL FIGURES.R
outdir_folder <- paste0(outdir, "Velocity plots/")
dir.create(outdir_folder, showWarnings = FALSE)

scriptname      <- "4. Relative slopes.R"

variable_types <- c("obesity","severe obesity", "underweight")
slope_types    <- c("velocity", "acceleration")

script <- paste0(scriptDir, "Adult/", scriptname)

for (my_sex in sexes) {
  for (variable_type in variable_types){
    for (slope_type in slope_types){
      rstudioapi::jobRunScript(script, 
                               name = paste(my_sex, variable_type, slope_type),
                               workingDir = paste0(parentdir, "Model", modelnum, ", ", mod_dir_name, "/Postprocessing/Scripts/Figures/"),
                               importEnv = TRUE)
      Sys.sleep(10)
    }
  }
}
