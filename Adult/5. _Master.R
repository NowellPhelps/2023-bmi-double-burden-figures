############## 5 MASTER FOR DECOMPOSITION PLOTS
# Source from 0.0 MASTER ALL FIGURES.R
outdir_folder <- paste0(outdir, "Decomposition plots/")
dir.create(outdir_folder, showWarnings = FALSE)

types      <- c("double burden", "obesity")
age_groups <- c("ageStd", "young", "mid", "old") 

for (type in types) {
  
  scriptname      <- paste0("5. Decomposition plots of change in double burden and obesity by country.R")
  script          <- paste0(functionsDir, scriptname)

  rstudioapi::jobRunScript(script, 
                           name = paste(type),
                           workingDir = paste0(parentdir, "Model", modelnum, ", ", mod_dir_name, "/Postprocessing/Scripts/Figures/"),
                           importEnv = TRUE)
  Sys.sleep(10)
}
