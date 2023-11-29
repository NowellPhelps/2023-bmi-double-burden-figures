### 2 MASTER #######################
# Master to create wheels

outdir_folder <- paste0(outdir, "Wheel plots/")
dir.create(outdir_folder, showWarnings = FALSE)

age_groups        <- c("ageStd", "young", "mid", "old")
names(age_groups) <- c("20+ year olds","20-39 year olds","40-59 year olds","60+ year olds")

errorbar_types  <- c("none") # takes none, segment or composite
sexes           <- c("male", "female")
types           <- c("double burden", "all", "obesity") # takes "double burden" or "all" or "obesity"

for (type in types){
  scriptname      <- paste0("2. Wheel plots double burden obesity and all.R")
  script          <- paste0(functionsDir, scriptname)
  
  rstudioapi::jobRunScript(script, 
                           name = paste("Wheel plots", type),
                           workingDir = paste0(parentdir, "Model", modelnum, ", ", mod_dir_name, "/Postprocessing/Scripts/Figures"),
                           importEnv = TRUE)
  Sys.sleep(10)
}

