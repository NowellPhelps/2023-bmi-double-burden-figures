### 2 MASTER #######################
# Master to create wheels
outdir_folder <- paste0(outdir, "Wheel plots/")
dir.create(outdir_folder, showWarnings = FALSE)

age_groups        <- c("ageStd")
names(age_groups) <- c("age standardised 5-19")

errorbar_types  <- c("none") # takes none, segment or composite
types           <- c("ado double burden", "ado all") # takes "double burden" or "all" or "obesity"

for (type in types){
  scriptname      <- paste0("2. Wheel plots double burden and all ado.R")
  script          <- paste0(functionsDir, scriptname)
  
  rstudioapi::jobRunScript(script, 
                           name = paste("Wheel plots", type),
                           workingDir = paste0(parentdir, "Model", modelnum, ", ", mod_dir_name, "/Postprocessing/Scripts/Figures"),
                           importEnv = TRUE)
  Sys.sleep(10)
}

