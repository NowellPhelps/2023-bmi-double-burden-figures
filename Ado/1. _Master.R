# 1. Master
# Nowell Phelps 2022

scriptname      <- "1. country trend plots.R"

# Create folders if needed
dir.create(paste0(parentdir, "Model", modelnum, ", ", mod_dir_name, "/Postprocessing/Figures"), showWarnings=FALSE)
dir.create(paste0(parentdir, "Model", modelnum, ", ", mod_dir_name, "/Postprocessing/Figures/Country trend plots/"), showWarnings=FALSE)

sexes     <- c("female","male")
variables <- c("prev_bmi_2sd","prev_bmi_neg2sd","prev_bmi_neg2sd_neg1sd","prev_bmi_neg1sd_1sd","prev_bmi_1sd_2sd","prev_bmi_2sd")

script <- paste0(scriptDir,"Ado/",scriptname)

for (sex in sexes) {
  for (variable in variables){
    rstudioapi::jobRunScript(script, name = paste(variable,sex),
                             workingDir = paste0(getwd()),
                             importEnv = TRUE)
    Sys.sleep(30)
  }
}
