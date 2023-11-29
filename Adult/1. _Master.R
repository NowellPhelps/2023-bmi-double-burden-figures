scriptname      <- "1. Country trend plots.R"
outdir_folder   <- paste0(outdir, "/Country trend plots/")
dir.create(outdir, showWarnings = F)
dir.create(outdir_folder, showWarnings = F)

variables  <- c("prev_bmi_l185","prev_bmi_185_20","prev_bmi_20_25","prev_bmi_25_30","prev_bmi_30_35","prev_bmi_35_40","prev_bmi_40", "prev_bmi_30", "prev_bmi_35")
sexes <- c("female", "male")
variables <- c("prev_bmi_30", "prev_bmi_35")

script <- paste0(scriptDir, "Adult/",scriptname)

for (sex in sexes) {
    for (variable in variables) {
        rstudioapi::jobRunScript(script, name = paste(variable,sex),
                                 workingDir = outdir_folder,
                                 importEnv = TRUE)
        Sys.sleep(30)
      }
}
