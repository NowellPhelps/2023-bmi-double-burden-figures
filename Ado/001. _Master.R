library(tidyverse)
scriptname      <- "001. Age specific trends by region ado new.R"
outdir_folder <- paste0(outdir, "Spaghetti/")
dir.create(outdir_folder, showWarnings = FALSE)

sexes <- c("female","male")
variables <- c("prev_bmi_neg2sd", "prev_bmi_neg2sd_neg1sd", "prev_bmi_neg1sd_1sd", "prev_bmi_1sd_2sd", "prev_bmi_2sd")


script <- paste0(scriptDir, "Ado/", scriptname)

for (sex in sexes) {
    for (variable in variables) {
        rstudioapi::jobRunScript(script, name = paste(variable,sex),
                     workingDir = getwd(),
                     importEnv = TRUE)
        Sys.sleep(4)
    }
}
