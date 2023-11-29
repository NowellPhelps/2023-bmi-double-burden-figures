library(tidyverse)
scriptname      <- "001. Age specific trends by region adult new.R"
outdir_folder <- paste0(outdir, "Age trends/")
dir.create(outdir_folder, showWarnings = FALSE)

sexes <- c("female","male")

variables <- c("prev_bmi_l185",
               "prev_bmi_30")

script <- paste0(scriptDir, "Adult/", scriptname)

for (sex in sexes) {
    for (variable in variables) {
        rstudioapi::jobRunScript(script, name = paste(variable,sex),
                     workingDir = getwd(),
                     importEnv = TRUE)
        Sys.sleep(4)
    }
}
