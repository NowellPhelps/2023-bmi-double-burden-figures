foldername      <- "Figures"
subfoldername   <- "Stacked trends by country and region"
mod_dir_name    <- "ado prev submission"
modelnum        <- 51
start.year.plot <- 1990
end.year.plot   <- 2021

parentdir <- "D:/prev_model/Models/"
maindir           <- paste0(parentdir,"Model",modelnum,", ", mod_dir_name, "/")
maindir_functions <- paste0(parentdir,"Model45, Model 41 with data update 1980/Postprocessing/Scripts/Figures/")

dir.create(paste0(maindir, "Postprocessing/", foldername), showWarnings=FALSE)
dir.create(paste0(maindir, "Postprocessing/", foldername, "/", subfoldername), showWarnings=FALSE)

sexes <- c("female","male")
types <- c("ado all", "ado double burden")

levels <- c("Country")

for (level in levels){
  for (sex in sexes) {
    for (type in types) {
      
      scriptname      <- paste0("5. Trends in all, obesity, and double burden by ", level, ".R")
      script          <- paste0(parentdir, "Model", modelnum, ", ", mod_dir_name, "/Postprocessing/Scripts/Figures/", scriptname)
      
      rstudioapi::jobRunScript(script, 
                               name = paste(level, sex, type),
                               workingDir = paste0(parentdir, "Model", modelnum, ", ", mod_dir_name, "/Postprocessing/Scripts/Figures"),
                               importEnv = TRUE)
      Sys.sleep(10)
    }
  }
}
