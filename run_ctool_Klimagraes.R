# take aver write the file scenarios and then run the functions 
aver_plot

run_in_folder <- function(i) {
  #browser()
  # Make a folder with scenario ID name
  folder_name <-
    paste(
      "D:/Franca/klimagraes2023/output_0702/",
      i[[1]],
      sep = ""
    )
  
  dir.create(folder_name)
  # set the working 
  
  setwd(folder_name)
  
  # Write data txt
  
  write.table(as.data.frame(i[[2]]), 
              paste(folder_name, "\\data.txt", sep = ""), 
              sep ="\t",
              dec = ".",
              row.names = FALSE,
              quote=FALSE)
  
  # Write input .txt
  
  writeLines(i[[3]], paste(folder_name, "\\input.txt", sep = ""), sep = "\t")
  
  # Make runscenarios .txt 
  
  runscn <-
    c(
      paste("input", paste(folder_name, "\\input.txt", sep = ""), sep = "\t"),
      paste("data", paste(folder_name, "\\data.txt", sep = ""), sep = "\t"),
      paste(
        "TemperatureData",
        paste("D:/Franca/klimagraes2023/data/",
              i[[4]], sep = ""),
        sep = "\t"
      ),
      paste("outputDir", paste(folder_name, "\\", sep = ""),  sep = "\t")
    )
  
  writeLines(runscn, paste(folder_name,
                           "\\runsenarios.txt",
                           sep = ""))
  
  
  writeLines("0", paste(folder_name, 
                        "\\mode.txt", 
                        sep = ""))
  
  # write.table(0, paste(folder_name, 
  #                            "\\mode.txt", 
  #                            sep = ""),  
  #            sep = "\t",
  #            row.names = FALSE,
  #            quote=FALSE)
  
  # copy ctool app
  
  file.copy(
    from = "D:/Franca/klimagraes2023/data/ctool2.3.exe",
    to = paste(folder_name,
               "/ctool2.3.exe",
               sep = ""))
  
  # Execute .exe in each folder
  
  system2(paste(folder_name,
                "\\ctool2.3.exe",
                sep = ""))
  
  # delete executable 
  
  file.remove(from = paste(folder_name,
                           "\\ctool2.3.exe",
                           sep = ""))
  
  setwd("D:\\Franca/klimagraes2023/")
  
}

#aver_plot <- readRDS("data/aver_plot_Kl2023.RDS")
lapply(aver_plot, run_in_folder)
