library(tidyverse)

aver_hal <- readRDS("aver_hal.RDS")

#location folder

out_func <- function(i) {
  
  #browser()
  
  loc_folder <- "C:\\Users\\au710823\\Dropbox\\Franca\\Aarhus\\ctool_II_hal\\"
  
  #Read 
  
  CO2 <- read.csv(
    paste(loc_folder,#gsub(" ","",
          i[[1]],
          #), 
          "\\C02.xls",
          sep = ""),
    sep = "\t", 
    header = TRUE
  )
  
  CO2 <- CO2 %>% select(
    "Foml1","Foml2",
    "Huml1","Huml2",
    "Roml1","Roml2"
  )
  
  tot_amount <- read.csv(
    paste(loc_folder,#gsub(" ","",
          i[[1]],
          #), 
          "\\totalAmount.xls",
          sep = ""),
    sep = "\t", 
    dec=".",
    colClasses = "numeric",
    check.names = FALSE,
    header = TRUE,
    row.names = NULL
  )
  
  colnames(tot_amount) <-
    c(
      "fomcPlant(1,1)","humcPlant(1,1)","romcPlant(1,1)",
      "fomcManure(1,1)","humcManure(1,1)","romcManure(1,1)",
      "fomcPlantC14(1,1)","humcPlantC14(1,1)","romcPlantC14(1,1)",
      "fomcManureC14(1,1)","humcManureC14(1,1)","romcManureC14(1,1)",
      "%","total(1,1)",
      "fomcPlant(2,1)","humcPlant(2,1)","romcPlant(2,1)",
      "fomcManure(2,1)","humcManure(2,1)","romcManure(2,1)",
      "fomcPlantC14(2,1)","humcPlantC14(2,1)","romcPlantC14(2,1)",
      "fomcManureC14(2,1)","humcManureC14(2,1)","romcManureC14(2,1)",
      "%","total(2,1)"
    )
  
  tot_amount <- tot_amount %>% dplyr::select(
    'fomcPlant(1,1)','humcPlant(1,1)','romcPlant(1,1)',
    'fomcManure(1,1)','humcManure(1,1)','romcManure(1,1)',
    'total(1,1)',
    'fomcPlant(2,1)','humcPlant(2,1)','romcPlant(2,1)',
    'fomcManure(2,1)','humcManure(2,1)','romcManure(2,1)',
    'total(2,1)'
  )
  
  transport <- read.csv(
    paste(loc_folder,#gsub(" ","",
          i[[1]],
          #), 
          "\\transport.xls",
          sep = ""),
    sep = "\t", 
    dec=".",
    colClasses = "numeric",
    check.names = FALSE,
    header = TRUE,
    row.names = NULL
  )
  
  transport <- transport %>% select(
    'Fom','Hum','Rom'
  )
  
  
  in_C <- i[[2]] %>% select(
    'Carbon deposited in the topsoil (t/ha)',
    'C deposited in the subsoil (t/ha)',
    'C deposited in the topsoil from manure (tC ha-1)'
  )
  
  
  
  out <- bind_cols("ID"=i[[1]],
                   year= rep((2020-nrow(in_C)+1):2020, 1),
                   in_C,tot_amount,CO2,transport)
  
  out
  
}

#i <- aver[[5]]

out_list <- lapply(aver_hal, out_func)

out_tbl_hal <- do.call(rbind, out_list)

dim(out_tbl_hal)

saveRDS(out_tbl_hal,"out_tbl_hal.RDS")

#check
nrow(out_tbl_hal)==((3*3*3*9*3)*55)+((3*3*3*9*3)*125)