library(tidyverse)

#Crop rotation

scenario <- c("Now", "More", "Equal")

stock <- c("conv_low","conv_high","org")

field <- c("4ha","7ha","9ha")

# Soil

#JB4 soil profile 

soiltype <- c("JB1", "JB4", "JB6")


soilCinit <- c("sC0.9", "sC1.5", "sC2.5")

# Cimate

#Foulum

clim <- c("DK", "Foulum", "Vegem")

# Initialization period
# arable land 

# 30 years

burn_time <- c("10","30","100")

## WW
init_landuse <- c("SB","WW","GC")

# Simulations

tbl_run <-
  expand_grid(scenario, stock, 
              soiltype, soilCinit, 
              clim, 
              burn_time, init_landuse
  )  |>  
  unite("soil", c('soiltype', 'soilCinit'), sep = "_", remove = F) |>  
  unite("init", c('burn_time', 'init_landuse'), sep = "_", remove = F) |>
  mutate(id=row_number())

#Parameters list----
#inp$`[Parameters]`

param <- c(
  "PLoweLayer",
  "offset",
  "depth",
  "PupperLayer",
  "Initial pMC(%)",
  "Initial C(t/ha)",
  "C/N",
  "Amended C",
  # Crop HUM
  "Crop_HUMdecompositionrate",
  #Crop FOM
  "Crop_FOMdecompositionrate",
  "Crop_clayfraction",
  "Crop_tF",
  # Crop ROM
  "Crop_ROMfraction",
  "Crop_ROMdecompositionrate",
  # Manure HUM
  "Manure_HUMdecompositionrate",
  "Manure_HumFraction",
  # Manure FOM
  "Manure_FOMdecompositionrate",
  "Manure_clayfraction",
  "Manure_tF",
  # Manure ROM
  "Manure_ROMfraction",
  "Manure_ROMdecompositionrate",
  # CropC14 HUM
  "CropC14_HUMdecompositionrate",
  # CropC14 FOM
  "CropC14_FOMdecompositionrate",
  "CropC14_clayfraction",
  "CropC14_tF",
  # CropC14 ROM
  "CropC14_ROMfraction",
  "CropC14_ROMdecompositionrate",
  "CropC14_decay rate",
  # ManureC14 HUM
  "ManureC14_HUMdecompositionrate",
  "ManureC14_HumFraction",
  # ManureC14 FOM
  "ManureC14_FOMdecompositionrate",
  "ManureC14_clayfraction",
  "ManureC14_tF",
  # ManureC14 ROM
  "ManureC14_ROMfraction",
  "ManureC14_ROMdecompositionrate",
  "ManureC14_decay rate",
  # Plant
  "FOMfractionPlantTopLayer",
  "FOMfractionPlantLowerLayer",
  # Plant C14
  "FOMfractionPlantTopLayerC14",
  "FOMfractionPlantLowerLayerC14"
)

inp_params <- data.frame(matrix(ncol = length(param),
                                nrow = nrow(tbl_run)))
colnames(inp_params) <- param
