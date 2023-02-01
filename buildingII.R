library(tidyverse)

# Crop rotation from Troels



crop_rot <- read.table("data/long_juan.txt", 
                       header =T, dec= ".", sep="\t",
                       na.strings = ".") 

init_tbl <- data.frame(rot_cycle=c(rep(10,10),rep(30,30),rep(100,100)),
  "year"=c(seq(2000 - 10, 2000 - 1, 1),
           seq(2000 - 30, 2000 - 1, 1),
           seq(2000 - 100, 2000 - 1, 1)))


scenario <- c("Now", "More", "Equal")

stock <- c("conv_high","conv_low","org")

field <- c("4ha","7ha","9ha")

grid_init <- 
  expand_grid(init_tbl,init_landuse,scenario,stock,field) |> 
  mutate(yield_MC=recode(init_landuse,
                               "SB"=4505,
                               "WW"=5000,
                               "GC"=9499),
         Crop=recode(init_landuse,
                     "SB"='Grain',
                     "WW"='Grain',
                     "GC"='Grass')) |> 
  mutate(rot_year=0)


# Initialization period

init_landuse <- c("SB","WW","GC")

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


# Simulations

tbl_run <-
  expand_grid(unique(bind_rows(grid_init, crop_rot)),
              soiltype,
              soilCinit,
              clim) |>
  mutate(scn=fct_cross(#c(
      init_landuse,
      as.factor(rot_cycle),
      scenario,
      stock,
      field,
      soiltype,
      soilCinit,
      clim
    #)
    ,
    sep = "_"
  ),
  soil=fct_cross(soiltype,soilCinit,
                 sep = "_"))

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

tbl_run <-
  bind_cols(tbl_run, inp_params)  |> 
  ## for input data
  mutate(
    "PLoweLayer" = 0.312,
    "offset" = 0,
    "depth" = 100,
    "PupperLayer" = 0.48,
    "Initial pMC(%)" = 0,
    # initial condition of C  
    "Initial C(t/ha)" = recode(
      soil,
      "JB1_sC0.9" = 70.4,
      "JB1_sC1.5" = 114.9,
      "JB1_sC2.5" = 173.4,
      
      "JB4_sC0.9" = 78.4,
      "JB4_sC1.5" = 114.8,
      "JB4_sC2.5" = 174.9,
      
      "JB6_sC0.9" = 70.2,
      "JB6_sC1.5" = 119.6,
      "JB6_sC2.5" = 174.2,
    ),
    # CN balance
    `C/N` = recode(
      soil,
      "JB1_sC0.9" = 0.86 / 0.12,
      "JB1_sC1.5" = 1.49 / 0.21,
      "JB1_sC2.5" = 2.47 / 0.35,
      
      "JB4_sC0.9" = 0.97 / 0.11,
      "JB4_sC1.5" = 1.53 / 0.18,
      "JB4_sC2.5" = 2.61 / 0.30,
      
      "JB6_sC0.9" = 0.81 / 0.09,
      "JB6_sC1.5" = 1.47 / 0.16,
      "JB6_sC2.5" = 2.39 / 0.26,
    ),
    "Amended C"= 0,
    # Crop HUM
    "Crop_HUMdecompositionrate"= 0.0028,
    #Crop FOM
    "Crop_FOMdecompositionrate"= 0.12,
    "Crop_clayfraction"= recode (
      soiltype,
      "JB1"=0.0405,
      "JB4"=0.0809,
      "JB6"=0.1272
    ),
    "Crop_tF"=0.003,
    # Crop ROM
    "Crop_ROMfraction"=0.012,
    "Crop_ROMdecompositionrate"=0.00003858,
    # Manure HUM
    "Manure_HUMdecompositionrate"= Crop_HUMdecompositionrate,
    "Manure_HumFraction"= recode (
      soiltype,
      "JB1"=0.192,
      "JB4"=0.176,
      "JB6"=0.161
    ),
    # Manure FOM
    "Manure_FOMdecompositionrate"=0.12,
    "Manure_clayfraction"= Crop_clayfraction,
    "Manure_tF"=0,
    # Manure ROM
    "Manure_ROMfraction"=0,
    "Manure_ROMdecompositionrate"=0,
    # CropC14 HUM
    "CropC14_HUMdecompositionrate"= Manure_HUMdecompositionrate,
    # CropC14 FOM
    "CropC14_FOMdecompositionrate"= 0,
    "CropC14_clayfraction"= Crop_clayfraction,
    "CropC14_tF"=0,
    # CropC14 ROM
    "CropC14_ROMfraction" = 0,
    "CropC14_ROMdecompositionrate"=0,
    "CropC14_decay rate"=0,
    # ManureC14 HUM
    "ManureC14_HUMdecompositionrate"= Manure_HUMdecompositionrate,
    "ManureC14_HumFraction"= Manure_HumFraction,
    # ManureC14 FOM
    "ManureC14_FOMdecompositionrate"= 0,
    "ManureC14_clayfraction"= Crop_clayfraction,
    "ManureC14_tF"= 0,
    # ManureC14 ROM
    "ManureC14_ROMfraction"= 0,
    "ManureC14_ROMdecompositionrate"= 0,
    "ManureC14_decay rate" = 0,
    # Plant
    "FOMfractionPlantTopLayer" = 0.032,
    "FOMfractionPlantLowerLayer" = 0.03,
    # Plant C14
    "FOMfractionPlantTopLayerC14"= FOMfractionPlantTopLayer,
    "FOMfractionPlantLowerLayerC14"= FOMfractionPlantLowerLayer
  ) 

tbl_run_test <- tbl_run |> 
  mutate(
    HI = as.numeric(
      recode(
        Crop,
        'Grain' = 0.75,
        'Grass' = 0.70,
        'Maize' = 0.85,
        'Pulses' = 0.42 #from Soy bean
      )
    ),
    SB = as.numeric(
      recode(
        Crop,
        'Grain' = 0,
        'Grass' = 0,
        'Maize' = 0,
        'Pulses' = 0.5 #from Soy bean
      )
    ),
    RB = as.numeric(
      recode(
        Crop,
        'Grain' = 0.17,
        'Grass' = 0.45,
        'Maize' = 0.15,
        'Pulses' = 0.1 #from Soy bean
      )
    ),
    RE =as.numeric(
      recode(
        Crop,
        'Grain' = 0.8,
        'Grass' = 0.9,
        'Maize' = 0.8,
        'Pulses' = 0.8 #from Soy bean
      )
    ),
    yield_MC= ifelse(is.na(yield_MC), 0 , yield_MC/1000),
    
    yield_CC=ifelse(is.na(yield_CC), 0 , yield_CC/1000),
    
    C_manure=if_else(is.na(C_manure),0,C_manure/1000)) |> 
  mutate(
    'Cresid'=as.numeric(((1/HI)-1-SB)*(yield_MC*0.43)),
    'Cresid_cc'=as.numeric(((1/HI)-1-SB)*(yield_CC*0.43)),
    
    'Cbelow'=as.numeric((RB/((1-RB)*HI))*(yield_MC*0.43)),
    'Cbelow_cc'=as.numeric((RB/((1-RB)*HI))*(yield_CC*0.43))
  ) |>  
  mutate(
    'Ctop'=ifelse(Cresid<0,0+(RE*Cbelow),Cresid+(RE*Cbelow)) +
      ifelse(Cresid_cc<0,0+(RE*Cbelow_cc),Cresid_cc+(RE*Cbelow_cc)),
    
    'Csub'=(1-RE)*Cbelow+(1-RE)*Cbelow_cc,
    
    'Cman'= C_manure
  )


