library(tidyverse)

library(fmsb)

# Crop rotation from Troels ----

# Initialization period
# arable land 

init_landuse <- c("SB","WW","GC")

burn_init <- c("10","30","100")

# rotations

scenario <- c("Now", "More", "Equal")

stock <- c("conv.high","conv.low","org")

field <- c("4ha","7ha","9ha")

crop_rot <- read.table("data/long_juan.txt", 
                       header =T, dec= ".", sep="\t",
                       na.strings = ".") |> 
  mutate(stock=recode(stock, 
             conv_high="conv.high",  conv_low="conv.low", 
             org_org="org", "1.50"="org")) |> 
  expand_grid(burn_init,init_landuse)

  crop_rot |> 
  filter(rot_cycle=="1") |> 
  group_by(scenario, stock) |> 
  summarise(Grain_prop=
              sum(Crop =="Grain" & field =="4ha")*4+
              sum(Crop =="Grain" & field =="7ha")*7+
              sum(Crop =="Grain" & field =="9ha")*9,  
            Grass_prop=
              sum(Crop =="Grass" & field =="4ha")*4+
              sum(Crop =="Grass" & field =="7ha")*7+
              sum(Crop =="Grass" & field =="9ha")*9,   
            Maize_prop=
              sum(Crop =="Maize" & field =="4ha")*4+
              sum(Crop =="Maize" & field =="7ha")*7+
              sum(Crop =="Maize" & field =="9ha")*9, 
            Pulses_prop=
              sum(Crop =="Pulses" & field =="4ha")*4+
              sum(Crop =="Pulses" & field =="7ha")*7+
              sum(Crop =="Pulses" & field =="9ha")*9, 
            CC_prop=
              sum(!is.na(C_CC) & field =="4ha")*4+
              sum(!is.na(C_CC) & field =="7ha")*7+
              sum(!is.na(C_CC) & field =="9ha")*9
            )



init_tbl <- data.frame(
  burn_init=as.factor(c(rep(10,10),rep(30,30),rep(100,100))),
  rot_cycle=0,
  year=c(seq(2000 - 10, 2000 - 1, 1),
           seq(2000 - 30, 2000 - 1, 1),
           seq(2000 - 100, 2000 - 1, 1)))

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

# Soil

#JB4 soil profile 

soiltype <- c("JB1", "JB4", "JB6")
soilCinit <- c("sC0.9", "sC1.5", "sC2.5")

# Climate

#Foulum

clim <- c("DK", "Foulum")#, "Vegem")


# 30 years
# Simulations

tbl_run <-
  expand_grid(unique(bind_rows(grid_init, crop_rot)),
              soiltype,
              soilCinit,
              clim) |>
  mutate(scn=fct_cross(#c(
      init_landuse,
      as.factor(burn_init),
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
                 sep = "_"),
  tempdata=fct_cross(clim, burn_init, sep = "_"))

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
  ) |>
  #tbl_run_test <- tbl_run |>
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
    SB = as.numeric(recode(
      Crop,
      'Grain' = 0,
      'Grass' = 0,
      'Maize' = 0,
      'Pulses' = 0.5 #from Soy bean
    )),
    RB = as.numeric(
      recode(
        Crop,
        'Grain' = 0.17,
        'Grass' = 0.45,
        'Maize' = 0.15,
        'Pulses' = 0.1 #from Soy bean
      )
    ),
    RE = as.numeric(
      recode(
        Crop,
        'Grain' = 0.8,
        'Grass' = 0.9,
        'Maize' = 0.8,
        'Pulses' = 0.8 #from Soy bean
      )),
      tempfile = recode(
        tempdata,
        DK_10="temp_DK10.txt",  
        Foulum_10="temp_fou10.txt",     
        DK_100="temp_DK100.txt", 
        Foulum_100="temp_fou100.txt",      
        DK_30="temp_DK30.txt",  
        Foulum_30="temp_fou30.txt"),
    
    yield_MC = ifelse(is.na(yield_MC), 0 , yield_MC / 1000),
    
    #yield_CC = ifelse(is.na(yield_CC), 0 , yield_CC / 1000),
    
    C_manure = if_else(is.na(C_manure), 0 , C_manure / 1000)
  ) |>
  mutate(
    'Cresid' = as.numeric(((1 / HI) - 1 - SB) * (yield_MC * 0.45)),
    
    'Cbelow' = as.numeric((RB / (( 1 - RB ) * HI)) * (yield_MC * 0.45))
  ) |>
  mutate(
    'Ctop' = ifelse(Cresid < 0, 0 + (RE * Cbelow), Cresid + (RE * Cbelow)) +
      ifelse(is.na(C_CC), 0, C_CC),
    
    'Csub' = (1 - RE) * Cbelow ,
    
    'Cman' = C_manure
  )


summCinp <- tbl_run |> filter(rot_cycle=="1" & 
                    init_landuse =="WW" & 
                    burn_init=="10" &
                    soiltype== "JB1" &
                    soilCinit == "sC1.5" & 
                    clim == "DK") |> 
  group_by(scenario, stock) |> 
  summarise(Grain_Ctop =
            sum(Ctop,Crop =="Grain" & field =="4ha")*4+
            sum(Ctop,Crop =="Grain" & field =="7ha")*7+
            sum(Ctop,Crop =="Grain" & field =="9ha")*9,  
            
            Grass_Ctop =
              sum(Ctop,Crop =="Grass" & field =="4ha")*4+
              sum(Ctop,Crop =="Grass" & field =="7ha")*7+
              sum(Ctop,Crop =="Grass" & field =="9ha")*9,   
            
            Maize_Ctop=
              sum(Ctop,Crop =="Maize" & field =="4ha")*4+
              sum(Ctop,Crop =="Maize" & field =="7ha")*7+
              sum(Ctop,Crop =="Maize" & field =="9ha")*9, 
            
            Pulses_Ctop=
              sum(Ctop,Crop =="Pulses" & field =="4ha")*4+
              sum(Ctop,Crop =="Pulses" & field =="7ha")*7+
              sum(Ctop,Crop =="Pulses" & field =="9ha")*9, 
            
            CC_Ctop=
              sum(C_CC, field =="4ha",na.rm = TRUE)*4+
              sum(C_CC, field =="7ha",na.rm = TRUE)*7+
              sum(C_CC, field =="9ha",na.rm = TRUE)*9,
            
            Grain_Csub =
              sum(Csub,Crop =="Grain" & field =="4ha")*4+
              sum(Csub,Crop =="Grain" & field =="7ha")*7+
              sum(Csub,Crop =="Grain" & field =="9ha")*9,  
            
            Grass_Csub=
              sum(Csub,Crop =="Grass" & field =="4ha")*4+
              sum(Csub,Crop =="Grass" & field =="7ha")*7+
              sum(Csub,Crop =="Grass" & field =="9ha")*9,   
            
            Maize_Csub=
              sum(Csub,Crop =="Maize" & field =="4ha")*4+
              sum(Csub,Crop =="Maize" & field =="7ha")*7+
              sum(Csub,Crop =="Maize" & field =="9ha")*9, 
            
            Pulses_Csub=
              sum(Csub,Crop =="Pulses" & field =="4ha")*4+
              sum(Csub,Crop =="Pulses" & field =="7ha")*7+
              sum(Csub,Crop =="Pulses" & field =="9ha")*9, 
            
            Cman=
              sum(Cman,  field =="4ha")*4+
              sum(Cman, field =="7ha")*7+
              sum(Cman, field =="9ha")*9
            
  )


## List with data ---- 
try_ave_spl <- tbl_run |> 
  #filter(id %in% as.vector(ok)) |> 
  group_split(scn)

# Temperature data ----

## DK ----

temp_ave_1874to2010 <- read.csv(
  "C:/Users/au710823/OneDrive - Aarhus universitet/ctool1st2022/ctool_allo/data/dk_month_temp_1_1874_12_2012.csv",
  header = TRUE
) |>  mutate(date = lubridate::my(Date), dk_tem = temp) |>
  mutate(date = lubridate::as_date(date))

temp_ave_2011to2021 <-
  read.csv(
    "C:/Users/au710823/OneDrive - Aarhus universitet/ctool1st2022/ctool_allo/data/dk_month_temp_1_2011_12_2021.csv",
    header = TRUE,
    sep = ";"
  ) |>
  mutate(date = lubridate::dmy(DateTime),
         dk_tem = Middel) |>
  mutate(date = lubridate::as_date(date))#|>
# mutate(date = lubridate::my(date))

temp_ave <- bind_rows(temp_ave_1874to2010,temp_ave_2011to2021) |> 
  select(date,dk_tem) |> 
  mutate(date=lubridate::as_date(date)) |> 
  filter(#date>='1990-01-01'
         #date>='1970-01-01'
         date>='1900-01-01' 
         & date<='2020-12-31')

write.table(
  temp_ave$dk_tem,
  "data/temp_DK100.txt",
  col.names = FALSE,
  sep = "\t",
  row.names = FALSE
)


## Foulum ----

library(readxl)

daily_Fou_to2013 <-
  read.csv("C:/Users/au710823/OneDrive - Aarhus universitet/ctool1st2022/temperature/Daily_Foulum_1987_01_01_to_2013_12_31.csv",
     header = TRUE) |>  mutate(date = lubridate::ymd(date))


daily_Fou_from2014 <-read.csv(
  "C:/Users/au710823/OneDrive - Aarhus universitet/ctool1st2022/temperature/Daily_Foulum_1_1_2014_to_1_1_2022.csv", 
  header = TRUE) |> mutate(date = lubridate::ymd(lubridate::dmy(date)))

monthly_Fou_1987_2022 <-
  bind_rows(daily_Fou_to2013, daily_Fou_from2014)  |>  mutate(
    month = lubridate::month(date),
    year = lubridate::year(date)
  ) |> 
  group_by(year, month)  |> 
  summarise(fou_tem = mean(temp)) |>  
  mutate(date=lubridate::ym(paste(year,month, sep = "-")))

# data_jo <- data_jo |>  mutate(
#   date=seq(from=lubridate::my("01-1966"),
#            to=lubridate::my("12/2020"),
#            by="month"))

monthly_Fou_1987_2022 <-
  bind_rows(daily_Fou_to2013, daily_Fou_from2014)  |>  mutate(
    month = lubridate::month(date),
    year = lubridate::year(date)
  ) |> 
  group_by(year, month) |> 
  summarise(fou_tem = mean(temp)) |>  
  mutate(date=lubridate::ym(paste(year,month, sep = "-")))

monthly_DK_1874_2010 <- 
  read.csv("C:/Users/au710823/OneDrive - Aarhus universitet/ctool1st2022/temperature/Monthly_Denmark_1_1874_to_12_2010.csv",
           header = TRUE) |>  mutate(date = lubridate::my(Date),
                                     dk_tem=temp)
all_temp <-
  full_join(monthly_DK_1874_2010, monthly_Fou_1987_2022, by = 'date')

train_temp <-
  inner_join(monthly_DK_1874_2010, monthly_Fou_1987_2022, by = 'date')

cor(train_temp$dk_tem,
    train_temp$fou_tem)

plot(train_temp$dk_tem,
     train_temp$fou_tem)

reg_t <- lm(fou_tem~dk_tem,train_temp);reg_t

summary(reg_t)

all_temp <- all_temp |>  mutate(
  tem_ok=if_else(is.na(fou_tem),(-0.4714+0.9875*dk_tem),fou_tem))


temp_fou <- all_temp |>   filter(
  between(date, as.Date("1900-01-01"), as.Date("2020-12-31")))

write.table(
  temp_fou$tem_ok,
  "data/temp_fou100.txt",
  col.names = FALSE,
  sep = "\t",
  row.names = FALSE
)

