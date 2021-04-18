library(tidyverse)
library(timetk)
library(lubridate)

# Carga de datos con riesgos relativos, índices climáticos y variables
# climáticas de MODIS
load('./Data/datos_totales.RData')

datos_totales<-datos_totales %>%
  drop_na()

##ESCOGENCIA DE REZAGO MÁXIMO ####
maxlag <- 24


cantones <- datos_totales %>%
  select(CCanton) %>% distinct()

ccf_dataset <- datos_totales %>%
  mutate(Date=make_date(Year,Month,1)) %>%
  select(Date,Canton,RR,Nino12SSTA,Nino3SSTA,Nino34SSTA,Nino4SSTA,EVI,NDVI,
       NDWI,LSD,LSN,TNA,Precip_t) %>%
  group_by(Canton) %>%
  tk_acf_diagnostics(Date,RR,
                     .ccf_vars=c(Nino12SSTA,Nino3SSTA,Nino34SSTA,Nino4SSTA,EVI,NDVI,
                                 NDWI,LSD,LSN,TNA,Precip_t),
                     .lags=0:24)

lags_ccf <- ccf_dataset %>% group_by(Canton) %>%
  filter(lag!=0) %>% 
  mutate(lagSSTA12 = which.max(abs(CCF_Nino12SSTA)),
         lagSSTA3 = which.max(abs(CCF_Nino3SSTA)),
         lagSSTA34 = which.max(abs(CCF_Nino34SSTA)),
         lagSSTA4 = which.max(abs(CCF_Nino4SSTA)),
         lagEVI = which.max(abs(CCF_EVI)),
         lagNDVI = which.max(abs(CCF_NDVI)),
         lagNDWI = which.max(abs(CCF_NDWI)),
         lagLSD = which.max(abs(CCF_LSD)),
         lagLSN = which.max(abs(CCF_LSN)),
         lagTNA = which.max(abs(CCF_TNA)),
         lagPrec = which.max(abs(CCF_Precip_t))) %>%
  select(Canton,starts_with('lag'),-lag) %>%
  distinct()
  
#write.csv(lags_ccf,'lags_dengue.csv')

bases_ent <- list() # Bases de entrenamiento por canton
bases_test <- list() # Bases de prueba por canton

lags_ccf <- as.matrix(lags_ccf[,-1])

for(i in 1:length(cantones$CCanton)){
  show(paste0('Ajuste y Predicción-Canton-',i))
  basecanton <- datos_totales %>% dplyr::filter(CCanton == cantones$CCanton[i]) 
  
  basecanton <- basecanton %>%
    bind_rows(data.frame(Year=2021,Canton=basecanton$Canton[1],Month=c(1,2,3),CCanton=cantones$CCanton[i]))
  
  basecanton <- basecanton %>%
    mutate(Nino12SSTAl1=lag(Nino12SSTA,lags_ccf[i,1]),Nino3SSTAl1=lag(Nino3SSTA,lags_ccf[i,2]),Nino34SSTAl1=lag(Nino34SSTA,lags_ccf[i,3]),Nino4SSTAl1=lag(Nino4SSTA,lags_ccf[i,4]),
           EVIl1=lag(EVI,lags_ccf[i,5]),NDVIl1=lag(NDVI,lags_ccf[i,6]),
           NDWIl1=lag(NDWI,lags_ccf[i,7]),LSDl1=lag(LSD,lags_ccf[i,8]),
           LSNl1=lag(LSN,lags_ccf[i,9]),TNAl1=lag(TNA,lags_ccf[i,10]),
           Precipl1 = lag(Precip_t, lags_ccf[i,11])) 
  
  
  base_ent <- basecanton %>% slice(1:(n()-3)) %>%
    drop_na(Nino12SSTAl1,Nino3SSTAl1,Nino34SSTAl1,Nino4SSTAl1,EVIl1,NDVIl1,
            NDWIl1,LSDl1,LSNl1,TNAl1,Precipl1)
  
  base_test <- basecanton %>%  slice((n()-2):n()) %>%
    drop_na(Nino12SSTAl1,Nino3SSTAl1,Nino34SSTAl1,Nino4SSTAl1,EVIl1,NDVIl1,
            NDWIl1,LSDl1,LSNl1,TNAl1,Precipl1) 
  
  bases_ent[[i]] <- base_ent
  bases_test[[i]] <- base_test
}

