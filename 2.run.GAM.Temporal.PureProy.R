library(tidyverse)
library(timetk)
library(lubridate)

# Carga de datos con riesgos relativos, índices climáticos y variables
# climáticas de MODIS
load('./Data/datos_totales.RData')


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
  
write.csv(lags_ccf,'lags_dengue.csv')
