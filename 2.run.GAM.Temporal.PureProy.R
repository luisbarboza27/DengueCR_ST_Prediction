library(tidyverse)
library(timetk)
library(lubridate)
library(gamlss)

# Carga de datos con riesgos relativos, índices climáticos y variables
# climáticas de MODIS
load('./Data/datos_totales.RData')


##ESCOGENCIA DE REZAGO MÁXIMO ####
maxlag <- 24


cantones <- datos_totales %>%
  dplyr::select(CCanton) %>% distinct()

ccf_dataset <- datos_totales %>%
  mutate(Date=make_date(Year,Month,1)) %>%
  dplyr::select(Date,Canton,RR,Nino12SSTA,Nino3SSTA,Nino34SSTA,Nino4SSTA,EVI,NDVI,
       NDWI,LSD,LSN,TNA,Precip_t) %>%
  group_by(Canton) %>%
  tk_acf_diagnostics(Date,RR,
                     .ccf_vars=c(Nino12SSTA,Nino3SSTA,Nino34SSTA,Nino4SSTA,EVI,NDVI,
                                 NDWI,LSD,LSN,TNA,Precip_t),
                     .lags=0:24)

lags_ccf <- ccf_dataset %>% group_by(Canton) %>%
  filter(lag!=0) %>% 
  mutate(lagSSTA12 = max(which.max(abs(CCF_Nino12SSTA)),3),
         lagSSTA3 = max(which.max(abs(CCF_Nino3SSTA)),3),
         lagSSTA34 = max(which.max(abs(CCF_Nino34SSTA)),3),
         lagSSTA4 = max(which.max(abs(CCF_Nino4SSTA)),3),
         lagEVI = max(which.max(abs(CCF_EVI)),3),
         lagNDVI = max(which.max(abs(CCF_NDVI)),3),
         lagNDWI = max(which.max(abs(CCF_NDWI)),3),
         lagLSD = max(which.max(abs(CCF_LSD)),3),
         lagLSN = max(which.max(abs(CCF_LSN)),3),
         lagTNA = max(which.max(abs(CCF_TNA)),3),
         lagPrec = max(which.max(abs(CCF_Precip_t))),3) %>%
  dplyr::select(Canton,starts_with('lag'),-lag) %>%
  distinct()


#write.csv(lags_ccf,'lags_dengue.csv')

bases_ent <- list() # Bases de entrenamiento por canton
bases_test <- list() # Bases de prueba por canton

lags_ccf <- as.matrix(lags_ccf[,-1])
valores_p <- NULL
AA <- NULL

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
  
  
  modelo_gamma <- gamlss(RR~Nino3SSTAl1+Precipl1+as.factor(Month),
                          data = base_ent,
                          family = ZAGA())
  modelo_normal <- gamlss(RR~Nino3SSTAl1+Precipl1+as.factor(Month),
                          data = base_ent)
  modelo_IG <- gamlss(RR~Nino3SSTAl1+Precipl1+as.factor(Month),
                          data = base_ent,
                          family = ZAIG())
  AA <- rbind(AA,t(AIC(modelo_gamma,modelo_normal,modelo_IG))[2,])
  uu <- drop1(modelo_gamma)
  valores_p <- rbind(valores_p,uu$`Pr(Chi)`)
  bases_ent[[i]] <- base_ent
  bases_test[[i]] <- base_test
}

