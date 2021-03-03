library(tidyverse)
#library(caret)
#library(rfinterval)
library(ranger)
#library(Hmisc)
library(dtw)
library(sf)
#library(xtable)

# Carga de datos con riesgos relativos, índices climáticos y variables
# climáticas de MODIS
load('./Data/datos_totales.RData')


##AJUSTE GAM SST34######
lagSST12 <- NULL
lagSST3 <- NULL
lagSST34 <- NULL
lagSST4 <- NULL
lagEVI <- NULL
lagNDVI <- NULL
lagNDWI <- NULL
lagLSD <- NULL
lagLSN <- NULL
lagTNA <- NULL
maxlag <- 24


cantones <- datos_totales %>%
  select(CCanton) %>% distinct()


for(i in 1:length(cantones$CCanton)){
  show(paste0('Autocorrelacion-Canton-',i))
  base1 <- datos_totales %>% dplyr::filter(CCanton == cantones$CCanton[i])
  cc1 <- ccf(base1$RR,base1$Nino12SSTA,lag.max = maxlag)
  lagSST12[i] <- cc1$lag[which.max(cc1$acf)]
  
  cc2 <- ccf(base1$RR,base1$Nino3SSTA,lag.max = maxlag)
  lagSST3[i] <- cc2$lag[which.max(cc2$acf)]
  
  cc3 <- ccf(base1$RR,base1$Nino34SSTA,lag.max = maxlag)
  lagSST34[i] <- cc3$lag[which.max(cc3$acf)]
  
  cc4 <- ccf(base1$RR,base1$Nino4SSTA,lag.max = maxlag)
  lagSST4[i] <- cc4$lag[which.max(cc4$acf)]
  
  cc5 <- ccf(base1$RR,base1$EVI,lag.max = maxlag)
  lagEVI[i] <- cc5$lag[which.max(cc5$acf)]
  
  cc6 <- ccf(base1$RR,base1$NDVI,lag.max = maxlag)
  lagNDVI[i] <- cc6$lag[which.max(cc6$acf)]
  
  cc7 <- ccf(base1$RR,base1$NDWI,lag.max = maxlag)
  lagNDWI[i] <- cc7$lag[which.max(cc7$acf)]
  
  cc8 <- ccf(base1$RR,base1$LSD,lag.max = maxlag)
  lagLSD[i] <- cc8$lag[which.max(cc8$acf)]
  
  cc9 <- ccf(base1$RR,base1$LSN,lag.max = maxlag)
  lagLSN[i] <- cc9$lag[which.max(cc9$acf)]
  
  cc10 <- ccf(base1$RR,base1$TNA,lag.max = maxlag)
  lagTNA[i] <- cc10$lag[which.max(cc10$acf)]
}

lagresultados <- data.frame(rbind(lagSST12,lagSST3,lagSST34,lagSST4,
                                  lagEVI,lagNDVI,lagNDWI,lagLSD,lagLSN,lagTNA))
colnames(lagresultados) <- cantones$CCanton
lagresultados <- abs(lagresultados)

MSEGAM <- NULL
baseGAM <- NULL
modRF <- NULL
predicciones <- list()
#lagGAM <- 3*matrix(1,nrow = 5,ncol = 5)
lagresultados[(lagresultados>24)] <- 24 
lagresultados[(lagresultados<3)] <- 3 
lagGAM <- lagresultados
lagGAM2 <- 12
perproy <- 3
epsilon <- 1e-3

modelosuni <- NULL
basetitles <- NULL

for(i in 1:length(cantones$CCanton)){
  show(paste0('Ajuste y Predicción-Canton-',i))
  basecanton <- datos_totales %>% dplyr::filter(CCanton == cantones$CCanton[i]) 
  
  basecanton <- basecanton %>%
    bind_rows(data.frame(Year=2020,Canton=basecanton$Canton[1],Month=c(1,2,3),CCanton=cantones$CCanton[i]))
  
  basecanton <- basecanton %>%
    mutate(RRl1=lag(RR),RRl2=lag(RR,lagGAM2),Nino12SSTAl1=lag(Nino12SSTA,lagGAM[1,i]),Nino3SSTAl1=lag(Nino3SSTA,lagGAM[2,i]),Nino34SSTAl1=lag(Nino34SSTA,lagGAM[3,i]),Nino4SSTAl1=lag(Nino4SSTA,lagGAM[4,i]),
           EVIl1=lag(EVI,lagGAM[5,i]),NDVIl1=lag(NDVI,lagGAM[6,i]),
           NDWIl1=lag(NDWI,lagGAM[7,i]),LSDl1=lag(LSD,lagGAM[8,i]),
           LSNl1=lag(LSN,lagGAM[9,i]),TNAl1=lag(TNA,lagGAM[9,i])) 
  
  
  base_ent <- basecanton %>% slice(1:(n()-3)) %>%
    drop_na(Nino12SSTAl1,Nino3SSTAl1,Nino34SSTAl1,Nino4SSTAl1,EVIl1,NDVIl1,
            NDWIl1,LSDl1,LSNl1,TNAl1,RRl2)
  
  base_test <- basecanton %>%  slice((n()-2):n()) %>%
    drop_na(Nino12SSTAl1,Nino3SSTAl1,Nino34SSTAl1,Nino4SSTAl1,EVIl1,NDVIl1,
            NDWIl1,LSDl1,LSNl1,TNAl1,RRl2) 
  
  basetitles <- basetitles %>% bind_rows(base_test)
  #RF
  modelocompletoset <- ranger(RR~RRl1+RRl2+Nino12SSTAl1+Nino3SSTAl1+Nino34SSTAl1+Nino4SSTAl1+EVIl1+NDVIl1+NDWIl1+LSDl1+LSNl1+TNAl1,
                              base_ent,seed = 1,num.trees = 500,quantreg = T)
  
  
  
  modRF[[i]] <- modelocompletoset
  predicciones[[i]] <- list()
  
  ##FALTA CORREGIR ESTO
  for(j in 1:perproy){
    # base_proy <- basecanton %>% filter(Year==2017,Month==(9+j)) %>%
    #   select(Nino12SSTAl1=Nino12SSTA,Nino3SSTAl1=Nino3SSTA,
    #          Nino34SSTAl1=Nino34SSTA,Nino4SSTAl1=Nino4SSTA,EVIl1=EVI,NDVIl1=NDVI,RRl2,OFF)
    base_proy <- base_test[j,]
    if(j>1){
      base_proy$RRl1 <- RRtemp
    }
    
    ppred <- predict(modelocompletoset,base_proy,type = 'quantiles',quantiles=c(0.05,0.5,0.95))
    
    RRtemp <- ppred$predictions[2]
    predicciones[[i]][[j]] <- ppred$predictions
  }
  
}

# Comparación de predicción vs observado
resultadoscomp <- list()


for(k in 1:length(cantones$CCanton)){
  show(paste0('Comparaciones-Canton-',k))
  resultadoscomp[[k]] <- basetitles %>% 
    filter(CCanton==cantones$CCanton[k]) %>%
    select(Canton,Month,Year)
  ptemp <- matrix(unlist(predicciones[[k]]),ncol = 3,byrow = T)
  colnames(ptemp) <- c('10th','Median','90th')
  resultadoscomp[[k]] <- resultadoscomp[[k]] %>% bind_cols(data.frame(ptemp))
  #resultadoscomp[[k]] <- resultadoscomp[[k]] %>%
  #  select(Month,RR,X10th,Median,X90th)
}

# Graficos ----
distritos_st <-st_read('Data/distritos_shape/Distritos_de_Costa_Rica.shp') %>%
  filter(CODIGO!=60110)

cantones_st <- distritos_st %>%
  mutate(DTA_C = str_sub(CODIGO,start = 1,end = 3)) %>%
  group_by(DTA_C) %>% summarise() %>%
  ungroup() %>% mutate(DTA_C=as.numeric(DTA_C))


Ene_20 <- map_dbl(1:length(cantones$CCanton),~resultadoscomp[[.]][1,5]$Median)
Feb_20 <- map_dbl(1:length(cantones$CCanton),~resultadoscomp[[.]][2,5]$Median)
Mar_20 <- map_dbl(1:length(cantones$CCanton),~resultadoscomp[[.]][3,5]$Median)

proyecciones_cant <- data.frame(DTA_C=cantones$CCanton,Ene_20,Feb_20,Mar_20)

cantones_st_plot <- cantones_st %>% left_join(proyecciones_cant) %>%
  pivot_longer(Ene_20:Mar_20,names_to = 'Mes',values_to = 'Riesgo') 

grafico_RR_proyectado <- ggplot(data = cantones_st_plot) +
  geom_sf(mapping = aes(fill=Riesgo,geometry=geometry))+
  facet_wrap(~Mes)+
  theme_bw()+
  theme(axis.ticks = element_blank(),axis.text = element_blank())+
  scale_fill_gradient(low = 'yellow',high = 'red',na.value = 'transparent')

#xtable(resultadoscomp[[1]],digits = 2)
