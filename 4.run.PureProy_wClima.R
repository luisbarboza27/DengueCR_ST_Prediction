library(tidyverse)
library(timetk)
library(lubridate)
library(gamlss)
library(ranger)
library(zoo)
library(boot)
library(dlnm)
library(vars)
library(MTS)

# Carga de datos con riesgos relativos, índices climáticos y variables
# climáticas de MODIS
load('./Data/datos_totales.RData')
set.seed(1)

##ESCOGENCIA DE REZAGO MÁXIMO ####
maxlag <- 18
minlag <- 3

cantones <- datos_totales %>%
  dplyr::select(CCanton) %>% distinct()

bases_ent <- list() # Bases de entrenamiento por canton
bases_test <- list() # Bases de prueba por canton

modGAM <- NULL
modRF <- NULL
predicciones_in_GAM <- NULL
predicciones_out_GAM <- NULL
predicciones_in_RF <- NULL
predicciones_out_RF <- NULL
quantiles_out_GAM <- NULL
quantiles_out_RF <- NULL


for(i in 1:length(cantones$CCanton)){
  show(paste0('Entrenamiento-Canton-',i))
  basecanton <- datos_totales %>% dplyr::filter(CCanton == cantones$CCanton[i]) 
  
  #basecanton <- basecanton %>%
  #  bind_rows(data.frame(Year=2021,Month=c(5,6,7),CCanton=cantones$CCanton[i]))
  
  basecanton <- basecanton %>% mutate(Month=as.factor(Month))
  ##########
  basecanton <- basecanton %>% mutate(RRl1 = lag(RR,1)) 
  basecanton <- basecanton %>% drop_na()
  #basis.P <- crossbasis(basecanton$Precip_t,lag = 12,argvar=list(fun="lin"),
  #                      arglag=list(fun="poly",degree=2))
  basis.P <- crossbasis(basecanton$Precip_t,
                        argvar = list(fun='bs'),
                        arglag = list(fun='lin'),
                        lag=18)
  colnames(basis.P) <- paste0('P',1:dim(basis.P)[2])
  basis.Nino <- crossbasis(basecanton$Nino34SSTA,
                           argvar = list(fun='bs'),
                           arglag = list(fun='lin'),
                           lag=18)
  colnames(basis.Nino) <- paste0('N',1:dim(basis.Nino)[2])
  basis.TNA <- crossbasis(basecanton$TNA,
                          argvar = list(fun='lin'),
                          arglag = list(fun='lin'),
                          lag=18)
  colnames(basis.TNA) <- paste0('T',1:dim(basis.TNA)[2])
  basis.LSD <- crossbasis(basecanton$LSD,
                          argvar = list(fun='lin'),
                          arglag = list(fun='lin'),
                          lag=18)
  colnames(basis.LSD) <- paste0('L',1:dim(basis.LSD)[2])
  basis.NDVI <- crossbasis(basecanton$NDVI,
                           argvar = list(fun='lin'),
                           arglag = list(fun='lin'),
                           lag=18)
  colnames(basis.NDVI) <- paste0('NV',1:dim(basis.NDVI)[2])
  
  basecanton <- basecanton %>% 
    bind_cols(data.frame(basis.P),
              data.frame(basis.Nino),
              data.frame(basis.TNA),
              data.frame(basis.LSD),
              data.frame(basis.NDVI)) %>%
    drop_na()
  
  
  base_ent <- basecanton %>% filter(Year<=2020)
  
  # formula <- RR~RRl1+
  #   P1+P2+
  #   N1+N2+
  #   T1+T2+
  #   L1+L2+
  #   NV1+NV2+Month
  
  formula <- RR~RRl1+Month+P1+P2+P3+P4+P5+P6+
    N1+N2+N3+N4+N5+N6+
    T1+T2+
    L1+L2+
    NV1+NV2
    
  
  modelo_gamma <- gamlss(formula,
                         sigma.formula = ~1,
                         nu.formula = ~1,
                         data = base_ent,
                         family = ZAGA(),
                         control = gamlss.control(trace = F))
  
  modelo_RF <- ranger(formula,
                      base_ent,
                      seed = 1,
                      num.trees = 500,
                      quantreg = T)
  
  base_test <- basecanton %>%  filter(Year>2020)
  
  
  bases_ent[[i]] <- base_ent
  bases_test[[i]] <- base_test
  
  ##Pronostico variables climáticas
  show(paste0('Pronostico-Clima-Canton-',i))
  bases_ent_clima <- base_ent  %>%
    dplyr::select(Precip_t,Nino34SSTA,NDVI,LSD,TNA)
  
  
  base_test_clima <- base_test %>%
    dplyr::select(Precip_t,Nino34SSTA,NDVI,LSD,TNA)
  
  
  seleccion <- VARselect(bases_ent_clima, season = 12, lag.max=12, type="both")
  
  order <- seleccion$selection[3] 
  modvar = vars::VAR(bases_ent_clima, p=order, season = 12, type="both")
  
  predicciones_out_clima <- predict(modvar, n.ahead = dim(base_test_clima)[1], 
                                    ci = 0.95)
  
  base_test <- base_test %>% 
    mutate(Precip_t = predicciones_out_clima$fcst$Precip_t[,1],
           Nino34SSTA = predicciones_out_clima$fcst$Nino34SSTA[,1],
           NDVI = predicciones_out_clima$fcst$NDVI[,1],
           LSD = predicciones_out_clima$fcst$LSD[,1],
           TNA = predicciones_out_clima$fcst$TNA[,1])
  
  
  basecanton <- bind_rows(bases_ent,base_test)
  
  basis.P <- crossbasis(basecanton$Precip_t,
                        argvar = list(fun='bs'),
                        arglag = list(fun='lin'),
                        lag=18)
  colnames(basis.P) <- paste0('P',1:dim(basis.P)[2])
  basis.Nino <- crossbasis(basecanton$Nino34SSTA,
                           argvar = list(fun='bs'),
                           arglag = list(fun='lin'),
                           lag=18)
  colnames(basis.Nino) <- paste0('N',1:dim(basis.Nino)[2])
  basis.TNA <- crossbasis(basecanton$TNA,
                          argvar = list(fun='lin'),
                          arglag = list(fun='lin'),
                          lag=18)
  colnames(basis.TNA) <- paste0('T',1:dim(basis.TNA)[2])
  basis.LSD <- crossbasis(basecanton$LSD,
                          argvar = list(fun='lin'),
                          arglag = list(fun='lin'),
                          lag=18)
  colnames(basis.LSD) <- paste0('L',1:dim(basis.LSD)[2])
  basis.NDVI <- crossbasis(basecanton$NDVI,
                           argvar = list(fun='lin'),
                           arglag = list(fun='lin'),
                           lag=18)
  colnames(basis.NDVI) <- paste0('NV',1:dim(basis.NDVI)[2])
  
  basecanton <- basecanton[,1:23]
  basecanton <- basecanton %>% 
    bind_cols(data.frame(basis.P),
              data.frame(basis.Nino),
              data.frame(basis.TNA),
              data.frame(basis.LSD),
              data.frame(basis.NDVI)) %>%
    drop_na()
  
  #base_test <- basecanton %>%  filter(Year>2019)
  ##Pronostico Riesgo Relativo
  
  ###GAM
  show(paste0('Pronostico-GAM-Canton-',i))
  modGAM[[i]] <- modelo_gamma
  predicciones_in_GAM[[i]] <- predict(modGAM[[i]],type = 'response',se.fit = T)
  predicciones_out_GAM[[i]] <- list()
  
  for(j in 1:dim(base_test_clima)[1]){
    base_proy <- base_test[j,]
    if(j>1){
      base_proy$RRl1 <- RRtemp
    }
    
    ppred <- predict(modGAM[[i]],newdata = base_proy,type = 'response')
    ppred_sigma <- predict(modGAM[[i]],newdata = base_proy,
                           type = 'response',what = 'sigma')
    RRtemp <- ppred
    predicciones_out_GAM[[i]][[j]] <- ppred
  }
  predicciones_out_GAM[[i]] <- unlist(predicciones_out_GAM[[i]])

  ##Nonparametric bootstrap  (pag 149)
  
  pred.function.NP <- function(data,i){
    d <- data[i,]
    modelo_gamma_b <- update(modelo_gamma,data = d)
    predicciones_out_b <- NULL
    for(j in 1:dim(base_test)[1]){
      base_proy <- base_test[j,]
      if(j>1){
        base_proy$RRl1 <- RRtemp
      }
      
      ppred <- predict(modelo_gamma_b,newdata = base_proy,type = 'response',d=d)
      RRtemp <- ppred
      predicciones_out_b[j] <- ppred
    }
    predicciones_out_b
  }
  show(paste0('Bootstrap-Canton-GAM',i))
  #incert.pred <- boot(base_ent,pred.function,R = 100)
  #incert.pred <- boot(base_ent,pred.function.NP,R = 100)
  incert.pred <- tsboot(base_ent,pred.function.NP,R = 100,sim = 'fixed',l = 6)
  quantiles_out_GAM[[i]] <- apply(incert.pred$t,2,quantile,
                                probs=c(0.025,0.975))
  
  ###RF
  show(paste0('Pronostico-RF-Canton-',i))
  
  modRF[[i]] <- modelo_RF
  predicciones_in_RF[[i]] <- predict(modRF[[i]],base_ent,
                                  type = 'response')$predictions
  predicciones_out_RF[[i]] <- list()
  
  for(j in 1:dim(base_test_clima)[1]){
    base_proy <- base_test[j,]
    if(j>1){
      base_proy$RRl1 <- RRtemp
    }
    
    ppred <- predict(modRF[[i]],base_proy,type = 'response')$predictions
    RRtemp <- ppred
    predicciones_out_RF[[i]][[j]] <- ppred
  }
  predicciones_out_RF[[i]] <- unlist(predicciones_out_RF[[i]])
  
  ##Nonparametric bootstrap  (pag 149)
  
  pred.function.NP.RF <- function(data,i){
    d <- data[i,]
    modelo_RF_b <- ranger(formula,
                          d,
                          seed = 1,
                          num.trees = 500,
                          quantreg = T)
    predicciones_out_b <- NULL
    for(j in 1:dim(base_test)[1]){
      base_proy <- base_test[j,]
      if(j>1){
        base_proy$RRl1 <- RRtemp
      }
      ppred <- predict(modelo_RF_b,base_proy,type = 'response')$predictions
      RRtemp <- ppred
      predicciones_out_b[j] <- ppred
    }
    predicciones_out_b
  }
  show(paste0('Bootstrap-Canton-RF',i))
  #incert.pred <- boot(base_ent,pred.function,R = 100)
  #incert.pred <- boot(base_ent,pred.function.NP.RF,R = 100)
  incert.pred <- tsboot(base_ent,pred.function.NP.RF,R = 100,sim = 'fixed',l = 6)
  quantiles_out_RF[[i]] <- apply(incert.pred$t,2,quantile,
                                probs=c(0.025,0.975))
}

nombres_cantones <- datos_totales %>%
  dplyr::select(Canton,CCanton) %>%
  distinct()

indices_cantones <- 1:32
indice <- indices_cantones

base_grafico_lista_2_GAM <- function(indice){
  RR_obs <- bases_test[[indice]] %>%
    dplyr::select(RR)
  fechas <- bases_test[[indice]] %>% 
    dplyr::select(Year,Month) %>% mutate(fecha=make_date(Year,Month,1))
  predicciones <- data.frame(fit=predicciones_out_GAM[[indice]])
  quantiles <- data.frame(low = quantiles_out_GAM[[indice]][1,],
                          up = quantiles_out_GAM[[indice]][2,])
  base_grafico <- predicciones %>% bind_cols(quantiles) %>%
    bind_cols(RR_obs) %>%
    mutate(fechas=fechas$fecha,Canton=nombres_cantones$Canton[indice])
  return(base_grafico)
}

base_grafico_lista_2_RF <- function(indice){
  RR_obs <- bases_test[[indice]] %>%
    dplyr::select(RR)
  fechas <- bases_test[[indice]] %>% 
    dplyr::select(Year,Month) %>% mutate(fecha=make_date(Year,Month,1))
  predicciones <- data.frame(fit=predicciones_out_RF[[indice]])
  quantiles <- data.frame(low = quantiles_out_RF[[indice]][1,],
                          up = quantiles_out_RF[[indice]][2,])
  base_grafico <- predicciones %>% bind_cols(quantiles) %>%
    bind_cols(RR_obs) %>%
    mutate(fechas=fechas$fecha,Canton=nombres_cantones$Canton[indice])
  return(base_grafico)
}

base_grafico_out_GAM <- map_dfr(indice,~base_grafico_lista_2_GAM(.))
base_grafico_out_RF <- map_dfr(indice,~base_grafico_lista_2_RF(.))

metricas <- function(tabla){
  NRMSE <- mean((tabla$fit-tabla$RR)^2)/mean(tabla$RR)
  NIS_95 <- mean((tabla$up-tabla$low)+
                   (2/0.05)*(tabla$low-tabla$RR)*(tabla$RR<tabla$low)+
                   (2/0.05)*(tabla$RR-tabla$up)*(tabla$RR>tabla$up))/mean(tabla$RR)
  return(data.frame(NRMSE,NIS_95))
}

base_grafico_out_g_GAM <- base_grafico_out_GAM %>% group_by(Canton)
base_grafico_out_g_RF <- base_grafico_out_RF %>% group_by(Canton)

metricas_tot_GAM <- base_grafico_out_g_GAM %>% group_modify(~metricas(.x))
metricas_tot_RF <- base_grafico_out_g_RF %>% group_modify(~metricas(.x))

save.image('Resultados_PureProy_wClima_defTS6.RData')


