library(tidyverse)
library(timetk)
library(lubridate)
library(gamlss)
library(zoo)
library(boot)
library(dlnm)

# Carga de datos con riesgos relativos, índices climáticos y variables
# climáticas de MODIS
load('./Data/datos_totales.RData')


##ESCOGENCIA DE REZAGO MÁXIMO ####
maxlag <- 18
minlag <- 3

cantones <- datos_totales %>%
  dplyr::select(CCanton) %>% distinct()

bases_ent <- list() # Bases de entrenamiento por canton
bases_test_1 <- list() # Bases de prueba por canton
bases_test_2 <- list() # Bases de prueba por canton


valores_p <- NULL
AA <- NULL

modGAM <- NULL
predicciones_in <- NULL
predicciones_out_1 <- NULL
quantiles_out_1 <- NULL
#length(cantones$CCanton)
for(i in 1:length(cantones$CCanton)){
  show(paste0('Ajuste y Predicción-Canton-',i))
  basecanton <- datos_totales %>% dplyr::filter(CCanton == cantones$CCanton[i]) 
  
  basecanton <- basecanton %>%
    bind_rows(data.frame(Year=2021,Month=c(5,6,7),CCanton=cantones$CCanton[i]))
  
  basecanton <- basecanton %>% mutate(Month=as.factor(Month))
  
  
  ##########
  basecanton <- basecanton %>% mutate(RRl1 = lag(RR,3),
                                      Precip_t_3 = lag(Precip_t,3),
                                      Nino3SSTA_3 = lag(Nino3SSTA,3),
                                      TNA_3 = lag(TNA,3)) 
  basecanton <- basecanton %>% drop_na()
  #basis.P <- crossbasis(basecanton$Precip_t,lag = 12,argvar=list(fun="lin"),
  #                      arglag=list(fun="poly",degree=2))
  basis.P <- crossbasis(basecanton$Precip_t_3,
                        argvar = list(fun='lin'),
                        arglag = list(fun='lin'),
                        lag=18)
  colnames(basis.P) <- paste0('P',1:dim(basis.P)[2])
  basis.Nino <- crossbasis(basecanton$Nino3SSTA_3,
                        argvar = list(fun='lin'),
                        arglag = list(fun='lin'),
                        lag=18)
  colnames(basis.Nino) <- paste0('N',1:dim(basis.Nino)[2])
  basis.TNA <- crossbasis(basecanton$TNA_3,
                           argvar = list(fun='lin'),
                           arglag = list(fun='lin'),
                           lag=18)
  colnames(basis.TNA) <- paste0('T',1:dim(basis.TNA)[2])
  
  basis.RR <- crossbasis(basecanton$RRl1,
                          argvar = list(fun='bs'),
                          arglag = list(fun='lin'),
                          lag=11)
  colnames(basis.RR) <- paste0('R',1:dim(basis.RR)[2])
  
  
  basecanton <- basecanton %>% 
    bind_cols(data.frame(basis.P),
              data.frame(basis.Nino),
              data.frame(basis.TNA),
              data.frame(basis.RR)) %>%
    drop_na()
  

  base_ent <- basecanton %>% filter(Year<=2018) 
  
  # modelo_gamma <- gamlss(RR~R1+R2+
  #                          P1+P2+
  #                          N1+N2+
  #                          T1+T2,
  #                        sigma.formula = ~1,
  #                        nu.formula = ~1,
  #                        data = base_ent,
  #                        family = ZAGA(),
  #                        control = gamlss.control(trace = F))
  
  modelo_gamma <- gamlss(RR~R1+R2+R3+R4+R5+R6+
                           P1+P2+
                           N1+N2+
                           T1+T2+
                           Month,
                         sigma.formula = ~1,
                         nu.formula = ~1,
                         data = base_ent,
                         family = ZAGA(),
                         control = gamlss.control(trace = F))
  ##########
  # basecanton <- basecanton %>%
  #   mutate(RRl1 = lag(RR),Nino12SSTAl1=lag(Nino12SSTA,lags_ccf[i,1]),Nino3SSTAl1=lag(Nino3SSTA,lags_ccf[i,2]),Nino34SSTAl1=lag(Nino34SSTA,lags_ccf[i,3]),Nino4SSTAl1=lag(Nino4SSTA,lags_ccf[i,4]),
  #          EVIl1=lag(EVI,lags_ccf[i,5]),NDVIl1=lag(NDVI,lags_ccf[i,6]),
  #          NDWIl1=lag(NDWI,lags_ccf[i,7]),LSDl1=lag(LSD,lags_ccf[i,8]),
  #          LSNl1=lag(LSN,lags_ccf[i,9]),TNAl1=lag(TNA,lags_ccf[i,10]),
  #          Precipl1 = lag(Precip_t, lags_ccf[i,11])) %>%
  #   dplyr::select(Year,Month,RR,RRl1,Nino12SSTAl1,Nino3SSTAl1,Precipl1,TNAl1)
  
  

  #    drop_na(Nino12SSTAl1,Nino3SSTAl1,Nino34SSTAl1,Nino4SSTAl1,EVIl1,NDVIl1,
  #            NDWIl1,LSDl1,LSNl1,TNAl1,Precipl1)
  
  base_test_1 <- basecanton %>%  filter(Year==2019) 
  #    drop_na(Nino12SSTAl1,Nino3SSTAl1,Nino34SSTAl1,Nino4SSTAl1,EVIl1,NDVIl1,
  #            NDWIl1,LSDl1,LSNl1,TNAl1,Precipl1) 
  
  base_test_2 <- basecanton %>%  slice((n()-2):n()) 
  # modelo_gamma <- gamlss(RR~RRl1+Nino3SSTAl1+TNAl1+Precipl1+Month,
  #                        sigma.formula = ~1,
  #                        nu.formula = ~1,
  #                        data = base_ent,
  #                        family = ZAGA(),
  #                        control = gamlss.control(trace = F))
  
  
  base_ent <- base_ent %>% 
    mutate(res = resid(modelo_gamma),
           fmu = fitted(modelo_gamma),
           fsigma = fitted(modelo_gamma,what = 'sigma'),
           fnu = fitted(modelo_gamma,what = 'nu'))
  #modelo_normal <- gamlss(RR~Nino3SSTAl1+Precipl1+as.factor(Month),
  #                        data = base_ent)
  #modelo_IG <- gamlss(RR~RRl1+Nino3SSTAl1+Precipl1,
  #                        data = base_ent,
  #                        family = ZAIG())
  #AA <- rbind(AA,t(AIC(modelo_gamma,modelo_normal,modelo_IG))[2,])
  #uu <- drop1(modelo_gamma)
  #valores_p <- rbind(valores_p,uu$`Pr(Chi)`)
  bases_ent[[i]] <- base_ent
  bases_test_1[[i]] <- base_test_1
  bases_test_2[[i]] <- base_test_2
  
  modGAM[[i]] <- modelo_gamma
  predicciones_in[[i]] <- predict(modGAM[[i]],type = 'response',se.fit = T)
  predicciones_out_1[[i]] <- list()
  
  for(j in 1:12){
    base_proy <- base_test_1[j,]
    if(j>1){
      base_proy$RRl1 <- RRtemp
    }
    
    ppred <- predict(modGAM[[i]],newdata = base_proy,type = 'response')
    ppred_sigma <- predict(modGAM[[i]],newdata = base_proy,
                           type = 'response',what = 'sigma')
    RRtemp <- ppred
    predicciones_out_1[[i]][[j]] <- ppred
  }
  predicciones_out_1[[i]] <- unlist(predicciones_out_1[[i]])
  
  ##Semi-parametric bootstrap  (pag 149)
  
  pred.function <- function(data,i){
    d <- data
    d$RR <- qZAGA(pNO(d$res[i]),mu = d$fmu,sigma = d$fsigma,nu = d$fnu)
    modelo_gamma_b <- update(modelo_gamma,data = d)
    predicciones_out_b <- NULL
    for(j in 1:12){
      base_proy <- base_test_1[j,]
      if(j>1){
        base_proy$RRl1 <- RRtemp
      }
      
      ppred <- predict(modelo_gamma_b,newdata = base_proy,type = 'response',d=d)
      RRtemp <- ppred
      predicciones_out_b[j] <- ppred
    }
    predicciones_out_b
  }
  
  ##Nonparametric bootstrap  (pag 149)
  
  pred.function.NP <- function(data,i){
    d <- data[i,]
    modelo_gamma_b <- update(modelo_gamma,data = d)
    predicciones_out_b <- NULL
    for(j in 1:12){
      base_proy <- base_test_1[j,]
      if(j>1){
        base_proy$RRl1 <- RRtemp
      }
      
      ppred <- predict(modelo_gamma_b,newdata = base_proy,type = 'response',d=d)
      RRtemp <- ppred
      predicciones_out_b[j] <- ppred
    }
    predicciones_out_b
  }
  show(paste0('Bootstrap-Canton-',i))
  #incert.pred <- boot(base_ent,pred.function,R = 100)
  incert.pred <- boot(base_ent,pred.function.NP,R = 100)
  quantiles_out_1[[i]] <- apply(incert.pred$t,2,quantile,
                                probs=c(0.025,0.975))
}

nombres_cantones <- datos_totales %>%
  dplyr::select(Canton,CCanton) %>%
  distinct()

indices_cantones <- c(5,6,7,14,19,23,25,26,27,31)
#indices_cantones <- 1:32

indice <- indices_cantones
base_grafico_lista <- function(indice){
  RR_obs <- bases_ent[[indice]] %>%
    dplyr::select(RR)
  fechas <- bases_ent[[indice]] %>% 
    dplyr::select(Year,Month) %>% mutate(fecha=make_date(Year,Month,1))
  base_grafico <- predicciones_in[[indice]] %>% bind_cols(RR_obs) %>%
    mutate(fechas=fechas$fecha,Canton=nombres_cantones$Canton[indice])
  return(base_grafico)
}

base_grafico <- map_dfr(indice,~base_grafico_lista(.))

grafico_in <- ggplot(data = base_grafico,mapping = aes(x = fechas,y = fit)) + 
  geom_line(col=2)+
  geom_ribbon(mapping = aes(ymin=fit-2*se.fit,ymax=fit+2*se.fit),alpha=0.5,fill=2)+
  geom_line(mapping = aes(y=RR))+
  theme_bw()+
  facet_wrap(facets = vars(Canton),ncol = 2,scales = 'free')+
  xlab('Dates')+ylab('Relative Risk')


base_grafico_lista_2 <- function(indice){
  RR_obs <- bases_test_1[[indice]] %>%
    dplyr::select(RR)
  fechas <- bases_test_1[[indice]] %>% 
    dplyr::select(Year,Month) %>% mutate(fecha=make_date(Year,Month,1))
  predicciones <- data.frame(fit=predicciones_out_1[[indice]])
  quantiles <- data.frame(low = quantiles_out_1[[indice]][1,],
                          up = quantiles_out_1[[indice]][2,])
  base_grafico <- predicciones %>% bind_cols(quantiles) %>%
    bind_cols(RR_obs) %>%
    mutate(fechas=fechas$fecha,Canton=nombres_cantones$Canton[indice])
  return(base_grafico)
}

base_grafico_out<- map_dfr(indice,~base_grafico_lista_2(.))

grafico_out <- ggplot(data = base_grafico_out,mapping = aes(x = fechas,y = fit)) + 
  geom_line(col=2)+
  geom_line(mapping = aes(y=RR))+
  geom_ribbon(mapping = aes(ymin = low, ymax = up),fill=2,alpha=0.5) +
  theme_bw()+
  facet_wrap(facets = vars(Canton),ncol = 2,scales = 'free')+
  xlab('Dates')+ylab('Relative Risk')


metricas <- function(tabla){
  MSE <- sum((tabla$fit-tabla$RR)^2)
  IS_95 <- mean((tabla$up-tabla$low)+
    (2/0.05)*(tabla$low-tabla$RR)*(tabla$RR<tabla$low)+
    (2/0.05)*(tabla$RR-tabla$up)*(tabla$RR>tabla$up))
  return(data.frame(MSE,IS_95))
}

base_grafico_out_g <- base_grafico_out %>% group_by(Canton)

metricas_tot <- base_grafico_out_g %>% group_modify(~metricas(.x))
write_csv(metricas_tot,file = 'metricas_2_Mes.csv')
#ggsave(filename = 'training.png',plot = grafico_in,scale = 2)
#ggsave(filename = 'testing.png',plot = grafico_out,scale = 2)
