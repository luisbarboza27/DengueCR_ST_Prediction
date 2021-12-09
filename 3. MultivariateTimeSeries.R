library(tidyverse)
library(timetk)
library(lubridate)
library(gamlss)
library(zoo)
library(boot)
library(dlnm)
library(vars)
library(MTS)

# Carga de datos con riesgos relativos, índices climáticos y variables
# climáticas de MODIS
load('./Data/datos_totales.RData')
set.seed(1)

cantones <- datos_totales %>%
  dplyr::select(CCanton,Canton) %>% distinct()

data <- datos_totales %>% 
              dplyr::select(Year,Month,Canton,CCanton,Precip_t,Nino34SSTA,NDVI,LSD,TNA) %>%
              drop_na()

#table(data$Year,data$Month)

bases_ent <- list() # Bases de entrenamiento por canton
bases_test <- list() # Bases de prueba por canton
predicciones_out <- list()
predicciones_in <- list()
model.info<- matrix(NA, nrow = nrow(cantones), ncol = 4)

for(i in 1:length(cantones$CCanton)){
  show(paste0('Ajuste y Predicción-Canton-',i))
  data.i <- data %>% dplyr::filter(CCanton == cantones$CCanton[i])  %>% 
                     mutate(fecha=make_date(Year,Month,1)) 
  
  bases_ent_i <- data.i  %>%
              dplyr::select(fecha,Precip_t,Nino34SSTA,NDVI,LSD,TNA) %>% 
              slice(1:(n()-12))
  
  bases_test_i <- data.i %>% 
              dplyr::select(fecha,Precip_t,Nino34SSTA,NDVI,LSD,TNA) %>%  
              slice((n()-11):n()) 
  
  bases_ent[[i]] <- bases_ent_i
  bases_test[[i]] <- bases_test_i
  
  bases_ent_i <- bases_ent_i %>% select(-fecha)
  bases_test_i <- bases_test_i %>% select(-fecha) 
  
  # plot.ts(bases_ent , main = "", xlab = "")
  # stats::acf(bases_ent,lag.max = 100)
  
  #seleccionar el orden de VAR de acuerdo a BIC
  seleccion <- VARselect(bases_ent_i, season = 12, lag.max=12, type="both")
  
  model.info[i,]<- seleccion$selection
  
  order <- seleccion$selection[3] 
  modvar = vars::VAR(bases_ent_i, p=order, season = 12, type="both")
  # summary(modvar)
  
  # stats::acf(residuals(modvar))
  # serial.test(modvar, lags.pt=12, type="PT.adjusted")
  # shapiro.test(residuals(modvar)[,1])
  # shapiro.test(residuals(modvar)[,2])
  # shapiro.test(residuals(modvar)[,3])
  # shapiro.test(residuals(modvar)[,4])
  # shapiro.test(residuals(modvar)[,5])
  # mvnormtest::mshapiro.test(t(residuals(modvar)))

  predicciones_in_i <- fitted(modvar)
  predicciones_out_i <- predict(modvar, n.ahead = 12, ci = 0.95)
  #fanchart(predicciones_out)
  predicciones_out_i <- predicciones_out_i$fcst
  
  predicciones_in[[i]] <- predicciones_in_i
  predicciones_out[[i]] <- predicciones_out_i
}  
  
model.info <- cbind(cantones,model.info)
colnames(model.info)[3:6] <- c("AIC","HQ","SC","FPE")

save(model.info,
     bases_ent,
     bases_test,
     predicciones_out,
     predicciones_in,
     file="./TSprediction/TSprediccion_climate.Rdata")





#i <- 1
for(i in 1:length(predicciones_in)){
  show(paste0('Predicción-Canton-',i))
TT <- nrow(predicciones_in[[i]])
bases_ent_i <- bases_ent[[i]][-c(1:model.info$SC[i]),]

bases_test_i <- bases_test[[i]]

bases_i <- rbind(bases_ent_i,bases_test_i)
bases_i <- bases_i %>% gather(.,"series","value", -fecha)

prediccion <- data.frame(predicciones_in[[i]])
prediccion$fecha <- bases_ent_i$fecha
data.pred <- prediccion %>% gather(.,"series","fcst", -fecha)

  summary.pred.out <- NULL
  prediccion.out <- predicciones_out[[i]]
for(j in 1:length(prediccion.out)){
  pred.out <- data.frame(prediccion.out[[j]])
  pred.out$series <- rep(names(prediccion.out)[j],nrow(pred.out))
  pred.out$fecha <- bases_test_i$fecha
  summary.pred.out <- rbind(summary.pred.out,pred.out)
}
  
  full_pred <- data.pred %>% full_join(.,summary.pred.out)
  
  data.join <- bases_i %>% full_join(.,full_pred)
  
  figure <- data.join %>%
    ggplot() + 
    geom_line(aes(x=`fecha`, y=`value`), col = "red")+ 
    geom_line(aes(x = `fecha`, y = `fcst`), col = "cadetblue4") +
    geom_ribbon(aes(x = `fecha`, ymin = `lower`, ymax = `upper`), 
                fill = "cadetblue4", alpha = 0.5) +
    xlab("time") +
    ylab("y") +
    theme_bw() +
    facet_wrap( ~series,scales="free",nrow=5)

  file <- paste0("TSprediction/TSprediction_",i,".jpg")
ggsave(figure, filename = file, height = 20, width = 20)
}





