library(dplyr)
library(pscl)
library(VGAM)
library(ggplot2)
library(tidyverse)
library(GGally)
library(gamlss)
library(astsa)

# 0. reading data ------------------------------------------------------------

load(file = './Data/datos_totales.RData')
str(datos_totales)
dim(datos_totales)
table(datos_totales$Year,datos_totales$Month)

datos_totales1<-na.omit(datos_totales)

dim(datos_totales)
dim(datos_totales1)

subset_year <- c(2019)
subset_month <- c(1:12)
subset_time <- datos_totales1 %>% filter(Year %in% subset_year, Month %in% subset_month)

dim(subset_time)

hist(subset_time$Cases)

# 1. Scatterplots ----------------------------------------------------------------

base1 <- subset_time %>% 
  dplyr::mutate(logCases=log(Cases+1))%>% 
  dplyr::select(Canton,Cases,RR,Nino12SSTA,Nino3SSTA,Nino4SSTA,Nino34SSTA,TNA,
                Poblacion, PoblacionCR,
                EVI,NDVI,NDWI,LSD,LSN,OFF) 

clog <- function(x) log(x + 0.5)
cfac <- function(x, breaks = NULL) {
  if(is.null(breaks)) breaks <- unique(quantile(x, 0:10/10))
  x <- cut(x, breaks, include.lowest = TRUE, right = FALSE)
  levels(x) <- paste(breaks[-length(breaks)], ifelse(diff(breaks) > 1,
                                                     c(paste("-", breaks[-c(1, length(breaks))] - 1, sep = ""), "+"), ""),
                     sep = "")
  return(x)
}

# 1.1. Casos --------------------------------------------------------------

attach(base1) 

plot(clog(Cases)~cfac(Nino12SSTA)) #índices de temperatura (superficie del mar)
plot(clog(Cases)~cfac(Nino3SSTA)) 
plot(clog(Cases)~cfac(Nino4SSTA)) 
plot(clog(Cases)~cfac(Nino34SSTA)) 
plot(clog(Cases)~cfac(Poblacion)) 
plot(clog(Cases)~cfac(TNA)) # el mismo índice pero caribe
plot(clog(Cases)~cfac(EVI)) # índice de vegetación
plot(clog(Cases)~cfac(NDVI)) # índice de vegetación normalizado
plot(clog(Cases)~cfac(NDWI)) # índice de vegetación 
plot(clog(Cases)~cfac(LSD)) # temperatura diurna (nivel de suelo)
plot(clog(Cases)~cfac(LSN)) # temperatura nocturna

plot(clog(Cases)~Nino12SSTA) #índices de temperatura (superficie del mar)
plot(clog(Cases)~Nino3SSTA)
plot(clog(Cases)~Nino4SSTA)
plot(clog(Cases)~Nino34SSTA) 
plot(clog(Cases)~Poblacion) 
plot(clog(Cases)~TNA) # el mismo índice pero caribe
plot(clog(Cases)~EVI) # índice de vegetación
plot(clog(Cases)~NDVI) # índice de vegetación normalizado
plot(clog(Cases)~NDWI) # índice de vegetación 
plot(clog(Cases)~LSD) # temperatura diurna (nivel de suelo)
plot(clog(Cases)~LSN) # temperatura nocturna



detach(base1) 

.# 2. Cases ---------------------------------------------------------------

# 2.1. Poisson ------------------------------------------------------

#modelo Poisson con todas las variables (lineal)
mod0 <- gamlss(Cases ~ 
                Nino12SSTA+Nino3SSTA+Nino4SSTA+Nino34SSTA+
                TNA+
                EVI+NDVI+NDWI+LSD+LSN+offset(OFF), family=PO, data=base1)  

summary(mod0)

#modelo Poisson con todas las variables (nolineal)
mod1 <- gamlss(Cases ~ 
                 pb(Nino12SSTA)+pb(Nino3SSTA)+pb(Nino4SSTA)+pb(Nino34SSTA)+
                 TNA+
                 pb(EVI)+pb(NDVI)+pb(NDWI)+pb(LSD)+pb(LSN)+offset(OFF),
               family=PO, data=base1)

summary(mod1)

AIC(mod0,mod1)

term.plot(mod1, pages=1, ask=FALSE)
wp(mod1, ylim.all=2) #residuales no apropiados

#modelo Poisson (visualmente simplifica el modelo con term.plot)
mod2 <- gamlss(Cases ~ 
                 Nino12SSTA+Nino3SSTA+Nino4SSTA+Nino34SSTA+
                 TNA+
                 EVI+NDVI+pb(NDWI)+pb(LSD)+LSN+offset(OFF),
               family=PO, data=base1)

AIC(mod0,mod1,mod2)
term.plot(mod2, pages=1, ask=FALSE)
wp(mod2, ylim.all=.6)


yhat<-predict(mod2)
fitted(mod2,"mu")
predict(mod2,type="response")
cbind(mod2$y,mod2$mu.fv,fitted(mod2,"mu"),predict(mod2,type="response"))

plot(fitted(mod2,"mu")~base1$Cases)
abline(0,1)

# 2.2. Negative binomial ------------------------------------------------

#modelo BN con todas las variables (no lineal)
mod3 <- gamlss(Cases ~ 
                 pb(Nino12SSTA)+pb(Nino3SSTA)+pb(Nino4SSTA)+pb(Nino34SSTA)+
                 TNA+
                 pb(EVI)+pb(NDVI)+pb(NDWI)+pb(LSD)+pb(LSN)+offset(OFF),
               family=NBI, data=base1)

term.plot(mod3, pages=1, ask=FALSE)
wp(mod3, ylim.all=2)

#modelo BN (visualmente simplifica el modelo con term.plot)

mod4 <- gamlss(Cases ~ 
                 Nino12SSTA+Nino3SSTA+Nino4SSTA+Nino34SSTA+
                 TNA+
                 pb(EVI)+NDVI+NDWI+LSD+LSN+offset(OFF),
               family=NBI, data=base1)

summary(mod4)

#modelo BN (se elimina algunas variables no significativas)
mod5 <- gamlss(Cases ~ 
                 TNA+
                 pb(EVI)+NDVI+NDWI+LSN+offset(OFF),
               family=NBI, data=base1)

summary(mod5)
plot(mod5)

AIC(mod2,mod3,mod4,mod5)
#se ve una mejora en el AIC de bin.neg comparado con poisson.

wp(mod5, ylim.all=2)

yhat<-predict(mod5)
fitted(mod5,"mu")
predict(mod5,type="response")
cbind(mod5$y,mod5$mu.fv,fitted(mod5,"mu"),predict(mod5,type="response"))

plot(fitted(mod5,"mu")~base1$Cases)
abline(0,1)


# 3. Zero-inflated --------------------------------------------------------

#intentos con ZI pero no mejoran el AIC.

# 3.1. ZIP ------------------------------------------------------

mod6 <- gamlss(Cases ~ 
                 Nino12SSTA+Nino3SSTA+Nino4SSTA+Nino34SSTA+
                 TNA+
                 EVI+NDVI+NDWI+LSD+LSN+offset(OFF),
               sigma.fo= ~
                 Nino12SSTA+Nino3SSTA+Nino4SSTA+Nino34SSTA+
                 TNA+
                 EVI+NDVI+NDWI+LSD+LSN+offset(OFF)
               , family=ZIP, data=base1)

summary(mod6)



# 3.2. ZINBI --------------------------------------------------------------

mod7 <- gamlss(Cases ~ 
                 pb(Nino12SSTA)+pb(Nino3SSTA)+pb(Nino4SSTA)+pb(Nino34SSTA)+
                 TNA+
                 pb(EVI)+pb(NDVI)+pb(NDWI)+pb(LSD)+pb(LSN)+offset(OFF),
               sigma.fo= ~
                 pb(Nino12SSTA)+pb(Nino3SSTA)+pb(Nino4SSTA)+pb(Nino34SSTA)+
                 TNA+
                 pb(EVI)+pb(NDVI)+pb(NDWI)+pb(LSD)+pb(LSN)+offset(OFF),
               nu.fo= ~
                 pb(Nino12SSTA)+pb(Nino3SSTA)+pb(Nino4SSTA)+pb(Nino34SSTA)+
                 TNA+
                 pb(EVI)+pb(NDVI)+pb(NDWI)+pb(LSD)+pb(LSN)+offset(OFF),
               family=NBI, data=base1)


mod8 <- gamlss(Cases ~ 
                 Nino12SSTA+Nino3SSTA+Nino4SSTA+Nino34SSTA+
                 TNA+
                 EVI+NDVI+NDWI+LSD+LSN+offset(OFF),
               sigma.fo= ~
                 Nino12SSTA+Nino3SSTA+Nino4SSTA+Nino34SSTA+
                 TNA+
                 EVI+NDVI+NDWI+LSD+LSN+offset(OFF),
               nu.fo= ~
                 Nino12SSTA+Nino3SSTA+Nino4SSTA+Nino34SSTA+
                 TNA+
                 EVI+NDVI+NDWI+LSD+LSN+offset(OFF)
               , family=ZINBI, data=base1)

summary(mod8)

mod9 <- gamlss(Cases ~ 
                 TNA+NDWI+LSN+offset(OFF),
               sigma.fo= ~
                 Nino12SSTA+Nino3SSTA+Nino4SSTA+Nino34SSTA+
                 LSD+offset(OFF),
               nu.fo= ~
                 Nino12SSTA+
                 EVI+NDVI+LSD+LSN+offset(OFF)
               , family=ZINBI, data=base1)

summary(mod9)

mod10 <- gamlss(Cases ~ 
                 pb(NDWI)+pb(LSN)+offset(OFF),
               sigma.fo= ~
                 Nino12SSTA+Nino3SSTA+pb(Nino34SSTA)+
                 pb(LSD),
               nu.fo= ~1
               , family=ZINBI, data=base1)

summary(mod10)

term.plot(mod10, pages=1, ask=FALSE)
term.plot(mod10, pages=1, what="sigma", ask=FALSE)


AIC(mod2,mod3,mod4,mod5,mod6,mod10)

plot(fitted(mod10,"mu")~base1$Cases)
abline(0,1)


