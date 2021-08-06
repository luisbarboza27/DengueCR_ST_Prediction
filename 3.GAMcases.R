library(tidyverse)
library(timetk)
library(lubridate)
library(gamlss)

# Carga de datos con riesgos relativos, índices climáticos y variables
# climáticas de MODIS
load('./Data/bases_analisis.RData')

bases_ent_completo <- bases_ent %>% reduce(rbind)

.# 2. Cases ---------------------------------------------------------------

# 2.1. Poisson ------------------------------------------------------

#modelo Poisson con todas las variables (lineal)
m0a <- gamlss(Cases ~ 
                 Nino12SSTA+Nino3SSTA+Nino34SSTA+Nino4SSTA+EVI+
                 NDVI+NDWI+LSD+LSN+TNA+Precip_t+
                 offset(OFF), 
               family=PO, 
               data=bases_ent_completo)  

m0b <- gamlss(Cases ~ 
                Nino12SSTAl1+Nino3SSTAl1+Nino34SSTAl1+Nino4SSTAl1+EVIl1+
                NDVIl1+NDWIl1+LSDl1+LSNl1+TNAl1+Precipl1+
                 offset(OFF), 
               family=PO, 
               data=bases_ent_completo)  


#modelo Poisson con todas las variables (nolineal)
m1a <- gamlss(Cases ~ 
                 pb(Nino12SSTAl1)+pb(Nino3SSTAl1)+pb(Nino34SSTAl1)+pb(Nino4SSTAl1)+pb(EVIl1)+
                 pb(NDVIl1)+pb(NDWIl1)+pb(LSDl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                 offset(OFF),
               family=PO, data=bases_ent_completo)

#Seasonal

m2a <- gamlss(Cases ~ 
                pb(Nino12SSTA)+pb(Nino3SSTA)+pb(Nino34SSTA)+pb(Nino4SSTA)+pb(EVI)+
                pb(NDVI)+pb(NDWI)+pb(LSD)+pb(LSN)+pb(TNA)+pb(Precip_t)+
                offset(OFF)+
                as.factor(Month),
              family=PO, data=bases_ent_completo)

m2b <- gamlss(Cases ~ 
                pb(Nino12SSTAl1)+pb(Nino3SSTAl1)+pb(Nino34SSTAl1)+pb(Nino4SSTAl1)+pb(EVIl1)+
                pb(NDVIl1)+pb(NDWIl1)+pb(LSDl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                offset(OFF)+
                as.factor(Month),
              family=PO, data=bases_ent_completo)


GAIC(m0a,m0b,m1a,m2a,m2b)

summary(m2b)

term.plot(m2b, pages=1, ask=FALSE)
wp(m2b, ylim.all=2) #residuales no apropiados


cbind(m2b$y,m2b$mu.fv,fitted(m2b,"mu"),predict(m2b,type="response"))

plot(fitted(m2b,"mu")~bases_ent_completo$Cases)
abline(0,1)
plot(log(fitted(m2b,"mu"))~log(bases_ent_completo$Cases))

# 2.2. Negative binomial ------------------------------------------------

#modelo BN con todas las variables (no lineal)

m3a <- gamlss(Cases ~ 
               pb(Nino12SSTA)+pb(Nino3SSTA)+pb(Nino34SSTA)+pb(Nino4SSTA)+pb(EVI)+
               pb(NDVI)+pb(NDWI)+pb(LSD)+pb(LSN)+pb(TNA)+pb(Precip_t)+
               offset(OFF),
             family=NBI, data=bases_ent_completo)

m3b <- gamlss(Cases ~ 
                pb(Nino12SSTA)+pb(Nino3SSTA)+pb(Nino34SSTA)+pb(Nino4SSTA)+pb(EVI)+
                pb(NDVI)+pb(NDWI)+pb(LSD)+pb(LSN)+pb(TNA)+pb(Precip_t)+
                offset(OFF)+
                as.factor(Month),
              family=NBI, data=bases_ent_completo)
m3c <- gamlss(Cases ~ 
               pb(Nino12SSTAl1)+pb(Nino3SSTAl1)+pb(Nino34SSTAl1)+pb(Nino4SSTAl1)+pb(EVIl1)+
               pb(NDVIl1)+pb(NDWIl1)+pb(LSDl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
               offset(OFF),
             family=NBI, data=bases_ent_completo)


m3d <- gamlss(Cases ~ 
                pb(Nino12SSTAl1)+pb(Nino3SSTAl1)+pb(Nino34SSTAl1)+pb(Nino4SSTAl1)+pb(EVIl1)+
                pb(NDVIl1)+pb(NDWIl1)+pb(LSDl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                offset(OFF)+
                as.factor(Month),
              family=NBI, data=bases_ent_completo)

term.plot(m3d, pages=1, ask=FALSE)
wp(m3d, ylim.all=2)

GAIC(m2b,m3a,m3b,m3c,m3d)

#m2a:PO  lag- season-pb
#m3a:NB !lag-!season-pb
#n3b:NB !lag- season-pb
#m3c:NB  lag-!season-pb
#n3b:NB  lag- season-pb

cbind(m3c$y,m3c$mu.fv,fitted(m3c,"mu"),predict(m3c,type="response"))

plot(fitted(m3c,"mu")~bases_ent_completo$Cases)
abline(0,1)
plot(log(fitted(m3c,"mu"))~log(bases_ent_completo$Cases+0.5))

summary(m3d)

#modelo BN (visualmente simplifica el modelo con term.plot)

mod4a <- gamlss(Cases ~ 
                 pb(Nino12SSTAl1)+pb(Nino4SSTAl1)+
                 pb(NDVIl1)+pb(NDWIl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                 offset(OFF)+
                 as.factor(Month),
               family=NBI, data=bases_ent_completo)

summary(mod4a)

mod4b <- gamlss(Cases ~ 
                  pb(Nino12SSTAl1)+pb(Nino4SSTAl1)+
                  pb(NDVIl1)+pb(NDWIl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                  offset(OFF),
                family=NBI, data=bases_ent_completo)

GAIC(m3b,m3c,m3d,mod4a,mod4b)

summary(mod4a)

wp(mod4a, ylim.all=2)
wp(mod4b, ylim.all=2)



# 3. Zero-inflated --------------------------------------------------------

#intentos con ZI pero no mejoran el AIC.

# 3.1. ZIP ------------------------------------------------------

#mod5: con las variables seleccionadas
mod5 <- gamlss(Cases ~ 
                 pb(Nino12SSTAl1)+pb(Nino4SSTAl1)+
                 pb(NDVIl1)+pb(NDWIl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                 offset(OFF)+
                 as.factor(Month),
               sigma.fo= ~1, family=ZIP, data=bases_ent_completo)

summary(mod5)
# ! convergence

# 3.2. ZINBI --------------------------------------------------------------

mod6 <- gamlss(Cases ~ 
                 pb(Nino12SSTAl1)+pb(Nino4SSTAl1)+
                 pb(NDVIl1)+pb(NDWIl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                 offset(OFF)+
                 as.factor(Month),
               sigma.fo= ~1,
               nu.fo= ~1,
               family=NBI, data=bases_ent_completo)

GAIC(m3b,m3c,m3d,mod4a,mod4b,mod6)

cbind(m3c$y,m3c$mu.fv,fitted(m3c,"mu"),predict(m3c,type="response"))

sum(m3c$y<1) #observations
sum(m3c$mu.fv<1) 
sum(mod6$mu.fv<1)

mod7 <- gamlss(Cases ~ 
                 pb(Nino12SSTAl1)+pb(Nino4SSTAl1)+
                 pb(NDVIl1)+pb(NDWIl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                 offset(OFF)+
                 as.factor(Month),
               sigma.fo= ~
                 pb(Nino12SSTAl1)+pb(Nino4SSTAl1)+
                 pb(NDVIl1)+pb(NDWIl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                 offset(OFF)+
                 as.factor(Month),
               nu.fo= ~
                 pb(Nino12SSTAl1)+pb(Nino4SSTAl1)+
                 pb(NDVIl1)+pb(NDWIl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                 offset(OFF)+
                 as.factor(Month),
               family=NBI, data=bases_ent_completo)
GAIC(m3b,m3c,m3d,mod4a,mod4b,mod6,mod7)
sum(mod7$y<1) #observations
sum(mod7$mu.fv<1) 
sum(m3c$mu.fv<1) 
summary(mod7)

#m2a:PO  lag- season-pb
#m3a:NB !lag-!season-pb
#n3b:NB !lag- season-pb
#m3c:NB  lag-!season-pb
#m3d:NB  lag- season-pb
#mod4a:NB  lag- season-pb (quitar alguans variables climaticas)
#mod4a:NB  lag-!season-pb (quitar alguans variables climaticas)
#mod6:NBI  lag- season-pb (con las variables seleccionadas en 4a)
#mod7:NBI  lag- season-pb (completo)
#mod7a:NBI  lag-!season-pb (completo)
mod7a <- gamlss(Cases ~ 
                 pb(Nino12SSTAl1)+pb(Nino4SSTAl1)+
                 pb(NDVIl1)+pb(NDWIl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                 offset(OFF),
               sigma.fo= ~
                 pb(Nino12SSTAl1)+pb(Nino4SSTAl1)+
                 pb(NDVIl1)+pb(NDWIl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                 offset(OFF),
               nu.fo= ~
                 pb(Nino12SSTAl1)+pb(Nino4SSTAl1)+
                 pb(NDVIl1)+pb(NDWIl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                 offset(OFF),
               family=NBI, data=bases_ent_completo)
GAIC(m2a,m3b,m3c,m3d,mod4a,mod4b,mod6,mod7,mod7a)





sum(mod7$y<1) #observations
sum(mod7$mu.fv<1) 
summary(mod7a)

bases_ent_completo <- bases_ent_completo %>% mutate(season=ifelse(Month %in% c(7,8,9),1,0))

mod7b <- gamlss(Cases ~ 
                 pb(Nino12SSTAl1)+pb(Nino4SSTAl1)+
                 pb(NDVIl1)+pb(NDWIl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                 offset(OFF)+
                 as.factor(season),
               sigma.fo= ~
                 pb(Nino12SSTAl1)+pb(Nino4SSTAl1)+
                 pb(NDVIl1)+pb(NDWIl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                 offset(OFF)+
                 as.factor(Month),
               nu.fo= ~1,
               family=NBI, data=bases_ent_completo)



summary(mod7b)

summary(mod7a)
term.plot(mod7a, pages=1, ask=FALSE)
term.plot(mod7a, pages=1, what="sigma", ask=FALSE)




