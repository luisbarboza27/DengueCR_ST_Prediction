library(dplyr)
library(pscl)
library(VGAM)
library(ggplot2)
library(tidyverse)
library(GGally)
library(gamlss)

# 0. reading data ------------------------------------------------------------

load(file = './Data/datos_totales.RData')
str(datos_totales)
dim(datos_totales)
table(datos_totales$Year,datos_totales$Month)

# creating time index
datos_totales$time <- paste0(datos_totales$Year,"-",datos_totales$Month,"-",15)
datos_totales$time <- as.Date(datos_totales$time, "%Y-%m-%d")

#32 cantones
Cantones_verified <- datos_totales %>% distinct(Canton)
nrow(Cantones_verified)

#randomly choose 5 cantones
set.seed(100)
cantones_sampled <- sample(Cantones_verified$Canton,5)

selected_canton <- datos_totales %>% filter(Canton %in% cantones_sampled)

ggplot(selected_canton, aes(x = time, y = Cases)) + 
  geom_line(aes(color = Canton), size = 1) +
  theme_minimal()

str(datos_totales)

sum(datos_totales$Cases==0)
sum(!datos_totales$Cases==0)

hist(datos_totales$Cases)
hist(log(datos_totales$Cases+0.5))

hist(datos_totales$RR)
hist(log(datos_totales$RR+0.5))
hist(datos_totales$OFF) #OFF=log(1/constRR)

plot(datos_totales$RR,datos_totales$Cases)

# 1. Scatterplots ----------------------------------------------------------------

datos_totales1 <- datos_totales %>% 
  dplyr::mutate(logCases=log(Cases+1))%>% 
  dplyr::select(Canton,Cases,RR,Nino12SSTA,Nino3SSTA,Nino4SSTA,Nino34SSTA,TNA,
                Poblacion, PoblacionCR,
         EVI,NDVI,NDWI,LSD,LSN) 

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

  datos_totales2<-na.omit(datos_totales1)

  attach(datos_totales2) 

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


# 1.2. RR -----------------------------------------------------------------

  plot(clog(RR)~cfac(Nino12SSTA)) #índices de temperatura (superficie del mar)
  plot(clog(RR)~cfac(Nino3SSTA)) 
  plot(clog(RR)~cfac(Nino4SSTA)) 
  plot(clog(RR)~cfac(Nino34SSTA)) 
  plot(clog(RR)~cfac(Nino34SSTA)) 
  plot(clog(RR)~cfac(Poblacion))
  plot(clog(RR)~cfac(TNA)) # el mismo índice pero caribe
  plot(clog(RR)~cfac(EVI)) # índice de vegetación
  plot(clog(RR)~cfac(NDVI)) # índice de vegetación normalizado
  plot(clog(RR)~cfac(NDWI)) # índice de vegetación 
  plot(clog(RR)~cfac(LSD)) # temperatura diurna (nivel de suelo)
  plot(clog(RR)~cfac(LSN)) # temperatura nocturna

# 1.3. OFF -----------------------------------------------------------------
  
  plot(OFF~cfac(Nino12SSTA)) #índices de temperatura (superficie del mar)
  plot(OFF~cfac(Nino3SSTA)) 
  plot(OFF~cfac(Nino4SSTA)) 
  plot(OFF~cfac(Nino34SSTA)) 
  plot(OFF~cfac(TNA)) # el mismo índice pero caribe
  plot(OFF~cfac(Poblacion))
  plot(OFF~cfac(EVI)) # índice de vegetación
  plot(OFF~cfac(NDVI)) # índice de vegetación normalizado
  plot(OFF~cfac(NDWI)) # índice de vegetación 
  plot(OFF~cfac(LSD)) # temperatura diurna (nivel de suelo)
  plot(OFF~cfac(LSN)) # temperatura nocturna
  
  plot(OFF~Nino12SSTA) #índices de temperatura (superficie del mar)
  plot(OFF~Nino3SSTA) 
  plot(OFF~Nino4SSTA)
  plot(OFF~Nino34SSTA)
  plot(OFF~TNA) # el mismo índice pero caribe
  plot(OFF~Poblacion)
  plot(OFF~EVI) # índice de vegetación
  plot(OFF~NDVI) # índice de vegetación normalizado
  plot(OFF~NDWI) # índice de vegetación 
  plot(OFF~LSD) # temperatura diurna (nivel de suelo)
  plot(OFF~LSN) # temperatura nocturna
  
  detach(datos_totales2) 

# 2. GAMLSS ---------------------------------------------------------------

  # 2.1. Continous case (RR) -----------------------------------------------
  
  #datos_totales2$RRt<-datos_totales2$RR+0.0001
  datos_totales2$RRt<-datos_totales2$RR+0.5
  
  # 2.1.1 Linear model ------------------------------------------------------
  
  m1a <- gamlss(RRt ~ 
                  Nino12SSTA+Nino3SSTA+Nino4SSTA+Nino34SSTA+
                  TNA+Poblacion+
                  EVI+NDVI+NDWI+LSD+LSN, family=NO, data=datos_totales2, trace=FALSE)  
  summary(m1a)  
  fitted(m1a, "sigma")[1] 
  plot(m1a)
  drop1(m1a)
  
  m1b <- gamlss(RRt ~ 
                  TNA+Poblacion+
                  EVI+NDWI+LSD+LSN, family=NO, data=datos_totales2, trace=FALSE)  
  summary(m1b)   
  plot(m1b)
  drop1(m1b)
  
  
  # 2.1.2. GLM --------------------------------------------------------------

  m2a <- gamlss(RRt ~ 
                  Nino12SSTA+Nino3SSTA+Nino4SSTA+Nino34SSTA+
                  TNA+Poblacion+
                  EVI+NDVI+NDWI+LSD+LSN, family=GA, data=datos_totales2)
  summary(m2a)
  plot(m2a)
  drop1(m2a)
  
  m2b <- gamlss(RRt ~ 
                  Nino12SSTA+
                  TNA+Poblacion+
                  EVI+NDVI+NDWI+LSD+LSN, family=GA, data=datos_totales2)
  summary(m2b)
  plot(m2a)
  
  AIC(m1b,m2a,m2b)

  # 2.1.3. GAM --------------------------------------------------------------
  
  m3a <- gamlss(RRt ~ 
                  pb(Nino12SSTA)+pb(Nino3SSTA)+pb(Nino4SSTA)+pb(Nino34SSTA)+
                  pb(TNA)+pb(Poblacion)+
                  pb(EVI)+pb(NDVI)+pb(NDWI)+pb(LSD)+pb(LSN), 
                family=GA , data=datos_totales2)

  summary(m3a)
  plot(m3a)
  term.plot(m3a, pages=1, ask=FALSE)
  
  AIC(m1b,m2a,m2b,m3a)
  
  m3b <- gamlss(RRt ~ 
                  Nino12SSTA+Nino3SSTA+Nino4SSTA+Nino34SSTA+
                  pb(TNA)+pb(Poblacion)+
                  pb(EVI)+pb(NDVI)+pb(NDWI)+pb(LSD)+pb(LSN), 
                family=GA , data=datos_totales2)

  summary(m3b) 
  AIC(m1a,m2a,m3a,m3b)
  
  m3c <- gamlss(RRt ~ 
                  pb(TNA)+pb(Poblacion)+
                  pb(EVI)+pb(NDVI)+pb(NDWI)+pb(LSD)+pb(LSN), 
                family=GA , data=datos_totales2)
  
  summary(m3c) 
  plot(m3c)
  AIC(m1a,m2a,m3c)
  
  #To check for the approximate significance of the contribution of the smoothers
  drop1(m3c)
  term.plot(m3c, pages=1, ask=FALSE)
  
  wp(m3c, ylim.all=.6)
  
  m3d <- gamlss(RRt ~ 
                  pb(TNA)+pb(Poblacion)+
                  pb(EVI)+NDVI+pb(NDWI)+pb(LSD)+pb(LSN), 
                family=GA , data=datos_totales2)
  
  summary(m3d) 
  plot(m3d) 
  
  drop1(m3d)  
  term.plot(m3d, pages=1, ask=FALSE)
  
  AIC(m1a,m2a,m3a,m3b,m3c,m3d)
  

# 2.1.4. GAMLSS -----------------------------------------------------------

  m4a <- gamlss(RRt ~ 
                  pb(TNA)+pb(Poblacion)+
                  pb(EVI)+NDVI+pb(NDWI)+pb(LSD)+pb(LSN), 
                sigma.fo=~
                  pb(TNA)+pb(Poblacion)+
                  pb(EVI)+NDVI+pb(NDWI)+pb(LSD)+pb(LSN),
                family=GA , data=datos_totales2)
 
  summary(m4a)
  plot(m4a)
  term.plot(m4a, pages=1, ask=FALSE)
  term.plot(m4a, pages=1, what="sigma", ask=FALSE)
  
  drop1(m4a)
  drop1(m4a, what="sigma")
  
  m4b <- gamlss(RRt ~ 
                  pb(Poblacion)+
                  EVI+NDVI+pb(NDWI), 
                sigma.fo=~
                  pb(Poblacion)+NDWI,
                family=GA , data=datos_totales2, trace=FALSE)
  
  summary(m4b)
  term.plot(m4b, pages=1, ask=FALSE)
  term.plot(m4b, pages=1, what="sigma", ask=FALSE)
  
  AIC(m4a, m4b)
  
  drop1(m4b)
  drop1(m4b, what="sigma")
  plot(m4b)
  
  wp(m4b, ylim.all=3)
  title("(b)")
  
  
  # ni
  m5a <- gamlss(RRt ~ 
                  pb(Poblacion)+pb(EVI)+NDVI+pb(NDWI), 
                sigma.fo=~pb(Poblacion)+pb(NDWI),
                nu.fo=~1,
                family=GA , data=datos_totales2, trace=FALSE)
  
  m5b <- gamlss(RRt ~ pb(Poblacion)+pb(EVI)+NDVI+pb(NDWI), 
                sigma.fo=~pb(Poblacion)+pb(NDWI),
                nu.fo=~pb(Poblacion)+pb(EVI)+NDVI+pb(NDWI),
                family=GA , data=datos_totales2, trace=FALSE)
  
  AIC(m4b, m5a, m5b)
  
  
  m6a <- gamlss(RRt ~ pb(Poblacion)+pb(EVI)+NDVI+pb(NDWI), 
                sigma.fo=~pb(Poblacion)+pb(NDWI),
                nu.fo=~1,
                family=BCCGo , data=datos_totales2, trace=FALSE)
  
  m6b <- gamlss(RRt ~ pb(Poblacion)+pb(EVI)+NDVI+pb(NDWI), 
                sigma.fo=~pb(Poblacion)+pb(NDWI),
                nu.fo=~pb(Poblacion)+pb(EVI)+NDVI+pb(NDWI),
                family=BCCGo , data=datos_totales2, trace=FALSE)  
  
  
    
# 2.2. Cases -----------------------------------------------
  
# 2.2.1 Poisson ------------------------------------------------------

  mm0a <- gamlss(Cases ~ 1, family=PO, data=datos_totales2)
  summary(mm0a)
  
  mm0b <- gamlss(Cases ~ 1, 
                 sigma.fo= ~ 1,
                 family=ZIP, data=datos_totales2)


  mm1a <- gamlss(Cases ~ 
                  pb(Nino12SSTA)+pb(Nino3SSTA)+pb(Nino4SSTA)+pb(Nino34SSTA)+
                   pb(TNA)+pb(Poblacion)+
                   pb(EVI)+pb(NDVI)+pb(NDWI)+pb(LSD)+pb(LSN),
                 family=PO, data=datos_totales2)
  summary(mm1a)
  plot(mm1a)
  drop1(mm2a)
  
  ZIP()
  mm2a <- gamlss(Cases ~ 
                   TNA+Poblacion+
                   EVI+NDVI+NDWI+LSD+LSN,
                 sigma.fo= ~
                   TNA+Poblacion+
                   EVI+NDVI+NDWI+LSD+LSN
                 , family=ZIP, data=datos_totales2)
  summary(mm2a)
  plot(mm1a)
  drop1(mm2a) 
  
  
  
  
  
  
