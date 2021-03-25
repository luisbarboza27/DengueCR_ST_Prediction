library(dplyr)
library(pscl)
library(VGAM)
library(ggplot2)
library(tidyverse)
library(timetk)
library(GGally)
library(gamlss)
library(xts)

# 0. reading data ------------------------------------------------------------

load(file = './Data/datos_totales.RData')
str(datos_totales)
dim(datos_totales)
table(datos_totales$Year,datos_totales$Month)

# creating time index
datos_totales$time <- paste0(datos_totales$Year,"-",datos_totales$Month,"-",15)
datos_totales$time <- as.Date(datos_totales$time, "%Y-%m-%d")

datos_totales1<-na.omit(datos_totales)

dim(datos_totales)
dim(datos_totales1)


# 1. Time lag --------------------------------------------------------------

Alajuela <- datos_totales1 %>% filter(Canton == "Alajuela")
clog <- function(x) log(x + 0.5)

attach(Alajuela)

lag2.plot (Nino12SSTA,clog(Cases), 12) 
lag2.plot (Nino3SSTA,clog(Cases), 12) 
lag2.plot (Nino4SSTA,clog(Cases), 12) 
lag2.plot (Nino34SSTA,clog(Cases), 12) 

lag2.plot (TNA,clog(Cases), 12) 
lag2.plot (EVI,clog(Cases), 12) 
lag2.plot (NDVI,clog(Cases), 12) 
lag2.plot (NDWI,clog(Cases), 12) 
lag2.plot (LSD,clog(Cases), 12) 
lag2.plot (LSN,clog(Cases), 12) 

##
ccf(clog(Cases),Nino12SSTA) #índices de temperatura (superficie del mar)
ccf(clog(Cases),Nino3SSTA)
ccf(clog(Cases),Nino4SSTA)
ccf(clog(Cases),Nino34SSTA)

ccf(clog(Cases),TNA) # el mismo índice pero caribe
ccf(clog(Cases),EVI) # índice de vegetación
ccf(clog(Cases),NDVI) # índice de vegetación normalizado
ccf(clog(Cases),NDWI) # índice de vegetación 
ccf(clog(Cases),LSD) # temperatura diurna (nivel de suelo)
ccf(clog(Cases),LSN) # tempera

detach(Alajuela)

# 2. 5 selected cantones -------------------------------------------------------

#32 cantones
Cantones_verified <- datos_totales %>% distinct(Canton)
nrow(Cantones_verified)

#randomly choose 5 cantones
set.seed(100)
cantones_sampled <- sample(Cantones_verified$Canton,5)
cantones_sampled <- c("Liberia","SantaCruz","Alajuela","Limon","Perez Zeledón")
selected_canton <- datos_totales1 %>% filter(Canton %in% cantones_sampled) %>%
  mutate(logCases = clog(Cases))

#ACF for Cases
selected_canton %>% group_by(Canton) %>%
  plot_acf_diagnostics(.date_var = time, 
                       .value = Cases,               
                       .lags = 24, 
                       .interactive = FALSE)

#ACF for logCases
selected_canton %>% group_by(Canton) %>%
  plot_acf_diagnostics(.date_var = time, 
                       .value = logCases,               
                       .lags = 24, 
                       .interactive = FALSE)

names(selected_canton)

# 
selected_canton %>% group_by(Canton) %>%
  plot_acf_diagnostics(.date_var = time, 
                       .value = logCases,        
                       .ccf_vars    = c(Nino12SSTA, Nino3SSTA,
                                        Nino4SSTA,Nino34SSTA), # CCFs
                       .show_ccf_vars_only = TRUE,
                       .show_white_noise_bars = TRUE,
                       .lags = 24, 
                       .white_noise_line_color = 2,
                       .interactive = FALSE)

# 
selected_canton %>% group_by(Canton) %>%
  plot_acf_diagnostics(.date_var = time, 
                       .value = logCases,        
                       .ccf_vars    = c(TNA,EVI,NDVI,NDWI,LSD,LSN), # CCFs
                       .show_ccf_vars_only = TRUE,
                       .show_white_noise_bars = TRUE,
                       .lags = 24, 
                       .white_noise_line_color = 2,
                       .interactive = FALSE)

# 3. defining lag variables -----------------------------------------------


  
  