library(tidyverse)
library(MODISTools)
library(sf)
library(plyr)
library(furrr)
library(doParallel)

# Carga de localizaciones y fechas----
load('./Data/localizaciones_MODIS.RData')
# fecha_maxima <- NULL
# extrae_maximo <- function(i,producto){
#   fechas <- mt_dates(product = producto,
#                       lat = localizaciones_MODIS[i,3],
#                       lon = localizaciones_MODIS[i,4])
#   return(fechas[dim(fechas)[1],2])
# }
#fechas_Q1 <- map(1:dim(localizaciones_MODIS)[1],~extrae_maximo(.,'MOD13Q1'))
#fechas_A2 <- map(1:dim(localizaciones_MODIS)[1],~extrae_maximo(.,'MOD11A2'))
#fechas1 <- mt_dates(product = 'MOD13Q1',lat = localizaciones_MODIS[12,3],lon = localizaciones_MODIS[12,4])
#fechas2 <- mt_dates(product = 'MOD11A2',lat = localizaciones_MODIS[12,3],lon = localizaciones_MODIS[12,4])
fechas_1 <- c('2021-02-02','2021-05-09')
fechas_2 <- c('2021-02-10','2021-05-17')

#fechas_1 <- c('2000-02-18','2000-03-31')
#fechas_2 <- c('2000-02-18','2000-03-31')

# Formateo de localizaciones según la función mt_batch_subset
df_MODIS <- localizaciones_MODIS %>%
#  filter(CCanton==605) %>%
  select(site_name=DTA,lat=Latitud,lon=Longitud)

# Descarga de datos:
show('1-NDVI')
baseNDVI <- NULL
for(i in 18:dim(df_MODIS)[1]){
  show(i)
  baseNDVI_t <- mt_batch_subset(df = df_MODIS[i,],
                              product = 'MOD13Q1',
                              band = '250m_16_days_NDVI',
                              start=fechas_1[1],
                              end = fechas_1[2],
                              ncores = 10)
  baseNDVI <- baseNDVI %>% bind_rows(baseNDVI_t) 
}

show('2-EVI')
baseEVI  <- mt_batch_subset(df = df_MODIS,
                            product = 'MOD13Q1',
                      band = '250m_16_days_EVI',
                      start = fechas_1[1],
                      end = fechas_1[2],
                      ncores = 10)
show('3-NIR')
baseNIR  <- mt_batch_subset(df = df_MODIS,
                            product = 'MOD13Q1',
                      band = '250m_16_days_NIR_reflectance',
                      start = fechas_1[1],
                      end = fechas_1[2],
                      ncores = 10)
show('4-MIR')
baseMIR  <- mt_batch_subset(df = df_MODIS,
                            product = 'MOD13Q1',
                      band = '250m_16_days_MIR_reflectance',
                      start = fechas_1[1],
                      end = fechas_1[2],
                      ncores = 10)
show('5-LST_Day')
baseLSTDia  <- mt_batch_subset(df=df_MODIS,
                               product = 'MOD11A2',
                         band = 'LST_Day_1km',
                         start = fechas_2[1],
                         end = fechas_2[2],
                         ncores = 10)
show('6-LST_Night')
baseLSTNoche  <- mt_batch_subset(df=df_MODIS,
                           product = 'MOD11A2',
                           band = 'LST_Night_1km',
                           start = fechas_2[1],
                           end = fechas_2[2],
                           ncores = 10)


save(baseEVI,baseNDVI,baseNIR,baseMIR,baseLSTDia,baseLSTNoche,
     file = 'MODISData.RData')



