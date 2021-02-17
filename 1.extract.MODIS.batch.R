library(tidyverse)
library(MODISTools)
library(sf)
library(plyr)
library(furrr)
library(doParallel)

# Carga de localizaciones y fechas----
load('./Data/localizaciones_MODIS.RData')
#fechas1 <- mt_dates(product = 'MOD13Q1',lat = localizaciones_MODIS[12,3],lon = localizaciones_MODIS[12,4])
#fechas2 <- mt_dates(product = 'MOD11A2',lat = localizaciones_MODIS[12,3],lon = localizaciones_MODIS[12,4])
fechas_1 <- c('2000-02-18','2021-01-17')
fechas_2 <- c('2000-02-18','2021-02-02')

#fechas_1 <- c('2000-02-18','2000-03-31')
#fechas_2 <- c('2000-02-18','2000-03-31')

# Formateo de localizaciones según la función mt_batch_subset
df_MODIS <- localizaciones_MODIS %>%
#  filter(CCanton==605) %>%
  select(site_name=DTA,lat=Latitud,lon=Longitud)

# Descarga de datos:
show('1-NDVI')
baseNDVI <- mt_batch_subset(df = df_MODIS,
                       product = 'MOD13Q1',
                band = '250m_16_days_NDVI',
                start=fechas_1[1],
                end = fechas_1[2],
                ncores = 10)
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



