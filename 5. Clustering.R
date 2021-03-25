library(dplyr)
library(pscl)
library(VGAM)
library(ggplot2)
library(tidyverse)
library(GGally)
library(gamlss)
library(astsa)

library(tidyverse)
library(reshape2)
library(dtw)
library(gridExtra)

# 0. reading data ------------------------------------------------------------

load(file = './Data/datos_totales.RData')
str(datos_totales)
dim(datos_totales)
table(datos_totales$Year,datos_totales$Month)

# creating time index
datos_totales$time <- paste0(datos_totales$Year,"-",datos_totales$Month,"-",15)
datos_totales$time <- as.Date(datos_totales$time, "%Y-%m-%d")

#datos_totales1<-na.omit(datos_totales)

dim(datos_totales)

datos_totales2 <- datos_totales %>% 
  dplyr::mutate(logCases=log(Cases+0.5),logRR=log(RR+0.5))%>% 
  dplyr::select(Canton,time,Cases,logCases,RR,logRR,Nino12SSTA,Nino3SSTA,Nino4SSTA,Nino34SSTA,TNA,
                Poblacion, PoblacionCR,
                EVI,NDVI,NDWI,LSD,LSN,OFF) 


# 1. Converting data.frame to wide format ---------------------------------

#logCases
logCases_wide <- dcast(datos_totales2, Canton ~ time, value.var="logCases")
logCases_wide1 <- logCases_wide %>% dplyr::select(-Canton)
dim(logCases_wide1)
distMatrix_logCases <- dist(logCases_wide1, method="DTW")

logCases_hc <- hclust(distMatrix_logCases, method="average")

plot(logCases_hc,  main="")
rect.hclust(logCases_hc, k = 3, border = 2)
rect.hclust(logCases_hc, k = 4, border = 3)
rect.hclust(logCases_hc, k = 5, border = 4) # 4 clusters
rect.hclust(logCases_hc, k = 6, border = 5)

cluster3_logCases <- cutree(logCases_hc, k = 3)
cluster4_logCases <- cutree(logCases_hc, k = 4)
cluster5_logCases <- cutree(logCases_hc, k = 5)
cluster6_logCases <- cutree(logCases_hc, k = 6)
logCases_wide$cluster3_logCases <- as.factor(cluster3_logCases)
logCases_wide$cluster4_logCases <- as.factor(cluster4_logCases)
logCases_wide$cluster5_logCases <- as.factor(cluster5_logCases)
logCases_wide$cluster6_logCases <- as.factor(cluster6_logCases)

results_logCases <- logCases_wide %>% dplyr::select(Canton,cluster3_logCases,cluster4_logCases, cluster5_logCases, cluster6_logCases)

#logRR
# con RR no fue efectiva?
logRR_wide <- dcast(datos_totales2, Canton ~ time, value.var="logRR")
logRR_wide1 <- logRR_wide %>% dplyr::select(-Canton)
dim(logRR_wide1)

distMatrix_logRR <- dist(logRR_wide1, method="DTW")

logRR_hc <- hclust(distMatrix_logRR, method="average")

plot(logRR_hc,  main="")
rect.hclust(logRR_hc, k = 3, border = 2)
rect.hclust(logRR_hc, k = 4, border = 3)
rect.hclust(logRR_hc, k = 5, border = 4) # 4 clusters
rect.hclust(logRR_hc, k = 6, border = 5)

cluster3_logRR <- cutree(logRR_hc, k = 3)
cluster4_logRR <- cutree(logRR_hc, k = 4)
cluster5_logRR <- cutree(logRR_hc, k = 5)
cluster6_logRR <- cutree(logRR_hc, k = 6)
logRR_wide$cluster3_logRR <- as.factor(cluster3_logRR)
logRR_wide$cluster4_logRR <- as.factor(cluster4_logRR)
logRR_wide$cluster5_logRR <- as.factor(cluster5_logRR)
logRR_wide$cluster6_logRR <- as.factor(cluster6_logRR)

results_logRR <- logRR_wide %>% dplyr::select(Canton,cluster3_logRR,cluster4_logRR, cluster5_logRR, cluster6_logRR)


#######################

#Comparación de clustering usando logCases y logRR.
table(results_logCases$cluster3_logCases,results_logRR$cluster3_logRR)
table(results_logCases$cluster4_logCases,results_logRR$cluster4_logRR)
table(results_logCases$cluster5_logCases,results_logRR$cluster5_logRR)
table(results_logCases$cluster6_logCases,results_logRR$cluster6_logRR)


datos_totales3 <- datos_totales2 %>% 
                          left_join(results_logCases, by= "Canton") %>% 
                          left_join(results_logRR, by= "Canton")


ggplot(datos_totales3, aes(x = time, y = logCases)) + 
  geom_line(aes(color = cluster3_logCases), size = 1) +
  theme_minimal()

ggplot(datos_totales3, aes(x = time, y = logCases)) + 
  geom_line(aes(color = cluster4_logCases), size = 1) +
  theme_minimal()

ggplot(datos_totales3, aes(x = time, y = logRR)) + 
  geom_line(aes(color = cluster3_logRR), size = 1) +
  theme_minimal()

ggplot(datos_totales3, aes(x = time, y = logRR)) + 
  geom_line(aes(color = cluster4_logRR), size = 1) +
  theme_minimal()


#Group_by

grupos <- results_logCases %>% dplyr::select(Canton,cluster3_logCases,cluster4_logCases, 
                                   cluster5_logCases, cluster6_logCases) %>% 
            group_by(cluster5_logCases) %>% 
  nest()

grupos$data[[1]]
grupos$data[[2]]
grupos$data[[3]]
grupos$data[[4]]
grupos$data[[5]]
