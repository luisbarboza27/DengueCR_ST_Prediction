library(tidyverse)
library(timetk)
library(lubridate)
library(gamlss)

load('./Data/bases_analisis.RData')
load("./Data/clustering.Rdata") #results_logRR

bases_ent_completo <- bases_ent %>% reduce(rbind) %>%
  mutate(Date=make_date(Year,Month,1))

# 0. Defining dataset  -----------------------------------------------------------

cluster1 <- results_logRR %>% filter(cluster5_logRR==1) %>% dplyr::select(Canton) %>% unlist()
cluster2 <- results_logRR %>% filter(cluster5_logRR==2) %>% dplyr::select(Canton) %>% unlist()
cluster3 <- results_logRR %>% filter(cluster5_logRR==3) %>% dplyr::select(Canton) %>% unlist()
cluster4 <- results_logRR %>% filter(cluster5_logRR==4) %>% dplyr::select(Canton) %>% unlist()
cluster5 <- results_logRR %>% filter(cluster5_logRR==5) %>% dplyr::select(Canton) %>% unlist()

bases_ent_completo1 <- bases_ent_completo %>% filter(Canton %in% cluster1)
bases_ent_completo2 <- bases_ent_completo %>% filter(Canton %in% cluster2)
bases_ent_completo3 <- bases_ent_completo %>% filter(Canton %in% cluster3)
bases_ent_completo4 <- bases_ent_completo %>% filter(Canton %in% cluster4)
bases_ent_completo5 <- bases_ent_completo %>% filter(Canton %in% cluster5)


# 1. Cluster 1 ------------------------------------------------------------

dataset <-bases_ent_completo1
dataset <-bases_ent_completo2
dataset <-bases_ent_completo3
dataset <-bases_ent_completo4
dataset <-bases_ent_completo5

sum(dataset$Cases==0)/length(dataset$Cases)

m1a <- gamlss(Cases ~ 
                pb(Nino12SSTAl1)+pb(Nino3SSTAl1)+pb(Nino34SSTAl1)+pb(Nino4SSTAl1)+pb(EVIl1)+
                pb(NDVIl1)+pb(NDWIl1)+pb(LSDl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                offset(OFF),
              family=PO, data=dataset)
summary(m1a)
m1b <- gamlss(Cases ~ 
                pb(Nino12SSTAl1)+pb(Nino3SSTAl1)+pb(Nino34SSTAl1)+pb(Nino4SSTAl1)+pb(EVIl1)+
                pb(NDVIl1)+pb(NDWIl1)+pb(LSDl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                offset(OFF),
              family=NBI, data=dataset)
summary(m1b)

m1c <- gamlss(Cases ~ 
                pb(Nino12SSTAl1)+pb(Nino3SSTAl1)+pb(Nino34SSTAl1)+pb(Nino4SSTAl1)+pb(EVIl1)+
                pb(NDVIl1)+pb(NDWIl1)+pb(LSDl1)+pb(LSNl1)+pb(TNAl1)+pb(Precip_t)+pb(Precipl1)+
                offset(OFF),
              family=NBI, data=dataset)
summary(m1c)

m1d <- gamlss(Cases ~ 
                pb(Nino12SSTAl1)+pb(Nino3SSTAl1)+pb(Nino34SSTAl1)+pb(Nino4SSTAl1)+pb(EVIl1)+
                pb(NDVIl1)+pb(NDWIl1)+pb(LSDl1)+pb(LSNl1)+pb(TNAl1)+pb(Precip_t)+
                offset(OFF),
              family=NBI, data=dataset)
summary(m1d)

m1e <- gamlss(Cases ~ 
                pb(Nino12SSTAl1)+pb(Nino3SSTAl1)+pb(Nino34SSTAl1)+pb(Nino4SSTAl1)+
              pb(LSDl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                offset(OFF),
              family=NBI, data=dataset)
summary(m1e)

plot(fitted(m1b,"mu")~dataset$Cases)
abline(0,1)
plot(log(fitted(m1b,"mu"))~log(dataset$Cases))

plot(dataset$Cases,type="l")
points(fitted(m1b,"mu"),col=2,type="l")

m1c <- gamlss(Cases ~ 
                 pb(Nino12SSTAl1)+pb(Nino4SSTAl1)+
                 pb(NDVIl1)+pb(NDWIl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                 offset(OFF)+
                 as.factor(Month),
               sigma.fo= ~1, family=ZIP, data=dataset)


m1d <- gamlss(Cases ~ 
                 pb(Nino12SSTAl1)+pb(Nino4SSTAl1)+
                 pb(NDVIl1)+pb(NDWIl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                 offset(OFF)+
                 as.factor(Month),
               sigma.fo= ~1,
               nu.fo= ~1,
               family=NBI, data=dataset)
GAIC(m1a,m1b,m1c,m1d)



dataset$predict <- fitted(m1b,"mu")


plot(dataset$Cases,type="l")
points(fitted(m1b,"mu"),col=2,type="l")
points(fitted(m1d,"mu"),col=3,type="l")


dataset %>%
  ggplot( aes(x=Date, y=Cases, group=Canton, color=Canton)) +
  geom_line()


plot(m1b)
wp(m1b)
Q.stats(m1b)



