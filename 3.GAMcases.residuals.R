library(tidyverse)
library(gamlss)
library(lubridate)
library(timetk)
library(gridExtra)

load('./Data/bases_analisis.RData')
load("./Data/clustering.Rdata")

bases_ent_completo <- bases_ent %>% reduce(rbind) %>%
  mutate(Date=make_date(Year,Month,1))

bases_ent_completo <- bases_ent_completo %>% left_join(results_logRR,by="Canton")


m1a <- gamlss(Cases ~ 
                pb(Nino12SSTAl1)+pb(Nino3SSTAl1)+pb(Nino34SSTAl1)+pb(Nino4SSTAl1)+pb(EVIl1)+
                pb(NDVIl1)+pb(NDWIl1)+pb(LSDl1)+pb(LSNl1)+pb(TNAl1)+pb(Precip_t)+
                offset(OFF),
              family=NBI, data=bases_ent_completo)

m1b <- gamlss(Cases ~ 
                pb(Nino12SSTAl1)+pb(Nino3SSTAl1)+pb(Nino34SSTAl1)+pb(Nino4SSTAl1)+pb(EVIl1)+
                pb(NDVIl1)+pb(NDWIl1)+pb(LSDl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                offset(OFF),
              family=NBI, data=bases_ent_completo)

m2a <- gamlss(Cases ~ 
                pb(Nino12SSTAl1)+pb(Nino3SSTAl1)+pb(Nino34SSTAl1)+pb(Nino4SSTAl1)+pb(EVIl1)+
                pb(NDVIl1)+pb(NDWIl1)+pb(LSDl1)+pb(LSNl1)+pb(TNAl1)+pb(Precip_t)+
                as.factor(Month)+
                offset(OFF),
              family=NBI, data=bases_ent_completo)

m2b <- gamlss(Cases ~ 
                pb(Nino12SSTAl1)+pb(Nino3SSTAl1)+pb(Nino34SSTAl1)+pb(Nino4SSTAl1)+pb(EVIl1)+
                pb(NDVIl1)+pb(NDWIl1)+pb(LSDl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                as.factor(Month)+
                offset(OFF),
              family=NBI, data=bases_ent_completo)

m3a <- gamlss(Cases ~ 
                pb(Nino12SSTAl1)+pb(Nino3SSTAl1)+pb(Nino34SSTAl1)+pb(Nino4SSTAl1)+pb(EVIl1)+
                pb(NDVIl1)+pb(NDWIl1)+pb(LSDl1)+pb(LSNl1)+pb(TNAl1)+pb(Precip_t)+
                as.factor(cluster5_logRR)+
                offset(OFF),
              family=NBI, data=bases_ent_completo)

m3b <- gamlss(Cases ~ 
                pb(Nino12SSTAl1)+pb(Nino3SSTAl1)+pb(Nino34SSTAl1)+pb(Nino4SSTAl1)+pb(EVIl1)+
                pb(NDVIl1)+pb(NDWIl1)+pb(LSDl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                as.factor(cluster5_logRR)+
                offset(OFF),
              family=NBI, data=bases_ent_completo)

m4a <- gamlss(Cases ~ 
                pb(Nino12SSTAl1)+pb(Nino3SSTAl1)+pb(Nino34SSTAl1)+pb(Nino4SSTAl1)+pb(EVIl1)+
                pb(NDVIl1)+pb(NDWIl1)+pb(LSDl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                as.factor(Month)+
                as.factor(cluster5_logRR)+
                offset(OFF),
              family=NBI, data=bases_ent_completo)

m4b <- gamlss(Cases ~ 
                pb(Nino12SSTAl1)+pb(Nino3SSTAl1)+pb(Nino34SSTAl1)+pb(Nino4SSTAl1)+pb(EVIl1)+
                pb(NDVIl1)+pb(NDWIl1)+pb(LSDl1)+pb(LSNl1)+pb(TNAl1)+pb(Precipl1)+
                as.factor(Month)+
                as.factor(cluster5_logRR)+
                as.factor(Month)*as.factor(cluster5_logRR)+
                offset(OFF),
              family=NBI, data=bases_ent_completo)

m4c <- gamlss(Cases ~ 
                pb(Nino12SSTAl1)+pb(Nino3SSTAl1)+pb(Nino34SSTAl1)+pb(Nino4SSTAl1)+pb(EVIl1)+
                pb(NDVIl1)+pb(NDWIl1)+pb(LSDl1)+pb(LSNl1)+pb(TNAl1)+
                as.factor(Month)+
                as.factor(cluster5_logRR)+
                as.factor(Month)*as.factor(cluster5_logRR)+
                offset(OFF),
              family=NBI, data=bases_ent_completo)

GAIC(m1a,m1b,m2a,m2b,m3a,m3b,m4a,m4b,m4c)
summary(m1a)
summary(m2a)
summary(m3b)
summary(m4a)
summary(m4c)
#
modelo<-m3b

term.plot(modelo, pages=1, ask=FALSE)
wp(modelo, ylim.all=3)

plot(fitted(modelo,"mu")~bases_ent_completo$Cases)
plot(log(fitted(modelo,"mu"))~log(bases_ent_completo$Cases))

res<-residuals(modelo)


# Diagnostics -------------------------------------------------------------

plot(res)

names(bases_ent_completo)

bases_ent_completo$res<-res
bases_ent_completo$fitted<-fitted(modelo,"mu")

plot(bases_ent_completo$res~bases_ent_completo$cluster5_logRR)
plot(bases_ent_completo$res~as.numeric(bases_ent_completo$cluster5_logRR))


cantones <- bases_ent_completo %>% dplyr::select(Canton) %>% distinct()
cantones <- unlist(cantones)
i<-1

graphs1<-list()
for(i in 1:length(cantones)){
graphs1[[i]]<-bases_ent_completo %>%
  filter(Canton == cantones[i]) %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y = Cases), color = "steelblue") + 
  geom_line(aes(y = fitted), color="darkred", linetype="dashed") 
}


do.call("grid.arrange", c(graphs1, ncol=5))

graphs1[[1]]

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


bases_ent_completo1 %>%
  ggplot( aes(x=Date, y=res, group=Canton, color=Canton)) +
  geom_line()



