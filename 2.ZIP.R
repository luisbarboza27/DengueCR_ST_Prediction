library(dplyr)
library(pscl)
library(VGAM)
library(ggplot2)

# 0. reading data ------------------------------------------------------------

#32 cantones

load(file = './Data/datos_totales.RData')
str(datos_totales)
dim(datos_totales)

#load(file = './Data/MODISData.RData') ?
str(datos_totales)

attach(datos_totales)

sum(Cases==0)
sum(!Cases==0)
sum(Cases==0)/length(Cases)*100

plot(table(Cases))
plot(table(Cases),xlim=c(0,40))

logCases<-log(Cases+1)
loglogCases<-log(logCases)
hist(Cases)
hist(logCases)
hist(loglogCases)




# 1. Descriptive ----------------------------------------------------------

datos_totales$time <- paste0(datos_totales$Year,"-",datos_totales$Month,"-",15)
datos_totales$time <- as.Date(datos_totales$time, "%Y-%m-%d")

set.seed(100)
muestra<-sample(names(table(datos_totales$Canton)),10)
base <- datos_totales %>% filter(Canton %in% muestra) 

table(base$Canton)
ggplot(base, aes(x = time, y = Cases)) + 
  geom_line(aes(color = Canton), size = 1) +
  theme_minimal()


clog <- function(x) log(x + 0.5)

cfac <- function(x, breaks = NULL) {
   if(is.null(breaks)) breaks <- unique(quantile(x, 0:10/10))
   x <- cut(x, breaks, include.lowest = TRUE, right = FALSE)
   levels(x) <- paste(breaks[-length(breaks)], ifelse(diff(breaks) > 1,
                   c(paste("-", breaks[-c(1, length(breaks))] - 1, sep = ""), "+"), ""),
                  sep = "")
   return(x)
   }

plot(clog(Cases)~cfac(Nino12SSTA)) #índices de temperatura (superficie del mar)
plot(clog(Cases)~cfac(Nino3SSTA)) 
plot(clog(Cases)~cfac(Nino4SSTA)) 
plot(clog(Cases)~cfac(Nino34SSTA)) 
plot(clog(Cases)~cfac(TNA)) # el mismo índice pero caribe
plot(clog(Cases)~cfac(Poblacion))
plot(clog(Cases)~cfac(EVI)) # índice de vegetación
plot(clog(Cases)~cfac(NDVI)) # índice de vegetación normalizado
plot(clog(Cases)~cfac(NDWI)) # índice de vegetación 
plot(clog(Cases)~cfac(LSD)) # temperatura diurna (nivel de suelo)
plot(clog(Cases)~cfac(LSN)) # temperatura nocturna


detach(datos_totales)


base <- datos_totales %>% 
              select(Cases,Nino12SSTA,Nino3SSTA,Nino4SSTA,Nino34SSTA,TNA,
                                   Poblacion,
                                   EVI,NDVI,NDWI,LSD,LSN) %>%
              mutate(logCases = log(Cases+0.5))
  
hist(base$logCases)
round(cor(base),4)

#posibles covariables Nino12SSTA,Poblacion, TNA, LSD, LSN


library(MASS)
library(vcd)
fit <- goodfit(base$Cases) 
summary(fit) 
rootogram(fit)
rootogram(fit,xlim=c(0,100))
          
distplot(base$Cases, type="poisson")


# 1. zero % ---------------------------------------------------------------

# Porcentaje=16.8% 
sum(base$Cases==0)/length(base$Cases)*100
#ggplot(base, aes(Cases)) + geom_histogram()
#ggplot(base, aes(Cases)) + geom_histogram() + scale_x_log10()

# 2. Poisson regression ---------------------------------------------------

m1 <- glm(Cases~
            Nino12SSTA+Nino3SSTA+Nino4SSTA+Nino34SSTA+TNA+
            Poblacion+
            EVI+NDVI+NDWI+LSD+LSN, family="poisson", data=base)

#m1 <- glm(Cases~., family="poisson", data=base)

summary(m1)

library(sandwich)
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)

r.est

m1a <- glm(Cases~
             Nino12SSTA+Nino4SSTA+TNA+
            Poblacion+LSN, family="poisson", data=base)
summary(m1a)

cov.m1a <- vcovHC(m1a, type="HC0")
std.err <- sqrt(diag(cov.m1a))
r.est <- cbind(Estimate= coef(m1a), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1a)/std.err), lower.tail=FALSE),
               LL = coef(m1a) - 1.96 * std.err,
               UL = coef(m1a) + 1.96 * std.err)
r.est


m1b <- update(m1a, . ~ 1 )
## test model differences with chi square test
anova(m1b, m1a, test="Chisq")

round(exp(m1a$coefficients),4)
#Poblacion y LSN es cuestionable?

m1c <- update(m1a, . ~ . -Poblacion )
summary(m1c)

anova(m1c, m1a, test="Chisq")

round(exp(m1c$coefficients),4)


#https://easystats.github.io/performance/reference/check_zeroinflation.html
performance::check_zeroinflation(m1c, tolerance = 0.05)

performance::check_zeroinflation(m1b, tolerance = 0.05)

performance::check_zeroinflation(m1a, tolerance = 0.05)
yhat<-predict(m1a,type="response")
plot(yhat~base$Cases)
plot(log(yhat)~log(base$Cases))




# 3. ZIP ------------------------------------------------------------------

# 3.1 pscl library --------------------------------------------------------


base1<- base %>% select(Cases,Nino12SSTA,Nino3SSTA,TNA,Poblacion,LSN,logCases)
cor(base1)

m2 <- zeroinfl(Cases ~ Nino12SSTA+Nino3SSTA+TNA+LSN 
               | Nino12SSTA+Nino3SSTA+TNA+LSN , data = base1)
summary(m2)

m2a <- zeroinfl(Cases ~ Nino12SSTA+Nino3SSTA+TNA+LSN 
               | TNA+LSN , data = base1)
summary(m2a)

m2b <- update(m2a, . ~ 1)

pchisq(2 * (logLik(m2a) - logLik(m2b)), df = 6, lower.tail = FALSE)


# Vuong test

vuong(m1c, m2b)

dput(coef(m1c, "count"))

dput(coef(m1c, "zero"))

AIC(m1c, m2b)

res <- residuals(m1c, type="deviance")
plot(log(predict(m1c)), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)



# 3.2. VGAM library -------------------------------------------------------


m3 <- vglm(Cases ~ Nino12SSTA+Nino3SSTA+TNA+LSN, zipoisson(), data = base1, crit = "coef")

summary(m3)
