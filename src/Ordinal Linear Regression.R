#Tomado de: https://www.youtube.com/watch?v=rrRrI9gElYA
#########################Ordinal regression

#install.packages("ordinal")
#install.packages("rcompanion")
#install.packages("MASS")
#install.packages("brant")
#install.packages("AER")

library(ordinal)  #ordinal regression package
library(rcompanion) #pseudo R square 
library(MASS) #plyr method (for getting data that allows the test of proportional odds)
library(brant)# test of proportional odds
library(tidyverse)
library(corrr)
library(knitr)
library(kableExtra)
library(GGally)
library(tidymodels)
library(rsample)
library(ggplot2)
library(robustbase)
library(AER)

options(scipen = 999) 

######################
setwd("/Users/miguelkiszkurno/Documents/EEA-tp-final")


archivo = paste0(getwd(), "/", "Datasets/", "ENFR-2018-Base usuario.txt")
ds <- read.table(archivo, sep="|", dec=".", header = TRUE, fill = TRUE)


#Vista general del dataset
glimpse(ds)


#label nominal data

names(ds)[names(ds) == "bhch03"] <- "sexo"
names(ds)[names(ds) == "bhch04"] <- "edad"
names(ds)[names(ds) == "bisg01"] <- "salud"
names(ds)[names(ds) == "consumo_tabaco_100"] <- "condicion_fumador"


Ordinaldf <- ds[, c("sexo", "edad", "salud", "condicion_fumador")]


View(Ordinaldf)#view data
attach(Ordinaldf) #attach file

Ordinaldf$salud<- 5 - Ordinaldf$salud 
Ordinaldf$salud<- factor(Ordinaldf$salud,
                          levels = c(4,3,2,1,0),
                          labels = c("excelente", "muy buena", "buena", "regular", "mala"))  #label ordinal var

Ordinaldf$sexo<- factor(Ordinaldf$sexo,
                         levels = c(1,2),
                         labels = c("Varón", "Mujer"))  #label sex

Ordinaldf$condicion_fumador<- factor(Ordinaldf$condicion_fumador,
                         levels = c(1,2,3),
                         labels = c("Fumador actual", "Ex fumador", "No fumador"))  #label sex


View(Ordinaldf)#view data
attach(Ordinaldf) #attach file

#####################null model
modelnull <- clm(as.factor(salud)~1,
                 data = Ordinaldf,
                 link = "logit")
##########
model1 <- clm(as.factor(salud)~edad+ as.factor(sexo)+ condicion_fumador,
              data = Ordinaldf,
              link = "logit")

anova(modelnull, model1)
nagelkerke(fit  = model1,
           null = modelnull)  

summary(model1)
confint(model1)
exp(coef(model1))
exp(confint(model1))

###MASS

modelt <- polr(as.factor(salud)~edad+ as.factor(sexo)+ condicion_fumador,   ##have to fit the model with a different function
               data = Ordinaldf,
               Hess=TRUE)

brant(modelt)

summary(modelt)

coeftest(modelt)


