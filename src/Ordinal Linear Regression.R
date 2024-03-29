#Tomado de: https://www.youtube.com/watch?v=rrRrI9gElYA
#Ver: https://rpubs.com/gustavomtzv/915914
# https://rpubs.com/Especializacion_2023/1063309
#########################Ordinal regression
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
names(ds)[names(ds) == "consumo_tabaco_100"] <- "condicion_fumador"

names(ds)[names(ds) == "bisg01"] <- "salud"

df_ENFR_2018 <- ds[, c("sexo", "edad", "salud", "condicion_fumador")]


#View(df_ENFR_2018)#view data
#attach(df_ENFR_2018) #attach file

df_ENFR_2018$salud<- 5 - df_ENFR_2018$salud 
df_ENFR_2018$salud<- factor(df_ENFR_2018$salud,
                          levels = c(4,3,2,1,0),
                          labels = c("excelente", "muy buena", "buena", "regular", "mala"))  #label ordinal var

df_ENFR_2018$sexo<- factor(df_ENFR_2018$sexo,
                         levels = c(1,2),
                         labels = c("Varón", "Mujer"))  #label sex

df_ENFR_2018$condicion_fumador<- factor(df_ENFR_2018$condicion_fumador,
                         levels = c(1,2,3),
                         labels = c("Fumador actual", "Ex fumador", "No fumador"))  #label sex


#Visuales de la variable a predecir (histograma o simil, correlaciones)
#Sacar lo de modelo nulo



View(df_ENFR_2018)#view data
attach(df_ENFR_2018) #attach file

#####################null model
modelnull <- clm(as.factor(salud)~1,
                 data = df_ENFR_2018,
                 link = "logit")
##########
model1 <- clm(as.factor(salud)~edad + as.factor(sexo) + condicion_fumador,
              data = df_ENFR_2018,
              link = "logit")

# Asumiendo que 'salud' es tu variable dependiente y 'model1' es tu modelo clm

nagelkerke(fit  = model1,
           null = modelnull)  


modelz <- clm(as.factor(salud)~edad + as.factor(sexo),
              data = df_ENFR_2018,
              link = "logit")


summary(modelnull)

summary(model1)
#Tengo un tipo que ex fumador y tiene 45 años. Predict de esto manualmente. 
#Cuanto me da la probabilidad de caer en cada categoria manualmente 
#luego hacer el predic


confint(model1)
exp(coef(model1))
exp(confint(model1))


##### 

model2 <- clm(as.factor(salud)~edad + as.factor(sexo) + condicion_fumador,
              data = df_ENFR_2018,
              link = "probit")

summary(model2)


###MASS

modelt <- polr(as.factor(salud)~edad+ as.factor(sexo)+ condicion_fumador,   ##have to fit the model with a different function
               data = df_ENFR_2018,
               Hess=TRUE)

#Tratar de buscar si hay otra forma de chequear supuestos en el clm.

brant(modelt)

summary(modelt)

coeftest(modelt)


