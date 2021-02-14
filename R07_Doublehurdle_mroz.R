rm(list=ls())

library(AER)
library(censReg)
library(truncreg)
library(mhurdle)
library(stargazer)
library(foreign)
library(MASS)
library(sampleSelection)
library(ggplot2)
library(jtools)


#####################################################################################################
# Explicación de los datos: la oferta de trabajo anual por parte de mujeres casadas
#####################################################################################################

# El archivo "mrozesp.csv" incluye información sobre las horas trabajadas por parte
# de 753 mujeres casadas, 428 de las cuales trabajaron por un salario fuera 
# del hogar durante el año; 325 trabajaron cero horas. Para las mujeres 
# que trabajaron un número de horas superior a cero, el rango es elevado, 
# desde 12 hasta cerca de 5000 horas anuales. Por tanto, las horas anuales 
# trabajadas es un buen candidato para un modelo Tobit.

# Descripción de las variables 

#variable     Explicación
#------------------------------------------------------------
# trabaja   	=1 si trabaja en 1975
# horas     	horas trabajadas por la mujer en 1975
# hijosmen6	  número de hijos menores de 6 años
# hijosmay6	  número de hijos entre 6-18 años
# edad	      edad de la mujer, en años
# educ	      años de escolarización de la mujer
# salario	    salario por hora de la mujer en 1975
# salariorep	salario por hora de la mujer repetido en 1976
# horasm    	horas trabajadas por el marido en 1975
# edadm	      edad del marido, en años
# educm	      años de escolarización del marido
# salariom  	salario por hora del marido en 1975
# ingfam	    ingreso familiar en 1975
# imp       	impuestos federales de la mujer
# educmad	    años de escolarización de la madre (de la mujer)
# educpad	    años de escolarización del padre (de la mujer)
# desempleo	  tasa de desempleo en la zona de residencia
# ciudad	    =1 si vive en una ciudad grande
# exper	      experiencia actual en el mercado laboral
# ingfamneto	(faminc - wage*hours)/1000, ingreso neto de la familia sin la mujer
# lsalario	  log(salario)
# exper2	    exper^2


#####################################################################################################
# Lectura de datos
#####################################################################################################

mroz <- read.csv("https://raw.githubusercontent.com/DabidH/datasets/master/mrozesp.csv")
class(mroz)
length(mroz)
str(mroz)
names(mroz)

summary(mroz)

N <- nrow(mroz)

# Conjunto de datos cuando el salario es positivo (muestra truncada)
mroz.trunc <- mroz[which(mroz$salario>0),]

#####################################################################################################
# Representación gráfica
#####################################################################################################

attach(mroz)

ggplot(mroz, aes(x=horas)) + 
  geom_histogram()

ggplot(mroz, aes(x=lsalario)) + 
  geom_histogram()
table(is.na(mroz$lsalario))

#####################################################################################################
# Modelo de doble valla (double hurdle model: Cragg 1971) mhurdle package
#####################################################################################################

# Tobit model

tobit.aer <- tobit(horas ~ hijosmen6 + hijosmay6 + edad + educ + ciudad,
                     left=0, right=Inf, data=mroz)
summary(tobit.aer)

tobit.mh <- mhurdle(horas~ 0 | hijosmen6 + hijosmay6 + edad + educ + ciudad,
                     data=mroz, dist = "n", h2 = TRUE, method = "bfgs", scaled = FALSE)
summary(tobit.mh)

(model.comparison <- cbind(c(coef(tobit.aer), tobit.aer$scale), coef(tobit.mh)))


# modelo de Cragg en dos partes (probit + truncada)

probit.cragg <- glm(I(horas > 0) ~ hijosmen6 + hijosmay6 + edad + educ + ciudad, data = mroz,
                    family = binomial(link = "probit"))
summary(probit.cragg)


trunc.cragg <- truncreg(horas ~ hijosmen6 + hijosmay6 + edad + educ + ciudad,
                        data=mroz.trunc, point = 0, direction = "left")
summary(trunc.cragg)

(coef.cragg <- c(coef(probit.cragg), coef(trunc.cragg)))


# modelo de Cragg en mhurdle

cragg.mh <- mhurdle(horas ~ hijosmen6 + hijosmay6 + edad + educ + ciudad|
                       hijosmen6 + hijosmay6 + edad + educ + ciudad,
                     data=mroz, dist = "n", scaled = FALSE)
summary(cragg.mh)

(model.comparison <- cbind(coef.cragg, coef(cragg.mh)))


# modelo de seleccion

heckman.ml <- heckit(trabaja~hijosmen6 + hijosmay6 + edad + educ + ciudad,
                     horas~hijosmen6 + hijosmay6 + edad + educ + ciudad, data=mroz,method="ml")
summary(heckman.ml)




# modelo de doble valla independiente

dhi.mh <- mhurdle(horas ~ hijosmen6 + hijosmay6 + edad + educ + ciudad|
                      hijosmen6 + hijosmay6 + edad + educ + ciudad|0,
                    data=mroz, dist = "n", h2=TRUE, scaled = FALSE)
summary(dhi.mh)

(model.comparison <- cbind(coef(cragg.mh), coef(dhi.mh)))

# modelo de doble valla dependiente

dhd.mh <- mhurdle(horas ~ hijosmen6 + hijosmay6 + edad + educ + ciudad|
                       hijosmen6 + hijosmay6 + edad + educ + ciudad|0,
                     data=mroz, dist = "n", h2=TRUE, scaled = FALSE, corr = TRUE)
summary(dhd.mh)



#####################################################################################################
# Comparación de modelos
#####################################################################################################

# Comparamos todos los modelos
model.comparison <- cbind(c(NA, NA, NA, NA, NA, NA, coef(tobit.mh),NA), c(coef(cragg.mh),NA),
                          c(coef(dhi.mh), NA), coef(dhd.mh) )

colnames(model.comparison) <- c("Tobit","Cragg","dhi","dhd")
model.comparison

# Contraste para modelos anidados: cragg vs tobit
vuongtest(cragg.mh, tobit.mh, type = "nested", hyp = TRUE)

# Rechazamos H0, es decir, rechazamos la hipótesis de parámetros constantes en la ecuación de
# intensidad y participación que implica el modelo Tobit

# Contraste para modelos no anidados: cragg vs dhi

vuongtest(cragg.mh, dhi.mh)

# No rechazamos H0, el contraste no es concluyente en lo que se refiere a si la ecuación de
# intensidad debe ser modelizada como una distribución truncada o no

# Contraste para modelos anidados: doble valla dependiente vs independiente

vuongtest(dhd.mh, dhi.mh, type = "nested", hyp = TRUE)

coef(summary(dhd.mh), "corr")

# No rechazamos H0, no hay evidencia de significatividad en el parámetro de correlación entre
# las ecuaciones de participación e intensidad
# intensidad debe ser modelizada como una distribución truncada o no