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
# Modelo Tobit
#####################################################################################################

tobit.model <- tobit(horas ~ hijosmen6 + hijosmay6 + edad + educ + ciudad,
                     left=0, right=Inf, data=mroz)
summary(tobit.model)

# Esperanza de Y en el modelo Tobit

xb.tobit <- predict(tobit.model)
s.tobit <- tobit.model$scale
imr.tobit <- dnorm(xb.tobit/s.tobit)/pnorm(xb.tobit/s.tobit)

(ey.incon.tobit <- mean (pnorm(xb.tobit/s.tobit) * (xb.tobit + (s.tobit*imr.tobit))))


#####################################################################################################
# Modelo de Cragg (1971)
#####################################################################################################

# La ecuación de participación se corresponde con un modelo probit
# Creamos una variable dummy para el modelo probit
probit.cragg <- glm(I(horas > 0) ~ hijosmen6 + hijosmay6 + edad + educ + ciudad, data = mroz,
                    family = binomial(link = "probit"))
summary(probit.cragg)

# La ecuación de intensidad se modeliza como una normal truncada

trunc.cragg <- truncreg(horas ~ hijosmen6 + hijosmay6 + edad + educ + ciudad,
                        data=mroz.trunc, point = 0, direction = "left")
summary(trunc.cragg)


# Esperanza de Y en el modelo de Cragg
xb1 <- predict(probit.cragg)

X <- cbind(1, mroz$hijosmen6, mroz$hijosmay6, mroz$edad, mroz$educ, mroz$ciudad)
betas <- cbind(trunc.cragg$coefficients[1:6])
xb2 <- X %*% betas
s <- trunc.cragg$coefficients["sigma"]
irm <- dnorm(xb2/s)/pnorm(xb2/s)                  

(ey.cragg <- mean(pnorm(xb1)*(xb2+s*irm)))


#####################################################################################################
# Contraste LR modelo de Cragg vs. Tobit
#####################################################################################################

## Coeficientes escalados

cbind(
  "Tobit"     = coef(tobit.model)[1:6] / tobit.model$scale,
  "Binary"    = coef(probit.cragg),
  "Truncated" = coef(trunc.cragg)[1:6] / coef(trunc.cragg)[7])

## likelihood ratio test
ll <- c("Tobit" = logLik(tobit.model),
        "Cragg" = as.vector(logLik(probit.cragg) + logLik(trunc.cragg)))


gl <- length(coef(probit.cragg))+ length(coef(trunc.cragg))-tobit.model$df 

(LRstat <- -2 * (logLik(tobit.model)-(as.vector(logLik(probit.cragg) + logLik(trunc.cragg)))) )

(LRstat <- 2 * diff(ll))

(critvalue <- qchisq(0.95, gl))

(pvalue <- pchisq(LRstat, gl, lower.tail = FALSE))

# Por tanto, la hipótesis de parámetros constantes en la ecuación de intensidad
# y participación que implica el modelo Tobit es rechazada.

