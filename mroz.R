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
mroz1 <- mroz[which(mroz$salario>0),]


#####################################################################################################
# Representación gráfica
#####################################################################################################

ggplot(mroz, aes(x=horas)) + 
  geom_histogram()

ggplot(mroz, aes(x=horas)) + 
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

#####################################################################################################
# Estimación MCO
#####################################################################################################

# Especificamos un model de salarios:
# HORAS_i = beta_1 + beta_2*HIJOS6 + beta_3*HIJOS18 + beta_4*EDAD + beta_5*EDUC + beta_6*CIUDAD + e

mco.model <- lm(horas ~ hijosmen6 + hijosmay6 + edad + educ + ciudad, data = mroz)
summary(mco.model)
confint(mco.model, level = 0.95)

#  Número de observaciones en el modelo MCO
nobs(mco.model) 

# Esperanza de Y en el modelo MCO
ey.mco <- mean(predict(mco.model))
ey.mco

#####################################################################################################
# Estimación modelo truncado
#####################################################################################################

trunc.model <- truncreg(horas ~ hijosmen6 + hijosmay6 + edad + educ + ciudad, data = mroz1,
                        point=0, direction="left", scaled=TRUE, marg.eff=TRUE)
summary(trunc.model)
confint(trunc.model)
nobs(trunc.model) 

# Valor esperado de Y cuando Y>0 (esperanza condicional) modelo TRUNC

X <- cbind(1, mroz$hijosmen6, mroz$hijosmay6, mroz$edad, mroz$educ, mroz$ciudad)
betas <- cbind(trunc.model$coefficients[1:6])
xb <- X %*% betas
s <- trunc.model$coefficients["sigma"]
irm <- dnorm(xb/s)/pnorm(xb/s)                  

(ey.trunc <- mean(xb+s*irm))

#####################################################################################################
# Estimación de efectos marginales en el modelo truncado
#####################################################################################################

## Efectos marginales promediados en X

delta <- xb/sigma
irm <- dnorm(xb/s)/pnorm(xb/s)  
mean.eff <- mean(1-(delta*irm)-(irm^2))
(efmarg.trunc <- trunc.model$coefficients[1:6]*mean.eff)


## Efectos marginales en media de X

betas.trunc <- trunc.model$coefficients[1:6]
Xm <- cbind(1, 
            mean(mroz$hijosmen6),
            mean(mroz$hijosmay6) ,
            mean(mroz$edad) ,
            mean(mroz$educ),
            mean(mroz$ciudad) )

xb1 <- Xm %*% betas.trunc

deltam <- xb1/sigma
irmm <- dnorm(xb1/sigma)/pnorm(xb1/sigma)  
mean.effm <- 1-(deltam*irmm)-(irmm^2)
(efmarg.trunc2 <- trunc.model$coefficients[1:6]*mean.effm)


#####################################################################################################
# Estimación modelo censurado (tobit)
#####################################################################################################

tobit.model.cr <- censReg(horas ~ hijosmen6 + hijosmay6 + edad + educ + ciudad,
                       left=0, data=mroz)

tobit.model <- tobit(horas ~ hijosmen6 + hijosmay6 + edad + educ + ciudad,
                    data=mroz)

summary(tobit.model)
confint(tobit.model)

nobs(tobit.model) 


## Esperanza incondicional en el modelo TOBIT

xb.tobit <- predict(tobit.model)
s.tobit <- tobit.model$scale
imr.tobit <- dnorm(xb.tobit/s.tobit)/pnorm(xb.tobit/s.tobit)

ey.incon.tobit <- mean (pnorm(xb.tobit/s.tobit) * (xb.tobit + (s.tobit*imr.tobit)))
ey.incon.tobit

ey.con.tobit <- mean (xb.tobit + (s.tobit*imr.tobit))
ey.con.tobit


stargazer(mco.model, tobit.model, type="text", ci=T, digits=2)

# Comparación estimacion MCO - Tobit

# La comparación entre la estimación por MCO y Tobit muestra algunos resultados
# interesantes. En primer lugar, los coeficientes del modelo Tobit tienen los 
# mismos signos que los correspondientes OLS, y la significación estadística 
# es similar. En segundo lugar, si bien resulta tentador comparar la magnitud 
# de los coeficientes estimados MCO y Tobit, esto no es muy informativo. 
# Debemos tener cuidado y no pensar que, porque el coeficiente Tobit de la 
# variable "hijosmen6" es más o menos el doble que el coeficiente MCO, esto 
# significa que en el modelo Tobit la respuesta de las horas trabajadas respecto a 
# los niños pequeños es mucho mayor.

#####################################################################################################
# Estimación de efectos marginales en tobit
#####################################################################################################

## Efectos marginales promediados en X

XB <- predict(tobit.model)
sigma <- tobit.model$scale
aju1 <- pnorm(XB/sigma)
aju1m <- mean(aju1)

efmarg1 <- aju1m*tobit.model$coefficients
efmarg1

## Efectos marginales en media de X

betas1 <- cbind(coef(tobit.model)[1:6])
betas1 <- coef(tobit.model)[1:6]
Xm <- cbind(1, 
            mean(mroz$hijosmen6),
            mean(mroz$hijosmay6) ,
            mean(mroz$edad) ,
            mean(mroz$educ),
            mean(mroz$ciudad) )

XB1 <- Xm %*% betas1
aju2 <- mean(pnorm(XB1/sigma))
efmarg2 <- aju2*betas1
efmarg2

# Utilizando el paquete censReg (efectos marginales en media de X)

tobit.model.cr <- censReg(horas ~ hijosmen6 + hijosmay6 + edad + educ + ciudad,
                          left=0, data=mroz)
summary(tobit.model.cr)

margEff(tobit.model.cr)
summary(margEff(tobit.model.cr))



###################################################
# 6. COMPARACION TOBIT - PROBIT
###################################################

tobit.model <- tobit(horas ~ hijosmen6 + hijosmay6 + edad + educ + ciudad,
                     left=0, right=Inf, data=mroz)
summary(tobit.model)

probit.model <- glm(I(horas > 0) ~ hijosmen6 + hijosmay6 + edad + educ + ciudad, data = mroz,
                  family = binomial(link = "probit"))

summary(probit.model)

stargazer(mco.model,tobit.model,probit.model, type="text", df=FALSE)

# Si el modelo Tobit es adecuado, las estimaciones del modelo Probit deberían ser cercanas
# al ratio de las estimación de coeficientes tobit y sigma.

(betas.comp <- cbind(tobit.model$coefficients[1:6]/tobit.model$scale))

