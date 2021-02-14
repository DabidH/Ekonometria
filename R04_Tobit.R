#####################################################################################################
##################           El modelo Tobit                                       ##################
##################           David Hoyos                                           ##################
##################           UPV/EHU, 2020                                         ##################
#####################################################################################################


#####################################################################################################
# Introducción
#####################################################################################################

# El modelo Tobit para soluciones de esquina (corner solutions) es habitual en modelizaciones de
# decisiones económicas donde una parte de la población elige consumir 0 unidades de algunos bienes.
# El estimador Tobit asume que una fracción finita de los individuos de la muestra elige 0, pero las 
# personas que eligen una cantidad positiva siguen lo que queda una distribución normal.
# - ¿Por qué no podemos utilizar el estimador MCO? Al igual que los modelos probabilísticos, en este 
# caso ignoramos restricciones en la distribución del error y predecimos valores de y negativos
# - ¿Por qué no podemos simplemente utilizar las observaciones de y>0? Esto implicaría una selección
# en el término de error porque tenderíamos a eliminar observaciones con errores negativos
# - ¿Por qué no podemos utilizar el ln(y)? Porque las observaciones con y=0 tienen un ln=-inf.
 


#####################################################################################################
# Explicación de los datos: la oferta de trabajo anual por parte de mujeres casadas
#####################################################################################################

# El archivo "mrozesp.csv" incluye información sobre las horas trabajadas por parte
# de 753 mujeres casadas en 1976, 428 de las cuales trabajaron por un salario fuera 
# del hogar durante el año 1975; 345 trabajaron cero horas en 1975. Para las mujeres 
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

ggplot(mroz, aes(x=horas)) + 
  geom_histogram()

ggplot(mroz, aes(x=horas)) + 
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

#####################################################################################################
# Estimación MCO
#####################################################################################################

# Especificamos un modelo de horas anuales trabajadas:

mco.model <- lm(horas ~ ingfamneto + hijosmen6 + hijosmay6 + edad + educ + exper + exper2, data = mroz)
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

trunc.model <- truncreg(horas ~ ingfamneto + hijosmen6 + hijosmay6 + edad + educ + exper + exper2, data = mroz.trunc,
                        point=0, direction="left", scaled=TRUE, marg.eff=TRUE)
summary(trunc.model)
confint(trunc.model)
nobs(trunc.model) 

# Valor esperado de Y cuando Y>0 (esperanza condicional) modelo TRUNC

X <- cbind(1, mroz$ingfamneto, mroz$hijosmen6, mroz$hijosmay6, mroz$edad, mroz$educ,  mroz$exper, mroz$exper2)
betas <- cbind(trunc.model$coefficients[1:8])
xb <- X %*% betas
s <- trunc.model$coefficients["sigma"]
irm <- dnorm(xb/s)/pnorm(xb/s)                  

(ey.trunc <- mean(xb+s*irm))

#####################################################################################################
# Estimación de efectos marginales en el modelo truncado
#####################################################################################################

## Efectos marginales promediados en X

delta <- xb/s
irm <- dnorm(xb/s)/pnorm(xb/s)  
mean.eff <- mean(1-(delta*irm)-(irm^2))
(efmarg.trunc <- trunc.model$coefficients[1:8]*mean.eff)


## Efectos marginales en media de X

betas.trunc <- trunc.model$coefficients[1:8]
Xm <- cbind(1, 
            mean(mroz$ingfamneto),
            mean(mroz$hijosmen6),
            mean(mroz$hijosmay6) ,
            mean(mroz$edad) ,
            mean(mroz$educ),
            mean(mroz$exper),
            mean(mroz$exper2))

xb1 <- Xm %*% betas.trunc

deltam <- xb1/s
irmm <- dnorm(xb1/s)/pnorm(xb1/s)  
mean.effm <- 1-(deltam*irmm)-(irmm^2)
mean.effm <- drop(mean.effm) # convierte matriz 1x1 en escalar
(efmarg.trunc2 <- betas.trunc*mean.effm)


#####################################################################################################
# Estimación modelo censurado (tobit)
#####################################################################################################

tobit.model.cr <- censReg(horas ~ ingfamneto + hijosmen6 + hijosmay6 + edad + educ + exper + exper2,
                       left=0, data=mroz)

tobit.model <- tobit(horas ~ ingfamneto + hijosmen6 + hijosmay6 + edad + educ + exper + exper2,
                    data=mroz)

summary(tobit.model)
confint(tobit.model)

nobs(tobit.model) 


## Esperanza incondicional en el modelo TOBIT

xb.tobit <- predict(tobit.model)
s.tobit <- tobit.model$scale
imr.tobit <- dnorm(xb.tobit/s.tobit)/pnorm(xb.tobit/s.tobit)

(ey.incon.tobit <- mean (pnorm(xb.tobit/s.tobit) * (xb.tobit + (s.tobit*imr.tobit))))

(ey.con.tobit <- mean (xb.tobit + (s.tobit*imr.tobit)))



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

# Podemos multiplicar los coeficientes estimados del modelo Tobit por el factor de
# ajuste para hacerlos más o menos comparables:

(fajuste <- mean(pnorm(xb.tobit/s.tobit)))

(efmarg.tobit <- fajuste*tobit.model$coefficients)
(coef.mco <- mco.model$coefficients[1:8])

# Por ejemplo, si comparamos el coef Tobit ajustado de educ (47.5 horas más) es casi el doble que
# el efecto marginal del modelo mco (28.76 horas más), así que incluso a la hora de estimar un
# efecto promedio, los coeficientes del modelo Tobit son mayores que los MCO. A excepción del 
# coeficiente de hijosmay6, todos los coeficientes escalados Tobit son de mayor magnitud.

# Vamos a ver, por ejemplo, cómo varía la E(horas|x) en función de la educación. En el modelo MCO
# (línea negra) se trata de una efecto constante, mientras que en el modelo Tobit este efecto
# marginal varía (línea roja, con el resto de variables explicativas en sus valores medios).
# Se observa cómo el modelo lineal predice mayores valores de horas esperadas trabajadas incluso 
# en niveles altos de educación. Por ejemplo, con 8 años de educación, el modelo lineal predice
# 617.5 horas trabajadas mientras que el Tobit 423.9. Con 12 años son 732.7 y 598.3, respectivamente.
# Ambas líneas se cruzan con 17 años de educación pero no existe ninguna mujer en la muestra con 
# este valor de la variable educ. La pendiente creciente de la línea roja indica que el efecto
# marginal de la educación sobre lsa horas trabajadas es creciente.

x<-0:20
df<-data.frame(x)
ggplot(df,aes(x))+
  ylim(0,1100)+
  stat_function(fun=function(x) 387.19 + 28.76*x,  lwd = 1.5) + 
  stat_function(fun=function(x) pnorm((-694.12 + 80.65*x)/1122.02)*(-694.12+80.65*x)+1122.02*dnorm((-694.12 + 80.65*x)/1122.02),
                lwd = 1.5, colour = "red")



#####################################################################################################
# Estimación de efectos marginales en Tobit
#####################################################################################################

## Efectos marginales promediados en X

xb.tobit <- predict(tobit.model)
s.tobit <- tobit.model$scale
fajuste <- mean(pnorm(xb.tobit/s.tobit))

(efmarg1 <- fajuste*tobit.model$coefficients)


## Efectos marginales en media de X

betas1 <- coef(tobit.model)[1:8]
Xm <- cbind(1, 
            mean(mroz$ingfamneto),
            mean(mroz$hijosmen6),
            mean(mroz$hijosmay6) ,
            mean(mroz$edad) ,
            mean(mroz$educ),
            mean(mroz$exper),
            mean(mroz$exper2))

XB1 <- Xm %*% betas1
aju2 <- mean(pnorm(XB1/s.tobit))
(efmarg2 <- aju2*betas1)

(efm.comparison <- cbind(efmarg1, efmarg2))


# Utilizando el paquete censReg (efectos marginales en media de X)

tobit.model.cr <- censReg(horas ~ ingfamneto + hijosmen6 + hijosmay6 + edad + educ + exper + exper2,
                          left=0, data=mroz)
summary(tobit.model.cr)

margEff(tobit.model.cr)
summary(margEff(tobit.model.cr))

(efm.comparison <- cbind(efmarg2,margEff(tobit.model.cr)))

###################################################
# 6. COMPARACION TOBIT - PROBIT
###################################################

# Una manera informal de evaluar si el modelo Tobit es adecuado, es compararlo con un probit.

tobit.model <- tobit(horas ~ ingfamneto + hijosmen6 + hijosmay6 + edad + educ + exper + exper2,
                     left=0, right=Inf, data=mroz)
summary(tobit.model)

probit.model <- glm(I(horas > 0) ~ ingfamneto + hijosmen6 + hijosmay6 + edad + educ + exper + exper2, data = mroz,
                  family = binomial(link = "probit"))

summary(probit.model)

stargazer(mco.model,tobit.model,probit.model, type="text", df=FALSE)

# Si el modelo Tobit es adecuado, las estimaciones del modelo Probit deberían ser cercanas
# al ratio de las estimación de coeficientes tobit y sigma. Las estimaciones nunca serán idénticas
# debido al error muestral.

(model.comparison <- cbind((tobit.model$coefficients[1:8]/tobit.model$scale),probit.model$coefficients))

# A pesar de encontrar diferencias, no parecen ser demasiado importantes.
