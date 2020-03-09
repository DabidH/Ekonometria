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

attach(mroz)

ggplot(mroz, aes(x=horas)) + 
  geom_histogram()

ggplot(mroz, aes(x=lsalario)) + 
  geom_histogram()
table(is.na(mroz$lsalario))


#####################################################################################################
# Modelo de Heckman
#####################################################################################################

#Estimacion en dos etapas

heckman.2s <- heckit(I(horas>0)~hijosmen6 + hijosmay6 + edad + educ + ciudad,
                     horas~hijosmen6 + hijosmay6 + edad + educ + ciudad, data=mroz,method="2step")
summary(heckman.2s)


# Estimacion en 2 etapas: paso a paso
# Primera etapa: Probit model

probit.model <- glm(I(horas > 0) ~ hijosmen6 + hijosmay6 + edad + educ + ciudad, data = mroz,
                    family = binomial(link = "probit"))
summary(probit.model)

#Calculamos el IRM
xb.probit <- predict(probit.model)
irm.probit <- dnorm(xb.probit)/pnorm(xb.probit)

#Directamente
library(sampleSelection)
irm2 <- invMillsRatio(probit.model)

# Segunda etapa: MCO con ratio de Mills

mco.irm <- lm(horas  ~ hijosmen6 + hijosmay6 + edad + educ + ciudad + irm.probit, data = mroz)
summary(mco.irm)

# Incorrecto: MCO sin ratio de Mills

mco.sin <-lm(horas  ~ hijosmen6 + hijosmay6 + edad + educ + ciudad, data = mroz)
summary(mco.sin)

stargazer(mco.irm,mco.sin, type="text", df=FALSE)

#Estimacion ML

heckman.ml <- heckit(I(horas>0)~hijosmen6 + hijosmay6 + edad + educ + ciudad,
                     horas~hijosmen6 + hijosmay6 + edad + educ + ciudad, data=mroz,method="ml")

# Da lo mismo utilizar como variable 1/0 I(horas>0) o trabaja
summary(heckman.ml)


X <- cbind(1, 
           mroz$hijosmen6,
           mroz$hijosmay6,
           mroz$edad,
           mroz$educ,
           mroz$ciudad)


betas1 <- coef(heckman.ml)[1:6]
betas2 <- coef(heckman.ml)[7:12]

s.ml <- coef(heckman.ml)["sigma"]
rho.ml <- coef(heckman.ml)["rho"]

xb1.ml <- X %*% betas1
xb2.ml <- X %*% betas2

irm.ml <- dnorm(xb2.ml/s.ml)/pnorm(xb2.ml/s.ml)    

(ey.ml <- mean ( pnorm(xb1.ml)*(xb2.ml+(rho.ml*s.ml*irm.ml)) ) )


#####################################################################################################
# Selección muestral o truncamiento accidental
#####################################################################################################

# Una forma habitual de selección muestral es lo que se conoce como truncamiento
# accidental. Un ejemplo típico se da cuando y = log(salario), donde "salario" es el 
# salario por hora que un individuo podría recibir en el 
# mercado laboral. Si la persona está precisamente trabajando cuando se realiza 
# la encuesta, entonces observamos el salario ofrecido porque asumimos que es el 
# salario observado. Pero no podemos observar el salario ofrecido a los 
# trabajadores desempleados. Por tanto, se dice que el truncamiento de la variable
# "salario" es accidental porque depende de otra variable, esto es, la participación 
# en el mercado laboral. Es importante destacar que, en general, observaríamos 
# otro tipo de información útil sobre el individuo, como su nivel de educación, 
# experiencia, género, estado matrimonial, etc.

# En el archivo de datos, mroz.csv, hay 753 mujeres en la muestra, 428 
# formaban parte de la población activa. La ecuación del salario ofrecido es estándar:
# 
#  log(salario) = lsalario = f(educ, exper, exper2)
#
# Con el fin de contrastar y corregir el posible sesgo de selección muestral
# debido a la no observabilidad del salario ofrecido a las mujeres fuera del 
# mercado laboral, necesitamos estimar un modelo probit para la participación en 
# el mercado laboral. Además de las variables de educación y experiencia 
# incluimos:
# 
#  Pr(trabaja) = f(educ, exper, exper2, ingfamneto, edad, hijosmen6, hijosmay6)
#
# El hecho de que estas cuatro variables son excluidas de la ecuación del salario
# ofrecido es una hipótesis: asumimos que, dada la productividad de los factores,
# ingfamneto, edad, hijosmen6 y hijosmay6 no tienen efecto sobre el salario ofrecido.

# Estimacion en 2 etapas
# Primera etapa: Probit model

probit.model <- glm(trabaja ~ educ + exper + exper2 + ingfamneto + edad + hijosmen6 + hijosmay6, data = mroz,
                    family = binomial(link = "probit"))
summary(probit.model)

#Calculamos el IRM
xb.probit <- predict(probit.model)
irm.probit <- dnorm(xb.probit)/pnorm(xb.probit)

# Segunda etapa: MCO con ratio de Mills

mco.irm <- lm(lsalario ~ educ + exper + exper2 + irm.probit, data = mroz)
summary(mco.irm)

# Incorrecto: MCO sin ratio de Mills

mco.sin <-lm(lsalario ~ educ + exper + exper2, data = mroz)
summary(mco.sin)

stargazer(mco.irm,mco.sin, type="text", df=FALSE)

# Atendiendo a los resultados obtenidos, no existe evidencia de que exista un
# problema de selección muestral al estimar la ecuación de salario ofertado. El 
# coeficiente de lambda tiene un estadístico t muy bajo (0.24). Igualmente 
# importante, no existen diferencias notables en la estimación de parámetros.

# La estimación MLE proporciona resultados muy similares. 

heckman <- heckit(trabaja ~ educ + exper + exper2 + ingfamneto + edad + hijosmen6 + hijosmay6,
                  lsalario ~ educ + exper + exper2, data=mroz,method="ml")
summary(heckman)

selection <- selection(trabaja ~ educ + exper + exper2 + ingfamneto + edad + hijosmen6 + hijosmay6,
                       lsalario ~ educ + exper + exper2, data=mroz)
summary(selection)


# La correlación entre los errores no es significativa (rho=0.0266), lo cual
# indica que no existe evidencia de problema de selección muestral.

# Sin embargo, con esta especificación alternativa sí obtenemos evidencia de selección muestral:

heckman <- heckit(trabaja ~ salariom + hijosmen6 + imp + educm + educ + ciudad,
       lsalario ~ educ + ciudad, data=mroz,method="ml")
summary(heckman)

#####################################################################################################
# Efectos marginales en el modelo de selección muestral
#####################################################################################################

# Cuando una variable explicativa aparece sólo en la regresión (X) pero no
# en la participación (Z), el efecto marginal será el del modelo probit o el
# del modelo de regresión truncado

# Sin embargo, cuando una variable explicativa aparece tanto en la regresión 
# como en la participación, el efecto marginal tiene en cuenta ambos efectos;

# Calculamos el efecto marginal de "educ"

heckman <- heckit(trabaja ~ educ + exper + exper2 + ingfamneto + edad + hijosmen6 + hijosmay6,
                  lsalario ~ educ + exper + exper2, data=mroz,method="ml")
summary(heckman)


X <- cbind(1, 
           mroz$educ,
           mroz$exper,
           mroz$exper2)

Z <- cbind(1, 
           mroz$educ,
           mroz$exper,
           mroz$exper2,
           mroz$ingfamneto,
           mroz$edad,
           mroz$hijosmen6,
           mroz$hijosmay6)

betas1 <- coef(heckman)[1:8]
betas2 <- coef(heckman)[9:12]

s.heck <- coef(heckman)["sigma"]
rho.heck <- coef(heckman)["rho"]

zb.heck <- Z %*% betas1
xb.heck <- X %*% betas2

irm.heck <- dnorm(zb.heck/s.heck)/pnorm(zb.heck/s.heck)    

(ey.heck <- mean ( coef(heckman)[10] + coef(heckman)[2]*rho.heck*s.heck*(((zb.heck/s.heck)^2)- (xb.heck*(zb.heck/s.heck))) ))
