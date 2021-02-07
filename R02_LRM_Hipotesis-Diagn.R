#####################################################################################################
##################      Diagnóstico del MRL: verificando las hipótesis básicas     ##################
##################      David Hoyos                                                ##################
##################      UPV/EHU, 2021                                              ##################
#####################################################################################################

rm(list=ls())
library(tidyverse)
library(broom)
library(ggfortify)
library(ggplot2)
library(gridExtra)
library(car)

# Fuentes
# https://www.machinelearningplus.com/machine-learning/complete-introduction-linear-regression-r/
# http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/ 
# https://rpubs.com/davoodastaraky/mtRegression



# Cargamos un conjunto de datos: modelo simple
data(cars)

str(cars)

?cars

# Revisamos las 6 primeras filas
head(cars, 6)

# Análisis gráfico

# El gráfico xy nos permite ver que existe una relación positiva entre ambas variables
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")

ggplot( cars, aes(x=speed,y=dist))+ 
  ggtitle("Dist ~ Speed")+
  geom_point() +
  geom_smooth(method= "lm")

# Podemos hacer un boxplot para detectar outliers
# En general, consideramos un outlier cualquier punto que está fuera del rango de 1.5*(rango_intercuartilico)
# Rango_intercuartilico = percentil(75) - percentil(25)

par(mfrow=c(1, 2))  # divide el gráfico en 2 columnas

boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out)) 

boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))

# Podemos utilizar un gráfico de densidad para testar la normalidad de las variables

par(mfrow=c(1, 2))  # # divide el gráfico en 2 columnas

plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(cars$speed), col="grey")

plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
polygon(density(cars$dist), col="grey")

# Correlación no implica causalidad. Pero podemos utilizar el coeficiente de correlación para entender
# la relación entre las variables

cor(cars$speed, cars$dist)  

# Especificamos un modelo para predecir la distancia recorrida (y) en función de la velocidad

lrm <- lm(dist ~ speed, data=cars)  
summary(lrm)

# guardamos el modelo como un objeto
modelSummary <- summary(lrm)  

# coeficientes del modelo
modelCoeffs <- modelSummary$coefficients  

# guardamos el coeficiente estimado de velocidad
beta.estimate <- modelCoeffs["speed", "Estimate"]

# guardamos el error estándar de velocidad  
std.error <- modelCoeffs["speed", "Std. Error"]  

# calculamos el estadistico t 
t_value <- beta.estimate/std.error  

# calculamos el valor p 
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  

# Estadistico F
f <- summary(lrm)$fstatistic

model_p <- pf(f[1], f[2], f[3], lower=FALSE)

AIC(lrm)
BIC(lrm)

##################################################################################

# En ocasiones, nos interesa diagnosticar la capacidad predictiva de um modelo
# Para ello, podemos utilizar el 80% de la muestra para predecir el 20% y compararlo
# Esto nos permitirá comprobar como se comporta un modelo con nuevos datos

# 1) Creamos datos de entrenamiento y test
set.seed(100)  # ponemos un seed para reproducir los resultados de muestreos aleatorios
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # filas indices de los datos de entrenamiento
trainingData <- cars[trainingRowIndex, ]  # datos de entrenamiento
testData  <- cars[-trainingRowIndex, ]   # datos para testar

# 2) Ajustamos el modelo a los datos de entrenaminento y predecimos "dist" en los datos de testeo

lmMod <- lm(dist ~ speed, data=trainingData)  # modelo de datos de entrenamiento
distPred <- predict(lmMod, testData)  # dist predichas

# 3) Revisar las medidas de diagnostico
summary(lmMod)
AIC(lmMod)

# El modelo reproduce el modelo original

# 4) Calculamos la exactitud de la predicción y el ratio de errores

actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 90.29%
head(actuals_preds)

# Calculo de la precisión min-max
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 73.11%, min_max accuracy
# Cuanto mayor sea este indicador (más cercano a 1), mejor será la capacidad predictiva
# del modelo

# Error de Predicción
# MAPE (Mean absolute Percentage Error) Error Porcentual Absoluto Medio 
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 49.59%, El error porcentual promedio es de un 50%

# El paquete DMwR permite calcular las métricas de predicción en un sólo paso
DMwR::regr.eval(actuals_preds$actuals, actuals_preds$predicteds)

# MAE (Mean Absolute Error): es la media del error absoluto de predicción
# MSE (Mean Squared Error): es la media del error de predicción al cuadrado
# RMSE (Root Mean Squared Error): es la raíz de la media del error de predicción al cuadrado
# MAPE (Mean absolute Percentage Error) Error Porcentual Absoluto Medio 

##################################################################################

# Testando las hipótesis básicas

# Partimos de un modelo estimado y el gráfico de la regresión

lrm <- lm(dist ~ speed, data=cars)  
summary(lrm)

ggplot( cars, aes(x=speed,y=dist))+ 
  ggtitle("Dist ~ Speed")+
  geom_point() +
  geom_smooth(method= "lm")

# Mediante la función augment() añadimos a nuestro datos valores ajustados y residuos
model.diag.metrics <- augment(lrm)
head(model.diag.metrics)

# .fitted: valores ajustados
# .resid: residuos
# .hat: valores hat, se utilizan para detectar valores extremos en la X
# .std.resid: residuos estandarizados, i.e. residuo dividido por su error estándar (para detectar outliers 
#             o valores extremos en la Y
# .cooksd: distancia de Cook, para detectar valores influyentes (puntos de alto apalancamiento o outliers)


ggplot(model.diag.metrics, aes(speed, dist)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = speed, yend = .fitted), color = "red", size = 0.3)

# Vamos a examinar la distribución de los residuos para poder contrastar las hipótesis del modelo

# Supuestos del modelo de regresión lineal:
 
# 1. Linealidad de los datos. Se supone que la relación entre las x y la variable dependiente (y) es lineal.
# 2. Normalidad de los residuos. Se supone que los residuos se distribuyen normalmente.
# 3. Homogeneidad de la varianza de los residuos. Se supone que los residuos tienen una varianza constante (homocedasticidad)
# 4. Independencia de los términos de error de los residuos.
# Debemos verificar si estas hipótesis son ciertas o no. Los problemas potenciales incluyen:
# a. No linealidad de la variable endógena 
# b. Heteroscedasticidad: Varianza no constante de términos de error.
# c. Presencia de valores influyentes en los datos que pueden ser:
#   Valores atípicos: valores extremos en la variable de resultado (y)
# d. Puntos de alto apalancamiento: valores extremos en las variables explicativas
# Todas estas suposiciones y problemas potenciales pueden verificarse produciendo algunos diagramas de diagnóstico 
# que visualizan los errores residuales.

autoplot(lrm, which = 1:6, label.size = 3, data = cars, colour = 'Species')

autoplot(lrm)

# Grafico 1. Residuos vs. ajuste. Linealidad de los datos 
# Idealmente, el gráfico no mostrará un patrón ajustado (i.e. línea azul debe ser aprox horizontal en cero). 
# La presencia de un patrón puede indicar un problema con algún aspecto del modelo lineal.

# Grafico 2. Normal Q-Q. Normalidad de los residuos
# La gráfica QQ de residuos se puede utilizar para verificar visualmente el supuesto de normalidad. 
# La gráfica de probabilidad normal de los residuos debe seguir aproximadamente una línea recta.
# En nuestro ejemplo, todos los puntos caen aproximadamente a lo largo de esta línea de referencia,
# por lo que podemos asumir  normalidad.

# Grafico 3. Scale-location. Homocedasticidad
# Este gráfico muestra si los residuos se distribuyen por igual a lo largo del rango de X.
# Es bueno si ve una línea horizontal con puntos de distribución iguales. 
# En nuestro ejemplo, este no es el caso. Puede observarse que la variabilidad (varianzas) de los puntos
# residuales aumenta con el valor de la variable de resultado ajustada, lo que sugiere varianzas no 
# constantes en los errores residuales (o heterocedasticidad).

# Una posible solución para reducir el problema de heterocedasticidad es utilizar una transformación 
# de raíz cuadrada o logarítmica de la variable de resultado (y).

# Contraste de heterocedasticidad
# Dentro del paquete car, este contraste asume una hipótesis nula de varianza constante de errores contra 
# la hipótesis alternativa de que la varianza del error cambia con el nivel de respuesta o con una 
# combinación lineal de predictores.

ncvTest(lrm)

# Al 5% de significatividad se rechaza la hipótesis nula por lo que sí hay heterocedasticidad


# Grafico 4. Outliers y puntos de apalancamiento alto 

# Valores atípicos:
#   
# Un valor atípico es un punto que tiene un valor de variable de resultado extremo. La presencia de 
# valores atípicos puede afectar la interpretación del modelo, porque aumenta el error estándar de la perturbación.

# Los valores atípicos se pueden identificar examinando el residuo estandarizado, que es el residuo 
# dividido por su error estándar estimado. Los residuos estandarizados se pueden interpretar como el 
# número de errores estándar fuera de la línea de regresión.
# Las observaciones cuyos residuos estandarizados son superiores a 3 en valor absoluto son posibles 
# valores atípicos (James et al. 2014).
# 
# Puntos de alto apalancamiento:
# Los puntos de alto apalancamiento son aquellas observaciones realizadas en valores extremos 
# de las variables independientes, de modo que la falta de observaciones vecinas significa que
# Esto se puede detectar examinando el estadístico de apalancamiento. Un valor de esta 
# estadística por encima de 2 (p + 1) / n indica una observación con alto apalancamiento (P. Bruce y Bruce 2017);
# donde, p es el número de predictores y n es el número de observaciones.
# 
# Los valores atípicos y los puntos de alto apalancamiento se pueden identificar inspeccionando el gráfico
# de Residuos vs Apalancamiento (Gráfico 4).
# 

# Contraste de autocorrelación de Durbin Watson
# Hipótesis nula de no-autocorrelación
durbinWatsonTest(lrm, max.lag = 5, reps=1000)

# Dado que ningun p-value de los 5 retardos es menor que 0,05, no podemos rechazar la hipótesis nula de no
# autocorrelación, luego asumimos que los errores son independientes

##################################################################################
# ¿Qué tipo de variables dependientes no son adecuadas para el MRL?
##################################################################################

# Cuando la variable dependiente (1) no es continua, (2) está limitada o 
# (3) se mide en una escala de intervalos o porcentajes, el modelo de regresión 
# no cumplirá con los supuestos de los modelos lineales.

# (1) Proporciones

# Las proporciones, valores entre 0 y 1, o los porcentajes (entre 0 y 100), 
# se vuelven realmente problemáticas si muchos de los datos están cerca de los
# límites. Si todos los datos caen en la parte media, digamos en el rango de 0.2 a
# 0.8, un modelo lineal puede dar resultados razonablemente buenos. Pero más allá 
# de eso, necesitamos usar modelos de regresión beta si las proporciones son 
# continuas o modelos logit si se trata de de proporciones que miden eventos con 
# un determinado resultado (e.g. proporción de respuestas respondidas correctamente)

# (2) Variables categóricas

# Tanto las variables binarias como las de múltiples categorías (3 o más valores) 
# claramente fallan los tres criterios. Pero existen otros modelos de regresión
# que funcionan bien para estas variables: probit y logit para las binarias o
# multinomiales si tiene más categorías.

# (3) Variables ordinales

# Estas variables se componen de categorías ordenadas, incluyendo rankings y variables
# con escalas Likert, entre otras. Aunque las variables ordinales parecen números,
# las distancias entre sus valores no son iguales en un verdadero sentido numérico,
# por lo que no tiene sentido aplicarles operaciones numéricas, como la suma y la 
# división. Por lo tanto, las medias, la base de los modelos lineales, no tienen
# sentido. 
# Las variables ordinales requieren modelos logit y probit especiales, como los 
# modelos ordered logit o el ordered probit.

# (4) Variables truncadas y censuradas

# Los datos censurados contienen toda la información de una variable dependiente
# sólo para una parte de los datos. La distribución se corta para algunos valores,
# a menudo al final o al principio de la distribución. Por ejemplo, encuestas que 
# tienen información exacta sobre los ingresos para todos hasta 200,000 euros, 
# pero más allá de eso, todos reciben "más de 200,000 euros". Otras veces, se trata de
# una cuestión de medición, por ejemplo, la demanda de bienes, limitada en 0.
# En estos casos, se emplea el modelo de regresión Tobit.

# (5) Variables de conteo

# Los datos de conteo no cumplen los supuestos de los modelos lineales por muchas
# razones. El más obvio es que la distribución normal de los modelos lineales permite
# cualquier valor en la escala numérica, pero los recuentos están limitados a 0. 
# Simplemente no tiene sentido predecir números negativos de visitas al médico, hijos 
# por familia o días de lluvia.
# El modelo de regresión de Poisson o el binomial negativo están diseñados para 
# modelizar correctamente datos de conteo.

# (6) Variables con inflación de ceros

# Los datos con inflación de ceros tienen un pico (apilamiento de datos) en cero.
# Son comunes en los datos de Poisson, pero pueden ocurrir con cualquier distribución.
# Incluso si el resto de la distribución es normal, no se puede transformar datos
# con inflación de ceros para que parezcan normales. Los modelos con inflación de
# ceros incoporan el alto número de ceros modelando simultáneamente 0 / No 0 como 
# una regresión binomial y todos los valores No-0 como otra distribución. 


