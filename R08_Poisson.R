#####################################################################################################
##################           El modelo de Poisson                                  ##################
##################           David Hoyos                                           ##################
##################           UPV/EHU, 2020                                         ##################
#####################################################################################################

####################################################################################################
# Introducción
#####################################################################################################

# El modelo de regresión de Poisson se usa para modelizar datos de conteo.
# 
# Ejemplo 1. El número de personas asesinadas por mulas o patadas de caballos en el ejército prusiano 
# por año. Ladislaus Bortkiewicz recolectó datos de 20 volúmenes de Preussischen Statistik. Estos 
# datos fueron recolectados en 10 cuerpos del ejército prusiano a fines del siglo XIX en el transcurso
# de 20 años.
# 
# Ejemplo 2. El número de personas en fila frente a usted en el supermercado. Las variables 
# explicativas pueden incluir la cantidad de artículos que se ofrecen actualmente a un precio 
# especial con descuento o si  un evento especial (por ejemplo, festivo, un gran evento deportivo)
# está a tres o menos días de distancia.
# 
# Ejemplo 3. El número de premios ganados por los estudiantes en una escuela secundaria. Las 
# variables explicativas de la cantidad de premios ganados incluyen el tipo de programa en el 
# que el estudiante se matriculó (por ejemplo, vocacional, general o académico) y la puntuación 
# en su examen final de matemáticas.
 
#####################################################################################################
# Descripción de los datos
#####################################################################################################

# Vamos a ver un ejemplo con datos simulados a partir del Ejemplo 3 anterior.

# En este ejemplo, premios" es la variable dependiente e indica el número de premios ganados 
# por los estudiantes en una escuela secundaria en un año, las matemáticas son una variable 
# predictiva continua y representan la puntuación de los estudiantes en su examen final de matemáticas,
# y prog es una variable predictiva categórica con tres niveles que indican el tipo de programa en 
# el que se matricularon los estudiantes. Se codifica como 1 = “general”, 2 = “académico” y 
# 3 = “vocacional”. 

# Empezaremos por cargar algunas librerías y leer los datos.

rm(list=ls())

library(ggplot2)
library(sandwich)
library(msm)
library(stargazer)

dat <-  read.csv("https://raw.githubusercontent.com/DabidH/datasets/master/pois.csv")

str(dat)
summary(dat)

attach(dat)

# Cada variable tiene 200 observaciones válidas y sus distribuciones parecen bastante razonables. 
# La media incondicional y la varianza de nuestra variable de resultado no son extremadamente diferentes. 
# Nuestro modelo supone que estos valores, condicionados a las variables predictoras, serán iguales 
# (o al menos aproximadamente).

# Podemos usar la función "tapply" para mostrar las estadísticas de resumen por tipo de programa. 
# La tabla a continuación muestra el número promedio de premios por tipo de programa y parece sugerir
# que el tipo de programa es un buen candidato para predecir el número de premios, la variable 
# dependiente, porque el valor medio del resultado parece variar según el programa. Además, 
# las medias y varianzas dentro de cada nivel de programa (las medias condicionadas y las 
# varianzas) son similares. Podemos hacer un histograma condicional por tipo de programa para 
# mostrar la distribución.

with(dat, tapply(premios, programa, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

ggplot(dat, aes(premios, fill = programa)) +
  geom_histogram(binwidth=.5, position="dodge")

#####################################################################################################
# Posibles modelos para analizar estos datos
#####################################################################################################

# A continuación se muestra una lista de algunos métodos de análisis que podrías considerar a la hora 
# de analizar estos datos. Algunos de los métodos enumerados son bastante razonables, mientras que 
# otros pueden tener diversas limitaciones o han caído en desuso.

# (1) Regresión de Poisson: la regresión de Poisson a menudo se usa para modelar datos de conteo. 
# La regresión de Poisson tiene varias extensiones útiles para los modelos de conteo.
 
# (2) Regresión binomial negativa: la regresión binomial negativa se puede usar para datos de recuento 
# sobredispersados, es decir, cuando la varianza condicional excede la media condicional. Se puede 
# considerar como una generalización de la regresión de Poisson, ya que tiene la misma estructura 
# media que la regresión de Poisson y tiene un parámetro adicional para modelar la sobredispersión. 
# Si la distribución condicional de la variable dependiente se dispersa en exceso, es probable que 
# los intervalos de confianza para la regresión binomial negativa sean más estrechos en comparación
# con los de una regresión de Poisson.
 
# (3) Modelo de regresión con inflación cero: los modelos con inflación cero intentan dar cuenta 
# del exceso de ceros. En otras palabras, se cree que existen dos tipos de ceros en los datos, 
# "ceros verdaderos" y "ceros en exceso". Los modelos con inflación cero estiman dos ecuaciones 
# simultáneamente, una para el modelo de conteo y otra para los ceros en exceso.
 
# (4) Regresión MCO: las variables de resultado de recuento a veces se transforman logarítmicamente 
# y se analizan mediante regresión OLS. Con este enfoque surgen muchos problemas, incluida la pérdida 
# de datos debido a valores indefinidos generados al tomar el registro de cero (que es indefinido) 
# y estimaciones sesgadas.


#####################################################################################################
# El modelo de regresión de Poisson
#####################################################################################################

# Para analizar los datos utilizando el modelo de regresión de Poisson utilizaremos la función "glm":

summary(poisson.model <- glm(premios ~ programa + mate, family="poisson", data=dat))

# Cameron y Trivedi (2009) recomiendan el uso de errores estándar robustos para las estimaciones de 
# los parámetros para controlar la violación leve del supuesto de distribución de que la varianza es 
# igual a la media. Usamos el paquete "sandwich" a continuación para obtener los errores estándar 
# robustos y calculamos los valores p e interavalos de confinza del 95% de los parámetros estimados
# y sus errores estándar robustos.

cov.pois <- vcovHC(poisson.model, type="HC0")
std.err <- sqrt(diag(cov.pois))
r.est <- cbind(Estimate= coef(poisson.model), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(poisson.model)/std.err), lower.tail=FALSE),
               LL = coef(poisson.model) - 1.96 * std.err,
               UL = coef(poisson.model) + 1.96 * std.err)
r.est

exp(coef(poisson.model)[4])

# Veamos ahora la salida de la función glm en detalle:

# (1) La salida comienza con la información sobre la dispersión de los residuos. La desviación de los 
# residuos se distribuyen aproximadamente normalmente si el modelo se especifica correctamente. 
# En nuestro ejemplo, muestra un poco de asimetría ya que la mediana no es del todo cero.

# (2) Luego vienen los coeficientes de regresión de Poisson para cada una de las variables junto 
# con los errores estándar, valores z, valores p e intervalos de confianza del 95% para los
# coeficientes. El coeficiente para las matemáticas es .07, es decir, cuando la nota de "mate" aumenta
# en una unidad, el número de premios aumenta en exp(0.07)-1= 7%. La variable programageneral mide la
# diferencia entre programa = “general” y programa = “academico”, que varía en exp(-1.084)-1=-66%. 
# La diferencia esperada en el número de premios entre el pograma "vocacional" y "académico" se estima
# en exp(-0.714)-1=-51%. 

# (3) También se proporciona la información sobre la desviación. Podemos usar la desviación residual 
# para realizar una prueba de bondad de ajuste para el modelo general. La desviación residual es la 
# diferencia entre la desviación del modelo actual y la desviación máxima del modelo ideal donde
# los valores predichos son idénticos a los observados. Por lo tanto, si la diferencia residual 
# es lo suficientemente pequeña, la prueba de bondad de ajuste no será significativa, lo que indica 
# que el modelo se ajusta a los datos. Concluimos que el modelo se ajusta razonablemente bien porque
# la prueba de chi-cuadrado de bondad de ajuste no es estadísticamente significativa. 
# Si la prueba hubiera sido estadísticamente significativa, indicaría que los datos no se ajustan 
# bien al modelo. En esa situación, podemos intentar determinar si hay variables explicativas
# omitidas, si nuestro supuesto de linealidad se cumple y / o si existe un problema de sobredispersión.

with(poisson.model, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

# También podemos probar el efecto general de programa comparando la desviación del modelo completo 
# con la desviación del modelo excluyendo esta variable cualitativa. La prueba de chi-cuadrado de
# dos grados de libertad indica que programa, en conjunto, es una variable relevante para explicar
# el número de premios.

m2 <- update(poisson.model, . ~ . - prog)
## contrastamos diferencias en los modelos con el contraste de chi cuadrado
anova(m2, poisson.model, test="Chisq")

# A veces, es útil presentar los resultados de la regresión como índices de incidencia y sus errores
# estándar, junto con el intervalo de confianza. Para calcular el error estándar de las relaciones
# de tasa de incidentes, utilizaremos el método Delta. Para ello, utilizamos la función deltamethod
# del paquete "msm":

s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4)), 
                   coef(poisson.model), cov.pois)

## calculamos el exp de las estimaciones anteirores quitando la tercera columna, los valores-p
rexp.est <- exp(r.est[, -3])
## y reemplazamos los SEs con las estimaciones de los coeficientes exponenciados
rexp.est[, "Robust SE"] <- s

rexp.est

# El resultado anterior indica que la tasa de incidentes para programa = "general", por ejemplo,
# es 0.33 la tasa de incidentes para el grupo de referencia (programa = "academico"). Del mismo 
# modo, la tasa de incidentes para el programa = "vocacional" es 0.489 veces la tasa de incidentes
# para el grupo de referencia.Por cada aumento unitario en la nota de "mate", el número de premios aumenta
# en un 7%.

# A veces, nos puede interesar calcular los valores esperados de las medias marginales. Por ejemplo, 
# ¿cuáles son los premios esperados para cada tipo de programa fijando la puntuación en "mate" en 
# su media general? Para responder a esta pregunta, podemos hacer uso de la función de predicción. 

(s1 <- data.frame(mate = mean(dat$mate),
                  programa = factor(1:3, levels = 1:3, labels = levels(dat$programa))))

predict(poisson.model, s1, type="response", se.fit=TRUE)


# En el resultado anterior, vemos que el número previsto de eventos para el nivel 1 de programa es 
# de aproximadamente .086, manteniendo las "mate" en su media. El número previsto de premios para el 
# nivel 2 de programa es mayor, 0.62, y el número previsto de eventos para el nivel 3 es aproximadamente
# 0.31. Las proporciones de estos conteos predicos ((frac {.2114} {.6249} = 0,338), (frac {.306} {.6249} 
# = 0.489)) coinciden con lo que vimos mirando la tasa de incidencia.

# También podemos realizar un gráfico de la evolución del número esperado de premios en función de la 
# nota de mate, por programa:


## calcula y guarda los valores predichos
dat$yhat <- predict(poisson.model, type="response")

## ordenamos por programa y luego por mate
dat <- dat[with(dat, order(programa, mate)), ]

## Gráfico de evolución del número esperado de premios en función de la nota de mate, por programa
ggplot(dat, aes(x = mate, y = yhat, colour = programa)) +
  geom_point(aes(y = premios), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "Nota de Mate", y = "Número de premios esperado")

#####################################################################################################
# Consideraciones finales
#####################################################################################################

# (1) Cuando parece haber un problema de dispersión, primero debemos verificar si nuestro modelo está
# correctamente especificado. Por ejemplo, si omitimos la variable explicativa "programa" en el ejemplo
# anterior, nuestro modelo parecería tener un problema de sobredispersión. En otras palabras, un modelo
# mal especificado podría presentar como síntoma un problema de sobredispersión.

# (2) Suponiendo que el modelo está correctamente especificado, se debe verificar la hipótesis de que
# la varianza condicional es igual a la media condicional. Hay varias pruebas, incluido el contraste de LR
# del parámetro alfa de sobredispersión a partir del la estimación del modelo de regresión binomial 
# negativo. El paquete R pscl (Laboratorio Computacional de Ciencias Políticas, Universidad de Stanford) 
# proporciona muchas funciones para datos binomiales y de conteo, incluyendo odTest para contrastar
# la sobredispersión.
 
# (3) Una causa común de sobredispersión es el exceso de ceros, que a su vez son generados por un 
# proceso adicional de generación de datos. En esta situación, se debe considerar el modelo de inflación 
# de ceros.
 
# (4) Si el proceso de generación de datos no permite ningún 0 (como el número de días de hospitalización),
# entonces un modelo truncado en cero puede ser más apropiado.
 
# (5) Los datos de conteo a menudo tienen una variable de exposición, que indica la cantidad de veces que
# el evento pudo haber sucedido. Esta variable debería incorporarse al modelo de Poisson utilizando 
# la opción "offset".
 
# (6) La variable dependiente de una regresión de Poisson no puede tener números negativos y la 
# exposición no puede tener ceros.
 
# (7) Existen muchas medidas diferentes de pseudo-R-cuadrado. Todos intentan proporcionar información
# similar a la proporcionada por el R cuadrado en la regresión MCO, aunque ninguno de ellos puede 
# interpretarse exactamente como el R cuadrado en la regresión MCO. 
 
# (8) La regresión de Poisson se estima por máxima verosimilitud que, oor lo general, precisa de un 
# tamaño de muestra grande.

#####################################################################################################
# Referencias
#####################################################################################################

# Long, J. S. 1997. Regression Models for Categorical and Limited Dependent Variables. 
# Thousand Oaks, CA: Sage Publications.
# Long, J. S. and Freese, J. 2006. Regression Models for Categorical Dependent Variables Using Stata, 
# Second Edition. College Station, TX: Stata Press.
# Cameron, A. C. and Trivedi, P. K. 2009. Microeconometrics Using Stata. College Station, 
# TX: Stata Press.
# Cameron, A. C. and Trivedi, P. K. 1998. Regression Analysis of Count Data. New York: Cambridge Press.
# Cameron, A. C. Advances in Count Data Regression Talk for the Applied Statistics Workshop, 
# March 28, 2009. http://cameron.econ.ucdavis.edu/racd/count.html .
# Dupont, W. D. 2002. Statistical Modeling for Biomedical Researchers: A Simple Introduction to 
# the Analysis of Complex Data. New York: Cambridge Press.
# Venables, W.N. and Ripley, B.D. 2002. Modern Applied Statistics with S, Fourth Edition.
# New York: Springer.
