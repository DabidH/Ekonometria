#####################################################################################################
##################           El modelo Binomial Negativo                           ##################
##################           David Hoyos                                           ##################
##################           UPV/EHU, 2020                                         ##################
#####################################################################################################

####################################################################################################
# Introducción
#####################################################################################################

# El modelo de regresión binomial negativo se usa para modelizar datos de conteo qu presentan 
# sobredispersión.

# Ejemplo 1. Los administradores escolares estudian el comportamiento de asistencia de los jóvenes 
# de secundaria en dos escuelas. Los regresores del número de días de ausencia incluyen el tipo de 
# programa en el que el estudiante está matriculado y un examen estandarizado en matemáticas.

# Ejemplo 2. Un investigador en salud está estudiando el número de visitas al hospital en los últimos
# 12 meses por parte de personas mayores en una comunidad en función de las características de los 
# individuos y los tipos de planes de salud bajo los cuales cada uno está cubierto.


#####################################################################################################
# Descripción de los datos
#####################################################################################################

# Vamos a ver un ejemplo con datos simulados a partir del Ejemplo 1 anterior. 

# Tenemos datos de asistencia sobre 314 estudiantes de secundaria en dos escuelas urbanas en el 
# archivo "negbin.csv". La variable depenidente de interés es el número de días ausentes, "ausencias".
# La variable "mate" recoge la puntuación matemática estandarizado para cada estudiante. La variable 
# cualitativa programa recoge el programa en que el estudiante está matriculado: "general", 
# "académico" y "vocacional".

# Empezaremos por cargar algunas librerías y leer los datos.

rm(list=ls())

library(ggplot2)
library(MASS)
library(foreign)


dat <-  read.csv("https://raw.githubusercontent.com/DabidH/datasets/master/negbin.csv")

str(dat)
summary(dat)

attach(dat)

ggplot(dat, aes(ausencias, fill = programa)) + 
  geom_histogram(binwidth = 1) + facet_grid(programa ~., margins = TRUE, scales = "free")

# Cada variable tiene 314 observaciones válidas y sus distribuciones parecen bastante razonables.
# La media incondicional de nuestra variable depediente es mucho más baja que su varianza.

# Continuemos con nuestra descripción de las variables en este conjunto de datos. La siguiente 
# tabla muestra el número promedio de días ausentes por tipo de programa y parece sugerir que el
# tipo de programa es un buen candidato para predecir el número de días ausentes, nuestra variable
# dependiente, porque el valor medio del número de días ausentes parece variar según el programa. 
# Las variaciones dentro de cada nivel de programa son más altas que las medias dentro de cada 
# nivel. Estos son los medios condicionales y las variaciones. Estas diferencias sugieren que 
# existe una sobredispersión en los datos y que un modelo binomial negativo sería apropiado.

with(dat, tapply(ausencias, programa, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

#####################################################################################################
# Posibles modelos para analizar estos datos
#####################################################################################################

# A continuación se muestra una lista de algunos métodos de análisis que podrías considerar a la hora 
# de analizar estos datos. Algunos de los métodos enumerados son bastante razonables, mientras que 
# otros pueden tener diversas limitaciones o han caído en desuso.

# (1) Regresión de Poisson: la regresión de Poisson a menudo se usa para modelar datos de conteo. 
# La regresión de Poisson tiene varias extensiones útiles para los modelos de conteo.
 
# (2) Regresión binomial negativa: la regresión binomial negativa se puede usar para datos de conteo 
# con sobredispersión, es decir, cuando la varianza condicional excede la media condicional. Se puede 
# considerar como una generalización de la regresión de Poisson, ya que tiene la misma estructura 
# media que la regresión de Poisson y tiene un parámetro adicional para modelar la sobredispersión. 
# Si la distribución condicional de la variable dependiente se dispersa en exceso, es probable que 
# los intervalos de confianza para la regresión binomial negativa sean más estrechos en comparación
# con los de una regresión de Poisson.
 
# (3) Modelo de regresión con inflación cero: los modelos con inflación cero intentan dar cuenta 
# del exceso de ceros. En otras palabras, se cree que existen dos tipos de ceros en los datos, 
# "ceros verdaderos" y "ceros en exceso". Los modelos con inflación cero estiman dos ecuaciones 
# simultáneamente, una para el modelo de conteo y otra para los ceros en exceso.
 
# (4) Regresión MCO: En ocasiones, los datos de conteo se transforman logarítmicamente 
# y se analizan mediante regresión OLS. Con este enfoque surgen muchos problemas, incluida la pérdida 
# de datos debido a valores indefinidos generados al tomar el log de cero (que es indefinido) 
# y estimaciones sesgadas.


#####################################################################################################
# El modelo de regresión binomial negativo
#####################################################################################################

# Para analizar los datos utilizando el modelo de regresión binomial negativo utilizaremos la 
# función "glm.nb" del paquete "MASS":

summary(negbin.model <- glm.nb(ausencias ~ mate + programa, data = dat))

# Lo primero que hace R es mostrar la desviación de los residuos. A continuación, vemos los coeficientes
# estimados, junto con los errores estándar, los valores del estadístico t y los valores p. La variable
# mate tiene un coeficiente de -0.006, que es estadísticamente significativo. Esto significa que por 
# cada aumento de una unidad en la nota de matemáticas, el número esperado de días ausentes disminuye
# en 0.006. Las variables ficticias programageneral y programavocacional recogen la diferencia esperada 
# en el logaritmo del número de ausencias entre grupo 2 (general) o grupo 3 (vocacional) y el grupo de 
# referencia (programa = general). En el primer caso esta diferencia es 0.44 unidades mayor y el segundo
# caso 0.84 unidades menor. Para determinar si el programa en sí mismo, en general, es estadísticamente 
# significativo, podemos comparar un modelo con y sin programa. La razón por la que es importante
# ajustar modelos separados es que, a menos que lo hagamos, el parámetro de sobredispersión se mantiene
# constante.

m2 <- update(negbin.model, . ~ . - programa)
anova(negbin.model, m2)


# El contraste de LR con dos grados de libertad indica que programa es una variable estadísticamente 
# relevante del número de ausencias. La desviación nula (Null deviance) se calcula a partir de un modelo 
# con término independiente solamente y 313 grados de libertad. La desviación residual (Residual Deviance),
# recoge la desviación del modelo completo. También se nos muestra el valor de AIC, el parámetro de dispersión
# theta (que es el inverso del parámetro de dispersión alpha estimado en otros paquetes como Stata).Por 
# lo tanto, el valor theta de 1.033 es equivalente al valor de alpha de 1 / 1.033 = 0.968.

(alpha <- 1/negbin.model$theta)

#####################################################################################################
# Contraste de las hipótesis del modelo
#####################################################################################################

# Como mencionamos anteriormente, los modelos binomiales negativos suponen que las medias condicionales
# no son iguales a las varianzas condicionales. Esta desigualdad se captura al estimar un parámetro de 
# dispersión (alpha = 1/theta) que se mantiene constante en el modelo de Poisson, por lo que el modelo 
# de Poisson está anidado en el modelo binomial negativo. En este caso podemos utilizar el contraste
# de LR para comparar ambos modelos y contrastar la sobredispersión de los datos que subyace en el modelo
# binomial negativo.

poisson.model <- glm(ausencias ~ mate + programa, family = "poisson", data = dat)
pchisq(2 * (logLik(negbin.model) - logLik(poisson.model)), df = 1, lower.tail = FALSE)

# El LR test con un grado de libertad está asociado con un valor p de <0.001, lo que indica que
# el modelo binomial negativo es claramente más apropiado que el modelo de poisson.

# Podemos obtener intervalos de confianza del 95% de los parámetros estimados

(est <- cbind(Estimate = coef(negbin.model), confint(negbin.model)))

# Podríamos estar interesados en analizar las tasas de incidencia en lugar de los coeficientes. 
# Para ello, podemos exponenciar los coeficientes estimados y los intervalos de confianza.

exp(est)

# En esta tabla vemos que el ratio de incidencia del programageneral es 1.55 veces el ratio de 
# incidencia del programa de referencia (academico). Por su parte, el ratio de incidencia del 
# programavocacional es 0.43 veces el ratio de incidencia del programa de referencia (academico),
# manteniendo constante el resto de factores. El cambio porcentual en el ratio de incidencia de 
# los días ausentes para cada aumento unitario de la nota de mate es una reducción del 1%.

#####################################################################################################
# Predicción
#####################################################################################################

# Para comprender mejor el modelo, podemos ver los conteos predichos para diferentes valores de las 
# variables explicativas. A continuación, creamos nuevos conjuntos de datos con valores de mate y 
# programa y luego utilizamos el comando de predicción para calcular el número de eventos pronosticado.

Primero, podemos ver los conteos pronosticados para cada valor de prog mientras mantenemos las matemáticas en su media. Para hacer esto, creamos un nuevo conjunto de datos con las combinaciones de prog y matemáticas para las que nos gustaría encontrar valores pronosticados, luego usamos el comando de predicción.

newdata1 <- data.frame(mate = mean(dat$mate), programa = factor(1:3, levels = 1:3, 
                                                            labels = levels(dat$programa)))
newdata1$phat <- predict(negbin.model, newdata1, type = "response")
newdata1

# Manteniendo la puntuación de mate en la media, el número de eventos predicho (días ausentes) para el 
# programa general es 10.24, 6.6 para el programa académico y 2.85 para el vocacional.

# Podemos obtener estas predicciones para todo el rango de valores de mate en cada uno de los tres
# programas (en sombreado intervalos de confianza del 95%)

newdata2 <- data.frame(
  mate = rep(seq(from = min(dat$mate), to = max(dat$mate), length.out = 100), 3),
  programa = factor(rep(1:3, each = 100), levels = 1:3, labels =
                  levels(dat$programa)))

newdata2 <- cbind(newdata2, predict(negbin.model, newdata2, type = "link", se.fit=TRUE))
newdata2 <- within(newdata2, {
  ausencias <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

ggplot(newdata2, aes(mate, ausencias)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = programa), alpha = .25) +
  geom_line(aes(colour = programa), size = 2) +
  labs(x = "Puntuación en Mate", y = "Ausencias predichas")

#####################################################################################################
# Consideraciones finales
#####################################################################################################

# (1) No se recomienda aplicar modelos binomiales negativos a muestras pequeñas.
# (2) Una causa común de sobredispersión es el exceso de ceros por un proceso adicional de generación 
# de datos. En este caso, se debe considerar el modelo de inflación de ceros.
# (3) Si el proceso de generación de datos no permite ningún 0 (como el número de días que pasa en 
# el hospital), entonces un modelo truncado a cero puede ser más apropiado.
# (4) Los datos de conteo a menudo tienen una variable de exposición, que indica la cantidad de veces 
# que el evento pudo haber sucedido. Esta variable debe incorporarse al modelo de regresión binomial 
# negativa con el uso de la opción "offset" (ver documentación de glm para más detalles).
# (5) La variable dependiente en una regresión binomial negativa no puede tener números negativos.
# (6) Deberá usar el comando negbin.model$resid para obtener los residuos del modelo si se quiere 
# contrastar otros spuestos del modelo binomial negativo.

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

# Documentación sobre el paquete glm:
http://stat.ethz.ch/R-manual/R-patched/library/stats/html/glm.html
