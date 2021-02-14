
#####################################################################################################
##################           El modelo Truncado                                    ##################
##################           David Hoyos                                           ##################
##################           UPV/EHU, 2020                                         ##################
#####################################################################################################

####################################################################################################
# Introducción
#####################################################################################################

# La regresión truncada se usa para modelizar variables dependientes para las cuales algunas de las
# observaciones no se incluyen en el análisis debido al valor de la variable dependiente.

# Ejemplo 1. Un estudio de estudiantes en un programa especial GATE (educación para alumnos con  
# talento especial) desea modelizar el resultado académico en función de las habilidades del lenguaje y el
# tipo de programa en el que el estudiante está actualmente matriculado. Una de las principales
# preocupaciones es que se requiere que los estudiantes tengan una puntuación de logro mínimo de 40
# para ingresar al programa especial. Por lo tanto, la muestra se trunca en este punto.

# Ejemplo 2. Un investigador tiene datos para una muestra de estadounidenses cuyos ingresos están
# por encima de la línea de pobreza. Por lo tanto, la parte inferior de la distribución del ingreso
# se trunca. Si el investigador tuviera una muestra de estadounidenses cuyos ingresos estuvieran
# por debajo de la línea de pobreza, la parte superior de la distribución del ingreso se vería
# truncada. En otras palabras, el truncamiento es el resultado de muestrear solo una parte de
# la distribución de la variable de resultado.

#####################################################################################################
# Descripción de los datos
#####################################################################################################

# Vamtos a ver un ejemplo con datos simulados a partir del Ejemplo 1 anterior. Empezaremos por cargar
# algunas librerías y leer los datos.

rm(list=ls())

library(ggplot2)
library(AER)
library(MASS)
library(car)
library(stargazer)
library(truncreg)
library(boot)

dat <-  read.csv("https://raw.githubusercontent.com/DabidH/datasets/master/trunc.csv")

# Tenemos un archivo de datos hipotéticos, trunc.csv, con 178 observaciones. La variable dependiente
# se llama "resul" y las variables explicativas son la puntuación en la prueba de lenguaje,
# "lengua" y la variable cualitativa "prog", con tres niveles que indican el tipo de programa en
# el que se matricularon los estudiantes: academico, general y vocacional.

str(dat)
summary(dat)

dat$acad <- dat$programa=="academico"
dat$gen <- dat$programa=="general"
dat$voc <- dat$programa=="vocacional"


ggplot(dat, aes(resul)) +
  geom_histogram()

ggplot(dat, aes(x = lengua, y = resul)) +
  geom_point() +
  xlim(25, 80) +
  ylim(0, 80) +
  geom_smooth(method= "lm")

ggplot(dat, aes(resul, fill = programa)) +
  geom_histogram(binwidth=3)

ggplot(dat, aes(programa, resul)) +
  geom_boxplot() +
  geom_jitter()

ggplot(dat, aes(x = lengua, y = resul, colour = factor(programa))) +
  geom_point()+
  geom_smooth(method= "lm")


ggplot(dat, aes(x = lengua, y = resul)) +
  geom_point() +
  stat_smooth(method = "loess") +
  facet_grid(. ~ programa, margins=TRUE)

#####################################################################################################
# Análisis de datos
#####################################################################################################

# Podemos analizar los datos con distintos modelos aunque, como veremos, algunos serán más razonables
# que otros, o tendrán limitaciones importantes:

# • Regresión MCO: se puede analizar estos datos utilizando la regresión MCO. La regresión lineal no
# ajustará las estimaciones de los coeficientes teniendo en cuenta el truncamiento de la muestra cuando
# resul=40, y los coeficientes pueden estar severamente sesgados. Esto puede conceptualizarse como
# un error de especificación del modelo (Heckman, 1979).

# • Regresión truncada: la regresión truncada aborda el sesgo introducido al usar la regresión MCO
# con datos truncados. Hay que tener en cuenta que con la regresión truncada, la varianza de la
# variable dependiente se reduce en comparación con la distribución que no se trunca. Además, si
# la parte inferior de la distribución está truncada, entonces la media de la variable truncada
# será mayor que la media de la variable no truncada; Si el truncamiento es desde arriba, la media
# de la variable truncada será menor que la variable no truncada.
# • Estos tipos de modelos también pueden conceptualizarse como modelos de selección de Heckman,
# que se utilizan para corregir el sesgo de selección de muestreo.

# • Regresión censurada: a veces los conceptos de truncamiento y censura son confusos. Con datos
# censurados tenemos todas las observaciones, pero no conocemos los valores "verdaderos" de algunos
# de ellos. Con el truncamiento, algunas de las observaciones no se incluyen en el análisis debido
# al valor de la variable dependiente. Es por ello que sería inapropiado analizar los datos del
# ejemplo anterior utilizando un modelo de regresión censurado.

# Podemos empezar con la estimación MCO

mco.model <- lm(resul ~ lengua + programa, data = dat)
mco.model <- lm(resul ~ lengua + programa -1, data = dat)
summary(mco.model)

# A continuación, utilizamos la función "truncreg" del paquete truncreg para estimar un modelo de
# regresión truncado. El argumento "punto" indica dónde se truncan los datos, y la dirección indica
# el truncamiento es hacia la izquierda o hacia la derecha.

trunc.model <- truncreg(resul ~ lengua + programa -1, data = dat, point = 40, direction = "left")
summary(trunc.model)

# Esperanza de Y según el estimador MCO (estimador inconsistente)
(ey.mco <- mean(predict(mco.model)) )

# Esperanza de Y* en el modelo truncado 
(ey.trunc <- mean(predict(trunc.model)))

# Podemos comprobar cómo a partir de la estimación truncada, E(y), recuperamos la esperanza
# truncada del estimador MCO  E(y|y>tau)
xb.t <- predict(trunc.model)
betas <- cbind(trunc.model$coefficients[1:4])
s <- trunc.model$coefficients["sigma"]
delt <- (xb.t-40)/s
irm <- dnorm(delt)/pnorm(delt)                  

(ey.trunc <- mean(xb.t+(s*irm)))


# • El modelo estimado muestra los coeficientes estimados de la regresión truncados,
# junto al error estándar de los coeficientes, estadístico t y el valor p.
# • El estadístico sigma es equivalente al error estándar de estimación en la regresión de MCO.
# El valor de 8.76 se puede comparar con la desviación estándar de resul, que fue de 8.96. Esto
# muestra una reducción modesta. La salida también contiene una estimación del error estándar
# de sigma.

# De cara a la interpretación de la estimación tenemos que tener cuidado, porque no es lo mismo
# situarse en el modelo latente o el modelo observado.
# Si nos interesa interpretar el modelo latente, como es este caso, dado que estamos interesados
# en analizar el resultado sin tener en cuenta el truncamiento (i.e. Y*) entonces:
# • La variable "lengua" es estadísticamente significativa: un aumento unitario en la puntuación
# de esta prueba conlleva un aumento de 0.71 unidades en el resultado previsto.
# • Las variables ficticias asociadas a "programa" son "casi" estadísticamente significativas.
# • El logro esperado de un estudiante de programa general cuando la nota de lenguaje es 0, 
# es de 15 puntos, mientras que en el programa general y vocacional son 11 y 10 respectivamente.

# Si estamos interesados en la variable observada, i.e. Y|Y>tau, entonces hay que recordar que los 
# efectos marginales no son constantes, el efecto marginal promedio sería:

eff.marg <- betas*mean((1-(delt*irm)-(delt*irm^2)))
eff.marg

#############################################################################################

# Para contrastar la significatividad del programa, podemos utilizar el contraste LR

m2 <- update(trunc.model, . ~ . - programa) # actualizamos el modelo suprimiendo "programa"

pchisq(-2 * (logLik(m2) - logLik(trunc.model)), df = 2, lower.tail = FALSE)

# El contraste de chi-cuadrado con 2 grados de libertad indica que "programa" es una variable
# estadísticamente significativa para explicar "resul". 

#############################################################################################

#Podemos obtener las medias para cada programa reparametrizando el modelo sin término constante:

dat <- within(dat, {mlengua <- lengua - mean(lengua)})

malt <- truncreg(resul ~ 0 + mlengua + programa, data = dat, point = 40)
summary(malt)

# Todo lo que ha variado es que el término constante ha desaparecido y las puntuaciones
# del programa son ahora los valores esperados cuando lengua está en su promedio para cada
# tipo de programa.

#############################################################################################

# También podríamos calcular los intervalos de confianza mediante simulación. Primero,
# definimos una función que devuelve los parámetros de interés, y luego usamos la función
# bootstrap.

f <- function(data, i) {
  require(truncreg)
  m <- truncreg(formula = resul ~ lengua + programa, data = data[i, ], point = 40)
  as.vector(t(summary(m)$coefficients[, 1:2]))
}

set.seed(10)

(res <- boot(dat, f, R = 1200, parallel = "snow", ncpus = 4))

# parametros estimados con percentiles e intervalos de confianzas
parms <- t(sapply(c(1, 3, 5, 7, 9), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5], bcaLL = bca[4],
              bcaLL = bca[5]))
}))

row.names(parms) <- names(coef(trunc.model)) # nombramos a las filas con los coeficientes
parms

confint(trunc.model, level=0.95)

#############################################################################################

# Podemos calcular una estimación aproximada del grado de relación para el modelo
# general, correlacionando "resul" con el valor predicho y elevándolo al cuadrado.

dat$yhat <- fitted(trunc.model)

(r <- with(dat, cor(resul, yhat)))
r^2

# El valor calculado de 0.31 es una estimación aproximada del R2 que obtendríamos
# en una regresión MCO. La correlación al cuadrado entre los valores de "resul"
# observados y estimados es de aproximadamente 0.31, lo que indica los valores
# estimados explican algo más del 30% de la variabilidad en la variable de resultado.











