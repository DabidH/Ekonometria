#####################################################################################################
##################           El modelo TOBIT                                       ##################
##################           David Hoyos                                           ##################
##################           UPV/EHU, 2020                                         ##################
#####################################################################################################

rm(list=ls())

library(ggplot2)
library(AER)
library(MASS)
library(car)
library(dplyr) 
library(gridExtra) 

#####################################################################################################
# Introducción
#####################################################################################################

# El modelo tobit, también llamado modelo de regresión censurado, está diseñado para estimar las 
# relaciones lineales entre variables cuando hay censura hacia la izquierda o hacia la derecha en 
# la variable dependiente (también conocida como censura desde abajo y desde arriba, respectivamente). 
# La censura desde arriba se lleva a cabo cuando los casos con un valor en o por encima de un umbral, 
# todos toman el valor de ese umbral, de modo que el valor verdadero podría ser igual al umbral, pero 
# también podría ser mayor. En el caso de censurar desde abajo, los valores que caen en o por debajo 
# de algún umbral se censuran.

# Ejemplo 1. En la década de 1980 existía una ley en EEUU que restringía las lecturas de velocímetros 
# a no más de 85 mph. Entonces, si quisiera intentar predecir la velocidad máxima de un vehículo a 
# partir de una combinación de potencia y tamaño del motor, obtendría una lectura no superior a 85, 
# independientemente de la velocidad con la que el vehículo realmente estuviera viajando. Este es un 
# caso clásico de censura a la derecha (censura desde arriba) de los datos. De lo único que estamos 
# seguros es de que esos vehículos viajaban al menos a 85 mph.
 
# Ejemplo 2. Un proyecto de investigación está estudiando el nivel de plomo en el agua potable de los 
# hogares en función de la edad y de los ingresos familiares. El kit de prueba de agua diseñado no 
# puede detectar concentraciones de plomo por debajo de 5 partes por billón (ppb). La EPA considera 
# que los niveles superiores a 15 ppb son peligrosos. Estos datos son un ejemplo de censura a la 
# izquierda (censura desde abajo).
# 
# Ejemplo 3. Considera la situación en la que tenemos una medida de aptitud académica (escala 200-800) 
# que queremos modelizar usando puntuaciones en pruebas de lectura y matemáticas, así como el tipo 
# de programa en el que está inscrito el estudiante (académico, general o vocacional). El problema 
# aquí es que los estudiantes que responden correctamente todas las preguntas en el examen de 
# aptitud académica reciben una puntuación de 800, a pesar de que es probable que estos estudiantes no 
# sean "verdaderamente" iguales en aptitud. Lo mismo se aplica a los estudiantes que responden 
# todas las preguntas incorrectamente. Todos esos estudiantes tendrían una puntuación de 200, aunque 
# puede que no todos tengan la misma aptitud.

#####################################################################################################
# Simulación de datos censurados
#####################################################################################################

simulador <- function(x){x + 20} 
observaciones <- simulador(seq(0,50,length.out = 200)) 

# Para introducir la varianza propia de los datos observacionales se añade # ruido 
# aleatorio y normal a cada observación 

set.seed(123) 
observaciones <- observaciones + rnorm(n = 200, mean = 0, sd = 12) 
datos <- data.frame(x = seq(0,50,length.out = 200), y = observaciones) 

# Regresión MCO con los datos no censurados

ggplot(data = datos, aes(x = x, y = y)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() + lims(y = c(0,100)) + labs(title = "Regresión OLS con los datos no censurados")


# Supongamos que el analista no observa valores de la variable Y inferiores a 30, 
# a pesar de que sí que son capaces de detectarlos y por lo tanto saben que existen. 
# La muestra observada pasaría a ser la siguiente:

ggplot(data = datos, aes(x = x, y = y)) + 
  geom_point(size = 2) + 
  geom_point(data = filter(datos, y < 30), size = 2, color = "grey") + 
  geom_hline(yintercept = 30, linetype = "dashed") + theme_bw() + 
  lims(y = c(0,100)) + labs(title = "Observaciones por encima y debajo\ndel límite de detección")

# En este contexto, el analista tendrá dos opciones: o considera "0" 
# todos los valores por debajo del límite de detección o los 
# considera "30". Podemos ver cómo afecta una opción y otra al ajuste
# MCO.

datos_0 <- datos 
datos_0$y[datos_0$y < 30] <- 0 
p1 <- ggplot(data = datos_0, aes(x = x, y = y)) + 
  geom_point(size = 2) + geom_point(data = filter(datos_0, y == 0), size = 2, color = "firebrick") +
  geom_smooth(data = datos,method = "lm", se = FALSE) + 
  geom_smooth(data = datos_0, method = "lm", se = FALSE, color = "firebrick") + 
  geom_hline(yintercept = 30, linetype = "dashed") + 
  lims(y = c(0,100)) + theme_bw() + 
  labs(title = "Observaciones censuradas ajustadas a 0")

datos_30 <- datos 
datos_30$y[datos_30$y < 30] <- 30 
p2 <- ggplot(data = datos_30, aes(x = x, y = y)) + 
  geom_point(size = 2) + geom_point(data = filter(datos_30, y == 30), size = 2, color = "firebrick") +
  geom_smooth(data = datos,method = "lm", se = FALSE) + 
  geom_smooth(data = datos_30, method = "lm", se = FALSE, color = "firebrick") + 
  geom_hline(yintercept = 30, linetype = "dashed") + theme_bw() + 
  lims(y = c(0,100)) + labs(title = "Observaciones censuradas ajustadas a 30") 
grid.arrange(p1, p2, ncol = 1)


#####################################################################################################
# Descripción de los datos
#####################################################################################################

# Vamos a ver un ejemplo con datos simulados a partir del Ejemplo 3 anterior. Empezaremos por cargar
# algunas librerías y leer los datos.

dat <-  read.csv("https://raw.githubusercontent.com/DabidH/datasets/master/tobit.csv")

# El conjunto de datos contiene 200 observaciones. La variable de aptitud académica es "aptitud", las 
# puntuaciones de las pruebas de lectura y matemáticas son "lectura" y "mate", respectivamente. 
# La variable "programa" es el tipo de programa en el que se encuentra el alumno, es una variable 
# categórica que toma tres valores: "academico" (prog = 1), "general" (prog = 2) y "vocacional" (prog = 3). 
# La variable obs" es una variable de identificación.

# Ahora veamos los datos descriptivamente. Hay que tener en cuenta que en este conjunto de datos, el 
# valor más bajo de "aptitud" es 352. Es decir, ningún estudiante recibió una puntuación de 200 
# (la puntuación más baja posible), lo que significa que aunque la censura desde abajo era posible, ésto
# no sucede en el conjunto de datos.

summary(dat)

# Veamos gráficamente cómo se produce la censura en este conjunto de datos:
# Para ello compararemos en un mismo gráfico el histograma de la variable "aptitud" y la densidad de una
# distribución normal con media y varianza de "aptitud":

f <- function(x, var, bw = 15) {
  dnorm(x, mean = mean(var), sd(var)) * length(var)  * bw
}

p <- ggplot(dat, aes(x = aptitud, fill=programa))


p + stat_bin(binwidth=15) +
  stat_function(fun = f, size = 1.2, colour ="darkblue",
                args = list(var = dat$aptitud))

# Mirando el histograma anterior, podemos ver la censura en los valores de "aptitd", es decir, hay muchos
# más casos con puntuaciones de 750 a 800 de lo que cabría esperar observando el resto de la distribución. 
# 
# A continuación se muestra un histograma alternativo que resalta aún más el exceso de casos donde 
# "aptitud" = 800. En él, la opción "bindwidth" produce un histograma en el que cada valor único de 
# "aptitud" tiene su propia barra (estableciendo interrupciones iguales a un vector que contiene valores 
# desde el mínimo de hasta el máximo de "aptitud"). Como esta variable es continua, la mayoría de los 
# valores de "aptitud" son únicos en el conjunto de datos, aunque cerca del centro de la distribución 
# hay algunos valores que tienen dos o tres casos. El pico en el extremo derecho del histograma es la 
# barra para casos donde "aptitud" = 800, la altura de esta barra en relación con todos los demás muestra 
# claramente el número excesivo de casos con este valor.

p + stat_bin(binwidth = 1) + stat_function(fun = f, size = 1, args = list(var = dat$aptitud, 
                                                                          bw = 1))
# A continuación, exploraremos las relaciones bivariante en nuestro conjunto de datos.

cor(dat[, c("lectura", "mate", "aptitud")])
ggpairs(dat[, c("lectura", "mate", "aptitud")])

# En la primera fila de la matriz de diagramas de dispersión, vemos los diagramas de dispersión que 
# muestran la relación entre "lectura" y "aptitud", así como "mate" y "aptitud". Debemos 
# tener en cuenta la acumulación de observaciones en la parte superior de estos dos diagramas de 
# dispersión, esto se debe a la censura en la distribución de "aptitud"

#####################################################################################################
# Análisis de datos
#####################################################################################################

# Podemos analizar los datos con distintos modelos aunque, como veremos, algunos serán más razonables 
# que otros, o tendrán limitaciones importantes:
# 
# • Regresión Tobit, el modelo para variables censuradas.
# • Regresión OLS: podemos analizar estos datos mediante la regresión MCO. La regresión MCO tratará 
# las puntuaciones 800 como valores reales y no como el límite inferior de la aptitud académica más alta. 
# Una limitación de este enfoque es que cuando la variable está censurada, MCO proporciona estimaciones 
# inconsistentes de los parámetros, lo que significa que los coeficientes estimados no necesariamente 
# se acercarán a los parámetros poblacionales (verdaderos) a medida que aumente el tamaño de la muestra. 
# • Regresión truncada: Cuando se censura una variable, los modelos de regresión para datos truncados 
# proporcionan estimaciones inconsistentes de los parámetros. 

summary(tobit.model <- tobit(aptitud ~ lectura + mate + programa, right = 800, data = dat))

confint(tobit.model)

# Vemos los resultados principales de la estimación:

# La tabla denominada coeficientes proporciona los coeficientes, errores estándar, estadísticos z y p.
# Los coeficientes de regresión de Tobit se interpretan de manera similar a los coeficientes de regresión 
# MCO; sin embargo, el efecto lineal está en la variable latente sin censura, no en el resultado observado. 
# Los coeficientes asociados a las variables "lectura", "mate" y programa="vocacional" son estadísticamente
# significativos.
# • Para un aumento de una unidad en "lectura", hay un aumento de 2.6981 puntos en el valor predicho
# de "aptitud".
# • Un aumento de una unidad en "mate" está asociado con un aumento de 5.9146 unidades en el valor 
# predicho de "aptitud".
# • Los coeficientes asociados a la variable "programa" tienen una interpretación ligeramente diferente. 
# El valor predicho de "aptitud" es -46.1419 puntos inferior para los estudiantes en un programa vocacional
# respecto de los estudiantes en un programa académico.
# • El coeficiente "(Intercept)" es la constante del modelo.
# • El coeficiente "Log(scale)" es un estadístico auxiliar. Si calculamos la exponencial de este valor, 
# obtenemos un estadístico análogo a la raíz cuadrada de la varianza residual en la regresión de MCO 
# (Scale). El valor de 65.6773 puede compararse con la desviación estándar de aptitud académica 
# que era 99.21, una reducción sustancial.
# • El máximo del logaritmo de la función de verosimilitud, -1041.0629, se muestra en la parte inferior, 
# que podremos utilizar en en comparaciones de modelos anidados.

# Podemos evaluar la importancia del tipo de programa en general ajustando un modelo sin programa y 
# realizando el contraste de razón de verosimilitud.

m1 <- tobit(aptitud ~ lectura + mate + programa, right = 800, data = dat)
m2 <- tobit(aptitud ~ lectura + mate, right = 800, data = dat)

(p <- pchisq(2 * (logLik(m1) - logLik(m2)), df = 2, lower.tail = FALSE))

# El LR test con dos grados de libertad está asociado con un valor p de 0.0032, lo que indica que
# el efecto general de "programa" es estadísticamente significativo.

# O el mismo contraste mediante el estadístico de Wald

linearHypothesis(tobit.model, c("programageneral = 0", "programavocacional = 0"), vcov=sandwich)

#####################################################################################################
# Análisis de residuos
#####################################################################################################

# Una vez estimado el modelo, podemos estar interesados en examinar qué tal se ajusta a los datos, para
# lo cual podemos hacer diferentes gráficos de los residuos

dat$aptitud.estim <- predict(tobit.model)
dat$res <- residuals(tobit.model, "response")
dat$res.d <- residuals(tobit.model, "deviance")

ggplot(dat, aes(x=aptitud.estim,y=res))+ 
  geom_point() + ggtitle("Ajustado vs Residuos")

ggplot(dat, aes(x=aptitud.estim,y=res.d))+ 
  geom_point() + ggtitle("Ajustado vs Residuos estandarizados")

ggplot(dat, aes(x=aptitud,y=res.d))+ 
  geom_point() + ggtitle("Observado vs Residuos estandarizados")

ggplot(dat, aes(x=aptitud,y=aptitud.estim))+ 
  geom_point() + ggtitle("Observado vs Ajustado")

library(ggpubr)
ggqqplot(dat$res)
ggqqplot(dat$res.d)

# La correlación entre los valores predichos y observados de "aptitud" es 0.7825. El cuadrado de 
# este estadístico nos indica que los valores predichos comparten el 61.23% de su varianza con 
# "aptitud". 

(r <- with(dat, cor(aptitud.estim, aptitud)))



