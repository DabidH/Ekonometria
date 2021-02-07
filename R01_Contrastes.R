#####################################################################################################
##################           Selección de modelos                                  ##################
##################           David Hoyos                                           ##################
##################           UPV/EHU, 2020                                         ##################
#####################################################################################################

# Vamos a ver el funcionamiento de los tres contrastes para modelos anidados: a) prueba de razón de 
# verosimilitud, b) prueba de Wald y c) LM test (score test). 

# Partiremos de los datos de mroz:


rm(list=ls())

mroz <- read.csv("https://raw.githubusercontent.com/DabidH/datasets/master/mroz.csv")

str(mroz)

mroz$educa[mroz$educ<=8] <- 1
mroz$educa[mroz$educ<=13 & mroz$educ>8 ] <- 2
mroz$educa[mroz$educ>13] <- 3

mroz$educa <- factor(mroz$educa)
table(mroz$educa)

attach(mroz)

# Supongamos que queremos explicar la probabilidad de que las mujeres participen en el 
# mercado laboral y proponemos un modelo (1) explicado por la edad de la mujer, su nivel
# de estudios y la edad del marido. Alternativamente, el modelo (2) omite el nivel 
# educativo de la mujer (3 niveles, dos variables ficticias)

model1 <- glm(inlf ~ age + educa + husage, family = binomial)

summary(model1)

model2 <- glm(inlf ~ age + husage, family = binomial)

summary(model2)

# Supongamos que queremos contrastar que la educación no influye en la probabilidad de 
# participar en el mercado laboral, es decir, H0: b3 = b4 = 0

# LR test

logLik(model1)
logLik(model2)

gl <- length(coef(model1)) - length(coef(model2)) 
gl

LRstat <- -2*(as.numeric(logLik(model2))-as.numeric(logLik(model1)))
LRstat

valor.crit <- qchisq(0.95, gl)
valor.crit

valor.p <- pchisq(LRstat, gl, lower.tail = FALSE)
valor.p


library(AER)
lrtest(model1,model2)

# Es decir, rechazamos la hipótesis nula, la educación sí influye.

# Estadístico de Wald 

R <- matrix(data = c(0,0,1,0,0,
                     0,0,0,1,0), nrow = 2, ncol = 5, byrow = T)
b <- coefficients(model1)
matrix(data = coefficients(model1), nrow = 5, ncol = 1) 
r <- c(0,0)



diff <- R%*%b - r 

Vdiff <- R%*%vcov(model1)%*%t(R)

W <- t(diff)%*%solve(Vdiff)%*%diff
W

gl <- length(r)

valor.crit <- qchisq(0.95, gl)
valor.crit

valor.p <- pchisq(W, gl, lower.tail = FALSE)
valor.p

# tambien podemos utilizar el paquete aod
library(aod)
wald.test(b = coef(model1), Sigma = vcov(model1), Terms = 3:4)

# o el paquete AER
library(AER)

waldtest(model1,model2)

# Y por tanto, obtenemos el mismo resultado, rechazamos la hipótesis nula.


# En el caso de modelos no anidados, podemos utilizar el Akaike Information Criterion (AIC)
# o el Bayesian Information Criteria (BIC). El modelo que mejor se ajusta a los datos será el que 
# tenga un valor de AIC/BIC más bajo.

AIC1 <- -2*as.numeric(logLik(model1)) + 2*length(coef(model1)) 
AIC1
AIC2 <- -2*as.numeric(logLik(model2)) + 2*length(coef(model2)) 
AIC2
AIC(model1)
AIC(model2)

BIC1 <- -2*as.numeric(logLik(model1)) + log(nrow(mroz))*length(coef(model1)) 
BIC1
BIC2 <- -2*as.numeric(logLik(model2)) + log(nrow(mroz))*length(coef(model2)) 
BIC2
BIC(model1)
BIC(model2)









