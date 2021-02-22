#####################################################################################################
##################           Modelos de conteo Long (1990)                         ##################
##################           David Hoyos                                           ##################
##################           UPV/EHU, 2021                                         ##################
#####################################################################################################

rm(list=ls())

library(ggplot2)
library(pscl)
library(boot)
library(MASS)
library(jtools)
library(huxtable)

dat <- read.csv("https://raw.githubusercontent.com/DabidH/datasets/master/long.csv")

str(dat)
summary(dat)
dat$mar <- relevel(dat$mar, ref = "Single")
dat$lart <- log(dat$art+0.5)

attach(dat)

ggplot(dat, aes(x=art)) + 
  geom_histogram()

#####################################################################################################
# El modelo de Poisson
#####################################################################################################

summary(poisson.model <- glm(art ~ fem + mar + kid5 + phd + ment, family="poisson", data=dat))

# E(y) en el modelo de Poisson
xb.poisson <- predict(poisson.model)
(lambda <- mean(exp(xb.poisson)))

# Alternativamente:
(lambda2 <- mean(predict(poisson.model, type = "response")))

# Interpretación de coeficientes: efectos parciales
(cambio.parcial <- lambda * coef(poisson.model))

# Interpretación de coeficientes: efectos porcentuales
(betas.poisson <- (exp(coef(poisson.model))-1)*100)

# Cambio discreto para variables dicotomicas: E(y) para hombres y mujeres 
w <- xb.poisson[fem=="Women"]
m <- xb.poisson[fem=="Men"]

(lambda.fem <- mean(exp(w)))
(lambda.men <- mean(exp(m)))

(woman <- lambda.fem - lambda.men)

lfem <- exp(0.30461 -0.224594 +0.155243*0.336 -0.184883*0.4951 +0.012823*3.103 +0.025543*8.767)
lmen <- exp(0.30461 +0.155243*0.336 -0.184883*0.4951 +0.012823*3.103 +0.025543*8.767)

(diff <- lfem - lmen)

#####################################################################################################
# Comparación de modelos de regresión para datos de conteo
#####################################################################################################

summary(linear.model <- lm(lart ~ fem + mar + kid5 + phd + ment, data=dat))
summary(poisson.model <- glm(art ~ fem + mar + kid5 + phd + ment, family="poisson", data=dat))
summary(nb.model <- glm.nb(art ~ fem + mar + kid5 + phd + ment, data=dat))
summary(zip.model <- zeroinfl(art ~ fem + mar + kid5 + phd + ment | fem + mar + kid5 + phd + ment, data = dat))
summary(zinb.model <- zeroinfl(art ~ fem + mar + kid5 + phd + ment | fem + mar + kid5 + phd + ment, data = dat, 
                               dist = "negbin"))
summary(ph.model <- hurdle(art ~ fem + mar + kid5 + phd + ment, data = dat, 
                           dist = "poisson", zero.dist = "binomial", link="logit"))
summary(nbh.model <- hurdle(art ~ fem + mar + kid5 + phd + ment, data = dat, 
                           dist = "negbin", zero.dist = "binomial", link="logit"))


# Contraste de sobredispersion
pscl::odTest(nb.model)


# Comparamos el ajuste de los modelos mediante el AIC y BIC:

AIC(poisson.model, nb.model, zip.model, zinb.model, ph.model, nbh.model)
BIC(poisson.model, nb.model, zip.model, zinb.model, ph.model, nbh.model)

# El modelo ZINB tiene menor AIC y el modelo NB tiene mejor BIC.

# Interpretación de coeficientes: efectos marginales porcentuales
betas.poisson <- (exp(coef(poisson.model))-1)*100
betas.nb <- (exp(coef(nb.model))-1)*100
betas.zip <- (exp(coef(zip.model, "count"))-1)*100
betas.zinb <- (exp(coef(zinb.model, "count"))-1)*100
betas.ph <- (exp(coef(ph.model, "count"))-1)*100
betas.nbh <- (exp(coef(nbh.model, "count"))-1)*100

(model.comparison <- cbind(betas.poisson,betas.nb, betas.zip, betas.zinb, betas.ph, betas.nbh))

effect_plot(poisson.model, pred = ment, interval = T)
effect_plot(poisson.model, pred = fem, interval = T)


#####################################################################################################
# E(y) en los modelos para datos de conteo
#####################################################################################################

# Podemos utilizar la función "predict" para obtener los conteos esperados medios para cada
# observación, cuya media sería la estimación de la esperanza incondicional de y 

ey.poisson <- mean(predict(poisson.model, type = "response"))
ey.nb      <- mean(predict(nb.model, type = "response"))
ey.zip     <- mean(predict(zip.model, type = "response"))
ey.zinb    <- mean(predict(zinb.model, type = "response"))
ey.ph      <- mean(predict(ph.model, type = "response"))
ey.nbh     <- mean(predict(nbh.model, type = "response"))


#####################################################################################################
# Predicción en los modelos para datos de conteo
#####################################################################################################

# Número de ceros observados
sum(dat$art == 0) 

# Número de ceros predichos por una distrib Poisson
round(colSums(predprob(poisson.model))[1])
# Número de ceros predichos por una distrib NB
round(colSums(predprob(nb.model))[1])
# Número de ceros predichos por una distrib PH
round(sum(predict(zip.model, type = "prob")[,1]))
# Número de ceros predichos por una distrib NBH
round(sum(predict(zinb.model, type = "prob")[,1]))
# Número de ceros predichos por una distrib PH
round(sum(predict(ph.model, type = "prob")[,1]))
# Número de ceros predichos por una distrib NBH
round(sum(predict(nbh.model, type = "prob")[,1]))

# Predicción de los modelos

obs.prob <-c(sum(dat$art==0), sum(dat$art==1), sum(dat$art==2), sum(dat$art==3),sum(dat$art==4),
             sum(dat$art==5), sum(dat$art==6), sum(dat$art==7), sum(dat$art==8),sum(dat$art==9),
             sum(dat$art==10), sum(dat$art==11), sum(dat$art==12), sum(dat$art==13),sum(dat$art==14),
             sum(dat$art==15), sum(dat$art==16), sum(dat$art==17), sum(dat$art==18),sum(dat$art==19)
              )


pois.prob <- round(colSums(predprob(poisson.model)))
nb.prob <- round(colSums(predprob(nb.model)))
zip.prob <- round(colSums(predict(zip.model, type = "prob")))
zinb.prob <- round(colSums(predict(zinb.model, type = "prob")))
ph.prob <- round(colSums(predict(ph.model, type = "prob")))
nbh.prob <- round(colSums(predict(nbh.model, type = "prob")))



preds <- data.frame(cbind(obs.prob, pois.prob,nb.prob,zip.prob,zinb.prob,ph.prob,nbh.prob))


#####################################################################################################
# Representación gráfica
#####################################################################################################

ggplot(preds, aes(x=0:19, y=obs.prob)) + 
   geom_line(color = "red") + geom_point( size=2, shape=21, fill="white") + 
  geom_line(y= pois.prob, color = "black") + 
   geom_line(y= nb.prob, color = "blue") + 
  geom_line(y= zip.prob, color = "yellow") + 
  geom_line(y= zinb.prob, color = "green") + 
  geom_line(y= ph.prob, color = "grey") + 
  geom_line(y= nbh.prob, color = "gold") + 
  labs(title= "Predicciones de modelos para datos de conteo",
       y="Científicos", x = "Número de artículos", color="Legend") +
  ggtitle("Predicciones de modelos para datos de conteo") +
  theme_minimal()


#####################################################################################################
# Referencias
#####################################################################################################

# Long, J. S. 1997. Regression Models for Categorical and Limited Dependent Variables. 
# Thousand Oaks, CA: Sage Publications. Everitt, B. S. and Hothorn, T. A Handbook of Statistical 
# Analyses Using R


  