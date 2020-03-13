#####################################################################################################
##################           Modelos de conteo Long (1990)                         ##################
##################           David Hoyos                                           ##################
##################           UPV/EHU, 2020                                         ##################
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

# Estimación de modelos de regresión
summary(linear.model <- lm(lart ~ fem + mar + kid5 + phd + ment, data=dat))
summary(poisson.model <- glm(art ~ fem + mar + kid5 + phd + ment, family="poisson", data=dat))
summary(nb.model <- glm.nb(art ~ fem + mar + kid5 + phd + ment, data=dat))
summary(zip.model <- zeroinfl(art ~ fem + mar + kid5 + phd + ment | fem + mar + kid5 + phd + ment, data = dat))
summary(zinb.model <- zeroinfl(art ~ fem + mar + kid5 + phd + ment | fem + mar + kid5 + phd + ment, data = dat, 
                               dist = "negbin", EM = TRUE))
summary(ph.model <- hurdle(art ~ fem + mar + kid5 + phd + ment, data = dat, 
                           dist = "poisson", zero.dist = "binomial", link="logit"))

summary(nbh.model <- hurdle(art ~ fem + mar + kid5 + phd + ment, data = dat, 
                           dist = "negbin", zero.dist = "binomial", link="logit"))


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


# Número de ceros observados
sum(dat$art < 1) 

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


# Representación gráfica

ggplot(preds, aes(x=0:19, y=obs.prob)) + 
   geom_line(color = "red") + geom_point( size=2, shape=21, fill="white") + 
  geom_line(y= pois.prob, color = "black") + 
   geom_line(y= nb.prob, color = "blue") + 
  geom_line(y= zip.prob, color = "yellow") + 
  geom_line(y= zinb.prob, color = "green") + 
  geom_line(y= ph.prob, color = "grey") + 
  geom_line(y= nbh.prob, color = "gold") + 
  labs(title= "Predicciones de modelos para datos de conteo",
       y="Científicos", x = "Número de artículos") +
  ggtitle("Predicciones de modelos para datos de conteo")
  theme_minimal()


  