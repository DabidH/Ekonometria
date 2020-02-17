########################################################
##### Introducción a R                ##################
##### David Hoyos                     ##################
##### UPV/EHU, 2020                   ##################
########################################################

# Instalación de paquetes básicos

rm(list=ls())

install.packages(c("ggplot2","dplyr","tidyr","tidyverse","reshape2"), dependencies = T)

install.packages(c("AER","censReg","truncreg","mhurdle","stargazer", "foreign", "sampleSelection"), dependencies = T)

install.packages(c("car","MASS","lmtest","glmpath"), dependencies = T)

install.packages(c("ISLR","markdown","knitr","kableExtra"), dependencies = T)


# Para saber que package contiene una función, utilizamos la función"find" 

find("mean")

find("ggplot")
