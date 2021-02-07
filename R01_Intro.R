#####################################################################################################
##################           Introducción a R                                      ##################
##################           David Hoyos                                           ##################
##################           UPV/EHU, 2020                                         ##################
#####################################################################################################

########################################################
# Conceptos básicos
########################################################

# R es un lenguaje de programación y un entorno para el análisis estadístico y gráfico.

# El símbolo <- es el operador para asignar. También se puede utilizar = (o menos 
# frecuente ->), aunque es preferible utilizar el <-.

# El símbolo # se utiliza para introducir un comentario. Todo lo que quede a la 
# derecha de # no se ejecutará.

# Normalmente, empezamos borrando todos los objetos definidos previamente:

rm(list=ls())

########################################################
## Operaciones básicas entre escalares
########################################################

3+5
#
x <- 10 + rnorm(1, mean=0, sd=2)
x
x*x
x^2
sqrt(x)
log(x)
exp(x)
#

# "Valores" especiales: NaN y Inf
0/0 # NaN: Not a Number, valor indefinido
1/0 # Inf: Infinito
-1/0

##### Multiplicación de escalares y vectores
#
a <- c(3, 7, 10) # un vector con 3 elementos
b <- 1:17        # b tiene 17 elementos
c <- 1:6         # c tiene 6 elementos


######################################################## 
## Vectores
########################################################

a
x*a # Cada elemento del vector a es multiplicado por el escalar x 

a
length(a)
c
length(c)

a*c # Multiplicación de dos vectores (longitud igual), elemento a elemento 2 veces

# Multiplicación de dos vectores (longitud diferente)
a
length(a)
b
length(b)

a*b # se multiplica elemento a elemento a pesar de que la longitud del vector mayor no sea proporcional
    # a la del vector menor longitud - warning message

# R nos da un mensaje de aviso (warning), no es lo mismo que un error. 
# Nos avisa que hay algo que no cuadra pero…realiza la operación que nosotros queremos.

# R utiliza funciones para realizar operaciones. Una función es, por ejemplo, mean(). 
# Para utilizar una función deben especificarse unos argumentos, que es lo que 
# escribimos dentro de los paréntesis. En el caso de la función round(), especificamos
# dos argumentos: el vector que queremos redondear (w) y el número de decimales del 
# redondeo (digits).

# media del vector de datos
w <- mean(a)
w

# redondeo
round(w, digits=0)

# Una cuestión muy importante que siempre tenemos que tener en cuenta cuando trabajamos
# con vectores es que en un vector sólo podemos concatenar elementos del mismo tipo. 
# ¿Qué tipos/clases de elementos (o datos) tenemos en R?
#   Carácter
#   Numéricos
#   Enteros
#   Complejos
#   Lógicos

rm(list=ls())
x <- c(1,2,3,4)    # creamos el vector x
class(x)           # devuelve el tipo de objeto

y <- c("a","b")
class(y)

z <- c(1L,2L,3L)   # escribimos L detrás del número para obligar a que sea entero
class(z)

w <- c(TRUE, F)    # en general, puede escribirse TRUE/FALSE o T/F
class(w)

t <- c(1+2i, 1+3i)
class(t)

# ¿Qué sucede si definimos vectores en el que no todos sus elementos son del mismo tipo?
# R fuerza a que todos los elementos del vector sean del mismo tipo. 
# A esto se le llama implicit coercion.

x <- c(1,2,"a")
y <- c(FALSE, 1)
z <- c("a",T)

class(x)
class(y)
class(z)

# También podemos ser nosotros quieens forcemos que todos los elementos del vector sean 
# del mismo tipo (esto es la explicit coercion). Para ello utilizamos las funciones 
# as.numeric() , as.character(), as.logical() … Si el resultado no tiene sentido R 
# producirá un mensaje de error o warning.

x
as.numeric(x)

as.character(x)

# Finalmente, podemos evaluar el tipo/clase de objeto con las funciones is.numeric(), 
# is.character(), is.na() etc.


########################################################
## Matrices
########################################################

# Matriz: definir una matriz, asignar nombres a filas/columnas, acceso a elementos de una matriz

M0 <- matrix (data= c(1,2,3,4), nrow=2, ncol=2) # o x <- matrix (c(1,2,3,4), nrow=2, ncol=2)
M0

?matrix 
M1 <- matrix( 1:9, ncol=3)
M1

# Por defecto, R create matrices rellenando por columnas.
# Esto se puede cambiar usando el argumento byrow=TRUE
M2 <- matrix( 9:1, byrow=TRUE, ncol=3)
M2
dim(M1)
summary(M1) # las columnas son tratadas como variables y las filas como observaciones
#
#
?colnames
colnames(M1) <- c("CL_1", "CL_2", "CL_3")
M1
colnames(M1)
summary(M1)

# Lo mismo para las nombrar las filas
rownames(M1) <- c("Fila_1", "Fila_2", "Fila_3")

# Para seleccionar elementos de una matriz utilizamos el símbolo de
# los corchetes []:

A <- matrix(1:16,4,4)
A

A[2,3]
A[c(1,2),c(2,4)]
A[1:3,2:4]
A[1,]
A[,2]
A[1:2,]
A[,2:3]
A[-c(1,3),]  # si utilizamos el signo *-* estamos indicando que queremos mantener todas las filas/columnas menos aquellas que tienen el signo
 
# El operar para multiplicar matrices es %*%

M3 <- M1 %*% M2 
M3

# La función rbind permite añadir filas, la función cbind permite 
# añadir columnas. 

x <- matrix(c(1,2,3,4),2,2)
y <- c(5,6)

# Si ahora queremos añadir, por filas, los datos contenidos en el 
# objeto y al objeto x,
z <- rbind(x,y)
z

# y si queremos añadir los datos de y a los de x por columnas:

z <- cbind(x,y)
z

# Warning!! En el caso que el número de filas (o columnas) del objeto 
# que añadimos (objeto y) no sea múltiplo del número de filas 
# (o columnas) del objeto al que se añaden los datos (objeto x), 
# R nos dará un mensaje de aviso (Warning message). R no da un error,
# nos avisa de que hay algo que “no cuadra”; con todo, realiza la 
# operación. 

x <- c(4,5)
x

y <- c(10,11,12)
y

z <- rbind(x,y)
z

## Subconjuntos en un vector

rm(list=ls())
vec <- 80:200
vec
vec[1] # muestra el primer elemento de "vec"
vec[1:10] # muestra los elementos 1-10 de "vec" (":" hace una secuencia)
vec[c(1:10, 20, 50, 100)] # muestra elementos seleccionados de "vec"
vec[-c(1:10)] # muestra todos los elementos de "vec" excepto los elementos 1-10 

## Subconjuntos en una matriz

rm(list=ls())
set.seed(10)
M5 <- matrix(rnorm(25), ncol=5)
M5
M5[2,3] # muestra el elemento en la fila 2, columna 3
# por defecto, este elemento es devuelto como un vector de longitud 1,
# aunque se puede cambiar utilizando el argumento drop=FALSE
 
M5[2,3, drop=FALSE]
 
M5[,2] # muestra la segunda columna de datos en forma de vector
M5[, 2, drop=FALSE] # muestra la segunda columna de datos en forma de matriz
M5[1,] # muestra la primera fila de datos en forma de vector
M5[1, , drop=FALSE] # muestra la primera fila de datos en forma de matriz
M5[1:2,]

########################################################
# Loops y condiciones
########################################################

# En caso de querer repetir una operación varias veces, podemos usar un loop:
# "for loop" -> sabemos el número de veces que queremos realizar la operación
# "while loop" -> no sabemos el número de veces que queremos realizar la operación
#                 pero tenemos una variable que se convertirá en FALSE cuando el loop se tiene que detener
# Podemos poner cualquier cosa en el vector iterativo 
for (i in c("Bilbao","World!")) {
  print(i)
}

# Lo utilizamos para un loop cuando sabemos el número de veces que queremos que se repita

for (i in 1:10) {
  print(mtcars[i,c("mpg","cyl","hp")])
}

# Condicionales (if-else)

# Evalúa y ejecuta un código sólo si alguna condición se evalúa como TRUE
#
i <- 100 + rnorm(1, mean = 0, sd = 10)
i
if (i > 100) {
    print(paste("i =", i, "por lo que i es mayor a 100."))
} else {
    print(paste("i =", i, "por lo que i es menor o igual a 100."))
}
#
# Para una condición simple de una línea se puede usar ifelse() 
?ifelse
ifelse(i > 100, "i > 100.", "i <= 100." )

########################################################
#### Operadores lógicos
########################################################

#    <         menor que
#    <=	       menor o igual que
#    >	       mayor que
#    >=	       mayor o igual que
#    ==	       exactamente igual a
#    !=	       no igual a
#    x | y	   x 'o' y
#    x & y	   x 'y' y
#    xor(TRUE, FALSE) evalua como TRUE si un elemento es considerado 
#                     como TRUE, mientras que otros son evaluados como FALSE
#    any()     TRUE si uno de los elementos es evaluados como TRUE
#    all()     TRUE si todos los elementos son evaluados como TRUE

rm(list=ls())

11 > 11
11 >= 11
8 == 9
8 != 9 
(8 == 9) | TRUE
FALSE & TRUE
any(T, F, F)
all(T, F, T, T)
#
#
# Condiciones lógicas utilizados con datos:
#
A <- -10:10
A

# Comprueba los datos y encuentra entradas con valores mayores que 3
A > 3
# Devuelve sólo los valores mayores que 3
A[A>3]
B <- A[A>3]
B # ten en cuenta que A y B no tienen la misma dimensión
#
## el comando which() se usa para encontrar la posición de elementos que cumplen una condición
which(A == 0)
A[11] # el elemento 11 del vector A es igual a 0
 

########################################################
# Data frames
########################################################

# Los data frame se usan para almacenar datos en forma de tablas 
# (filas / columnas), como estamos habituados en Excel, Spss, etc.
# Los data frame pueden almacenar objetos/datos de distinto tipo: 
# numéricos, carácter, … En las matrices todos los elementos tenían 
# que ser enteros o numéricos.

# Para acceder a los elementos de un data frame utilizamos los símbolos
# $ o []. La forma de proceder es similar a la que se ha visto con 
# vectores o matrices.

# Leemos un archivo de datos
mroz <- read.csv("https://raw.githubusercontent.com/DabidH/datasets/master/mroz.csv")

# Visualizamos su contenido

class(mroz)
str(mroz)

length(mroz) # número de variables
dim(mroz) # número de observaciones (filas) y variables (columnas)
summary(mroz)  # resumen de todas las variables de la base de datos
head(mroz) # Primeras 5 observaciones
tail(mroz) # Ultimas 5 observaciones

# Usamos el $ para seleccionar una variable

mroz$hours

# Para añadir una variable, basta con utilizar el mismo simbolo $

mroz$id <- 1:753

# o podemos crear la nueva variable, por ejemplo la variable obs 
# (de observación) y después combinarla con nuestro data frame x.

obs <- 1:753
mroz <- cbind(obs,mroz)

# Selección de datos (subsetting)

mroz2 <- mroz[,1:3] # seleccionamos las tres primeras variables
mroz3 <- mroz[,c(1,2,4)] # seleccionamos las variables 1, 2 y 4

mroz4 <- mroz[1:6,]
mroz5 <- mroz[seq(1,nrow(mroz),5),]
mroz6 <- mroz[seq(1,nrow(mroz),5), c(1,2,4)] 

# En ocasiones estamos interesados en seleccionar los casos para los que cierta variable 
# toma determinado valor.

mroz7 <- mroz[mroz$age<=30, c(1,2)] 
mroz8 <- mroz[mroz$age>=30 & mroz$educ<=12, c(1,2)] 
mroz9 <- mroz[mroz$age>=30 | mroz$educ<=12, c(1,2)]
mroz10 <- mroz[mroz$age==30 , c(1,2)]

# También podemos utilizar la función subset()
mroz11 <- subset(mroz, mroz$age==30 & mroz$educ<=12, select=c(1,2))
mroz12 <- subset(mroz, mroz$age==30 & mroz$educ<=12, select=c(obs,inlf))

## Factores

# Los factores, que pueden ser ordenados o no ordenados, se utilizan para representar 
# variables de naturaleza categórica.

factor_nominal <- factor(rep(c("Ford","Seat","Renault"),10))
levels(factor_nominal)     # ordena los factores por orden alfabético
table(factor_nominal)

## Dos maneras de convertir una variable continua en ficticia
mroz$horas2 <- factor(mroz$horas>0) # como un factor
mroz$horas3 <- I(mroz$horas>0) # como una variable lógica


########################################################
# Creando variables
########################################################

mroz$salhora <- mroz$wage/mroz$hours

# Creamos una variable categorica con ifelse

mroz$edadmenor40 <- ifelse(mroz$age<40,1,0)
summary(mroz$edadmenor40)

mroz$edades <- 999
mroz$edades[mroz$age<40] <- 1
mroz$edades[40 <= mroz$age & mroz$age <50] <- 2
mroz$edades[50 <= mroz$age & mroz$age <60] <- 3
mroz$edades[mroz$age >=60] <- 4
table(mroz$edades)

mroz$kidslt6 <- factor(mroz$kidslt6)
is.na(mroz$kidslt6)
table(mroz$kidslt6)
levels(mroz$kidslt6)
levels(mroz$kidslt6) <- c("N", "U", "D", "T")
levels(mroz$kidslt6)[levels(mroz$kidslt6)=="N" |
                       levels(mroz$kidslt6)=="U"] <- "menos uno"

#Reordenando niveles de un factor
mar <- factor(mar, levels = c("Single","Married"))
levels(mar)


########################################################
# Detección de missing values
########################################################

# Missing values

v1 <- c(-10, 5, 3, 0, NA, 1, -1, NA, 4)
class(v1)
length(v1)
summary(v1) # Nos muestra el número de missing values 
 
# Es importante entender el tratamiento que hace R de los missing values

v1 >= 0 # (NA >= 0) is evaluated as NA
mean(v1)    
range(v1)   
# mean(), var(), sd() y otros estadísticos básicos no pueden cancularse a no ser
# que los missing values sean tratados adecuadamente

mean(v1, na.rm=TRUE) # Los NA son ignorados en el cálculo
is.na(v1) # Comprueba el objeto y devuelve TRUE cuando se cumple la condicion de ser NA

# Dos formas de deshacerse de los missing values:
v2 <- v1[!is.na(v1)] 
v3 <- c(na.omit(v1)) 

# Missing values en data frames (NAs)

# Por defecto, sólo celdas vacías y "NA" son interpretadas por R como NAs.
# (en conjuntos de datos lógicos, numéricos, enteros y complejos)
# Cualquier otro NA's (na.strings), como ".", "XXX" o "Missing value" debe ser tratado
# explícitamente en las funciones read.table / read.csv

?read.csv

miss <- is.na(mroz)

completos <- complete.cases(mroz)  # nos dirá si tenemos datos de todas las variables para cada individuo
summary(completos)

