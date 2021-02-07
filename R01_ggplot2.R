#####################################################################################################
##################           Gráficos en R                                         ##################
##################           David Hoyos                                           ##################
##################           UPV/EHU, 2020                                         ##################
#####################################################################################################

########################################################
# ¿Por qué ggplot2? 
########################################################

# Gráficos muy profesionales
# Una forma sistemática de preparar gráficos
# Gráficos sofisticados en pocas líneas - el código no necesita cambiar aunque cambien los datos
# Un enfoque unificado de representación gráfica orientado a la estadística
# GG significa Grammar of Graphics

#  Algunas webs sobre ggplot2:
#
# http://docs.ggplot2.org/current/
# http://www.r-bloggers.com/basic-introduction-to-ggplot2/
# http://onepager.togaware.com/GGPlot2O.pdf


########################################################
# Lectura de datos: salarios en Euskadi 2014
########################################################

rm(list=ls())
library(ggplot2) # install.packages("ggplot2")
library(ggthemes)
library(ggpubr)
library(gridExtra)

EES <- read.csv("https://raw.githubusercontent.com/DabidH/datasets/master/EES2014EUSR.csv")

EES <- EES[EES$SALHORA<75,]

head(EES)
summary(EES)
str(EES)

attach(EES)

########################################################
# Gráfico de dispersión, SALHORA ~ ANTIGUEDAD
########################################################

# Utilizando el comando básico de R:
plot(SALHORA~ANTIGUEDAD) # {graphics} package

# utlizando ggplot2:
ggplot(EES, aes(x=ANTIGUEDAD,y=SALHORA))+
  # Primero, creamos un objeto de ggplot y le insertamos los datos,
  # además de algunos parámetros estéticos básicos (aes=aesthetics)
  geom_point() 
  # este argumento define el tipo de gráfico: gráfico de dispersión
 
########################################################
# Añadir colores en función de un factor
########################################################

# Añadimos colores en función del "SEXO" ("Hombre", "Mujer)
ggplot(EES, aes(x=ANTIGUEDAD,y=SALHORA, colour=SEXO))+ # corrige los parámetros estéticos
  geom_point()
# Alternativamente:
ggplot(EES)+ # aquí solamente definimos el dataset
  geom_point(aes(x=ANTIGUEDAD, y=SALHORA, colour=SEXO)) # aes() se define para geom_points
# .. puede resultar útil si aparecen en el gráfico diferentes tipos de líneas y puntos

########################################################
# Añadir títulos a los gráficos
########################################################

# Cada característica del gráfico es una "capa" que se expresa en líneas adicionales de código

ggplot(EES, aes(x=ANTIGUEDAD,y=SALHORA))+ # es importante añadir al sintaxis `+` 
  geom_point(aes(colour=SEXO))+
  ggtitle("Relación entre salarios y antigüedad") + # nueva "capa"
  theme(plot.title = element_text(hjust = 0.5)) # centrado

# También podemos cambiar las etiquetas de los ejes

ggplot(EES, aes(x=ANTIGUEDAD,y=SALHORA))+ # es importante añadir al sintaxis `+` 
  geom_point(aes(colour=SEXO))+
  +labs(title="Relación entre salarios y antigüedad",
        x ="Años", y = "Salario por hora") +
  theme(plot.title = element_text(hjust = 0.5)) # centrado


########################################################
# Añadir facetas (facets)
########################################################

# Repetimos el mismo gráfico para cada tipo de corte
ggplot(EES, aes(x=ANTIGUEDAD,y=SALHORA))+ 
  geom_point(aes(colour=SEXO))+
  ggtitle("Relación entre salarios y antigüedad") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~PROV) # esta nueva capa define las facetas

# En caso de querer todas las facetas en una sola columna:
ggplot(EES, aes(x=ANTIGUEDAD,y=SALHORA)) + 
  geom_point(aes(colour=SEXO))+
  ggtitle("Relación entre salarios y antigüedad")+
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~PROV, ncol=1)

# Siempre que queramos saber más sobre algún argumento:
?facet_wrap

# Así podremos, por ejemplo, saber hacer el gráfico anterior en 3 filas

ggplot(EES, aes(x=ANTIGUEDAD,y=SALHORA))+ 
  geom_point(aes(colour=SEXO))+
  ggtitle("Relación entre salarios y antigüedad")+
  facet_wrap(~PROV, nrow=3) 

# O en 2 filas y 3 columnas

ggplot(EES, aes(x=ANTIGUEDAD,y=SALHORA))+ 
  geom_point(aes(colour=SEXO))+
  ggtitle("Relación entre salarios y antigüedad")+
  facet_wrap(~PROV, ncol=2, nrow=2) 

########################################################
# Añadir una línea de tendencia
########################################################

ggplot(EES, aes(x=ANTIGUEDAD,y=SALHORA))+ 
  geom_point(aes(colour=SEXO))+
  ggtitle("Relación entre salarios y antigüedad")+
  facet_wrap(~PROV, ncol=1) +
  geom_smooth(method= "lm") # una nueva capa define la línea de tendencia

?geom_smooth

# Vemos que, por ejemplo, por defecto incluye intervalos de confianza del 95%, pero podemos ajustarlos:


ggplot(EES, aes(x=ANTIGUEDAD,y=SALHORA))+ 
  geom_point(aes(colour=SEXO))+
  ggtitle("Relación entre salarios y antigüedad")+
  facet_wrap(~PROV, ncol=1) +
  geom_smooth(method= "lm", se=T, level = 0.99) # tendencia con un nivel de confianza del 99%

# Con una sola regresión
ggplot(EES, aes(x=ANTIGUEDAD,y=SALHORA))+ 
  geom_point(aes(colour=SEXO))+
  ggtitle("Relación entre salarios y antigüedad")+
  geom_smooth(method= "lm") # una nueva capa define la línea de tendencia



########################################################
# Otros ejemplos
########################################################

# Histogramas: 
ggplot(EES, aes(x=SALHORA)) + 
  geom_histogram()
#

mean(SALNETO)
median (SALNETO)

# Histograma con media y mediana:
ggplot(EES, aes(x=SALNETO)) + 
  geom_histogram(fill="gold3") +
  geom_vline(aes(xintercept=mean(SALNETO, show.legend = T)),   
             color="grey50", linetype="dashed", size=1) +
  annotate(geom = "text", x=2600, y = 350, label="Media = 1.744", color="grey50") +
  geom_vline(aes(xintercept=median(SALNETO, show.legend = T)),   
             color="grey20", linetype="dashed", size=1) +
  annotate(geom = "text", x=650, y = 350, label="Mediana = 1.653", color="grey20") +
  theme_economist() +
  scale_color_economist(name = "Seasons:") 
 
?geom_vline


# Histograma con distinto grosor de columnas:
ggplot(EES, aes(x=SALHORA)) + 
  geom_histogram(bins = 10) 

# Muestra la composición de cada columna:
ggplot(EES) +
  geom_histogram(aes(x=SALHORA, fill=SEXO), bins = 10)

# Los gráficos de densidad pueden describir mejor las variables continuas:
ggplot(EES, aes(x=SALHORA, fill=SEXO)) +
  geom_density()

# Podemos incluir trnasparencias para distinguir mejor las distribuciones:
ggplot(EES, aes(x=SALHORA,fill=SEXO)) +
  geom_density(alpha=0.5)

# O utilizar líneas de colores en lugar de rellenarlas:
ggplot(EES, aes(x=SALHORA,colour=SEXO)) +
  geom_density()

# Box plots
ggplot(EES, aes(x=SEXO, y=SALHORA, fill=SEXO)) + 
  geom_boxplot() +
  theme_minimal() 

?theme_minimal()

# Otros temas

ggplot(EES, aes(x=SEXO, y=SALHORA, fill=SEXO)) + 
  geom_boxplot() +
  theme_dark()


#############################################################################
# ggarrange: varios gráficos en uno sólo
#############################################################################

figure <- ggarrange(sp, bp + font("x.text", size = 10),
                    ncol = 1, nrow = 2)
annotate_figure(figure,
                top = text_grob("Visualizing mpg", color = "red", face = "bold", size = 14),
                bottom = text_grob("Data source: \n mtcars data set", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
                right = "I'm done, thanks :-)!",
                fig.lab = "Figure 1", fig.lab.face = "bold" )


#############################################################################
# Opciones de theme sobre leyendas en ggplot2 
#############################################################################

theme(
  # Legend title and text labels
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # Title font color size and face
  legend.title = element_text(color, size, face),
  # Title alignment. Number from 0 (left) to 1 (right)
  legend.title.align = NULL,             
  # Text label font color size and face
  legend.text = element_text(color, size, face), 
  # Text label alignment. Number from 0 (left) to 1 (right)
  legend.text.align = NULL,
  
  # Legend position, margin and background
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # Legend position: right, left, bottom, top, none
  legend.position = "right", 
  # Margin around each legend
  legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
  # Legend background
  legend.background = element_rect(fill, color, size, linetype),
  
  # Legend direction and justification
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # Layout of items in legends ("horizontal" or "vertical")
  legend.direction = NULL, 
  # Positioning legend inside or outside plot 
  # ("center" or two-element numeric vector) 
  legend.justification = "center", 
  
  # Background underneath legend keys
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  legend.key = element_rect(fill, color),  # Key background
  legend.key.size = unit(1.2, "lines"),    # key size (unit)
  legend.key.height = NULL,                # key height (unit)
  legend.key.width = NULL,                 # key width (unit)
  
  # Spacing between legends. 
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  legend.spacing = unit(0.4, "cm"), 
  legend.spacing.x = NULL,                 # Horizontal spacing
  legend.spacing.y = NULL,                 # Vertical spacing
  
  # Legend box
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # Arrangement of multiple legends ("horizontal" or "vertical")
  legend.box = NULL, 
  # Margins around the full legend area
  legend.box.margin = margin(0, 0, 0, 0, "cm"), 
  # Background of legend area: element_rect()
  legend.box.background = element_blank(), 
  # The spacing between the plotting area and the legend box
  legend.box.spacing = unit(0.4, "cm")
)


# Más en:
# https://www.datacamp.com/
# https://cedricscherer.netlify.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/
  

