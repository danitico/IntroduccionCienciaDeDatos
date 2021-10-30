if (!requireNamespace("tidyverse", quietly = T)) {
    install.packages("tidyverse")
}

if (!requireNamespace("moments", quietly = T)) {
    install.packages("moments")
}

if (!requireNamespace("Hmisc", quietly = T)) {
    install.packages("Hmisc")
}

if (!requireNamespace("gmodels", quietly = T)) {
    install.packages("gmodels")
}

if (!requireNamespace("ggmosaic", quietly = T)) {
    install.packages("ggmosaic")
}

if (!requireNamespace("GGally", quietly = T)) {
    install.packages("GGally")
}


source("utils.R")
library(tidyverse)
library(moments)
library(Hmisc)
library(ggplot2)
library(gmodels)
library(ggmosaic)
library(GGally)

set.seed(42)

tae <- read.keel("tae/tae.dat")

tae <- tae %>% mutate_if(is.character, as.integer)
tae <- tae %>% mutate(
    Native=as.factor(Native),
    Instructor=as.factor(Instructor),
    Course=as.factor(Course),
    Semester=as.factor(Semester)
)

#UNIVARIATE ANALYSIS

# Variable Native. Variable categórica
table(tae$Native)

# Diagrama de barras de Native
tae %>% ggplot(aes(x=Native)) + geom_bar()
# Diagrma de barras de Native según su clase
tae %>% ggplot(aes(x=Native, fill=Class)) + geom_bar()

# Variable Instructor. Variable categórica
table(tae$Instructor)

# Diagrama de barras de Instructor
tae %>% ggplot(aes(x=Instructor)) + geom_bar()
# Diagrma de barras de Instructor según su clase
tae %>% ggplot(aes(x=Instructor, fill=Class)) + geom_bar()

# Variable Course. Variable categórica
table(tae$Course)

# Diagrama de barras de Course
tae %>% ggplot(aes(x=Course)) + geom_bar()
# Diagrma de barras de Course según su clase
tae %>% ggplot(aes(x=Course, fill=Class)) + geom_bar()

# Variable Semester. Variable categórica
table(tae$Semester)

# Diagrama de barras de Semester
tae %>% ggplot(aes(x=Semester)) + geom_bar()
# Diagrma de barras de Semester según su clase
tae %>% ggplot(aes(x=Semester, fill=Class)) + geom_bar()

# Variable Size. Variable numérica discreta

## Se obtiene el min, mediana, media, maximo y los cuartiles, además el IQR
summary(tae$Size)
IQR(tae$Size)

## Se calcula la varianza y desviación tipica de Size
var(tae$Size)
sd(tae$Size)

## Se calcula la moda
getMode(tae$Size)

## Se renderiza un boxplot de la variable Size
tae %>% ggplot(aes(y=Size)) + geom_boxplot()

## Boxplot de size por clase
tae %>% ggplot(aes(y=Size, fill=Class)) + geom_boxplot() + facet_wrap(~ Class)

## Comprobar asimetria y la forma del pico de la distribucíon

skewness(tae$Size) # asimetrica por la derecha. Se puede reducir haciendo una raiz cuadrada o logaritmo de los datos 
kurtosis(tae$Size) # El pico de la distribución es más alto que el de una distribución normal

## Con el grafico se puede ver que es asimetrica por la derecha,
## que el pico de la distribución es más alto que el pico de una distribución normal
tae %>% ggplot(aes(x=Size)) + geom_histogram(bins = 11)

## Histograma de Size por clase
tae %>% ggplot(aes(x=Size, fill=Class)) + geom_histogram(bins = 11) + facet_wrap(~ Class)


# Test de shafiro para ver si la variable Size sigue una distribución normal
# Como el p-value que se obtiene es menor que 0.05, se puede afirmar que la variable Size no sigue una distribución normal
shapiro.result <- shapiro.test(tae$Size)
shapiro.result


# Variable a predecir. Estamos ante un dataset balanceado
summary(tae$Class)

describe(tae)


# Bivariate analysis

# Native - Instructor

# Al haber tantas categorias en Instructor, es imposible de leer de una manera adecuada el crosstable
# El test de Pearson X^2 nos da un p-value de 0.0164026, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipótesis nula (las dos variables son independientes), y se puede afirmar que las variables
# native e instructor son dependientes.

CrossTable(tae$Native, tae$Instructor, prop.t = T, prop.r = T, prop.c = T, chisq = T)
tae %>% ggplot() + geom_mosaic(aes(x = product(Class, Instructor, Native), fill=Class))

# Native - Course

# Al haber tantas categorias en course, es imposible de leer de una manera adecuada el crosstable
# El test de Pearson X^2 nos da un p-value de 0.3331524, el cual es mayor que 0.05, por lo que se acepta
# la hipótesis nula y se puede afirmar que las dos variables son independientes

CrossTable(tae$Native, tae$Course, prop.t = T, prop.r = T, prop.c = T, chisq = T)
tae %>% ggplot() + geom_mosaic(aes(x = product(Class,Course, Native), fill=Class))

# Native - Semester
# El test de Pearson X^2 nos da un p-value de 0.01890861, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipótesis nula (las dos variables son independientes), y se puede afirmar que las variables
# native y semester son dependientes (En verano hay más gente extranjera porque blablabla... )

CrossTable(tae$Native, tae$Semester, prop.t = T, prop.r = T, prop.c = T, chisq = T)
tae %>% ggplot() + geom_mosaic(aes(x = product(Class, Native, Semester), fill=Class))

# Instructor - Course
# El test de Pearson X^2 nos da un p-value de 1.554445e-107, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipótesis nula (las dos variables son independientes), y se puede afirmar que las variables
# Instructor y Course son dependientes

CrossTable(tae$Instructor, tae$Course, prop.t = T, prop.r = T, prop.c = T, chisq = T)

# El gráfico no es legible debido a que hay muchas categorías
tae %>% ggplot() + geom_mosaic(aes(x = product(Class, Instructor, Course), fill=Class))

# Instructor - semester
# El test de Pearson X^2 nos da un p-value de 0.0003923984, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipótesis nula (las dos variables son independientes), y se puede afirmar que las variables
# Instructor y semester son dependientes (algunos instructores solo dan en verano)

CrossTable(tae$Instructor, tae$Semester, prop.t = T, prop.r = T, prop.c = T, chisq = T)
tae %>% ggplot() + geom_mosaic(aes(x = product(Class, Instructor, Semester), fill=Class))


# Course - semester
# El test de Pearson X^2 nos da un p-value de 0.007008467, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipótesis nula (las dos variables son independientes), y se puede afirmar que las variables
# Course y semester son dependientes (algunos tipos de cursos se dan en verano)

CrossTable(tae$Course, tae$Semester, prop.t = T, prop.r = T, prop.c = T, chisq = T)
tae %>% ggplot() + geom_mosaic(aes(x = product(Class, Instructor, Semester), fill=Class))

# Multivariate analysis

ggpairs(data=tae, columns = 1:5, cardinality_threshold = NULL)
