if (!requireNamespace("tidyverse", quietly = T)) {
    install.packages("tidyverse")
}

if (!requireNamespace("moments", quietly = T)) {
    install.packages("moments")
}

if (!requireNamespace("Hmisc", quietly = T)) {
    install.packages("Hmisc")
}

if (!requireNamespace("ggmosaic", quietly = T)) {
    install.packages("ggmosaic")
}

if (!requireNamespace("GGally", quietly = T)) {
    install.packages("GGally")
}

set.seed(42)

source("utils.R")
library(tidyverse)
library(moments)
library(Hmisc)
library(ggplot2)
library(ggmosaic)
library(GGally)
library(gmodels)
library(vcd)

tae <- read.keel("tae/tae.dat")

tae <- tae %>% mutate_if(is.character, as.integer)
tae <- tae %>% mutate(
    Native=factor(Native, levels = c(1, 2), labels = c("yes", "no")),
    Instructor=as.factor(Instructor),
    Course=as.factor(Course),
    Semester=factor(Semester, levels = c(1, 2), labels = c("summer", "regular")),
    Class=factor(Class, levels = c(1, 2, 3), labels = c("low", "medium", "high"))
)

#UNIVARIATE ANALYSIS

# Variable Native. Variable categórica
table(tae$Native)

# Diagrama de barras de Native
tae %>% ggplot(
    aes(y=Native)
) + geom_bar(
    show.legend = F
)  + stat_count(
    geom = "text",
    colour = "white",
    size = 3.5,
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)
) + labs(title = "Frecuencia de la variable Native", x = "Número de casos")

# Diagrma de barras de Native según su clase
tae %>% ggplot(aes(y=Native, fill=Class)) + geom_bar() + stat_count(
    geom = "text",
    colour = "white",
    size = 3.5,
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)
) + labs(title = "Frecuencia de la variable Native en función de Class", x = "Número de casos")

# Diagrma de barras de Native según el semestre
tae %>% ggplot(aes(y=Native, fill=Semester)) + geom_bar() + stat_count(
    geom = "text",
    colour = "white",
    size = 3.5,
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)
) + labs(title = "Frecuencia de la variable Native en función de Semester", x = "Número de casos")

# Proporciones de altas puntuaciones dependiendo de si se es nativo o no
tae %>% group_by(Native) %>% summarise(
    proportion=(length(Class[Class %in% c("medium", "high")])/n()) * 100
)

# Variable Instructor. Variable categórica
table(tae$Instructor)

# Diagrama de barras de Instructor
tae %>% ggplot(aes(x=Instructor)) + geom_bar() + stat_count(
    geom = "text",
    colour = "white",
    size = 3.5,
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)
) + labs(title = "Frecuencia de la variable Instructor ", y = "Número de casos")

# Diagrma de barras de Instructor según su clase
tae %>% ggplot(aes(x=Instructor, fill=Class)) + geom_bar() + stat_count(
    geom = "text",
    colour = "white",
    size = 3.5,
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)
) + labs(title = "Frecuencia de la variable Instructor en función de Class", y = "Número de casos")

dynamicNamesFacetWrap <- function(var, name) {
    varLevels <- levels(var)
    labels <- paste0(name, " ", varLevels)
    names(labels) <- varLevels
    
    labels
}

# Diagrama de barras que muestra el rendimiento de los profesores que dan mas de una asignatura y su rendimiento
# Instructors with more than one course
x <- tae %>% group_by(Instructor) %>% distinct(Course) %>% filter(n() > 1) %>% distinct(Instructor)
tae %>% filter(
    Instructor %in% x$Instructor & Semester != "summer" # Quitamos summer ya ninguno tiene más de una asignatura en ese semestre
) %>% ggplot(
    aes(x=Course, fill=Class)
) + geom_bar() + stat_count(
    geom = "text",
    colour = "white",
    size = 3.5,
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)
) + labs(
    title = "Rendimiento de las clases de los profesores que imparten más de una asignatura", y = "Número de casos"
) + facet_wrap(
    ~ Instructor + Semester,
    scales = "free",
    labeller = labeller(
        Instructor = dynamicNamesFacetWrap(x$Instructor, "Instructor: ")
    )
)



# Variable Course. Variable categórica
table(tae$Course)

# Diagrama de barras de Course
tae %>% ggplot(aes(x=Course)) + geom_bar() + stat_count(
    geom = "text",
    colour = "white",
    size = 3.5,
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)
) + labs(title = "Frecuencia de la variable Course ", y = "Número de casos")

# Diagrma de barras de Course según su clase
tae %>% ggplot(aes(x=Course, fill=Class)) + geom_bar() + stat_count(
    geom = "text",
    colour = "white",
    size = 3.5,
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)
) + labs(title = "Frecuencia de la variable Course en función de Class", y = "Número de casos")


# Variable Semester. Variable categórica
table(tae$Semester)

# Diagrama de barras de Semester
tae %>% ggplot(aes(x=Semester)) + geom_bar() + stat_count(
    geom = "text",
    colour = "white",
    size = 3.5,
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)
) + labs(title = "Frecuencia de la variable Semester ", y = "Número de casos")

# Diagrma de barras de Semester según su clase
tae %>% ggplot(aes(x=Semester, fill=Class)) + geom_bar() + stat_count(
    geom = "text",
    colour = "white",
    size = 3.5,
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)
) + labs(title = "Frecuencia de la variable Semester en función de Class", y = "Número de casos")

# Proporciones de altas puntuaciones dependiendo del semestre
tae %>% group_by(Semester) %>% summarise(
    proportion=(length(Class[Class %in% c("medium", "high")])/n()) * 100
)

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
tae %>% ggplot(aes(y=Size)) + geom_boxplot() + labs(title = "Boxplot de la variable Size")

## Boxplot de size por clase
tae %>% ggplot(aes(y=Size, fill=Class)) + geom_boxplot() + facet_wrap(~ Class) + labs(title = "Boxplot de la variable Size en función de Class")

## Boxplot de size en según el semestre

tae %>% ggplot(aes(y=Size, fill=Semester)) + geom_boxplot() + facet_wrap(~ Semester) + labs(title = "Boxplot de la variable Size en función de Semester")


## Comprobar asimetria y la forma del pico de la distribucíon

skewness(tae$Size) # asimetrica por la derecha. Se puede reducir haciendo una raiz cuadrada o logaritmo de los datos 
kurtosis(tae$Size) # El pico de la distribución es más alto que el de una distribución normal

## Con el grafico se puede ver que es asimetrica por la derecha,
## que el pico de la distribución es más alto que el pico de una distribución normal
tae %>% ggplot(
    aes(x=Size)
) + geom_histogram(
    bins = 11
) + labs(
    title = "Histograma de la variable Size",
    y = "Número de casos"
)

## Histograma de Size por clase
tae %>% ggplot(aes(x=Size, fill=Class)) + geom_histogram(bins = 11) + facet_wrap(~ Class) + labs(
    title = "Histograma de la variable Size en función de Class",
    y = "Número de casos"
)

# Test de shafiro para ver si la variable Size sigue una distribución normal
# Como el p-value que se obtiene es menor que 0.05, se puede afirmar que la variable Size no sigue una distribución normal
shapiro.result <- shapiro.test(tae$Size)
shapiro.result


# Variable a predecir. Estamos ante un dataset balanceado
summary(tae$Class)

describe(tae)

tae %>% ggplot(
    aes(y=Class)
) + geom_bar(
    show.legend = F
)  + stat_count(
    geom = "text",
    colour = "white",
    size = 3.5,
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)
) + labs(title = "Distribución de la etiqueta de clase", x = "Número de observaciones")


# Bivariate analysis

# No se utiliza el test de pearson X^2 porque esta asume que las frecuencias por celda tiene que ser 10 o mayor, por lo que esto no se cumple y dará calculos incorrectos
# Para ello se va a utilizar fisher, que puede asumir frecuencias más pequeñas por celda

# Native - Instructor

# El test de fisher nos da un p-value de 0.02349, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipótesis nula (las dos variables son independientes), y se puede afirmar que las variables
# native e instructor son dependientes.

fisher.test(tae$Native, tae$Instructor, simulate.p.value = T)
mosaic(~ Instructor + Native, data=tae)
tae %>% ggplot() + geom_mosaic(aes(x = product(Class, Instructor, Native), fill=Class))

# Native - Course

# Al haber tantas categorias en course, es imposible de leer de una manera adecuada el crosstable
# El test de fisher nos da un p-value de 0.4093, el cual es mayor que 0.05, por lo que se acepta
# la hipótesis nula y se puede afirmar que las dos variables son independientes

fisher.test(tae$Native, tae$Course, simulate.p.value = T)
mosaic(~ Course + Native, data=tae)
tae %>% ggplot() + geom_mosaic(aes(x = product(Class,Course, Native), fill=Class))

# Native - Semester
# El test de fisher nos da un p-value de 0.01778, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipótesis nula (las dos variables son independientes), y se puede afirmar que las variables
# native y semester son dependientes (En verano hay más gente extranjera porque blablabla... )

fisher.test(tae$Native, tae$Semester)
mosaic(~ Semester + Native, data=tae)
tae %>% ggplot() + geom_mosaic(aes(x = product(Class, Native, Semester), fill=Class))

# Instructor - Course
# El test de fisher nos da un p-value de 0.0004998, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipótesis nula (las dos variables son independientes), y se puede afirmar que las variables
# Instructor y Course son dependientes

fisher.test(tae$Instructor, tae$Course, simulate.p.value = T)
# El gráfico no es legible debido a que hay muchas categorías
tae %>% ggplot() + geom_mosaic(aes(x = product(Class, Instructor, Course), fill=Class))

# Instructor - semester
# El test de fisher nos da un p-value de 0.001999, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipótesis nula (las dos variables son independientes), y se puede afirmar que las variables
# Instructor y semester son dependientes (algunos instructores solo dan en verano)

fisher.test(tae$Instructor, tae$Semester, simulate.p.value = T)
mosaic(~ Instructor + Semester, data=tae)
tae %>% ggplot() + geom_mosaic(aes(x = product(Class, Instructor, Semester), fill=Class))


# Course - semester
# El test de fisher nos da un p-value de 0.01949, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipótesis nula (las dos variables son independientes), y se puede afirmar que las variables
# Course y semester son dependientes (algunos tipos de cursos se dan en verano)

fisher.test(tae$Course, tae$Semester, simulate.p.value = T)
mosaic(~ Course + Semester, data=tae)
tae %>% ggplot() + geom_mosaic(aes(x = product(Class, Instructor, Semester), fill=Class))


# Size - Native

tae %>% ggplot(
    aes(y=Size, fill=Native)
) + geom_boxplot() + facet_wrap(
    ~ Native
) + labs(
    title = "Boxplot de size en función de Native",
)

# No hay diferencias significativas en el tamaño de la clase dependiendo de sí el profesor es nativo o no
kruskal.result <- kruskal.test(Size ~ Native, data=tae)
kruskal.result

# Size - Instructor

tae %>% ggplot(
    aes(y=Size)
) + geom_boxplot() + facet_wrap(
    ~ Instructor
) + labs(
    title = "Boxplot de size en función de Instructor",
)

# Hay diferencias significativas en el tamaño de la clase dependiendo del profesor ayudante 
kruskal.result <- kruskal.test(Size ~ Instructor, data=tae)
kruskal.result

# Size - Course
tae %>% ggplot(
    aes(y=Size)
) + geom_boxplot() + facet_wrap(
    ~ Course
) + labs(
    title = "Boxplot de size en función de Course",
)

# Hay diferencias significativas en el tamaño de la clase dependiendo de la asignatura
kruskal.result <- kruskal.test(Size ~ Course, data=tae)
kruskal.result

# Size - Semester
# El gráfico se ha hecho antes
tae %>% ggplot(
    aes(y=Size)
) + geom_boxplot() + facet_wrap(
    ~ Semester
) + labs(
    title = "Boxplot de size en función de Semester",
)

# Hay diferencias significativas en el tamaño de la clase dependiendo del semestre
kruskal.result <- kruskal.test(Size ~ Semester, data=tae)
kruskal.result


# Size - Class
# El gráfico se ha hecho antes
tae %>% ggplot(
    aes(y=Size)
) + geom_boxplot() + facet_wrap(
    ~ Class
) + labs(
    title = "Boxplot de size en función de Class",
)

# No hay diferencias significativas en el tamaño de la clase dependiendo de la evaluación del profesor ayudante
kruskal.result <- kruskal.test(Size ~ Class, data=tae)
kruskal.result

# class - native
# El test de fisher nos da un p-value de 0.00326, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipótesis nula (las dos variables son independientes), y se puede afirmar que las variables
# Class y Native son dependientes
fisher.test(tae$Class, tae$Native)

# class - Instructor
# El test de fisher.test nos da un p-value de 0.01199, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipótesis nula (las dos variables son independientes), y se puede afirmar que las variables
# Class y Instructor son dependientes
fisher.test(tae$Class, tae$Instructor, simulate.p.value = T)

# class - Course
# El test de fisher.test nos da un p-value de 0.0004998, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipótesis nula (las dos variables son independientes), y se puede afirmar que las variables
# Class y Course son dependientes
fisher.test(tae$Class, tae$Course, simulate.p.value = T)

# class - Semester
# El test de fisher.test nos da un p-value de 0.002026, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipótesis nula (las dos variables son independientes), y se puede afirmar que las variables
# Class y Semester son dependientes
fisher.test(tae$Class, tae$Semester)


# Multivariate analysis

ggpairs(data=tae, cardinality_threshold = NULL)
