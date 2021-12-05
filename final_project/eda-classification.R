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

# Variable Native. Variable categorica
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
) + labs(title = "Frecuencia de la variable Native", x = "Numero de casos")

# Diagrma de barras de Native segun su clase
tae %>% ggplot(aes(y=Native, fill=Class)) + geom_bar() + stat_count(
    geom = "text",
    colour = "white",
    size = 3.5,
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)
) + labs(title = "Frecuencia de la variable Native en funcion de Class", x = "Numero de casos")

# Diagrma de barras de Native segun el semestre
tae %>% ggplot(aes(y=Native, fill=Semester)) + geom_bar() + stat_count(
    geom = "text",
    colour = "white",
    size = 3.5,
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)
) + labs(title = "Frecuencia de la variable Native en funcion de Semester", x = "Numero de casos")

# Proporciones de altas puntuaciones dependiendo de si se es nativo o no
tae %>% group_by(Native) %>% summarise(
    proportion=(length(Class[Class %in% c("medium", "high")])/n()) * 100
)

# Variable Instructor. Variable categorica
table(tae$Instructor)

# Diagrama de barras de Instructor
tae %>% ggplot(aes(x=Instructor)) + geom_bar() + stat_count(
    geom = "text",
    colour = "white",
    size = 3.5,
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)
) + labs(title = "Frecuencia de la variable Instructor ", y = "Numero de casos")

# Diagrma de barras de Instructor segun su clase
tae %>% ggplot(aes(x=Instructor, fill=Class)) + geom_bar() + stat_count(
    geom = "text",
    colour = "white",
    size = 3.5,
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)
) + labs(title = "Frecuencia de la variable Instructor en funcion de Class", y = "Numero de casos")

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
    Instructor %in% x$Instructor & Semester != "summer" # Quitamos summer ya ninguno tiene mas de una asignatura en ese semestre
) %>% ggplot(
    aes(x=Course, fill=Class)
) + geom_bar() + stat_count(
    geom = "text",
    colour = "white",
    size = 3.5,
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)
) + labs(
    title = "Rendimiento de las clases de los profesores que imparten mas de una asignatura", y = "Numero de casos"
) + facet_wrap(
    ~ Instructor + Semester,
    scales = "free",
    labeller = labeller(
        Instructor = dynamicNamesFacetWrap(x$Instructor, "Instructor: ")
    )
)



# Variable Course. Variable categorica
table(tae$Course)

# Diagrama de barras de Course
tae %>% ggplot(aes(x=Course)) + geom_bar() + stat_count(
    geom = "text",
    colour = "white",
    size = 3.5,
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)
) + labs(title = "Frecuencia de la variable Course ", y = "Numero de casos")

# Diagrma de barras de Course segun su clase
tae %>% ggplot(aes(x=Course, fill=Class)) + geom_bar() + stat_count(
    geom = "text",
    colour = "white",
    size = 3.5,
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)
) + labs(title = "Frecuencia de la variable Course en funcion de Class", y = "Numero de casos")


# Variable Semester. Variable categorica
table(tae$Semester)

# Diagrama de barras de Semester
tae %>% ggplot(aes(x=Semester)) + geom_bar() + stat_count(
    geom = "text",
    colour = "white",
    size = 3.5,
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)
) + labs(title = "Frecuencia de la variable Semester ", y = "Numero de casos")

# Diagrma de barras de Semester segun su clase
tae %>% ggplot(aes(x=Semester, fill=Class)) + geom_bar() + stat_count(
    geom = "text",
    colour = "white",
    size = 3.5,
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)
) + labs(title = "Frecuencia de la variable Semester en funcion de Class", y = "Numero de casos")

# Proporciones de altas puntuaciones dependiendo del semestre
tae %>% group_by(Semester) %>% summarise(
    proportion=(length(Class[Class %in% c("medium", "high")])/n()) * 100
)

# Variable Size. Variable numerica discreta

## Se obtiene el min, mediana, media, maximo y los cuartiles, ademas el IQR
summary(tae$Size)
IQR(tae$Size)

## Se calcula la varianza y desviacion tipica de Size
var(tae$Size)
sd(tae$Size)

## Se calcula la moda
getMode(tae$Size)

## Se renderiza un boxplot de la variable Size
tae %>% ggplot(aes(y=Size)) + geom_boxplot() + labs(title = "Boxplot de la variable Size")

## Boxplot de size por clase
tae %>% ggplot(aes(y=Size, fill=Class)) + geom_boxplot() + facet_wrap(~ Class) + labs(title = "Boxplot de la variable Size en funcion de Class")

## Boxplot de size en segun el semestre

tae %>% ggplot(aes(y=Size, fill=Semester)) + geom_boxplot() + facet_wrap(~ Semester) + labs(title = "Boxplot de la variable Size en funcion de Semester")


## Comprobar asimetria y la forma del pico de la distribucion

skewness(tae$Size) # asimetrica por la derecha. Se puede reducir haciendo una raiz cuadrada o logaritmo de los datos 
kurtosis(tae$Size) # El pico de la distribucion es mas alto que el de una distribucion normal

## Con el grafico se puede ver que es asimetrica por la derecha,
## que el pico de la distribucion es mas alto que el pico de una distribucion normal
tae %>% ggplot(
    aes(x=Size)
) + geom_histogram(
    bins = 11
) + labs(
    title = "Histograma de la variable Size",
    y = "Numero de casos"
)

## Histograma de Size por clase
tae %>% ggplot(aes(x=Size, fill=Class)) + geom_histogram(bins = 11) + facet_wrap(~ Class) + labs(
    title = "Histograma de la variable Size en funcion de Class",
    y = "Numero de casos"
)

# Test de shafiro para ver si la variable Size sigue una distribucion normal
# Como el p-value que se obtiene es menor que 0.05, se puede afirmar que la variable Size no sigue una distribucion normal
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
) + labs(title = "Distribucion de la etiqueta de clase", x = "Numero de observaciones")


# Bivariate analysis

# No se utiliza el test de pearson X^2 porque esta asume que las frecuencias por celda tiene que ser 10 o mayor, por lo que esto no se cumple y dara calculos incorrectos
# Para ello se va a utilizar fisher, que puede asumir frecuencias mas chicas por celda

# Native - Instructor

# El test de fisher nos da un p-value de 0.02349, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipotesis nula (las dos variables son independientes), y se puede afirmar que las variables
# native e instructor son dependientes.

fisher.test(tae$Native, tae$Instructor, simulate.p.value = T)
mosaic(~ Instructor + Native, data=tae)
tae %>% ggplot() + geom_mosaic(aes(x = product(Class, Instructor, Native), fill=Class))

# Native - Course

# Al haber tantas categorias en course, es imposible de leer de una manera adecuada el crosstable
# El test de fisher nos da un p-value de 0.4093, el cual es mayor que 0.05, por lo que se acepta
# la hipotesis nula y se puede afirmar que las dos variables son independientes

fisher.test(tae$Native, tae$Course, simulate.p.value = T)
mosaic(~ Course + Native, data=tae)
tae %>% ggplot() + geom_mosaic(aes(x = product(Class,Course, Native), fill=Class))

# Native - Semester
# El test de fisher nos da un p-value de 0.01778, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipotesis nula (las dos variables son independientes), y se puede afirmar que las variables
# native y semester son dependientes (En verano hay mas gente extranjera porque blablabla... )

fisher.test(tae$Native, tae$Semester)
mosaic(~ Semester + Native, data=tae)
tae %>% ggplot() + geom_mosaic(aes(x = product(Class, Native, Semester), fill=Class))

# Instructor - Course
# El test de fisher nos da un p-value de 0.0004998, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipotesis nula (las dos variables son independientes), y se puede afirmar que las variables
# Instructor y Course son dependientes

fisher.test(tae$Instructor, tae$Course, simulate.p.value = T)
# El grafico no es legible debido a que hay muchas categorias
tae %>% ggplot() + geom_mosaic(aes(x = product(Class, Instructor, Course), fill=Class))

# Instructor - semester
# El test de fisher nos da un p-value de 0.001999, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipotesis nula (las dos variables son independientes), y se puede afirmar que las variables
# Instructor y semester son dependientes (algunos instructores solo dan en verano)

fisher.test(tae$Instructor, tae$Semester, simulate.p.value = T)
mosaic(~ Instructor + Semester, data=tae)
tae %>% ggplot() + geom_mosaic(aes(x = product(Class, Instructor, Semester), fill=Class))


# Course - semester
# El test de fisher nos da un p-value de 0.01949, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipotesis nula (las dos variables son independientes), y se puede afirmar que las variables
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
    title = "Boxplot de size en funcion de Native",
)

# No hay diferencias significativas en el tamano de la clase dependiendo de si el profesor es nativo o no
kruskal.result <- kruskal.test(Size ~ Native, data=tae)
kruskal.result

# Size - Instructor

tae %>% ggplot(
    aes(y=Size)
) + geom_boxplot() + facet_wrap(
    ~ Instructor
) + labs(
    title = "Boxplot de size en funcion de Instructor",
)

# Hay diferencias significativas en el tamano de la clase dependiendo del profesor ayudante 
kruskal.result <- kruskal.test(Size ~ Instructor, data=tae)
kruskal.result

# Size - Course
tae %>% ggplot(
    aes(y=Size)
) + geom_boxplot() + facet_wrap(
    ~ Course
) + labs(
    title = "Boxplot de size en funcion de Course",
)

# Hay diferencias significativas en el tamano de la clase dependiendo de la asignatura
kruskal.result <- kruskal.test(Size ~ Course, data=tae)
kruskal.result

# Size - Semester
# El grafico se ha hecho antes
tae %>% ggplot(
    aes(y=Size)
) + geom_boxplot() + facet_wrap(
    ~ Semester
) + labs(
    title = "Boxplot de size en funcion de Semester",
)

# Hay diferencias significativas en el tamano de la clase dependiendo del semestre
kruskal.result <- kruskal.test(Size ~ Semester, data=tae)
kruskal.result


# Size - Class
# El grafico se ha hecho antes
tae %>% ggplot(
    aes(y=Size)
) + geom_boxplot() + facet_wrap(
    ~ Class
) + labs(
    title = "Boxplot de size en funcion de Class",
)

# No hay diferencias significativas en el tamano de la clase dependiendo de la evaluacion del profesor ayudante
kruskal.result <- kruskal.test(Size ~ Class, data=tae)
kruskal.result

# class - native
# El test de fisher nos da un p-value de 0.00326, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipotesis nula (las dos variables son independientes), y se puede afirmar que las variables
# Class y Native son dependientes
fisher.test(tae$Class, tae$Native)

# class - Instructor
# El test de fisher.test nos da un p-value de 0.01199, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipotesis nula (las dos variables son independientes), y se puede afirmar que las variables
# Class y Instructor son dependientes
fisher.test(tae$Class, tae$Instructor, simulate.p.value = T)

# class - Course
# El test de fisher.test nos da un p-value de 0.0004998, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipotesis nula (las dos variables son independientes), y se puede afirmar que las variables
# Class y Course son dependientes
fisher.test(tae$Class, tae$Course, simulate.p.value = T)

# class - Semester
# El test de fisher.test nos da un p-value de 0.002026, el cual es menor que 0.05, por lo tanto no se puede
# aceptar la hipotesis nula (las dos variables son independientes), y se puede afirmar que las variables
# Class y Semester son dependientes
fisher.test(tae$Class, tae$Semester)


# Multivariate analysis

ggpairs(data=tae, cardinality_threshold = NULL)
