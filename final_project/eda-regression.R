if (!requireNamespace("tidyverse", quietly = T)) {
    install.packages("tidyverse")
}

if (!requireNamespace("moments", quietly = T)) {
    install.packages("moments")
}

if (!requireNamespace("Hmisc", quietly = T)) {
    install.packages("Hmisc")
}

if (!requireNamespace("corrplot", quietly = T)) {
    install.packages("corrplot")
}

source("utils.R")

library("tidyverse")
library("moments")
library("Hmisc")
library("corrplot")

wankara <- read.keel("wankara/wankara.dat")
wankara <- wankara %>% mutate(Mean_temperature=as.numeric(Mean_temperature))

str(wankara)

# 1) TITLE OF DATABASE: Ankara Weather Database
# 
# 2) SOURCE: www.wunderground.com
# http://www.wunderground.com/global/stations/17128.html
# by: M.Erdem Kurul & Eray Tuzun
# erdemkurul@hotmail.com
# eraytuzun@hotmail.com
# 
# 3) RELEVANT INFORMATION:
#     This file contains the wheather information of Ankara
# from 01/01/1994 to 28/05/1998
# From given features, the goal is to predict the mean
# temperature.
# 
# 4) NUMBER OF INSTANCES: 1609
# 
# 5) NUMBER OF FEATURES: 10
# 
# 6) FEATURE INFORMATION
# Features:
# 1  -  max temperature (F) : continuous
# 2  -  min temperature (F) : continuous
# 3  -  dewpoint (F) : continuous
# 4  -  precipitation (in): continuous
# 5  -  sea level pressure (in) : continuous
# 6  -  standard pressure (in) : continuous
# 7  -  visibility (mi) : continuous
# 8  -  wind speed (mph) : continuous
# 9  -  max wind speed (mph) : continuous
# 10 -  mean temperature (F) : continuous
# 
# 7) MISSING FEATURE VALUES
# No missing values
# There is almost no missing values for the features
# except precipitation. All the missing values are handled
# by taking the average of the feature column and using this
# average value for missing fields.

describe(wankara)

# UNIVARIATE ANALYSIS

# Variable Max_temperature Variable numérica discreta

## Se obtiene el min, mediana, media, maximo y los cuartiles, además el IQR
summary(wankara$Max_temperature)
IQR(wankara$Max_temperature)

## Se calcula la varianza y desviación tipica de Max_temperature
var(wankara$Max_temperature)
sd(wankara$Max_temperature)

## Se calcula la moda
getMode(wankara$Max_temperature)

## Se renderiza un boxplot de la variable Max_temperature
wankara %>% ggplot(aes(y=Max_temperature)) + geom_boxplot()

## Comprobar asimetria y la forma del pico de la distribucíon

skewness(wankara$Max_temperature) # asimetrica por la derecha. Se puede reducir haciendo una raiz cuadrada o logaritmo de los datos 
kurtosis(wankara$Max_temperature) # El pico de la distribución es más alto que el de una distribución normal

## Con el grafico se puede ver que es asimetrica por la derecha,
## y que el pico de la distribución es más alto que el pico de una distribución normal
wankara %>% ggplot(aes(x=Max_temperature)) + geom_histogram(bins = 20)


# Test de shafiro para ver si la variable Max_temperature sigue una distribución normal
# Como el p-value que se obtiene (< 2.2e-16) es menor que 0.05, se puede afirmar que la variable Max_temperature no sigue una distribución normal
shapiro.result <- shapiro.test(wankara$Max_temperature)
shapiro.result

qqnorm(wankara$Max_temperature)
qqline(wankara$Max_temperature)




# Variable Min_temperature Variable numérica discreta

## Se obtiene el min, mediana, media, maximo y los cuartiles, además el IQR
summary(wankara$Min_temperature)
IQR(wankara$Min_temperature)

## Se calcula la varianza y desviación tipica de Min_temperature
var(wankara$Min_temperature)
sd(wankara$Min_temperature)

## Se calcula la moda
getMode(wankara$Min_temperature)

## Se renderiza un boxplot de la variable Min_temperature
# Existe un outlier. Hay que investigar si realmente lo es ,porque puede ser un caso extremo debido a la temática que se está tratando, la temperatura
wankara %>% ggplot(aes(y=Min_temperature)) + geom_boxplot()

## Comprobar asimetria y la forma del pico de la distribucíon

skewness(wankara$Min_temperature) # asimetrica por la izquierda. Se puede reducir haciendo elevando los datos al cuadrado, pero hay que tener en cuenta que son datos de temperatura!
kurtosis(wankara$Min_temperature) # El pico de la distribución es más alto que el de una distribución normal

## Con el grafico se puede ver que es asimetrica por la izquierda,
## que el pico de la distribución es más alto que el pico de una distribución normal
wankara %>% ggplot(aes(x=Min_temperature)) + geom_histogram(bins = 20)


# Test de shafiro para ver si la variable Min_temperature sigue una distribución normal
# Como el p-value que se obtiene (5.531e-14) es menor que 0.05, se puede afirmar que la variable Min_temperature no sigue una distribución normal
shapiro.result <- shapiro.test(wankara$Min_temperature)
shapiro.result

qqnorm(wankara$Min_temperature)
qqline(wankara$Min_temperature)





# Variable Dewpoint Variable numérica discreta

## Se obtiene el min, mediana, media, maximo y los cuartiles, además el IQR
summary(wankara$Dewpoint)
IQR(wankara$Dewpoint)

## Se calcula la varianza y desviación tipica de Dewpoint
var(wankara$Dewpoint)
sd(wankara$Dewpoint)

## Se calcula la moda
getMode(wankara$Dewpoint)

## Se renderiza un boxplot de la variable Dewpoint
# Existen varios outlier. Hay que investigar si realmente lo es, porque pueden ser casos extremos debido a la temática que se está tratando, la temperatura
wankara %>% ggplot(aes(y=Dewpoint)) + geom_boxplot()

## Comprobar asimetria y la forma del pico de la distribucíon

skewness(wankara$Dewpoint) # asimetrica por la izquierda. Se puede reducir haciendo elevando los datos al cuadrado, pero hay que tener en cuenta que son datos de temperatura!
kurtosis(wankara$Dewpoint) # El pico de la distribución es más alto que el de una distribución normal

## Con el grafico se puede ver que es asimetrica por la izquierda,
## que el pico de la distribución es más alto que el pico de una distribución normal
wankara %>% ggplot(aes(x=Dewpoint)) + geom_histogram(bins = 20)

# Test de shafiro para ver si la variable Dewpoint sigue una distribución normal
# Como el p-value que se obtiene (2.723e-15) es menor que 0.05, se puede afirmar que la variable Dewpoint no sigue una distribución normal
shapiro.result <- shapiro.test(wankara$Dewpoint)
shapiro.result

qqnorm(wankara$Dewpoint)
qqline(wankara$Dewpoint)





# Variable Precipitation Variable numérica discreta

## Se obtiene el min, mediana, media, maximo y los cuartiles, además el IQR
summary(wankara$Precipitation)
IQR(wankara$Precipitation)

## Se calcula la varianza y desviación tipica de Precipitation
var(wankara$Precipitation)
sd(wankara$Precipitation)

## Se calcula la moda
getMode(wankara$Precipitation)

## Se renderiza un boxplot de la variable Precipitation
# Todos son outliers. Se puede deber a periodos de sequía o al mal funcionamiento del pluviometro
wankara %>% ggplot(aes(y=Precipitation)) + geom_boxplot()

## Comprobar asimetria y la forma del pico de la distribucíon

skewness(wankara$Precipitation) # asimetrica por la izquierda. Se puede reducir haciendo elevando los datos al cuadrado, pero hay que tener en cuenta que son datos de temperatura!
kurtosis(wankara$Precipitation) # El pico de la distribución es más alto que el de una distribución normal

## Con el grafico se puede ver que es asimetrica por la izquierda,
## que el pico de la distribución es más alto que el pico de una distribución normal
wankara %>% ggplot(aes(x=Precipitation)) + geom_histogram(bins = 20)

# Test de shafiro para ver si la variable Precipitation sigue una distribución normal
# Como el p-value que se obtiene (< 2.2e-16) es menor que 0.05, se puede afirmar que la variable Precipitation no sigue una distribución normal
shapiro.result <- shapiro.test(wankara$Precipitation)
shapiro.result

qqnorm(wankara$Precipitation)
qqline(wankara$Precipitation)




# Variable Sea_level_pressure Variable numérica discreta

## Se obtiene el min, mediana, media, maximo y los cuartiles, además el IQR
summary(wankara$Sea_level_pressure)
IQR(wankara$Sea_level_pressure)

## Se calcula la varianza y desviación tipica de Sea_level_pressure
var(wankara$Sea_level_pressure)
sd(wankara$Sea_level_pressure)

## Se calcula la moda
getMode(wankara$Sea_level_pressure)

## Se renderiza un boxplot de la variable Sea_level_pressure
# # Existen varios outlier. Hay que investigar si realmente lo es, porque pueden ser casos extremos debido a la temática que se está tratando, la meteorología
wankara %>% ggplot(aes(y=Sea_level_pressure)) + geom_boxplot()

## Comprobar asimetria y la forma del pico de la distribucíon

skewness(wankara$Sea_level_pressure) # asimetrica por la derecha. Se puede reducir haciendo una raiz cuadrada o logaritmo de los datos, aunque estamos trabando con datos meteorologicos
kurtosis(wankara$Sea_level_pressure) # El pico de la distribución es más alto que el de una distribución normal

## Con el grafico se puede ver que es asimetrica por la derecha,
## y que el pico de la distribución es más alto que el pico de una distribución normal
wankara %>% ggplot(aes(x=Sea_level_pressure)) + geom_histogram(bins = 30)

# Test de shafiro para ver si la variable Sea_level_pressure sigue una distribución normal
# Como el p-value que se obtiene (9.76e-12) es menor que 0.05, se puede afirmar que la variable Sea_level_pressure no sigue una distribución normal
shapiro.result <- shapiro.test(wankara$Sea_level_pressure)
shapiro.result

qqnorm(wankara$Sea_level_pressure)
qqline(wankara$Sea_level_pressure)




# Variable Standard_pressure Variable numérica discreta

## Se obtiene el min, mediana, media, maximo y los cuartiles, además el IQR
summary(wankara$Standard_pressure)
IQR(wankara$Standard_pressure)

## Se calcula la varianza y desviación tipica de Standard_pressure
var(wankara$Standard_pressure)
sd(wankara$Standard_pressure)

## Se calcula la moda
getMode(wankara$Standard_pressure)

## Se renderiza un boxplot de la variable Standard_pressure
# # Existen varios outlier. Hay que investigar si realmente lo es, porque pueden ser casos extremos debido a la temática que se está tratando, la meteorología
wankara %>% ggplot(aes(y=Standard_pressure)) + geom_boxplot()

## Comprobar asimetria y la forma del pico de la distribucíon

skewness(wankara$Standard_pressure) # asimetrica por la izquierda. Se puede reducir haciendo elevando los datos al cuadrado, aunque estamos trabando con datos meteorologicos.
kurtosis(wankara$Standard_pressure) # El pico de la distribución es más alto que el de una distribución normal

## Con el grafico se puede ver que es asimetrica por la izquierda,
## y que el pico de la distribución es más alto que el pico de una distribución normal
wankara %>% ggplot(aes(x=Standard_pressure)) + geom_histogram(bins = 30)

# Test de shafiro para ver si la variable Standard_pressure sigue una distribución normal
# Como el p-value que se obtiene (4.617e-05) es menor que 0.05, se puede afirmar que la variable Standard_pressure no sigue una distribución normal
shapiro.result <- shapiro.test(wankara$Standard_pressure)
shapiro.result

qqnorm(wankara$Standard_pressure)
qqline(wankara$Standard_pressure)





# Variable Visibility Variable numérica discreta

## Se obtiene el min, mediana, media, maximo y los cuartiles, además el IQR
summary(wankara$Visibility)
IQR(wankara$Visibility)

## Se calcula la varianza y desviación tipica de Visibility
var(wankara$Visibility)
sd(wankara$Visibility)

## Se calcula la moda
getMode(wankara$Visibility)

## Se renderiza un boxplot de la variable Visibility
# # Existen varios outlier. Hay que investigar si realmente lo es, porque pueden ser casos extremos debido a la temática que se está tratando, la meteorología
wankara %>% ggplot(aes(y=Visibility)) + geom_boxplot()

## Comprobar asimetria y la forma del pico de la distribucíon

skewness(wankara$Visibility) # asimetrica por la izquierda. Se puede reducir haciendo elevando los datos al cuadrado, aunque estamos trabando con datos meteorologicos.
kurtosis(wankara$Visibility) # El pico de la distribución es más alto que el de una distribución normal

## Con el grafico se puede ver que es asimetrica por la izquierda,
## y que el pico de la distribución es más alto que el pico de una distribución normal
wankara %>% ggplot(aes(x=Visibility)) + geom_histogram(bins = 30)

# Test de shafiro para ver si la variable Visibility sigue una distribución normal
# Como el p-value que se obtiene (< 2.2e-16) es menor que 0.05, se puede afirmar que la variable Visibility no sigue una distribución normal
shapiro.result <- shapiro.test(wankara$Visibility)
shapiro.result

qqnorm(wankara$Visibility)
qqline(wankara$Visibility)



# Variable Wind_speed Variable numérica discreta

## Se obtiene el min, mediana, media, maximo y los cuartiles, además el IQR
summary(wankara$Wind_speed)
IQR(wankara$Wind_speed)

## Se calcula la varianza y desviación tipica de Wind_speed
var(wankara$Wind_speed)
sd(wankara$Wind_speed)

## Se calcula la moda
getMode(wankara$Wind_speed)

## Se renderiza un boxplot de la variable Wind_speed
# # Existen varios outlier. Hay que investigar si realmente lo es, porque pueden ser casos extremos debido a la temática que se está tratando, la meteorología
wankara %>% ggplot(aes(y=Wind_speed)) + geom_boxplot()

## Comprobar asimetria y la forma del pico de la distribucíon

skewness(wankara$Wind_speed) # asimetrica por la derecha Se puede reducir haciendo una raiz cuadrada o logaritmo de los datos, aunque estamos trabajando con datos meteorológicos
kurtosis(wankara$Wind_speed) # El pico de la distribución es más alto que el de una distribución normal

## Con el grafico se puede ver que es asimetrica por la derecha,
## y que el pico de la distribución es más alto que el pico de una distribución normal
wankara %>% ggplot(aes(x=Wind_speed)) + geom_histogram(bins = 30)

# Test de shafiro para ver si la variable Wind_speed sigue una distribución normal
# Como el p-value que se obtiene (< 2.2e-16) es menor que 0.05, se puede afirmar que la variable Wind_speed no sigue una distribución normal
shapiro.result <- shapiro.test(wankara$Wind_speed)
shapiro.result

qqnorm(wankara$Wind_speed)
qqline(wankara$Wind_speed)






# Variable Max_wind_speed Variable numérica discreta

## Se obtiene el min, mediana, media, maximo y los cuartiles, además el IQR
summary(wankara$Max_wind_speed)
IQR(wankara$Max_wind_speed)

## Se calcula la varianza y desviación tipica de Max_wind_speed
var(wankara$Max_wind_speed)
sd(wankara$Max_wind_speed)

## Se calcula la moda
getMode(wankara$Max_wind_speed)

## Se renderiza un boxplot de la variable Max_wind_speed
# # Existen varios outlier. Hay que investigar si realmente lo es, porque pueden ser casos extremos debido a la temática que se está tratando, la meteorología
wankara %>% ggplot(aes(y=Max_wind_speed)) + geom_boxplot()

## Comprobar asimetria y la forma del pico de la distribucíon

skewness(wankara$Max_wind_speed) # asimetrica por la derecha Se puede reducir haciendo una raiz cuadrada o logaritmo de los datos, aunque estamos trabajando con datos meteorológicos
kurtosis(wankara$Max_wind_speed) # El pico de la distribución es más alto que el de una distribución normal

## Con el grafico se puede ver que es asimetrica por la derecha,
## y que el pico de la distribución es más alto que el pico de una distribución normal
wankara %>% ggplot(aes(x=Max_wind_speed)) + geom_histogram(bins = 30)

# Test de shafiro para ver si la variable Max_wind_speed sigue una distribución normal
# Como el p-value que se obtiene (< 2.2e-16) es menor que 0.05, se puede afirmar que la variable Max_wind_speed no sigue una distribución normal
shapiro.result <- shapiro.test(wankara$Max_wind_speed)
shapiro.result

qqnorm(wankara$Max_wind_speed)
qqline(wankara$Max_wind_speed)



# Bivariate analysis

correlation <- cor(wankara)
corrplot(correlation, method = "number")

wankara %>% pairs(.)


