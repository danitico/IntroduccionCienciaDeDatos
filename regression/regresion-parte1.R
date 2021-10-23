source("utils.R")

if (!requireNamespace("tidyverse")) {
    install.packages("tidyverse")
}
if (!requireNamespace("ggplot2")) {
    install.packages("ggplot2")
}

library(tidyverse)
library(ggplot2)

# We need to predict MedianHouseValue
df <- read.keel("california.dat")
df <- df %>% mutate(MedianHouseValue=as.numeric(MedianHouseValue))

str(df)

pairs(df)

fit1 <- lm(MedianHouseValue~Longitude, data=df)
summary(fit1)

fit2 <- lm(MedianHouseValue~Latitude, data=df)
summary(fit2)

fit3 <- lm(MedianHouseValue~HousingMedianAge, data=df)
summary(fit3)

fit4 <- lm(MedianHouseValue~TotalRooms, data=df)
summary(fit4)

fit5 <- lm(MedianHouseValue~TotalBedrooms, data=df)
summary(fit5)

fit6 <- lm(MedianHouseValue~Population, data=df)
summary(fit6)

fit7 <- lm(MedianHouseValue~Households, data=df)
summary(fit7)

fit8 <- lm(MedianHouseValue~MedianIncome, data=df)
summary(fit8)

# The best model with one independent variable which is MedianIncome

rmsefit8 <- sqrt(sum(fit8$residuals^2)/(length(fit8$residuals)-2))
rmsefit8

# Prediction
yprime <- predict(fit8, data.frame(MedianIncome=df$MedianIncome))

# Plotting original relation between the two variables and the new model
df %>% ggplot(
    aes(x=MedianIncome, y=MedianHouseValue)
) + geom_point(
    alpha=0.3
) + geom_line(
    aes(
        x=MedianIncome,
        y=yprime
    ),
    color="blue"
)


# En este modelo añadimos todas las variables independientes
# Este modelo es mucho mejor que cuando solo se utiliza MedianIncome como variable independiente
multipleVariablesFit1 <- lm(MedianHouseValue~., data=df)
summary(multipleVariablesFit1)


# No linealidad

# No mejora el modelo en el que se utilizan todas las variables. Mejora levemente
# respecto al modelo fit8, en el que no se tiene en cuenta la variable al cuadrado
noLinealityFit1 <- lm(MedianHouseValue~MedianIncome + I(MedianIncome^2), df)
summary(noLinealityFit1)
df %>% ggplot(
    aes(x=MedianIncome, y=MedianHouseValue)
) + geom_point(
    alpha=0.3
) + geom_line(
    aes(
        x=MedianIncome,
        y=predict(noLinealityFit1, data.frame(MedianIncome=MedianIncome))
    ),
    color="blue"
)

# Interacciones

interactionsFit1 <- lm(MedianHouseValue~TotalRooms*TotalBedrooms, data=df)
summary(interactionsFit1)

# El mejor modelo con interacciones
interactionsFit2 <- lm(
    MedianHouseValue~. + Households*TotalBedrooms + Households*MedianIncome + HousingMedianAge*MedianIncome,
    data=df
)
summary(interactionsFit2)
