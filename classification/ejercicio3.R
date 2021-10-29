if(!requireNamespace("MASS")) {
    install.packages("MASS")
}

if(!requireNamespace("ISLR")) {
    install.packages("ISLR")
}

if(!requireNamespace("ggplot2")) {
    install.packages("ggplot2")
}

if(!requireNamespace("tidyverse")) {
    install.packages("tidyverse")
}

library(MASS)
library(ISLR)
library(ggplot2)
library(tidyverse)

str(Smarket)

Smarket.2005 <- subset(Smarket, Year==2005)

# LDA
lda.fit <- lda(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5, data=Smarket, subset=Year<2005)
lda.fit

lda.pred <- predict(lda.fit, Smarket.2005)
table(lda.pred$class,Smarket.2005$Direction)
lda.accuracy <- mean(lda.pred$class==Smarket.2005$Direction)

# Logistic regression

logisticRegression.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5, data=Smarket, subset=Year<2005, family=binomial(link = "logit"))
summary(logisticRegression.fit)

logisticRegression.pred <- predict(lda.fit, Smarket.2005)
table(logisticRegression.pred$class,Smarket.2005$Direction)
logisticRegression.accuracy <- mean(logisticRegression.pred$class==Smarket.2005$Direction)


# QDA
qda.fit <- qda(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5, data=Smarket, subset=Year<2005)
qda.fit

qda.pred <- predict(qda.fit, Smarket.2005)

table(qda.pred$class,Smarket.2005$Direction)
qda.accuracy <- mean(qda.pred$class==Smarket.2005$Direction)

# PLOT

df <- data.frame(
    methods = c("LDA", "Logistic Regression", "QDA"),
    accuracy = c(lda.accuracy, logisticRegression.accuracy, qda.accuracy)
)

df %>% ggplot(aes(y=methods, x=accuracy)) + geom_col(aes(fill = methods), show.legend = F)
