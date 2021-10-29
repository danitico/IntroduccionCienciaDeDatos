library(MASS)
library(ISLR)

Smarket

# First check LDA assumtions!
#1 The observations are a random sample: we will assume there are...

#2 Each class has a normal distribution...
# Check it for every predictor variable...
library(tidyverse)
library(ggplot2)

# I'll do Lag1 and Lag2... TODO: check the rest of predictors
ggplot(Smarket %>% select("Lag1", "Direction"), aes(x=Lag1)) + geom_density() + facet_wrap(~ Direction)
ggplot(Smarket %>% select("Lag2", "Direction"), aes(x=Lag2)) + geom_density() + facet_wrap(~ Direction)

up <- Smarket %>% filter(Direction == "Up")
down <- Smarket %>% filter(Direction == "Down")

qqnorm(y = up$Lag1)
qqline(y = up$Lag1)

qqnorm(y = down$Lag1)
qqline(y = down$Lag1)

qqnorm(y = up$Lag2)
qqline(y = up$Lag2)

qqnorm(y = down$Lag2)
qqline(y = down$Lag2)

?shapiro.test
shapiro.test(rnorm(100, mean = 5, sd = 3))
shapiro.test(runif(100, min = 2, max = 4))

shapiro.test(up$Lag1)
shapiro.test(down$Lag1)

shapiro.test(up$Lag2)
shapiro.test(down$Lag2)

#3 Same covariance matrix

covariance <- c(var(Smarket$Lag1), var(Smarket$Lag2))
covariance

ggplot(Smarket %>% pivot_longer(c(Lag1,Lag2)), aes(x=name, y=value)) + geom_boxplot()

# DO NOT RUN! Our data is NOT normally distributed
bartlett.test(Lag1 ~ Direction, Smarket)
bartlett.test(Lag2 ~ Direction, Smarket)

library(car)

?leveneTest

leveneTest(Lag1 ~ Direction, Smarket)
leveneTest(Lag2 ~ Direction, Smarket)

# Linear Discriminant Analysis
lda.fit <- lda(Direction~Lag1+Lag2, data=Smarket, subset=Year<2005)
lda.fit

plot(lda.fit, type="both")

Smarket.2005 <- subset(Smarket, Year==2005)
lda.pred <- predict(lda.fit, Smarket.2005)

data.frame(lda.pred)[1:5,]

table(lda.pred$class,Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)

x <- seq(min(Smarket.2005$Lag1), max(Smarket.2005$Lag1), length.out=dim(Smarket.2005)[1])
y <- seq(min(Smarket.2005$Lag2), max(Smarket.2005$Lag2), length.out=dim(Smarket.2005)[1])

Xcon <- matrix(c(rep(x,length(y)), rep(y, rep(length(x), length(y)))),2) #Set all possible pairs of x and y on a grid

out <- predict(lda.fit, data.frame(Lag1=Xcon[,1], Lag2=Xcon[,2]))$post    #posterior probabilities of a point belonging to each class 

pr <- data.frame(x=rep(x, length(y)), y=rep(y, each=length(x)), z=as.vector(out))

ggplot(Smarket.2005, aes(x=Lag1, y=Lag2)) + 
    geom_point(size = 3, aes(pch = Direction,  col=Direction)) + 
    geom_contour(data = pr, aes(x=x, y=y, z=z), breaks=c(0,.5))

# QDA assumptions
# The same as LDA except for the covariance matrix...

# QDA
qda.fit <- qda(Direction~Lag1+Lag2, data=Smarket, subset=Year<2005)
qda.fit

qda.pred <- predict(qda.fit, Smarket.2005)

data.frame(qda.pred)[1:5,]

table(qda.pred$class,Smarket.2005$Direction)
mean(qda.pred$class==Smarket.2005$Direction)

out <- predict(qda.fit, data.frame(Lag1=Xcon[,1], Lag2=Xcon[,2]))$post    #posterior probabilities of a point belonging to each class 

pr <- data.frame(x=rep(x, length(y)), y=rep(y, each=length(x)), z=as.vector(out))

ggplot(Smarket.2005, aes(x=Lag1, y=Lag2)) + 
    geom_point(size = 3, aes(pch = Direction,  col=Direction)) + 
    geom_contour(data = pr, aes(x=x, y=y, z=z), breaks=c(0,.5))

data(iris)
iris.lda <- lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
iris.lda

iris.qda <- qda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
iris.qda

library(caret)

TrainData <- iris[,1:4]
TrainClasses <- iris[,5]

ldaFit <- train(TrainData, TrainClasses,
                method = "lda",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"))

ldaFit$finalModel

confusionMatrix(ldaFit)

qdaFit <- train(TrainData, TrainClasses,
                method = "qda",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"))

qdaFit$finalModel

confusionMatrix(qdaFit)




