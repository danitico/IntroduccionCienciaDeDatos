source("utils.R")

if (!requireNamespace("tidyverse")) {
    install.packages("tidyverse")
}
if (!requireNamespace("ggplot2")) {
    install.packages("ggplot2")
}
if (!requireNamespace("kknn")) {
    install.packages("kknn")
}

library(tidyverse)
library(ggplot2)
library(kknn)

# We need to predict MedianHouseValue
df <- read.keel("california.dat")
df <- df %>% mutate(MedianHouseValue=as.numeric(MedianHouseValue))

# Knn consigue mejores resultados que una regresión lineal multiple
# Con knn el rmse es 339.597 mientras que con la regresión lineal múltiple es de 613.7
fitknn1 <- kknn(MedianHouseValue ~ ., df, df)
yprime1 <- fitknn1$fitted.values

# Plot
df %>% ggplot(aes(x=MedianIncome, y=MedianHouseValue)) + geom_point() + geom_point(aes(x=MedianIncome, y=yprime1), color="blue")

# RMSE
sqrt(sum((df$MedianHouseValue - yprime1)^2)/length(yprime1))


# Otros tipos de modelos con otras variables

fitknn2 <- kknn(MedianHouseValue ~ MedianIncome, df, df)
yprime2 <- fitknn2$fitted.values
sqrt(sum((df$MedianHouseValue - yprime2)^2)/length(yprime2))
df %>% ggplot(aes(x=MedianIncome, y=MedianHouseValue)) + geom_point() + geom_point(aes(x=MedianIncome, y=yprime2), color="blue")

fitknn3 <- kknn(MedianHouseValue~MedianIncome + I(MedianIncome^2), df, df)
yprime3 <- fitknn3$fitted.values
sqrt(sum((df$MedianHouseValue - yprime3)^2)/length(yprime3))
df %>% ggplot(aes(x=MedianIncome, y=MedianHouseValue)) + geom_point() + geom_point(aes(x=MedianIncome, y=yprime3), color="blue")

fitknn4 <- kknn(MedianHouseValue~TotalRooms*TotalBedrooms, df, df)
yprime4 <- fitknn4$fitted.values
sqrt(sum((df$MedianHouseValue - yprime4)^2)/length(yprime4))
df %>% ggplot(aes(x=MedianIncome, y=MedianHouseValue)) + geom_point() + geom_point(aes(x=MedianIncome, y=yprime4), color="blue")

fitknn5 <- kknn(MedianHouseValue~. + Households*TotalBedrooms + Households*MedianIncome + HousingMedianAge*MedianIncome, df, df)
yprime5 <- fitknn5$fitted.values
sqrt(sum((df$MedianHouseValue - yprime5)^2)/length(yprime5))
df %>% ggplot(aes(x=MedianIncome, y=MedianHouseValue)) + geom_point() + geom_point(aes(x=MedianIncome, y=yprime5), color="blue")

# Cross validation

#------------- 5-fold cross-validation LM todas las variables
nombre <- "california"
run_lm_fold <- function(i, x, tt = "test") {
    file <- paste(x, "-5-", i, "tra.dat", sep="")
    x_tra <- read.csv(file, comment.char="@", header=FALSE)
    file <- paste(x, "-5-", i, "tst.dat", sep="")
    x_tst <- read.csv(file, comment.char="@", header=FALSE)
    In <- length(names(x_tra)) - 1
    names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tra)[In+1] <- "Y"
    names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tst)[In+1] <- "Y"
    if (tt == "train") {
        test <- x_tra
    }
    else {
        test <- x_tst
    }
    fitMulti=lm(Y~.,x_tra)
    yprime=predict(fitMulti,test)
    sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}

lmMSEtrain<-mean(sapply(1:5,run_lm_fold,nombre,"train"))
lmMSEtest<-mean(sapply(1:5,run_lm_fold,nombre,"test"))

#------------- 5-fold cross-validation KNN todas las variables
nombre <- "california"
run_knn_fold <- function(i, x, tt = "test") {
    file <- paste(x, "-5-", i, "tra.dat", sep="")
    x_tra <- read.csv(file, comment.char="@", header=FALSE)
    file <- paste(x, "-5-", i, "tst.dat", sep="")
    x_tst <- read.csv(file, comment.char="@", header=FALSE)
    In <- length(names(x_tra)) - 1
    names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tra)[In+1] <- "Y"
    names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tst)[In+1] <- "Y"
    if (tt == "train") {
        test <- x_tra
    }
    else {
        test <- x_tst
    }
    fitMulti=kknn(Y~.,x_tra,test)
    yprime=fitMulti$fitted.values
    sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}

knnMSEtrain<-mean(sapply(1:5,run_knn_fold,nombre,"train"))
knnMSEtest<-mean(sapply(1:5,run_knn_fold,nombre,"test"))
