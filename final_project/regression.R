if (!requireNamespace("tidyverse", quietly = T)) {
    install.packages("tidyverse")
}

if (!requireNamespace("kknn", quietly = T)) {
    install.packages("kknn")
}


source("utils.R")

library("tidyverse")
library("kknn")

wankara <- read.keel("wankara/wankara.dat")
wankara <- wankara %>% mutate(Mean_temperature=as.numeric(Mean_temperature))

par(mfrow=c(2, 3))

fit1 <- lm(Mean_temperature ~ Max_temperature, data=wankara)
summary(fit1)
plot(Mean_temperature ~ Max_temperature, wankara)
abline(fit1, col = "red")
sqrt(sum(fit1$residuals^2)/(length(fit1$residuals)-2))

fit2 <- lm(Mean_temperature ~ Min_temperature, data=wankara)
summary(fit2)
plot(Mean_temperature ~ Min_temperature, wankara)
abline(fit2, col = "red")
sqrt(sum(fit2$residuals^2)/(length(fit2$residuals)-2))

fit3 <- lm(Mean_temperature ~ Dewpoint, data=wankara)
summary(fit3)
plot(Mean_temperature ~ Dewpoint, wankara)
abline(fit3, col = "red")
sqrt(sum(fit3$residuals^2)/(length(fit3$residuals)-2))

fit4 <- lm(Mean_temperature ~ Sea_level_pressure, data=wankara)
summary(fit4)
plot(Mean_temperature ~ Sea_level_pressure, wankara)
abline(fit4, col = "red")
sqrt(sum(fit4$residuals^2)/(length(fit4$residuals)-2))

fit5 <- lm(Mean_temperature ~ Visibility, data=wankara)
summary(fit5)
plot(Mean_temperature ~ Visibility, wankara)
abline(fit5, col = "red")
sqrt(sum(fit5$residuals^2)/(length(fit5$residuals)-2))


# ------------------------------------------------

fit6 <- lm(Mean_temperature~., data=wankara)
summary(fit6)
sqrt(sum(fit6$residuals^2)/(length(fit6$residuals)-2))

# quitamos la variable Precipitation
fit7 <- lm(Mean_temperature~.-Precipitation, data=wankara)
summary(fit7)
sqrt(sum(fit7$residuals^2)/(length(fit7$residuals)-2))


# ------------------------------------------------

nombre <- "wankara/wankara"
run_lm_fold <- function(i, x, tt = "test") {
    x_tra <- read.keel(paste(x, "-5-", i, "tra.dat", sep=""))
    x_tra <- x_tra %>% mutate(output=as.numeric(output))
    
    if (tt == "train") {
        #train dataset as test
        test <- x_tra
    } else {
        # test dataset as test. To avoid reading the test file without using it,
        # we are moving it to the logic in which we check if it is going to be used
        test <- read.keel(
            paste(x, "-5-", i, "tst.dat", sep="")
        ) %>% mutate(
            output=as.numeric(output)
        )    
    }

    fitMulti <- lm(output~., data=x_tra)

    yprime <- predict(fitMulti, test)

    sum(abs(test$output-yprime)^2)/length(yprime)
}

lmMSEtrain <- mean(
    sapply(
        1:5,
        run_lm_fold,
        nombre,
        "train"
    )
)

lmMSEtest <- mean(
    sapply(
        1:5,
        run_lm_fold,
        nombre,
        "test"
    )
)

run_knn_fold <- function(i, x, tt = "test") {
    x_tra <- read.keel(paste(x, "-5-", i, "tra.dat", sep=""))
    x_tra <- x_tra %>% mutate(output=as.numeric(output))
    
    if (tt == "train") {
        #train dataset as test
        test <- x_tra
    } else {
        # test dataset as test. To avoid reading the test file without using it,
        # we are moving it to the logic in which we check if it is going to be used
        test <- read.keel(
            paste(x, "-5-", i, "tst.dat", sep="")
        ) %>% mutate(
            output=as.numeric(output)
        )    
    }
    
    knnModel <- kknn(output~., x_tra, test)
    
    yprime <- knnModel$fitted.values
    
    sum(abs(test$output-yprime)^2)/length(yprime)
}

knnMSEtrain <- mean(
    sapply(
        1:5,
        run_knn_fold,
        nombre,
        "train"
    )
)

knnMSEtest<-mean(
    sapply(
        1:5,
        run_knn_fold,
        nombre,
        "test"
    )
)

regr_test <- read.csv('regr_test_alumnos.csv', row.names = 1)

# utilizar mse del k-fold
regr_test["wankara", ]$out_test_lm <- lmMSEtest
regr_test["wankara", ]$out_test_kknn <- knnMSEtest

# Normalizar tabla ya que wilcoxon falla para valores == 0

diffs <- (regr_test[, 1] - regr_test[, 2]) / regr_test[, 1]

wilcox_normalized_data <- cbind(
    ifelse(diffs < 0, abs(diffs) + 0.1, 0.1),
    ifelse(diffs > 0, abs(diffs) + 0.1, 0.1)
)

wilcox.result <- wilcox.test(
    wilcox_normalized_data[, 1],
    wilcox_normalized_data[, 2],
    alternative = "two.sided",
    paired = T
)

wilcox.result

friedman.result <- friedman.test(as.matrix(regr_test))
friedman.result

tam <- dim(regr_test)
groups <- rep(1:tam[2], each=tam[1])
post.hoc <- pairwise.wilcox.test(as.matrix(regr_test), groups, p.adjust = "holm", paired = TRUE)
post.hoc


# train errors

regr_train <- read.csv('regr_train_alumnos.csv', row.names = 1)

# utilizar mse del k-fold
regr_train["wankara", ]$out_train_lm <- lmMSEtrain
regr_train["wankara", ]$out_train_kknn <- knnMSEtrain

# Normalizar tabla ya que wilcoxon falla para valores == 0

diffs_train <- (regr_train[, 1] - regr_train[, 2]) / regr_train[, 1]

wilcox_normalized_data_train <- cbind(
    ifelse(diffs_train < 0, abs(diffs_train) + 0.1, 0.1),
    ifelse(diffs_train > 0, abs(diffs_train) + 0.1, 0.1)
)

wilcox.result.train <- wilcox.test(
    wilcox_normalized_data_train[, 1],
    wilcox_normalized_data_train[, 2],
    alternative = "two.sided",
    paired = T
)

wilcox.result.train

friedman.result.train <- friedman.test(as.matrix(regr_train))
friedman.result.train

tam <- dim(regr_train)
groups <- rep(1:tam[2], each=tam[1])
post.hoc <- pairwise.wilcox.test(as.matrix(regr_train), groups, p.adjust = "holm", paired = TRUE)
post.hoc

