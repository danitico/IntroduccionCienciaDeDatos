if(!requireNamespace("philentropy", quietly = T)) {
    install.packages("philentropy")
}

if(!requireNamespace("tidyverse", quietly = T)) {
    install.packages("tidyverse")
}

if(!requireNamespace("ggplot2", quietly = T)) {
    install.packages("ggplot2")
}

if(!requireNamespace("car", quietly = T)) {
    install.packages("car")
}

if(!requireNamespace("caret", quietly = T)) {
    install.packages("caret")
}

if(!requireNamespace("MASS", quietly = T)) {
    install.packages("MASS")
}


library("philentropy")
library("tidyverse")
library("ggplot2")
library("car")
library("caret")
library("MASS")

source("utils.R")

originalTae <- read.keel(
    "tae/tae.dat"
) %>% mutate_if(
    is.character, as.integer
) %>% mutate(
    Native=factor(Native, levels = c(1, 2), labels = c("yes", "no")),
    Instructor=as.factor(Instructor),
    Course=as.factor(Course),
    Semester=factor(Semester, levels = c(1, 2), labels = c("summer", "regular")),
)

universalDummy <- dummyVars(~., data=originalTae)

my_knn <- function (train, train_labels, test=NULL, k=1, metric="euclidean") {
    if (k <= 0) {
        print("K can not be zero or negative")
        return
    }
    
    if (!(metric %in% getDistMethods())) {
        paste(metric, " distance is not available")
        return
    }
    
    train <- as.data.frame(train)
    train_labels <- as.data.frame(train_labels)
    
    if (!is.null(test)) {
        test <- as.data.frame(test)
        
        if (dim(test)[2] != dim(train)[2]) {
            print("Train and test have different number of columns")
        }
    }
    
    if (dim(train)[1] != dim(train_labels)[1]) {
        print("Train and train_labels have different number of rows")
        return
    }
    
    distanceMatrix <- NULL
    
    # Get distance matrix
    if (is.null(test)) {
        distanceMatrix <- distance(train, method = metric, mute.message = T)
    } else {
        distanceMatrix <- apply(
            train,
            1,
            function (x) {
                apply(
                    test,
                    1,
                    function (y) {
                        as.numeric(
                            distance(
                                rbind(x, y),
                                method = metric,
                                mute.message = T
                            )
                        ) 
                    }
                )
            }
        )
    }
    
    rownames(distanceMatrix) <- NULL
    
    # When train == test, we start to take the k min values from the second position because the first position will be the distance between 
    # the train observation and itself, which is 0
    # Otherwise, we take the first k min values from the first position
    if (is.null(test)) {
        response <- apply(
            distanceMatrix,
            1,
            function (x) {
                getMode(
                    train_labels[
                        order(x, decreasing = F)[2:(k+1)],
                    ]
                )
            }
        )
    } else {
        response <- apply(
            distanceMatrix,
            1,
            function (x) {
                getMode(
                    train_labels[
                        order(x, decreasing = F)[1:k],
                    ]
                )
            }
        )
    }
    
    response
}

# KNN
nombre <- "tae/tae"
run_knn_fold <- function(i, x, k, tt = "test") {
    x_tra <- read.keel(
        paste(x, "-10-", i, "tra.dat", sep="")
    ) %>% mutate_if(
        is.character, as.integer
    ) %>% mutate(
        Native=factor(Native, levels = c(1, 2), labels = c("yes", "no")),
        Instructor=as.factor(Instructor),
        Course=as.factor(Course),
        Semester=factor(Semester, levels = c(1, 2), labels = c("summer", "regular")),
    )
    
    x_tra <- as.data.frame(predict(universalDummy, newdata = x_tra))
    
    if (tt == "train") {
        #train dataset as test
        test <- x_tra
    } else {
        # test dataset as test. To avoid reading the test file without using it,
        # we are moving it to the logic in which we check if it is going to be used
        test <- read.keel(
            paste(x, "-10-", i, "tst.dat", sep="")
        ) %>% mutate_if(
            is.character, as.integer
        ) %>% mutate(
            Native=factor(Native, levels = c(1, 2), labels = c("yes", "no")),
            Instructor=as.factor(Instructor),
            Course=as.factor(Course),
            Semester=factor(Semester, levels = c(1, 2), labels = c("summer", "regular")),
        )
        
        test <- as.data.frame(predict(universalDummy, newdata = test))
    }
    
    yprime <- my_knn(x_tra %>% dplyr::select(-Class), x_tra$Class, test %>% dplyr::select(-Class), k=k)
    
    mean(test$Class == yprime)
}


knntrainAccuracy <- sapply(
    1:15,
    function (k) {
        mean(
            sapply(
                1:10,
                run_knn_fold,
                nombre,
                k,
                "train"
            )
        )
    }
)

knntestAccuracy <- sapply(
    1:15,
    function (k) {
        mean(
            sapply(
                1:10,
                run_knn_fold,
                nombre,
                k,
                "test"
            )
        )
    }
)

results <- data.frame(cbind(1:15, knntrainAccuracy, knntestAccuracy))
colnames(results) <- c("k", "training", "test")
results

# Gráfico con la evolución del CCR en knn con distintas k
results %>% gather(
    `Conjunto de datos`, accuracy, 2:3
) %>% ggplot(
    aes(x=k, y=accuracy, color=`Conjunto de datos`)
) + geom_line() + labs(
    title = "Evolución de la precisión de training y test",
    y = "Tasa de acierto"
) + scale_x_continuous(breaks = seq(1, 15, 2))


# Comprobando asunciones de LDA y QDA
class1 <- originalTae %>% filter(Class == 1) %>% dplyr::select(-Class)
class2 <- originalTae %>% filter(Class == 2) %>% dplyr::select(-Class)
class3 <- originalTae %>% filter(Class == 3) %>% dplyr::select(-Class)

shapiro.test(class1$Size)
shapiro.test(class2$Size)
shapiro.test(class3$Size)

leveneTest(Size ~ factor(Class), originalTae)


# LDA
run_lda_fold <- function(i, x, tt = "test") {
    x_tra <- read.keel(
        paste(x, "-10-", i, "tra.dat", sep="")
    ) %>% mutate_if(
        is.character, as.integer
    ) %>% mutate(
        Native=factor(Native, levels = c(1, 2), labels = c("yes", "no")),
        Instructor=as.factor(Instructor),
        Course=as.factor(Course),
        Semester=factor(Semester, levels = c(1, 2), labels = c("summer", "regular")),
        Class=factor(Class)
    )
    
    if (tt == "train") {
        #train dataset as test
        test <- x_tra
    } else {
        # test dataset as test. To avoid reading the test file without using it,
        # we are moving it to the logic in which we check if it is going to be used
        test <- read.keel(
            paste(x, "-10-", i, "tst.dat", sep="")
        ) %>% mutate_if(
            is.character, as.integer
        ) %>% mutate(
            Native=factor(Native, levels = c(1, 2), labels = c("yes", "no")),
            Instructor=as.factor(Instructor),
            Course=as.factor(Course),
            Semester=factor(Semester, levels = c(1, 2), labels = c("summer", "regular")),
            Class=factor(Class)
        )
    }
    
    lda.fit <- lda(Class ~ Size, data=x_tra)
    
    yprime <- predict(lda.fit, test)$class

    mean(test$Class == yprime)
}

ldaCCRtrainFolds <- sapply(
    1:10,
    run_lda_fold,
    nombre,
    "train"
)

ldaCCRtrainmean <- mean(ldaCCRtrainFolds)

ldaCCRtestFolds <- sapply(
    1:10,
    run_lda_fold,
    nombre,
    "test"
)

ldaCCRtestmean <- mean(ldaCCRtestFolds)


# QDA
run_qda_fold <- function(i, x, tt = "test") {
    x_tra <- read.keel(
        paste(x, "-10-", i, "tra.dat", sep="")
    ) %>% mutate_if(
        is.character, as.integer
    ) %>% mutate(
        Native=factor(Native, levels = c(1, 2), labels = c("yes", "no")),
        Instructor=as.factor(Instructor),
        Course=as.factor(Course),
        Semester=factor(Semester, levels = c(1, 2), labels = c("summer", "regular")),
        Class=factor(Class)
    )
    
    if (tt == "train") {
        #train dataset as test
        test <- x_tra
    } else {
        # test dataset as test. To avoid reading the test file without using it,
        # we are moving it to the logic in which we check if it is going to be used
        test <- read.keel(
            paste(x, "-10-", i, "tst.dat", sep="")
        ) %>% mutate_if(
            is.character, as.integer
        ) %>% mutate(
            Native=factor(Native, levels = c(1, 2), labels = c("yes", "no")),
            Instructor=as.factor(Instructor),
            Course=as.factor(Course),
            Semester=factor(Semester, levels = c(1, 2), labels = c("summer", "regular")),
            Class=factor(Class)
        )
    }
    
    qda.fit <- qda(Class ~ Size, data=x_tra)
    
    yprime <- predict(qda.fit, test)$class
    
    mean(test$Class == yprime)
}

qdaCCRtrainFolds <- sapply(
    1:10,
    run_qda_fold,
    nombre,
    "train"
)

qdaCCRtrainmean <- mean(qdaCCRtrainFolds)

qdaCCRtestFolds <- sapply(
    1:10,
    run_qda_fold,
    nombre,
    "test"
)

qdaCCRtestmean <- mean(qdaCCRtestFolds)

# CCR train

clasif_train <- read.csv('clasif_train_alumnos.csv', row.names = 1)

clasif_train["tae", ]$out_train_knn <- knntrainAccuracy[1]
clasif_train["tae", ]$out_train_lda <- ldaCCRtrainmean
clasif_train["tae", ]$out_train_qda <- qdaCCRtrainmean

friedman.result.train <- friedman.test(as.matrix(clasif_train))
friedman.result.train

# CCR test

clasif_test <- read.csv('clasif_test_alumnos.csv', row.names = 1)

clasif_test["tae", ]$out_test_knn <- knntestAccuracy[1]
clasif_test["tae", ]$out_test_lda <- ldaCCRtestmean
clasif_test["tae", ]$out_test_qda <- qdaCCRtestmean

friedman.result.train <- friedman.test(as.matrix(clasif_test))
friedman.result.train
