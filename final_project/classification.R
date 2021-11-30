if(!requireNamespace("philentropy", quietly = T)) {
    install.packages("philentropy")
}

if(!requireNamespace("tidyverse", quietly = T)) {
    install.packages("tidyverse")
}

if(!requireNamespace("ggplot2", quietly = T)) {
    install.packages("ggplot2")
}

library("philentropy")
library("tidyverse")
library("ggplot2")

getMode <- function (v) {
    uniqueValues <- unique(v)
    as.vector(
        uniqueValues[
            which.max(
                tabulate(
                    match(v, uniqueValues)
                )
            )
        ]
    )
}

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

nombre <- "tae/tae"

run_kknn_fold <- function(i, x, k, tt = "test") {
    x_tra <- read.keel(
        paste(x, "-10-", i, "tra.dat", sep="")
    ) %>% mutate_if(
        is.character, as.integer
    ) %>% mutate(
        Native=factor(Native, levels = c(1, 2), labels = c("yes", "no")),
        Instructor=as.factor(Instructor),
        Course=as.factor(Course),
        Semester=factor(Semester, levels = c(1, 2), labels = c("summer", "regular")),
        Class=factor(Class, levels = c(1, 2, 3), labels = c("low", "medium", "high"))
    )
    
    if (tt == "train") {
        #train dataset as test
        test <- x_tra %>% select(-Class)
    } else {
        # test dataset as test. To avoid reading the test file without using it,
        # we are moving it to the logic in which we check if it is going to be used
        test <-read.keel(
            paste(x, "-10-", i, "tst.dat", sep="")
        ) %>% mutate_if(
            is.character, as.integer
        ) %>% mutate(
            Native=factor(Native, levels = c(1, 2), labels = c("yes", "no")),
            Instructor=as.factor(Instructor),
            Course=as.factor(Course),
            Semester=factor(Semester, levels = c(1, 2), labels = c("summer", "regular")),
            Class=factor(Class, levels = c(1, 2, 3), labels = c("low", "medium", "high"))
        )    
    }
    
    yprime <- my_knn(x_tra %>% select(-Class), x_tra$Class, test %>% select(-Class), k=k)
    
    sum(abs(test$Class-yprime)^2)/length(yprime)
}

euclideanResults <- NULL
manhattanResults <- NULL

for (k in seq(1, 21, 2)) {
    euclideanPred <- my_knn(x_train, y_train, x_test, k, metric = "euclidean")
    manhattanPred <- my_knn(x_train, y_train, x_test, k, metric = "manhattan")
    
    euclideanResults <- c(euclideanResults, mean(euclideanPred == y_test))
    manhattanResults <- c(manhattanResults, mean(manhattanPred == y_test))
}

resultMatrix <- as.data.frame(cbind(euclideanResults, manhattanResults))
resultMatrix <- resultMatrix %>% mutate(k = seq(1, 21, 2)) %>% rename(euclidean=euclideanResults, manhattan=manhattanResults)

resultMatrix %>% gather(method, accuracy, 1:2) %>% ggplot(aes(y=accuracy, x=k)) + geom_col() + facet_wrap(~ method) + scale_x_continuous(breaks = seq(1, 21, 2))