if(!requireNamespace("caret")) {
  install.packages("caret")
}

if(!requireNamespace("philentropy")) {
  install.packages("philentropy")
}

library("caret")
library("philentropy")
library("class")
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

data <- read.csv("https://resources.oreilly.com/examples/9781784393908/raw/ac9fe41596dd42fc3877cfa8ed410dd346c43548/Machine%20Learning%20with%20R,%20Second%20Edition_Code/Chapter%2003/wisc_bc_data.csv")
data <- data %>% select(-id)
data <- data %>% mutate(diagnosis = factor(diagnosis, labels = c("Benign", "Malignant")))
data <- data %>% mutate_if(is.numeric, scale, center = TRUE, scale = TRUE)

# Create training and test data (holdout 90%-10%)
# Create labels for training and test data
shuffle_ds <- sample(dim(data)[1])
pct90 <- (dim(data)[1] * 90) %/% 100

x_train <- data[shuffle_ds[1:pct90], -1]
x_test <- data[shuffle_ds[(pct90+1):dim(data)[1]], -1]
y_train <- data[shuffle_ds[1:pct90], 1]
y_test <- data[shuffle_ds[(pct90+1):dim(data)[1]], 1]


euclideanResults <- NULL
manhattanResults <- NULL

for (k in seq(1, 21, 2)) {
  euclideanPred <- my_knn(x_train, y_train, x_test, k, metric = "euclidean")
  manhattanPred <- my_knn(x_train, y_train, x_test, k, metric = "manhattan")
  
  euclideanResults <- c(euclideanResults, mean(euclideanPred == y_test))
  manhattanResults <- c(manhattanResults, mean(manhattanPred == y_test))
}

colnames(resultMatrix) <- c("euclidean", "manhattan")
resultMatrix <- resultMatrix %>% mutate(kNeighbours = seq(1, 21, 2))

resultMatrix %>% gather(method, accuracy, 1:2) %>% ggplot(aes(y=accuracy, x=kNeighbours)) + geom_line() + facet_wrap(~ method)


