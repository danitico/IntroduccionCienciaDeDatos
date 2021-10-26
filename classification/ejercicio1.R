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

getMode <- function (v) {
  uniqueValues <- unique(v)
  uniqueValues[
    which.max(
      tabulate(
        match(v, uniqueValues)
      )
    )
  ]
}

my_knn <- function (train, train_labels, test=NA, k=1, metric="euclidean") {
  if (k <= 0) {
    print("K can not be zero or negative")
    return
  }
  
  if (!(metric %in% getDistMethods())) {
    paste(metric, " distance is not available")
    return
  }
  
  train <- as.data.frame(train)
  test <- as.data.frame(test)
  train_labels <- as.data.frame(train_labels)

  if (!all(is.na(test))) {
    if (dim(test)[2] != dim(train)[2]) {
      print("Train and test have different number of columns")
      return
    }
  }

  if (dim(train)[1] != dim(train_labels)[1]) {
    print("Train and train_labels have different number of rows")
    return
  }

  distanceMatrix <- NULL
  
  # Get distance matrix
  if (!all(is.na(test))) {
    for (i in 1:nrow(test)) {
      distanceRow <- NULL

      for (j in 1:nrow(train)) {
        distanceRow <- c(
          distanceRow,
          as.numeric(distance(
            rbind(test[i,], train[j,]),
            method = metric,
            mute.message = T
          ))
        )
      }
      
      distanceMatrix <- rbind(distanceMatrix, distanceRow)
    }
  } else {
    distanceMatrix <- distance(train, method = metric, mute.message = T)
  }
  
  if (!is.na(test)) {
    response <- apply(
      distanceMatrix,
      1,
      function (x) {
        getMode(
          train_labels[
            sort(x)[1:k],
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
            sort(x)[2:(k+1)],
          ]
        )
      }
    )
  }
  
  # Determine class label
  # apply(
  #   distanceMatrix,
  #   1,
  #   function (x) {
  #     getMode(
  #       train_labels[
  #         sort(x)[ifelse(all(is.na(test)), 2:(k+1), 1:k)]
  #       ]
  #     )
  #   }
  # )
}

wbcd <- read.csv("https://resources.oreilly.com/examples/9781784393908/raw/ac9fe41596dd42fc3877cfa8ed410dd346c43548/Machine%20Learning%20with%20R,%20Second%20Edition_Code/Chapter%2003/wisc_bc_data.csv")
wbcd <- wbcd %>% select(-id)
wbcd <- wbcd %>% mutate(diagnosis = factor(diagnosis, labels = c("Benign", "Malignant")))
wbcd_n <- wbcd %>% mutate_if(is.numeric, scale, center = TRUE, scale = TRUE)
mwbcd <- wbcd %>% pivot_longer(cols = -diagnosis)

# Create training and test data (holdout 90%-10%)
shuffle_ds <- sample(dim(wbcd_n)[1])
pct90 <- (dim(wbcd_n)[1] * 90) %/% 100
wbcd_train <- wbcd_n[shuffle_ds[1:pct90], -1]
wbcd_test <- wbcd_n[shuffle_ds[(pct90+1):dim(wbcd_n)[1]], -1]

# Create labels for training and test data
wbcd_train_labels <- wbcd_n[shuffle_ds[1:pct90], 1]
wbcd_test_labels <- wbcd_n[shuffle_ds[(pct90+1):dim(wbcd_n)[1]], 1]


b <- my_knn(wbcd_train, wbcd_train_labels, wbcd_test, 21)
b







