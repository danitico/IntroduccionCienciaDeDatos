# Load data
wbcd <- read.csv("https://resources.oreilly.com/examples/9781784393908/raw/ac9fe41596dd42fc3877cfa8ed410dd346c43548/Machine%20Learning%20with%20R,%20Second%20Edition_Code/Chapter%2003/wisc_bc_data.csv")

# Examine the structure of the wbcd data frame
str(wbcd)
wbcd

# Drop the id feature
library(tidyverse)

wbcd <- wbcd %>% select(-id)

# Table of diagnosis
table(wbcd$diagnosis)

# Recode diagnosis
wbcd <- wbcd %>% mutate(diagnosis = factor(diagnosis, labels = c("Benign", "Malignant")))
table(wbcd$diagnosis)

# Table or proportions with more informative labels
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

# Summarize features
summary(wbcd)

mwbcd <- wbcd %>% pivot_longer(cols = -diagnosis)
mwbcd

library(ggplot2)

# Plot boxplots
ggplot(data=mwbcd, aes(x=name, y=value)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90))

?scale

# Normalize the wbcd data
wbcd_n <- wbcd %>% mutate_if(is.numeric, scale, center = TRUE, scale = TRUE)

# Confirm that normalization worked
summary(wbcd_n[,c("radius_mean", "area_mean", "smoothness_mean")])

mwbcd_n <- wbcd_n %>% pivot_longer(cols = -diagnosis)

# Plot boxplots
ggplot(data=mwbcd_n, aes(x=name, y=value)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90))

# Notices that scales does not affect other properties...

cor(wbcd[,2:5])
cor(wbcd_n[,2:5])

# Create training and test data (holdout 90%-10%)
shuffle_ds <- sample(dim(wbcd_n)[1])
pct90 <- (dim(wbcd_n)[1] * 90) %/% 100
wbcd_train <- wbcd_n[shuffle_ds[1:pct90], -1]
wbcd_test <- wbcd_n[shuffle_ds[(pct90+1):dim(wbcd_n)[1]], -1]

# Create labels for training and test data
wbcd_train_labels <- wbcd_n[shuffle_ds[1:pct90], 1]
wbcd_test_labels <- wbcd_n[shuffle_ds[(pct90+1):dim(wbcd_n)[1]], 1]

print(dim(wbcd_train)[1])
print(dim(wbcd_test)[1])

library(class)

?knn

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
wbcd_test_pred

# Evaluating model performance
table(wbcd_test_pred, wbcd_test_labels)

install.packages("caret")
library(caret)

?train

knnModel <- train(x = wbcd_train, y = wbcd_train_labels, method = "knn", 
                  trControl = trainControl(method = "cv"))

knnModel

knnModel <- train(wbcd_train, wbcd_train_labels, method="knn", metric="Accuracy", tuneGrid = data.frame(.k=1:15))
knnModel

knnModel <- train(x = wbcd[shuffle_ds[1:pct90],-1], y = wbcd[shuffle_ds[1:pct90],1],
                  method = "knn", preProc = c("center", "scale"))
knnModel

knnPred <- predict(knnModel, newdata = wbcd[shuffle_ds[(pct90+1):dim(wbcd)[1]], -1])
knnPred

postResample(pred = knnPred, obs = as.factor(wbcd[shuffle_ds[(pct90+1):dim(wbcd)[1]], 1]))

table(knnPred, wbcd[shuffle_ds[(pct90+1):dim(wbcd)[1]], 1])

install.packages("philentropy")
library(philentropy)

getDistMethods()

?distance

xmat1 <- rbind(wbcd_test[1,], wbcd_train[1,])
xmat2 <- rbind(wbcd_test[1,], wbcd_train[5,])

distance(as.data.frame(xmat1), method='euclidean')
distance(as.data.frame(xmat1), method='chebyshev')

distance(as.data.frame(xmat2), method='euclidean')
distance(as.data.frame(xmat2), method='chebyshev')


