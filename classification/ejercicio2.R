if(!requireNamespace("caret")) {
    install.packages("caret")
}

if(!requireNamespace("class")) {
    install.packages("class")
}

if(!requireNamespace("tidyverse")) {
    install.packages("tidyverse")
}

library("caret")
library("class")
library("tidyverse")

data <- read.csv("https://resources.oreilly.com/examples/9781784393908/raw/ac9fe41596dd42fc3877cfa8ed410dd346c43548/Machine%20Learning%20with%20R,%20Second%20Edition_Code/Chapter%2003/wisc_bc_data.csv")
data <- data %>% select(-id)
data <- data %>% mutate(diagnosis = factor(diagnosis, labels = c("Benign", "Malignant")))
data <- data %>% mutate_if(is.numeric, scale, center = TRUE, scale = TRUE)

logisticRegressionModel <- train(diagnosis ~ ., data = data, trControl = trainControl(method = "cv", number = 10), method = "glm", family="binomial", control = glm.control(maxit=250))
logisticRegressionModel
