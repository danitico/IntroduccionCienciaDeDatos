library(ISLR)

names(Smarket)
summary(Smarket)

?Smarket

class(Smarket$Direction)
levels(Smarket$Direction)

library(GGally)

ggpairs(Smarket, aes(col=Direction))

# Direction is derive from Today
cor(as.numeric(Smarket$Direction),Smarket$Today)

library(tidyverse)

cor(Smarket %>% select(-Direction)) # Note that Volume has some correlation with Year...

ggplot(Smarket, aes(x=Year, group=Year, y=Volume)) + geom_boxplot()

glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)

glm.probs <- predict(glm.fit, type="response") 
glm.probs

glm.pred <- ifelse(glm.probs>0.5,"Up","Down")
glm.pred

table(glm.pred,Smarket$Direction)
mean(glm.pred==Smarket$Direction)

# Make training and test set
train <- (Smarket$Year < 2005)
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Volume, data=Smarket[train,], family=binomial)
summary(glm.fit)

glm.probs <- predict(glm.fit, newdata=Smarket[!train,], type="response") 
glm.pred <- ifelse(glm.probs>0.5,"Up","Down")

Direction.2005 <- Smarket$Direction[!train]
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)

glm.fit <- glm(Direction~Lag1+Lag2, data=Smarket[train,], family=binomial)
summary(glm.fit)

glm.probs <- predict(glm.fit,newdata=Smarket[!train,],type="response") 
glm.pred <- ifelse(glm.probs > 0.5,"Up","Down")

table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)

require(caret)
glmFit <- train(Smarket %>% select(-Direction), y = Smarket[, "Direction"], method = "glm", 
                preProcess = c("center", "scale"), tuneLength = 10, 
                control=glm.control(maxit=1500), trControl = trainControl(method = "cv"))
glmFit

glmFit <- train(Smarket %>% select(-c(Direction, Today)), y = Smarket[, "Direction"], method = "glm", 
                preProcess = c("center", "scale"), tuneLength = 10, 
                control=glm.control(maxit=500), trControl = trainControl(method = "cv"))
glmFit


