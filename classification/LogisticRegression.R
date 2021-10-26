# Getting sample data
library(foreign)

mydata <- read.dta("https://dss.princeton.edu/training/Panel101.dta")

mydata

# Running a logit model
logit <- glm(y_bin ~ x1 + x2 + x3, family=binomial(link="logit"), data=mydata)

summary(logit)

logit <- glm(y_bin ~ x1 + x2 + x3 + opinion, family=binomial(link="logit"), data=mydata)

summary(logit)

# Wroooooonggggg!!!!
logit <- glm(opinion ~ x1 + x2 + x3, family=binomial(link="logit"), data=mydata)

summary(logit)

library(MASS)

?polr

# Running the ordered logit model
m1 <- polr(opinion ~ x1 + x2 + x3, data=mydata, Hess=TRUE)

summary(m1)

# Getting coefficients and p-values
m1.coef <- data.frame(coef(summary(m1)))
m1.coef$pval = round((pnorm(abs(m1.coef$t.value), lower.tail = FALSE) * 2), 4)

m1.coef

?pt

dim(mydata)[1]

# Find the p-value for a t-value of 3.9418
pt(3.9418, dim(mydata)[1]-3, lower.tail=FALSE)*2

# Getting coefficients and p-values
m1.coef <- data.frame(coef(summary(m1)))
m1.coef$pval = round(pt(abs(m1.coef$t.value), dim(mydata)[1]-3, lower.tail = FALSE) * 2, 4)

m1.coef

pred <- predict(m1, mydata, type="class")
table(pred, mydata$opinion)

mean(pred==mydata$opinion)

m1.pred <- predict(m1, type="probs")

m1.pred

# Load the library
library(brant)

# Run the Brant test on the model: proportional odds assumption or the parallel regression assumption
brant(m1)

?brant

mdata <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")

mdata

# Set the reference group for prog to be 1
mdata$prog <- relevel(mdata$prog, ref=1)

# Load the package
library(nnet)

# Run the model
model <- multinom(prog ~ ses + write, data=mdata)

summary(model)

# Calculate z-values
zvalues <- summary(model)$coefficients / summary(model)$standard.errors

# Show z-values
zvalues

pnorm(abs(zvalues), lower.tail=FALSE)*2


