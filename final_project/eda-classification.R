source("utils.R")
library(tidyverse)


tae <- read.keel("tae/tae.dat")

tae <- tae %>% mutate_if(is.character, as.integer)
tae <- tae %>% mutate(
    Native=as.factor(Native),
    Instructor=as.factor(Instructor),
    Course=as.factor(Course),
    Semester=as.factor(Semester)
)

str(tae$Native)

# Variable Native. Variable categórica
table(tae$Native)

# Variable Instructor. Variable categórica
table(tae$Instructor)

# Variable Course. Variable categórica
table(tae$Course)

# Variable Semester. Variable categórica
table(tae$Semester)

# Variable Size. Variable numérica discreta
summary(tae$Size)

# Variable a predecir. Estamos ante un dataset balanceado
summary(tae$Class)



