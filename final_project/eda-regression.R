source("utils.R")

library("tidyverse")

wankara <- read.keel("wankara/wankara.dat")
wankara <- wankara %>% mutate(Mean_temperature=as.numeric(Mean_temperature))

str(wankara)
