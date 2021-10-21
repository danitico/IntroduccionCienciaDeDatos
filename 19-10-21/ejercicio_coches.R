library(tidyverse)
library(Amelia)

df <- read_csv("car_example.csv")

str(df)

missmap(df)

df %>% gather("Variables", "Values", c(2:13, 15, 16)) %>% ggplot(., aes(y=Values)) + geom_histogram() + facet_wrap(~ Variables)

df %>% ggplot(., aes(x=horsepower_bhp, y=top_speed_mph)) + geom_point(alpha = 0.4)

df %>% ggplot(., aes(x=top_speed_mph)) + geom_histogram(fill = "blue")

df %>% ggplot(., aes(y=top_speed_mph)) + geom_boxplot()

df1 <- df %>% filter(between(top_speed_mph, 150, 159))

df1 %>% gather("Variables", "Values", c(2:13, 15, 16)) %>% ggplot(., aes(y=Values)) + geom_boxplot() + facet_wrap(~ Variables, scales = "free")
df1 %>% gather("Variables", "Values", c(2:13, 15, 16)) %>% ggplot(., aes(x=Values)) + geom_histogram() + facet_wrap(~ Variables, scales = "free")
df1 %>% ggplot(., aes(x = as.factor(top_speed_mph))) + geom_bar()

df %>% ggplot(., aes(x = top_speed_mph)) + geom_bar(fill = "blue") + facet_wrap(~ decade)

df2 <- df %>% group_by(make_nm) %>% filter(top_speed_mph ==  155, year >= 1990) %>% summarize(count=n()) %>% arrange(desc(count))


df %>% arrange(desc(top_speed_mph)) %>% head(., n=10) %>% select(car_full_nm)

