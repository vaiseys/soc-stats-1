library(tidyverse)

2+2
5*6

data(mtcars)
mean(mtcars$hp)

summary(mtcars)
summary(mtcars$hp)

head(mtcars)
glimpse(mtcars)

hist(mtcars$hp)
plot(mtcars$hp, mtcars$mpg)

mean(mtcars$hp)

data("starwars")
glimpse(starwars)

mean(starwars$height, na.rm = TRUE)

sd(mtcars$mpg)
var(mtcars$mpg)
IQR(mtcars$mpg)