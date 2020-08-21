x <- runif(100)
y <- (6 * x) + rnorm(100)

library(ggplot2)
qplot(x, y)
qplot(sample(x, 100), y)

summary(lm(y ~ x))
summary(lm(y ~ sample(x)))
