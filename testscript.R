library(MASS)

set.seed(1)
d <- mvrnorm(n         = 50,
             mu        = c(20,10),
             Sigma     = matrix(data = c(100,70,70,100),
                                nrow = 2),
             empirical = TRUE) |>
  data.frame() |>
  setNames(c("x1", "x2"))

t.test(x = d$x1, y = d$x2, paired = TRUE)

mean(d$x1) - mean(d$x2)
length(d$x1)

s <- sqrt(var(d$x1)/length(d$x1) + var(d$x2)/length(d$x2))
sE <- s * sqrt(1 - cor(d)[2,1])
sE
10/sE
t.test(x = d$x1, y = d$x2, paired = TRUE)$statistic

