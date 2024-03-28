library(MASS)

set.seed(1)
d <- mvrnorm(n         = 50,
             mu        = c(20,10),
             Sigma     = matrix(data = c(100,70,70,100),
                                nrow = 2),
             empirical = TRUE) |>
  data.frame() |>
  setNames(c("x1", "x2"))
#d[c(3,5),1] <- NA
#d[c(3,5),2] <- NA
d <- rbind(d, data.frame(x1 = c(NA,NA), x2 = c(NA,NA)))
d
sd(d$x2)
cor(d, use = "complete.obs")[1,2]

t.test(x = d$x1, y = d$x2, paired = TRUE)
#paired
tTest2(x = d$x1, y = d$x2, paired = TRUE)
tTest2(x = d$x1, y = mean(d$x2, na.rm=TRUE), sdy = sd(d$x2, na.rm=TRUE), ny = length(na.omit(d$x2)), rxy = cor(d, use = "complete.obs")[1,2], paired = TRUE)
tTest2(x = mean(d$x1, na.rm=TRUE), y = d$x2, sdx = sd(d$x1, na.rm=TRUE), nx = length(na.omit(d$x1)), rxy = cor(d, use = "complete.obs")[1,2], paired = TRUE)
tTest2(x = mean(d$x1, na.rm=TRUE), y = mean(d$x2, na.rm=TRUE), sdx = 10, nx = 50, sdy = 10, ny = 50, rxy = 0.7, paired = TRUE)

#indep. samples, welch
t.test(x = d$x1, y = d$x2, paired = FALSE)
tTest2(x = d$x1, y = d$x2, paired = FALSE)
tTest2(x = d$x1, y = mean(d$x2, na.rm=TRUE), sdy = sd(d$x2, na.rm=TRUE), ny = length(na.omit(d$x2, na.omit=TRUE)), paired = FALSE)
tTest2(x = mean(d$x1, na.rm=TRUE), y = d$x2, sdx = sd(d$x1, na.rm=TRUE), nx = length(na.omit(d$x1, na.omit=TRUE)), paired = FALSE)
tTest2(x = mean(d$x1, na.rm=TRUE), y = mean(d$x2, na.rm=TRUE), sdx = sd(d$x1, na.rm=TRUE), nx = length(na.omit(d$x1, na.omit=TRUE)), sdy = sd(d$x2, na.rm=TRUE), ny = length(na.omit(d$x2, na.omit=TRUE)), paired = FALSE)

#indep. samples, student
t.test(x = d$x1, y = d$x2, paired = FALSE, var.equal = TRUE)
tTest2(x = d$x1, y = d$x2, paired = FALSE, var.equal = TRUE)
tTest2(x = d$x1, y = mean(d$x2, na.rm=TRUE), sdy = 10, ny = 50, paired = FALSE, var.equal = TRUE)
tTest2(x = mean(d$x1, na.rm=TRUE), y = d$x2, sdx = 10, nx = 50, paired = FALSE, var.equal = TRUE)
tTest2(x = mean(d$x1, na.rm=TRUE), y = mean(d$x2, na.rm=TRUE), sdx = 10, nx = 50, sdy = 10, ny = 50, paired = FALSE, var.equal = TRUE)

#one-sample
t.test(x = d$x1, mu=18)
tTest2(x = d$x1, mu=18)
tTest2(x = mean(d$x1, na.rm=TRUE), sdx = 10, nx = 50, mu=18)

View(q)
mean(d$x1) - mean(d$x2)
length(d$x1)

s <- sqrt(var(d$x1)/length(d$x1) + var(d$x2)/length(d$x2))
sE <- s * sqrt(1 - cor(d)[2,1])
sE
10/sE
t.test(x = d$x1, y = d$x2, paired = TRUE)$statistic

tTest2(x=3, sdx=2, nx=21)
length(NULL)

mdma::m(c(1,2,3))
