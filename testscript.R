classificationPlots(aSAH$s100b, aSAH$outcome, levels=c("Good", "Poor"))
q$thresholds
cbind(q, c(NA, sort(unique(aSAH$s100b))))
c(min(aSAH$s100b),max(aSAH$s100b))

ls(pos = "package:mdma")

# roc en auc-functies vervangen, pROC lozen:
## voor ROC eerst thresholds nodig, dan sens en spec berekenen
## AUC obv MWU/n1*n2 (https://stats.stackexchange.com/questions/206911/relationship-between-auc-and-u-mann-whitney-statistic)

q <- classificationPlots(aSAH$s100b, aSAH$outcome, levels=c("Good", "Poor"))
q$thresholds
x <- aSAH$s100b
obs <- aSAH$outcome
xsort <- sort(unique(x))
xsort
thresholds <- (c(xsort,Inf)+c(-Inf,xsort))/2

x
# maak een lijst met length(thresholds) kopieen van x
# bepaal voor elk volgend element van de lijst welke waarden groter zijn van elk volgend element van thresholds

tlist <- rep(list(x), length(thresholds))
tlist

state <- "Poor"
obsBool <- obs == state
cbind(obs, obsBool)
out <- mapply(.Primitive(">="), tlist, thresholds) #WOW!!!
#colnames(out) <- paste0("x", thresholds)
View(out)
colMeans(out[obsBool == TRUE, ])
cbind(q[,c(3,1,2)],
      FPR = 1 - q[,2],
      sens = colMeans(out[obsBool == TRUE, ]),
      spec = 1 - colMeans(out[obsBool == FALSE, ]),
      FPR2 = colMeans(out[obsBool == FALSE, ]))
colnames(out)[20]
table(out[,4], obs)
lapply(data.frame(out), function(x) sweep(table(x, obs=="Good"), 2, colSums(table(x, obs=="Good")), '/'))

auc <- 1 - wilcox.test(x ~ obs)$statistic / prod(table(obs))
plot(density(x[obs=="Good"]), xlim=c(0,3))
lines(density(x[obs=="Poor"]), lty=3)

### cleanup
c(obs,"blaat") %in% c("Good", "Poor")

c(1,2,2,2,2,1,2,1,1,1,2,3)[c(1,2,2,2,2,1,2,1,1,1,2,3) %in% c(1,2)]

library(pROC)
detach("package:pROC", unload=TRUE)
r <- mdma::roc(pROC::aSAH$s100b, pROC::aSAH$outcome, c("Good","Poor"), "Poor")
class(r)
r
methods::showMethods("roc")
sloop::s3_methods_class("roc")

detach("package:mdma", unload=TRUE)
library(mdma)

#creation of QIDS data
set.seed(4)
n <- 100
QIDS <- round(MASS::mvrnorm(n = n, mu = 14.5, Sigma = matrix(25)), 0)
QIDS[QIDS < 6] <- 6
QIDS[QIDS > 27] <- 27
hist(QIDS, breaks = 20)
jitter <- 3.5 * sample(rep(c(-1,1), n/2))
depressed <- (QIDS + jitter) > 14.5
depression <- rep(NA, n)
depression[depressed==TRUE] <- "Yes"
depression[depressed==FALSE] <- "No"
depression <- as.factor(depression)
a <- roc(QIDS$QIDS, QIDS$depression, c("Yes","No"), "Yes")
plot(a, ylim.3 = c(0,.2), xlab.3= "hoi", cutoffs.1 = 14.5, cutoffs.2 = 14.5, cutoffs.3 = 14.5)
mdma::classificationPlots(QIDS, depression, c("Yes","No"), "Yes")
rm(roc)
sloop::s3_methods_class("roc")
QIDS <- data.frame(QIDS, depression)
save(QIDS, file='data/QIDS.rda')

p <- load("data/QIDS.rda")
rm (p)

rm(QIDS)
QIDS
