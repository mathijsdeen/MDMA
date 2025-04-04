% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pMM.R
\name{pMM}
\alias{pMM}
\title{Posterior model probability}
\usage{
pMM(...)
}
\arguments{
\item{...}{objects of class \code{(g)lm}, given as separate arguments.}
}
\value{
\code{pMM} returns to posterior model probabilities for the models provided.
}
\description{
Calculate the posterior model probability for a set of models.

\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
}
\details{
Posterior model probabilities are calculated for every model \eqn{i} as \deqn{\mathrm{pMO}_i = \frac{\mathrm{exp}\Big[-\frac{1}{2}\Delta_i\mathrm{BIC}\Big]}{\sum_{j = 1}^K\mathrm{exp}\Big[-\frac{1}{2}\Delta_j\mathrm{BIC}\Big]},}
where the minimal BIC value is subtracted from all BICs. In other words: the model with the lowest BIC has \eqn{\Delta\mathrm{BIC}=0}.
}
\examples{
lm.1 <- lm(mpg ~ hp + wt, data = mtcars)
lm.2 <- lm(mpg ~ hp * wt, data = mtcars)
lm.3 <- lm(mpg ~ hp * wt + gear, data = mtcars)
pMM(lm.1, lm.2, lm.3)
}
\author{
Mathijs Deen
}
