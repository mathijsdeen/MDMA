% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/posteriorModelOdds.R
\name{posteriorModelOdds}
\alias{posteriorModelOdds}
\title{Posterior model odds}
\usage{
posteriorModelOdds(...)
}
\arguments{
\item{...}{objects of class \code{(g)lm}, given as separate arguments.}
}
\value{
\code{posteriorModelOdds} returns to posterior model odds for the models provided.
}
\description{
Calculate the posterior model odds for a set of models.

\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
}
\details{
Posterior model odds are calculated for every model \eqn{i} as \deqn{\mathrm{pMO}_i = \frac{\mathrm{exp}\Big[-\frac{1}{2}\Delta_i\mathrm{BIC}\Big]}{\sum_{j = 1}^K\mathrm{exp}\Big[-\frac{1}{2}\Delta_j\mathrm{BIC}\Big]},}
where the minimal BIC value is subtracted from all BICs. In other words: the model with the lowest BIC has \eqn{\Delta\mathrm{BIC}=0}.
}
\examples{
lm.1 <- lm(mpg ~ hp + wt, data = mtcars)
lm.2 <- lm(mpg ~ hp * wt, data = mtcars)
lm.3 <- lm(mpg ~ hp * wt + gear, data = mtcars)
posteriorModelOdds(lm.1, lm.2, lm.3)
}
\author{
Mathijs Deen
}
