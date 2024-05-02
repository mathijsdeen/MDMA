#' @name gnomes
#' @title Gnomes mushroom insulation cost-effectiveness data
#' @description The \code{gnomes} dataframe consists of 300 observations of gnomes
#'     that had their housing unit (i.e., their mushroom) insulated. The insulation
#'     was done by the most skilled insulation animals in the forest: squirrels (Sciurus vulgaris).
#'     They either used the common insulation technique consisting of leafs of the
#'     common beech tree (Fagus sylvatica) or an experimental form of insulation using leafs of the
#'     less common (and thus, more expensive) sessile oak tree (Quercus petraea).
#'     Over the year before insulation and the year after insulation, the gnomes
#'     filled out the SLIM (Satisfaction List concerning Insulation of Mushrooms),
#'     a well-validated list that rates mushroom insulation satisfaction on a scale
#'     of 0 to 50. Differences between pre and post measurement were calculated on
#'     a higher-is-better basis. The squirrels were paid in acorns.
#'
#' `r lifecycle::badge("stable")`
#' @usage gnomes
#' @format the following variables are available:
#' \itemize{
#'  \item \code{diffSLIM}: the difference in SLIM scores between the year before and the year after insulation.
#'  \item \code{Cost}: insulation costs in acorns.
#' }
#' @docType data
#' @author Mathijs Deen
NULL
