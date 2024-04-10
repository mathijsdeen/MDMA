#to be removed after more rigorous testing of new tTest function (esp. with missings)
tTest_old <- function(x,
                      y           = NULL,
                      sdx         = NULL,
                      sdy         = NULL,
                      nx          = length(x),
                      ny          = length(y),
                      alternative = c("two.sided", "greater", "less"),
                      mu          = 0,
                      paired      = FALSE,
                      var.equal   = FALSE,
                      conf.level  = 0.95){

  m <- function(a) mean(a, na.rm=TRUE)
  alternative <- match.arg(arg = alternative, several.ok = FALSE)
  mx <- m(x)

  # checks for possibly given group means in x and/or y
  if(length(x) == 1 & is.null(sdx)) stop("x of length 1 is only accepted with specified sdx", call. = FALSE)
  if(length(x) == 1 & nx == 1) stop("x of length 1 is only accepted with specified nx", call. = FALSE)
  if(length(y) == 1 & is.null(sdy)) stop("y of length 1 is only accepted with specified sdy", call. = FALSE)
  if(length(y) == 1 & ny == 1) stop("y of length 1 is only accepted with specified ny", call. = FALSE)
  if(is.null(sdx)) sdx <- sd(x, na.rm = TRUE)
  if(is.null(sdy)) sdy <- sd(y, na.rm = TRUE)

  if(!missing(mu) && (length(mu) != 1 || is.na(mu))) stop("'mu' must be a single number")
  if(!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) || conf.level < 0 || conf.level > 1)) stop("'conf.level' must be a single number between 0 and 1")

  vx <- sdx^2

  if(!is.null(y)) {
    # paired and independent samples
    dname <- paste(deparse1(substitute(x)),"and",
                   deparse1(substitute(y)))
    if(paired)
      xok <- yok <- complete.cases(x,y)
    else {
      yok <- !is.na(y)
      xok <- !is.na(x)
    }
    y <- y[yok]
  }
  else {
    # one-sample
    dname <- deparse1(substitute(x))
    if (paired) stop("'y' is missing for paired test")
    xok <- !is.na(x)
    yok <- NULL
  }
  x <- x[xok]
  if (paired) {
    x <- x - y
    y <- NULL
  }
  if(is.null(y)){
    # paired or one-sample t-test
    df <- nx - 1
    stdError <- sqrt(vx / nx)
    tVal <- (mx - mu) / stdError
    method <- if(paired) "Paired t-test" else "One Sample t-test"
    estimate <- setNames(mx, if(paired) "mean difference" else "mean of x")
  }else{
    # independent samples t-test
    my <- m(y)
    vy <- sdy^2
    method <- paste(if(!var.equal) "Welch", "Two Sample t-test")
    estimate <- c(mx,my)
    names(estimate) <- c("mean of x","mean of y")
    if(var.equal){
      # Student
      df <- nx + ny - 2
      v <- (nx - 1) * vx + (ny - 1) * vy
      vPooled <- v / df
      stdError <- sqrt(vPooled * (1 / nx + 1 / ny))
    }
    else{
      # Welch
      stdErrorx <- sqrt(vx / nx)
      stdErrory <- sqrt(vy / ny)
      stdError <- sqrt(stdErrorx^2 + stdErrory^2)
      df <- stdError^4 / (stdErrorx^4/(nx - 1) + stdErrory^4/(ny - 1))
    }
    tVal <- (mx - my - mu) / stdError
  }
  if (alternative == "less") {
    pVal <- pt(tVal, df, lower.tail = TRUE)
    cint <- c(-Inf, tVal + qt(conf.level, df) )
  }
  else if (alternative == "greater") {
    pVal <- pt(tVal, df, lower.tail = FALSE)
    cint <- c(tVal - qt(conf.level, df), Inf)
  }
  else {
    pVal <- 2 * pt(-abs(tVal), df)
    alpha <- 1 - conf.level
    cint <- qt(1 - alpha/2, df)
    cint <- tVal + c(-cint, cint)
  }
  cint <- mu + cint * stdError
  names(tVal) <- "t"
  names(df) <- "df"
  names(mu) <- if(paired) "mean difference"
    else if(!is.null(y)) "difference in means"
    else "mean"
  attr(cint,"conf.level") <- conf.level
  rval <- list(statistic = tVal, parameter = df, p.value = pVal,
               conf.int = cint, estimate = estimate, null.value = mu,
               stderr = stderr,
               alternative = alternative,
               method = method, data.name = dname)
  class(rval) <- c("tTest", "htest")
  return(rval)
}



