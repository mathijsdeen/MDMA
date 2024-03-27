tTest2 <- function(x,
                   y           = NULL,
                   sdx         = NULL,
                   sdy         = NULL,
                   nx          = length(na.rm(x)),
                   ny          = length(na.rm(y)),
                   alternative = c("two.sided", "greater", "less"),
                   mu          = 0,
                   paired      = FALSE,
                   rxy         = NULL,
                   var.equal   = FALSE,
                   conf.level  = 0.95){

  alternative <- match.arg(arg = alternative, several.ok = FALSE)
  #mx <- m(x)

  # checks for possibly given group means in x and/or y
  if(length(x) == 1 & is.null(sdx)) stop("x of length 1 is only accepted with specified sdx", call. = FALSE)
  if(length(x) == 1 & nx == 1) stop("x of length 1 is only accepted with specified nx", call. = FALSE)
  if(length(y) == 1 & is.null(sdy)) stop("y of length 1 is only accepted with specified sdy", call. = FALSE)
  if(length(y) == 1 & ny == 1) stop("y of length 1 is only accepted with specified ny", call. = FALSE)
  if(is.null(sdx)) sdx <- sd(x, na.rm = TRUE)
  if(is.null(sdy)) sdy <- sd(y, na.rm = TRUE)

  if(!missing(mu) && (length(mu) != 1 || is.na(mu))) stop("'mu' must be a single number")
  if(!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) || conf.level < 0 || conf.level > 1)) stop("'conf.level' must be a single number between 0 and 1")

  #vx <- sdx^2

  xObserved <- ifelse(length(x) == 1, FALSE, TRUE)
  yObserved <- ifelse(length(y) == 1, FALSE, TRUE)
  if(is.null(y)) yObserved <- FALSE

  if(!is.null(y)){
    type <- ifelse(paired, "paired", "independent")
  }else{
    type <- "one-sample"
  }

  if(type %in% c("paired", "independent")){
    dname <- paste(deparse1(substitute(x)),"and",
                   deparse1(substitute(y)))
    if(paired){
      method <- "Paired t-test"
      df <- nx - 1
      if(xObserved & yObserved){
        # both variables are observed, regular case
        xOK <- yOK <- complete.cases(x,y)
        x <- x[xOK]
        y <- y[yOK]
        x <- x - y
        mx <- m(x)
        vx <- var(x)
        stdError <- sqrt(vx / nx)
        tVal <- (mx - mu) / stdError
      }else{
        # at least one of these is already a group mean
        if(is.null(rxy)) stop("argument rxy should be provided when at least one of the groups is not observed", call. = FALSE)
        x <- na.omit(x)
        y <- na.omit(y)
        mx <- m(x) - m(y)
        s <- sqrt(sdx^2/nx + sdy^2/ny)
        stdError <- sqrt((sdx^2/nx + sdy^2/ny) * (1 - rxy))
        tVal <- (mx - mu) / stdError
      }
      estimate <- setNames(mx, "mean difference")
    }else{
      method <- paste(if(!var.equal) "Welch", "Two Sample t-test")
      mx  <- m(x)
      my  <- m(y)
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
  }else{
    method <- "One Sample t-test"
    dname <- deparse1(substitute(x))
    if (paired) stop("'y' is missing for paired test")
    x   <- na.omit(x)
    df  <- nx - 1
    mx  <- m(x)
    vx  <- var(x)
    stdError <- sqrt(vx / nx)
    tVal <- (mx - mu) / stdError
    estimate <- setNames(mx, "mean of x")
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

tTest2(x=c(1,3), paired=TRUE)
tTest2(x=2, y=c(1))
