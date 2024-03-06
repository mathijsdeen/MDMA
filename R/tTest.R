tTest <- function(x1,
                  x2,
                  sd1         = sd(x1, na.rm=TRUE),
                  sd2         = sd(x2, na.rm=TRUE),
                  n1          = length(x1),
                  n2          = length(x2),
                  r12,
                  paired      = FALSE,
                  var.equal   = FALSE,
                  effect.size = "g",
                  conf.level  = 0.95){
  if(paired == TRUE){
    #paired t-test
    #length(x1) should be length(x2)
    #compute correlation
    #compute t, p
  }
  if(paired == FALSE){
    #independent samples t-test
    if(var.equal == TRUE){
      #Student's
    }
    if(var.equal == FALSE){
      #Welch
    }
  }
}


