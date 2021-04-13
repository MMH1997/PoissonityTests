#' U test
#' @export
#' @param x numeric variable
#' @param n.boot numeric variable
testu = function(x,n.boot){
  u <- function(z) {
    if (sum(z) == 0) {
      0
    }
    else if (sum(z) == 1) {
      0
    }
    else {
      b =var(z)
      c = mean(z)
      d = (b/c - 1)
      e = length(z)-1
      f = 2*(1-1/sum(z))
      g = sqrt(e/f)
      return (d * g)
    }
  }
  pv = 0
  n = length(x)
  lambda.hat=mean(x)
  T.obs=u(x)
  for (i in 1:n.boot){
    x.boot=rpois(n,lambda.hat)
    T.boot=u(x.boot)
    pv=pv+as.numeric(T.boot>T.obs)
  }
  names(T.obs) <- "test statistic"
  media = mean(x)
  names(media) <- "mean"

  if (n.boot > 1) {
    e = list(method = paste("U poissonity test",
                            sep = ""),
             statistic = T.obs, p.value = pv/n.boot,
             data.name =  paste("sample size ", n, ", replicates ",
                               n.boot, sep = ""), estimate = media)
    class(e) <- "htest"
    e
  }

  else {e = list(method = paste("U poissonity test",
                                sep = ""),
                 statistic = T.obs, p.value = 2*(1-pnorm(abs(T.obs))),
                 data.name = paste("sample size ", n, ", normal approximation ",
                                   sep = ""), estimate = media)
  class(e) <- "htest"
  e
  }
}


