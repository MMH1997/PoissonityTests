#' Delta inf test
#' @export
#' @param x numeric variable
#' @param n.boot numeric variable

testdeltainf = function(x,n.boot){
  fdeltainf=function(z) {
    if (sum(z) == 0) {
      0
    }
    else {
      fdeltainfabs= function(t) abs((mean(t^z))-(mean(((t+1)/2)^z)^2))
      OptDeltaInf=optimize(fdeltainfabs, lower = 0, upper = 1,
                           maximum = T)
      return(as.numeric(OptDeltaInf[2]))
    }
  }

  pv = 0
  n = length(x)
  lambda.hat=mean(x)
  T.obs=fdeltainf(x)
  for (i in 1:n.boot){
    x.boot=rpois(n,lambda.hat)
    T.boot=fdeltainf(x.boot)
    pv=pv+as.numeric(T.boot>T.obs)
  }
  names(T.obs) <- "test statistic"
  media = mean(x)
  names(media) <- "mean"
  e = list(method = paste("Delta infinity poissonity test",
                          sep = ""),
           statistic = T.obs, p.value = pv/n.boot,
           data.name = paste("sample size ", n, ", replicates ",
                             n.boot, sep = ""), estimate = media)
  class(e) <- "htest"
  e

}
