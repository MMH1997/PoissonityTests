#' Kn test
#' @export
#' @param x numeric variable
#' @param n.boot numeric variable

testKn = function(x,n.boot){
  options(warn = -1)
  Kn=function(z) {if (sum(z) == 0) {0}
    else {ecdfz=ecdf(z)
    f1=function(y) {sqrt(length(z)) * (ecdfz(y) - ppois(y, mean(z)))}
    resf11=f1(min(z):max(z))
    Kn=max(resf11)
    return(Kn)
    }
  }
  pv = 0
  n = length(x)
  lambda.hat=mean(x)
  T.obs=Kn(x)
  for (i in 1:n.boot){
    x.boot=rpois(n,lambda.hat)
    T.boot=Kn(x.boot)
    pv=pv+as.numeric(T.boot>T.obs)
  }
  names(T.obs) <- "test statistic"
  media = mean(x)
  names(media) <- "mean"
  e = list(method = paste("Kn poissonity test",
                          sep = ""),
           statistic = T.obs, p.value = pv/n.boot,
           data.name = paste("sample size ", n, ", replicates ",
                             n.boot, sep = ""), estimate = media)
  class(e) <- "htest"
  e
}

