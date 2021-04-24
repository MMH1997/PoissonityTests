#' Cn* test
#' @export
#' @param x numeric variable
#' @param n.boot numeric variable

testCn_ = function(x,n.boot){
  options(warn = -1)
    Cn_=function(z) {if (sum(z) == 0) {0}
    else {ecdfz=ecdf(z)
    f1=function(y) {(ecdfz(y) - ppois(y, mean(z)))^2*
    (sum(z == y)/length(z))}
    resf11=sum(f1(min(z):max(z)))
    Cn_=length(z) * resf11
    }
  }
  pv = 0
  n = length(x)
  lambda.hat=mean(x)
  T.obs=Cn_(x)
  for (i in 1:n.boot){
    x.boot=rpois(n,lambda.hat)
    T.boot=Cn_(x.boot)
    pv=pv+as.numeric(T.boot>T.obs)
  }
  names(T.obs) <- "test statistic"
  media = mean(x)
  names(media) <- "mean"
  e = list(method = paste("Cn* poissonity test",
                          sep = ""),
           statistic = T.obs, p.value = pv/n.boot,
           data.name = paste("sample size ", n, ", replicates ",
                             n.boot, sep = ""), estimate = media)
  class(e) <- "htest"
  return(e)

}




