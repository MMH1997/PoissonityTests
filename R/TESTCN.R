#' Cn test
#' @export
#' @param x numeric variable
#' @param n.boot numeric variable

testCn = function(x,n.boot){
  Cn=function(z) {if (sum(z) == 0) {0}
    else {ecdfz=ecdf(z)
    f1=function(y) {(ecdfz(y) - ppois(y, mean(z)))*
        dpois(y,mean(z))}
    resf11=sum(f1(min(z):max(z)))
    Cn=length(z) * resf11
    return(Cn)
    }
  }
  pv = 0
  n = length(x)
  lambda.hat=mean(x)
  T.obs=Cn(x)
  for (i in 1:n.boot){
    x.boot=rpois(n,lambda.hat)
    T.boot=Cn(x.boot)
    pv=pv+as.numeric(T.boot>T.obs)
  }
  names(T.obs) <- "test statistic"
  media = mean(x)
  names(media) <- "mean"
  e = list(method = paste("Cn poissonity test",
                          sep = ""),
           statistic = T.obs, p.value = pv/n.boot,
           data.name = paste("sample size ", n, ", replicates ",
                             n.boot, sep = ""), estimate = media)
  class(e) <- "htest"
  return(e)

}

