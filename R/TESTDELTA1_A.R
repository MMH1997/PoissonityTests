#' Delta1,a test
#' @export
#' @param x numeric variable
#' @param n.boot numeric variable

testdelta1_a = function(x,a,n.boot){
  fdelta1_a=function(z, c){
    fdelta1b1 =function(y,t, b) {
      ((1/length(y)) * sum(t^y) -
        ((1/length(y)) * sum(((t+1)/2)^y))^2) * t^b
    }
    i =integrate(f = Vectorize(fdelta1b1,vectorize.args = 't'), lower = 0, upper = 1,
                 y = z, b = c)
    i$value
  }
  pv = 0
  n = length(x)
  lambda.hat=mean(x)
  T.obs=fdelta1_a(x,a)
  for (i in 1:n.boot){
    x.boot=rpois(n,lambda.hat)
    T.boot=fdelta1_a(x.boot,a)
    pv=pv+as.numeric(T.boot>T.obs)
  }
  names(T.obs) <- "test statistic"
  media = mean(x)
  names(media) <- "mean"
  e = list(method = paste("Delta 1,a poissonity test",
                          sep = ""),
           statistic = T.obs, p.value = pv/n.boot,
           data.name = paste("sample size ", n, ", replicates ",
                             n.boot, sep = ""), estimate = media)
  class(e) <- "htest"
  e
}



