#' Delta2,a test
#' @export
#' @param x numeric variable
#' @param n.boot numeric variable

testdelta2_a = function(x,n.boot,a){
  fdelta2_a=function(z,c){
    fdelta2b1 =function(y,t,b) {
      (((1/length(y)) * sum(t^y) -
         ((1/length(y)) * sum(((t+1)/2)^y))^2)^2) * t^b
    }
    i =integrate(f = Vectorize(fdelta2b1,vectorize.args = 't'), lower = 0, upper = 1,
                 y = z, b = c)
    i$value
  }
  pv = 0
  n = length(x)
  lambda.hat=mean(x)
  T.obs=fdelta2_a(x,a)
  for (i in 1:n.boot){
    x.boot=rpois(n,lambda.hat)
    T.boot=fdelta2_a(x.boot,a)
    pv=pv+as.numeric(T.boot>T.obs)
  }
  names(T.obs) <- "test statistic"
  media = mean(x)
  names(media) <- "mean"
  e = list(method = paste("Delta2,a poissonity test",
                          sep = ""),
           statistic = T.obs, p.value = pv/n.boot,
           data.name = paste("sample size ", n, ", replicates ",
                             n.boot, sep = ""), estimate = media)
  class(e) <- "htest"
  e
}




