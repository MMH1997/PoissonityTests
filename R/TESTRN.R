#' Rn test
#' @export
#' @param x numeric variable
#' @param n.boot numeric variable

testRn = function(x,n.boot) {
testRn2 = function(x,n.boot){
Rn2 = function(z){
  Gn = function(t,y){
  (sqrt(length(y))*((1/length(y)) * sum(t^y) -
     exp(mean(y)*(t-1))))^2
  }
    i = integrate(f = Vectorize(Gn,vectorize.args = 't'), lower = 0, upper = 1,
             y = z)
    i$value
}
pv = 0
n = length(x)
lambda.hat=mean(x)
T.obs=Rn2(x)
for (i in 1:n.boot){
  x.boot=rpois(n,lambda.hat)
  T.boot=Rn2(x.boot)
  pv=pv+as.numeric(T.boot>T.obs)
}
names(T.obs) <- "test statistic"
media = mean(x)
names(media) <- "mean"
e = list(method = paste("Rn poissonity test",
                        sep = ""),
         statistic = T.obs, p.value = pv/n.boot,
         data.name = paste("sample size ", n, ", replicates ",
                           n.boot, sep = ""), estimate = media)
class(e) <- "htest"
e
}


testRn1 = function(x,n.boot){
  Rn1 = function(z){
    aux = outer(z,z, "+") + 1
    t = sum(1/aux)
    coc1 = t/length(z)
    r = 0
    for (i in 1:length(z)) {
      r = r + ((exp(-mean(z))/((-mean(z))^(z[i] + 1))) * as.numeric(gammainc(a = (z[i] + 1), x = (-mean(z)))[1]))
    }
    coc2 = 2*r
    num3 = length(z) * exp(-2*mean(z))
    den3 = (-2 * mean(z))
    coc3 = (num3/den3)* as.numeric(gammainc(a = 1, x = (-2*mean(z)))[1])
    coc1 - coc2 + coc3
  }
  pv = 0
  n = length(x)
  lambda.hat=mean(x)
  T.obs=Rn1(x)
  for (i in 1:n.boot){
    x.boot=rpois(n,lambda.hat)
    T.boot=Rn1(x.boot)
    pv=pv+as.numeric(T.boot>T.obs)
  }
  names(T.obs) <- "test statistic"
  media = mean(x)
  names(media) <- "mean"
  e = list(method = paste("Rn poissonity test",
                          sep = ""),
           statistic = T.obs, p.value = pv/floor(n.boot),
           data.name = paste("sample size ", n, ", replicates ",
                             n.boot, sep = ""), estimate = media)
  class(e) <- "htest"
  e
}

if (length(x) > 10) {
  return(testRn2(x,n.boot))
}
else {return(testRn1(x,n.boot))}
}



