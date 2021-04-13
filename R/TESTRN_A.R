#' Rn,a test
#' @export
#' @param x numeric variable
#' @param n.boot numeric variable

testRna = function(x,a,n.boot){

testRna1 = function(x,a,n.boot){
  Rna1 = function(z,c){
    Gna = function(t,y,b){
      (sqrt(length(y))*((1/length(y)) * sum(t^y) -
                          exp(mean(y)*(t-1))))^2 * t^b
    }
    i = integrate(f = Vectorize(Gna,vectorize.args = 't'), lower = 0, upper = 1,
                  y = z, b = c)
    i$value
  }
  pv = 0
  n = length(x)
  lambda.hat=mean(x)
  T.obs=Rna1(x,a)
  for (i in 1:n.boot){
    x.boot=rpois(n,lambda.hat)
    T.boot=Rna1(x.boot,a)
    pv=pv+as.numeric(T.boot>T.obs)
  }
  names(T.obs) <- "test statistic"
  media = mean(x)
  names(media) <- "mean"
  e = list(method = paste("Rn,a poissonity test",
                          sep = ""),
           statistic = T.obs, p.value = pv/n.boot,
           data.name = paste("sample size ", n, ", replicates ",
                             n.boot, sep = ""), estimate = media)
  class(e) <- "htest"
  e
}





testRna2 = function(x,a,n.boot){
  Rna2 = function(z,b){
    aux = outer(z,z, "+") + b + 1
    t = sum(1/aux)
    coc1 = t/length(z)
    r = 0
    for (i in 1:length(z)) {
      r = r + ((exp(-mean(z))/((-mean(z))^(z[i] + b + 1))) * as.numeric(gammainc(a = (z[i] + b + 1), x = (-mean(z)))[1]))
    }
    coc2 = 2*r
    num3 = length(z) * exp(-2*mean(z))
    den3 = (-2 * mean(z))^(b + 1)
    coc3 = (num3/den3)* as.numeric(gammainc(a = (b + 1), x = (-2*mean(z)))[1])
    coc1 - coc2 + coc3
  }
  pv = 0
  n = length(x)
  lambda.hat=mean(x)
  T.obs=Rna2(x,a)
  for (i in 1:n.boot){
    x.boot=rpois(n,lambda.hat)
    T.boot=Rna2(x.boot,a)
    pv=pv+as.numeric(T.boot>T.obs)
  }
  names(T.obs) <- "test statistic"
  media = mean(x)
  names(media) <- "mean"
  e = list(method = paste("Rn,a poissonity test",
                          sep = ""),
           statistic = T.obs, p.value = pv/n.boot,
           data.name = paste("sample size ", n, ", replicates ",
                             n.boot, sep = ""), estimate = media)
  class(e) <- "htest"
  e
}

if (length(x) > 10) {
  return(testRna1(x,a,n.boot))
}
else {return(testRna2(x,a,n.boot))}
}
