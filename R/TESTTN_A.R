#' Tn,a test
#' @export
#' @param x numeric variable
#' @param n.boot numeric variable

testTn_a = function(x,a,n.boot){
testTn_a2 = function(x,a,n.boot){
  Tn_a2=function(z,a) {
    f2noder= function(t) {mean(z) * mean(sum(t^z))}
    f2der= function(t) {mean(sum(t^z*z/t))}
    f3_a=function(t,b) {((f2noder(t) - f2der(t)) ^2) * t^b}
    b_a= integrate(Vectorize(f3_a, vectorize.args = 't'), lower=0, upper=1, b = a)
    b_aSol=as.numeric(b_a[1])
    return(b_aSol/length(z))
  }

  pv = 0
  n = length(x)
  lambda.hat=mean(x)
  T.obs=Tn_a2(x,a)
  for (i in 1:n.boot){
    x.boot=rpois(n,lambda.hat)
    T.boot=Tn_a2(x.boot,a)
    pv=pv+as.numeric(T.boot>T.obs)
  }
  names(T.obs) <- "test statistic"
  media = mean(x)
  names(media) <- "mean"
  e = list(method = paste("Tn,a poissonity test",
                      sep = ""),
           statistic = T.obs, p.value = pv/n.boot,
       data.name = paste("sample size ", n, ", replicates ",
                         n.boot, sep = ""), estimate = media)
  class(e) <- "htest"
  e
}







testTn_a1 = function(x,a,n.boot){
  Tna1 = function(x,a){
    aux = outer(x,x,"+")
    aux1 = outer(x,x,"*")
    num1 = mean(x)^2
    den1 = aux + a + 1
    coc1 = sum(num1/den1)
    num2 = mean(x)*(aux)
    den2 = aux + a
    coc2 = sum(num2/den2)
    num3 = aux1
    den3 = aux + a - 1
    coc3 = sum(num3/den3)
    (coc1 - coc2 + coc3)/length(x)
  }

  pv = 0
  n = length(x)
  lambda.hat=mean(x)
  T.obs=Tna1(x,a)
  for (i in 1:n.boot){
    x.boot=rpois(n,lambda.hat)
    T.boot=Tna1(x.boot,a)
    pv=pv+as.numeric(T.boot>T.obs)
  }
  names(T.obs) <- "test statistic"
  media = mean(x)
  names(media) <- "mean"
  e = list(method = paste("Tn,a poissonity test",
                          sep = ""),
           statistic = T.obs, p.value = pv/n.boot,
           data.name = paste("sample size ", n, ", replicates ",
                             n.boot, sep = ""), estimate = media)
  class(e) <- "htest"
  e
}
if (length(x) > 200) {
  return(testTn_a2(x,a,n.boot))
}
else {return(testTn_a1(x,a,n.boot))}
}
