#' Tn test
#' @export
#' @param x numeric variable
#' @param n.boot numeric variable

testTn = function(x,n.boot) {
  options(warn = -1)
testTn1 = function(x,n.boot){
  Tn1=function(z) {auxsuma=outer(z,z,"+")
  auxmult=outer(z,z,"*")
  tt=table(z)

  dd=as.numeric(names(tt))+1
  ni=rep(0,(max(z)+1))
  ni[dd]=as.numeric(tt)



  mauxsum=auxsuma[auxmult>0]
  mauxmult=auxmult[auxmult>0]



  TN=(1/length(z))*(sum(mean(z)^2/(auxsuma+1))+sum(mauxmult/(mauxsum-1))) - mean(z)*(length(z)-(1/length(z))*(ni[1])^2)
  return(TN)
  }

  pv = 0
  n = length(x)
  lambda.hat=mean(x)
  T.obs=Tn1(x)
  for (i in 1:n.boot){
    x.boot=rpois(n,lambda.hat)
    T.boot=Tn1(x.boot)
    pv=pv+as.numeric(T.boot>T.obs)
  }
  names(T.obs) <- "test statistic"
  media = mean(x)
  names(media) <- "mean"
  e = list(method = paste("Tn poissonity test",
                          sep = ""),
           statistic = T.obs, p.value = pv/n.boot,
           data.name = paste("sample size ", n, ", replicates ",
                             n.boot, sep = ""), estimate = media)
  class(e) <- "htest"
  e
}



testTn2 = function(x,n.boot){
  Tn2=function(z) {
    f2noder= function(t) {mean(z) * mean(sum(t^z))}
    f2der= function(t) {mean(sum(t^z*z/t))}
    f3_a=function(t,b) {((f2noder(t) - f2der(t)) ^2)}
    b_a= integrate(Vectorize(f3_a, vectorize.args = 't'), lower=0, upper=1)
    b_aSol=as.numeric(b_a[1])
    return(b_aSol/length(z))
  }

  pv = 0
  n = length(x)
  lambda.hat=mean(x)
  T.obs=Tn2(x)
  for (i in 1:n.boot){
    x.boot=rpois(n,lambda.hat)
    T.boot=Tn2(x.boot)
    pv=pv+as.numeric(T.boot>T.obs)
  }
  names(T.obs) <- "test statistic"
  media = mean(x)
  names(media) <- "mean"
  e = list(method = paste("Tn poissonity test",
                          sep = ""),
           statistic = T.obs, p.value = pv/n.boot,
           data.name = paste("sample size ", n, ", replicates ",
                             n.boot, sep = ""), estimate = media)
  class(e) <- "htest"
  e
}

if (x > 140) {
  return(testTn2(x, n.boot))
}
else {return(testTn1(x,n.boot))}
}

