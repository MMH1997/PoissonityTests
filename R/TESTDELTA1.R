#' Delta1 test
#' @export
#' @param x numeric variable
#' @param n.boot numeric variable


testdelta1 = function(x,n.boot){
testdelta1b = function(x,n.boot){
    fdelta1b=function(z) {
      aux=outer(z,z,"+")
      num= 2-2^(-aux)
      den=aux+1
      coc=num/den
      mean(coc)
      Delta1Clas=mean(1/(z+1))-mean(coc)
      Delta1Clas
    }
    pv = 0
    n = length(x)
    lambda.hat=mean(x)
    T.obs=fdelta1b(x)
    for (i in 1:n.boot){
      x.boot=rpois(n,lambda.hat)
      T.boot=fdelta1b(x.boot)
      pv=pv+as.numeric(T.boot>T.obs)
    }
    names(T.obs) <- "test statistic"
    media = mean(x)
    names(media) <- "mean"
    e = list(method = paste("Delta 1 poissonity test",
                            sep = ""),
             statistic = T.obs, p.value = pv/n.boot,
             data.name = paste("sample size ", n, ", replicates ",
                               n.boot, sep = ""), estimate = media)
    class(e) <- "htest"
    e
  }
testdelta1c = function(x,n.boot){
    fdelta1c=function(z){
      fdelta1c1 =function(y,t) {
        (1/length(y)) * sum(t^y) -
          ((1/length(y)) * sum(((t+1)/2)^y))^2
      }
      i =integrate(f = Vectorize(fdelta1c1,vectorize.args = 't'), lower = 0, upper = 1,
                   y = z)
      i$value
    }
    pv = 0
    n = length(x)
    lambda.hat=mean(x)
    T.obs=fdelta1c(x)
    for (i in 1:n.boot){
      x.boot=rpois(n,lambda.hat)
      T.boot=fdelta1c(x.boot)
      pv=pv+as.numeric(T.boot>T.obs)
    }
    names(T.obs) <- "test statistic"
    media = mean(x)
    names(media) <- "mean"
    e = list(method = paste("Delta 1 poissonity test",
                            sep = ""),
             statistic = T.obs, p.value = pv/n.boot,
             data.name = paste("sample size ", n, ", replicates ",
                               n.boot, sep = ""), estimate = media)
    class(e) <- "htest"
    e
  }
if (length(x) > 60) {
  return(testdelta1b(x,n.boot))
}
else {return(testdelta1c(x,n.boot))}
}

