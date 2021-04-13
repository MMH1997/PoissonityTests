#' Delta2 test
#' @export
#' @param x numeric variable
#' @param n.boot numeric variable
#
# testdelta2 = function(x,n.boot){
#   fdelta2=function(y) {
#     aux=outer(y,y,"+")
#     aux2=outer(aux,aux,"+")
#     num1=2-2^-aux2
#     den1=aux2+1
#     coc1=num1/den1
#     length(coc1)
#     sum2=mean(coc1)
#     aux=as.vector(outer(y,y,"+"))
#     num2=1
#     den2=aux+1
#     coc2=num2/den2
#     coc2
#     sum1=mean(coc2)
#     ff1=function(u,x){ # u y x son numeros
#       z=0:u;
#       yy=choose(u,z);
#       w=1/(z+x+1);
#       sal=sum(yy*w)*2^(-u); return(sal)}
#     ff2=function(x,u){ # x es un mnumero, u es un vector
#       sal=mean(unlist(lapply(u,ff1,x))); return(sal)}
#
#     suma3=mean(unlist(lapply(y,ff2,aux)))
#     sumatotal=sum1+sum2-2*suma3
#     return(sumatotal)
#   }
#   pv = 0
#   n = length(x)
#   lambda.hat=mean(x)
#   T.obs=fdelta2(x)
#   for (i in 1:n.boot){
#     x.boot=rpois(n,lambda.hat)
#     T.boot=fdelta2(x.boot)
#     pv=pv+as.numeric(T.boot>T.obs)
#   }
#   list(
#     "method" = "Delta2 test of Poisson distribution",
#     "data" = as.character(paste("Sample size",length(x), ";",
#                                 "replicates", n.boot, sep = " ")),
#     "test statistic" = T.obs,
#     "mean" = mean(x),
#     "index of Dispersion" = var(x)/mean(x),
#     "pvalue" = pv/n.boot)
# }
#



# testdelta2 = function(x,n.boot){
#   fdelta2=function(y) {
#     aux=outer(y,y,"+")
#     aux2=outer(aux,aux,"+")
#     num1=2-2^-aux2
#     den1=aux2+1
#     coc1=num1/den1
#     length(coc1)
#     sum2=mean(coc1)
#     aux=as.vector(outer(y,y,"+"))
#     num2=1
#     den2=aux+1
#     coc2=num2/den2
#     coc2
#     sum1=mean(coc2)
#     ff1=function(u,x){ # u y x son numeros
#       z=0:u;
#       yy=choose(u,z);
#       w=1/(z+x+1);
#       sal=sum(yy*w)*2^(-u); return(sal)}
#     ff2=function(x,u){ # x es un mnumero, u es un vector
#       sal=mean(unlist(lapply(u,ff1,x))); return(sal)}
#
#     suma3=mean(unlist(lapply(y,ff2,aux)))
#     sumatotal=sum1+sum2-2*suma3
#     return(sumatotal)
#   }
#   pv = 0
#   n = length(x)
#   lambda.hat=mean(x)
#   T.obs=fdelta2(x)
#   for (i in 1:n.boot){
#     x.boot=rpois(n,lambda.hat)
#     T.boot=fdelta2(x.boot)
#     pv=pv+as.numeric(T.boot>T.obs)
#   }
#   names(T.obs) <- "test statistic"
#   media = mean(x)
#   names(media) <- "mean"
#   e = list(method = paste("Delta2",
#                           sep = ""),
#            statistic = T.obs, p.value = pv/n.boot,
#            data.name = paste("sample size ", n, ", replicates ",
#                              n.boot, sep = ""), estimate = media)
#   class(e) <- "htest"
#   e
# }


testdelta2 = function(x,n.boot){
  fdelta2b=function(z){
    fdelta2b1 =function(y,t) {
      ((1/length(y)) * sum(t^y) -
         ((1/length(y)) * sum(((t+1)/2)^y))^2)^2
    }
    i =integrate(f = Vectorize(fdelta2b1,vectorize.args = 't'), lower = 0, upper = 1,
                 y = z)
    i$value
  }
  pv = 0
  n = length(x)
  lambda.hat=mean(x)
  T.obs=fdelta2b(x)
  for (i in 1:n.boot){
    x.boot=rpois(n,lambda.hat)
    T.boot=fdelta2b(x.boot)
    pv=pv+as.numeric(T.boot>T.obs)
  }
  names(T.obs) <- "test statistic"
  media = mean(x)
  names(media) <- "mean"
  e = list(method = paste("Delta2 poissonity test",
                          sep = ""),
           statistic = T.obs, p.value = pv/n.boot,
           data.name = paste("sample size ", n, ", replicates ",
                             n.boot, sep = ""), estimate = media)
  class(e) <- "htest"
  e
}
