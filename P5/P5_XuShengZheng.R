#Autor: XuSheng Zheng

#----------------------------------------------------------------------------------
muestra<-scan()

logl<-function(theta)
{
  a<-theta[1]
  b<-theta[2]
  l<-sum(log(dgamma(x=muestra,shape=a,scale=b)))
  return(-l)
}

logl(c(1,1))

b0<-a0<-1
res<-optim(par=c(a0,b0),fn=logl)
res$par
res

#----------------------------------------------------------------------------------
install.packages("Rsolnp")
library(Rsolnp)

res<-NULL
res<-solnp(pars=c(a0,b0),fun=logl,LB=c(0,0))
res$pars

res<-NULL
a0<-var(muestra)/mean(muestra)
b0<-mean(muestra)/a0
res<-solnp(pars=c(a0,b0),fun=logl,LB=c(0,0))
res$pars

#----------------------------------------------------------------------------------
install.packages("maxLik")
library(maxLik)

logl2<-function(theta) -logl(theta)
maxLik(logl2,start=c(1,1))

a0<-var(muestra)/mean(muestra)
b0<-mean(muestra)/a0
maxLik(logl2,start=c(a0,b0))

A<-matrix(c(1,0,0,1),2)
B<-c(0,0)
maxLik(logl2,start=c(1,1),constraints=list(ineqA=A,ineqB=B))

#----------------------------------------------------------------------------------
f<-function(a) log(a)-digamma(a)- log(mean(muestra))+mean(log(muestra))
res<-uniroot(f,c(0.1,100))
res
b<-mean(muestra)/res$root
b

#----------------------------------------------------------------------------------
#Ejercicio 1
medias <- function(x){
  if(!is.numeric(x))
    stop("El argumento debe ser numérico")
  
  x<-x[!is.na(x)]
  geo<-if(min(x)>0) exp(mean(log(x))) else if (min(x)==0) 0 else NA
  arm<-if(all(x!=0)) 1/mean(1/x) else NA
  
  if(is.na(geo))
    warning("No se ha podido calcular la media geométrica")
  
  if(is.na(arm))
    warning("No se ha podido calcular la media armónica")
  
  return(list(media.aritmética=mean(x), media.geométrica=geo, media.armónica=arm))
}
medias(1:10)
medias(c(1:10, NA))
medias(0:10)
medias(-1:10)

#----------------------------------------------------------------------------------
#Ejercicio 2
mediana<-function(x){
  if(!is.numeric(x))
    stop("El argumento debe ser numérico")
  
  x<-x[!is.na(x)]
  x<-sort(x)
  
  n<-length(x)
  mediana <- x[n%/%2+1]
  if(n%%2==0)
    mediana <- (mediana+x[n/2])/2
  
  return(mediana)
}

mediana(1:5)
mediana(1:6)
mediana(c(1:6,NA))
mediana("hola")

#----------------------------------------------------------------------------------
#Ejercicio 3
#----------------------------------------------------------------------------------
#a)
cuartiles<-function(x){
  if(!is.numeric(x))
    stop("El argumento debe ser numérico")
  
  x<-x[!is.na(x)]
  Q2 <- mediana(x)

}

