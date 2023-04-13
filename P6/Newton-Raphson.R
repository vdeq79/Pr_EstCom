f<-function(x) x^2-5
curve(f,0,10)
abline(h=0,col=2)
f.prima<-function(x) 2*x
x0<-2
x1<-x0-f(x0)/f.prima(x0); x1
x2<-x1-f(x1)/f.prima(x1); x2
x3<-x2-f(x2)/f.prima(x2); x3
x4<-x3-f(x3)/f.prima(x3); x4

f<-function(x) x^3-2*x-5
curve(f,-5,5)
abline(h=0,col=2)
f.prima<-function(x) 3*x*2-2


x0<-2
x1<-x0-f(x0)/f.prima(x0); x1
x2<-x1-f(x1)/f.prima(x1); x2
x3<-x2-f(x2)/f.prima(x2); x3
x4<-x3-f(x3)/f.prima(x3); x4


x0<--1
x1<-x0-f(x0)/f.prima(x0); x1
x2<-x1-f(x1)/f.prima(x1); x2
x3<-x2-f(x2)/f.prima(x2); x3
x4<-x3-f(x3)/f.prima(x3); x4


algoritmo.NR<-function(f, f.prima, x0, tol, nmax, dibuja=TRUE){
  if(!is.function(f) | !is.function(f.prima))
    stop('Los dos primeros argumentos deben ser funciones')
  
  if(dibuja){
    curve(f,x0-5,x0+5)
    abline(h=0,col=2)
  }

  xn<-x0
  for (n in 1:nmax){
    ant <- xn
    xn <- ant-f(ant)/f.prima(ant)
    
    print(xn)
    if(abs(xn-ant)<tol | is.nan(xn) | is.na(xn) ) break
  }
  
  if (n==nmax)
    warning('Se ha alcanzado el máximo de iteraciones')

  return(list(raiz=xn, nivel_tol = abs(xn-ant), niter=n ))  
}

algoritmo.NR(f,f.prima,2,0.001,100)


install.packages("numDeriv")
library(numDeriv)

algoritmo2.NR<-function(f, f.prima, x0, tol, nmax, dibuja=TRUE){
  
    if (missing(f.prima))
      f.prima<-function(x) genD(func=f, x=x)$D[1] 
  
    if(!is.function(f) | !is.function(f.prima))
    stop('Los dos primeros argumentos deben ser funciones')
  
  if(dibuja){
    curve(f,x0-5,x0+5)
    abline(h=0,col=2)
  }
  
  xn<-x0
  for (n in 1:nmax){
    ant <- xn
    xn <- ant-f(ant)/f.prima(ant)
    
    print(xn)
    if(abs(xn-ant)<tol | is.nan(xn) | is.na(xn) ) break
  }
  
  if (n==nmax)
    warning('Se ha alcanzado el máximo de iteraciones')
  
  return(list(raiz=xn, nivel_tol = abs(xn-ant), niter=n ))  
}

f<-function(x) x^2-5
f.prima <- function(x) 2*x
algoritmo.NR(f,f.prima,2,0.001,100)
algoritmo2.NR(f,x0=2,tol=0.001,nmax=100)
uniroot(f,c(0,5))

f<-function(x) x^3-2*x-5
f.prima <- function(x) 3*x^2-2
algoritmo.NR(f,f.prima,2,0.001,100)
algoritmo2.NR(f,x0=2,tol=0.001,nmax=100)
uniroot(f,c(0,5))


f<-function(x) exp(2*x)-x-6
f.prima <- function(x) exp(2*x)*2-1
algoritmo.NR(f,f.prima,2,0.001,100)
algoritmo2.NR(f,x0=2,tol=0.001,nmax=100)
uniroot(f,c(0,5))


  