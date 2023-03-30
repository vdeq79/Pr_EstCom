#Autor: XuSheng Zheng

#----------------------------------------------------------------------------------
#Optimización directa
muestra<-scan(text = 
"0.97 1.01 1.06 0.99 1.13 1.08 0.98 0.93 1.03 0.97
0.83 1.07 1.06 0.49 0.84 1.07 1.01 0.81 1.01 0.90
0.93 0.98 1.01 0.77 0.90 0.93 0.88 1.08 1.03 1.09
0.88 0.91 0.89 0.98 0.91 0.90 0.85 0.91 0.90 0.81
1.01 0.91 1.00 1.04 0.81 0.86 0.94 0.69 1.01 0.86")

logl<-function(theta){
  a<-theta[1]
  rho<-theta[2]
  
  #dweibull es la función de densidad de la distribución Weibull
  l<-sum(log(dweibull(x = muestra, shape = a, scale = rho)))
  return(-l)
}

#Elegimos valores iniciales arbitrarios
r0<-a0<-1
res<-optim(par=c(a0,r0),fn=logl)
res$par

#Ha necesitado 103 llamadas a la función
res$counts

#----------------------------------------------------------------------------------
#Resolución de las ecuaciones normales
f<-function(a) sum(muestra^a*log(muestra))/sum(muestra^a)-1/a-mean(log(muestra))
res<-uniroot(f,c(0.1,100))
a<-res$root
rho<-mean(muestra^a)^(1/a)
a;rho

#----------------------------------------------------------------------------------
#Usando fitdist 
#install.packages("fitdistrplus")
library(fitdistrplus)
#fitdistr reconece la distribución Weibull
fitdistr(muestra, "weibull")

#Los resultados con los 3 métodos son parecidos con ligeras diferencias