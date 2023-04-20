# Ejercicio 1

set.seed(1)
x<-runif(100)

mx<-mean(x)
which.min(abs(x-mx))
x[58]

y<-x[x<mx]

x<-x[!(x<mx)]

A<-cbind(x,abs(x-mx))  

# Ejercicio 2
#1.
hatco<-read.table("hatco.txt", header = TRUE)
str(hatco)

#2.
factor(hatco$x8, levels=c(0,1), labels=c("pequeña", "grande"))

#3.
hatco$cliente<-as.character(hatco$cliente)
hatco$cliente

#4.
tapply(hatco$x8, hatco$x8, length)

hatco_peq<-hatco[hatco$x8==0,]
hatco_peq[sample(1:nrow(hatco_peq),1 ),]

hatco_gra<-hatco[hatco$x8==1,]
hatco_gra[sample(1:nrow(hatco_gra),1 ),]

#5.
mean(hatco$y)
mean(hatco[hatco$x8==0,"y"])
fid_peq<-mean(hatco_peq$y)
mean(hatco[hatco$x8==1,"y"])
fid_gra<-mean(hatco_gra$y)

#6.
nrow(hatco_peq[hatco_peq$y>fid_peq,])
nrow(hatco_gra[hatco_gra$y>fid_gra,])

#7.
hatco2 <- scale(Filter(is.numeric, hatco))
hatco2

# Ejercicio 3
pa <- function(n, a1, d){
  v<-a1+d*0:(n-1)
  return(list(v=v, suma=sum(v), producto=prod(v) ))
}

pa(5,2,3)

pa.2 <- function(n, a1, d, explicit=FALSE){
  v<-a1+d*0:(n-1)
  suma<-if(explicit) n*(a1+v[n])/2 else sum(v)
  producto<-if(explicit) d^n*gamma(a1/d+n)/gamma(a1/d) else prod(v)
    
  return(list(v=v, suma=suma, producto=producto ))
}

pa.2(5,2,3)
pa.2(5,2,3,TRUE)