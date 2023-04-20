#-> Duda planteada por Paco

ejemplo <- function(...) {
 s<-sum(...)
 comp<-list(...)
 return(list(suma=s,componentes=comp))
}
x<-2
y<-c(2,3)
z<-1:10
ejemplo(x,y,z)
ejemplo(x,y,z)$componentes
ejemplo(x,y,z)$componentes[[1]]

ejemplo(x,y)
ejemplo(x,y)$componentes
ejemplo(x,y)$componentes[[2]]

#################################

#-> Duda planteada por Juanma

grafico<-function(x,...){
 options(warn=-1)			#ignorar warnings
 z<-(x-mean(x))/sd(x)
 graf<-plot(x,z,...)
 diferencia<-diff(x,...)
 return(c(graf,diferencia))
}

x <- cumsum(cumsum(1:10))
grafico(x,main="Tittle")
grafico(x,col="red",xlab="HOLA")
grafico(x,col="red",xlab="HOLA",differences = 1)
grafico(x,col="red",xlab="HOLA",differences = 2)
grafico(x,col="red",xlab="HOLA",differences = 3)