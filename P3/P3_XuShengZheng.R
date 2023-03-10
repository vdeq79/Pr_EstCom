#Autor: XuSheng Zheng

#----------------------------------------------------------------------------------
#Ejercicio 1
lista<-list(x1=1:5,x2=2:6,x3=3:7)
lista

#----------------------------------------------------------------------------------
#a)
x<-runif(10)
lista[[4]]<-x
lista

#----------------------------------------------------------------------------------
#b)
y<-rnorm(10)
lista[[5]]<-y
lista

#----------------------------------------------------------------------------------
#c)
lapply(lista, sum)      #Devuelve una lista con los mismos nombres
sapply(lista, sum)      #Devuelve un vector con los mismos nombres

#----------------------------------------------------------------------------------
#d)
reg<-lm(y~x)
reg
class(reg)
typeof(reg)

#----------------------------------------------------------------------------------
#d)
sapply(reg, typeof)

#----------------------------------------------------------------------------------
#f)
M<-cbind(reg$residuals,reg$fitted.values,x,y)
colnames(M)<-c('residuals', 'fitted.values', 'x', 'y')
M

#----------------------------------------------------------------------------------
#Ejercicio 2
xi<-c(1.2,1.8,2.2,2.5,1.1)
yi<-c(15,18,10,12,16)
ni<-c(12,23,5,9,11)
datos<-data.frame(xi,yi,ni)
datos

#----------------------------------------------------------------------------------
#a)
datos[3]
n<-sum(datos[3])
n

#----------------------------------------------------------------------------------
#b)
mx<-sum(datos[1]*datos[3])/n; mx
my<-sum(datos[2]*datos[3])/n; my
sx2<-sum(datos[3]*(datos[1]-mx)^2)/(n-1); sx2
sy2<-sum(datos[3]*(datos[2]-my)^2)/(n-1); sy2

#----------------------------------------------------------------------------------
#c)
datos.n<-data.frame(xi=rep(datos[,1],datos[,3]),yi=rep(datos[,2],datos[,3]))
datos.n

#----------------------------------------------------------------------------------
#d)
mean(datos.n[,1])
mean(datos.n[,2])
var(datos.n[,1])
var(datos.n[,2])

#----------------------------------------------------------------------------------
#e)
datos2<-transform(datos.n, xzi=(xi-mx)/sqrt(sx2), yzi=(yi-my)/sqrt(sy2))
mean(datos2[,3])
var(datos2[,3])
mean(datos2[,4])
var(datos2[,4])

datos3<-within(datos.n,{
  xzi<-(xi-mx)/sqrt(sx2) 
  yzi<-(yi-my)/sqrt(sy2)
})
mean(datos3[,3])
var(datos3[,3])
mean(datos3[,4])
var(datos3[,4])

#----------------------------------------------------------------------------------
#Ejercicio 3
help("ChickWeight")

#----------------------------------------------------------------------------------
#a)
head(ChickWeight,5)
tail(ChickWeight,3)

#----------------------------------------------------------------------------------
#b)
str(ChickWeight)

#----------------------------------------------------------------------------------
#c)
summary(ChickWeight)

#----------------------------------------------------------------------------------
#d)
ChickWeight$Diet
peso.dieta<-tapply(ChickWeight$weight, ChickWeight$Diet, summary)
class(peso.dieta)

#----------------------------------------------------------------------------------
#e)
peso.dieta2<-data.frame(t(sapply(peso.dieta, `[`)))
peso.dieta2

#----------------------------------------------------------------------------------
#f)
peso.dieta2<- aggregate(ChickWeight$weight,by=list(ChickWeight$Diet),summary)
peso.dieta2
class(peso.dieta2)          

#----------------------------------------------------------------------------------
#g)
Chick100<-ChickWeight[sample(nrow(ChickWeight),100),]

#----------------------------------------------------------------------------------
#h)
colnames(Chick100)
Chick100[,sample(ncol(Chick100),ncol(Chick100))]

#----------------------------------------------------------------------------------
#i)
sort(colnames(Chick100))
Chick100[,sort(colnames(Chick100))]

#----------------------------------------------------------------------------------
#j)
Chick100[order(Chick100$Diet),]
Chick100[order(Chick100$Diet,Chick100$weight),]

#----------------------------------------------------------------------------------
#k)
new<-Chick100[order(Chick100$weight,decreasing = TRUE),]
new[!duplicated(new$Diet),]
