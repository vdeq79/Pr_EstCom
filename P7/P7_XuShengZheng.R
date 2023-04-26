#Autor: XuSheng Zheng

#----------------------------------------------------------------------------------
#Ejercicio propuesto
#1.
air<-airquality
xs<-seq(min(air$Ozone,na.rm=TRUE), max(air$Ozone,na.rm=TRUE),by=10)
xs<-c(xs,max(air$Ozone,na.rm=TRUE))
hist(air$Ozone,breaks=xs,freq=FALSE)


#2.
curve(dnorm(x,mean=mean(air$Ozone, na.rm=TRUE),sd=sd(air$Ozone, na.rm=TRUE)),add=TRUE,col='blue')
#Mediante la representación se puede apreciar que los datos no se podrían modelizar mediante una normal 

#3.
qqnorm(air$Ozone)
#Los puntos no aparecen alineados, luego da señal de que no es normal

ks.test(air$Ozone, pnorm, mean=mean(air$Ozone, na.rm=TRUE), sd=sd(air$Ozone, na.rm=TRUE))
#El test de Kolmogorov-Smirnov produce un p-valor menor que 0.05, podemos rechar H0 con dicho nivel de significación 

shapiro.test(air$Ozone)
#El test de Shapiro-Wilk produce un p-valor mucho menor, lo cuál nos afirma de que no sería adecuado modelizar con una normal

#4.
boxplot(air$Ozone)
#No tiene muchos outliers y existen datos con valores mayores que el resto que hacen que el tercer cualtil sea mayor

#5.
boxplot(air$Ozone~air$Month)
#Se puede apreciar que los meses de julio y agosto son los meses con mayor contaminación

#6.
plot(air$Wind, air$Ozone)
#Se puede apreciar en que cuando menos viento hay, existe una mayor contaminación

plot(air$Temp, air$Ozone)
#De mismo modo, cuando mayor sea la temperatura, más contaminación hay