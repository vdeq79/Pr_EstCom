#Autor: XuSheng Zheng

hatco<-read.csv('hatco2.csv', header = TRUE, as.is=NA)
plot(hatco[,c(6:13)])

mod1<-lm(fidelidad~velocidad+precio+flexprec+imgfabri+servconj+imgfvent+calidadp,hatco)
mod1

summary(mod1)
anova(mod1)

#x2
#p-valor=0.7461, no hay evidencias para rechazar H0, por lo tanto el precio parece no influir

#x3
#p-valor=1.56*10^(-12), se rechaza H0, la flexibilidad sí influye en la fidelidad

#x4
#p-valor=0.02585, se rechaza H0, la imagen del fabricante sí influye

#x5
#p-valor=0.05913, no se rechaza H0 aunque está cerca de ser rechazada, podemos ver que el servicio puede influir en la fidelidad

#x6
#p-valor=0.13311, no se rechaza H0, la imagen de fuerza de ventas parece no influir

#x7
#p-valor=0.11667, no se rechaza H0, la calidad parece no influir

#Beta_0
#p-valor=0.044, se rechaza para 5% de nivel de significación por lo que no podemos prescindir de este término. Para 1% no se rechaza H0 pero
#no nos lleva directamente a prescindir del término en el modelo

#Homocedasticidad
ri<-rstandard(mod1)
par(mfrow=c(2,4))
plot(mod1$fitted.values, ri )
abline(h=0,col=2)

for (i in 6:12){
  plot(hatco[,i], ri, xlab=names(hatco)[i])
  abline(h=0,col=2)
}

#Incorrelación
par(mfrow=c(1,1))
plot(hatco$empresa, ri)
abline(h=0,col=2)
library(lmtest)
dwtest(mod1)
#El p-valor es grande, así que no podemos rechazar H0, los errores son incorrelados

#Normalidad
ks.test(ri,pnorm)
#No se rechaza H0, los errores son normales
qqnorm(ri)
qqline(ri)
#Los puntos están aproximadamente alineados


#Linealidad
library(car)
crPlots(mod1)


#Identificación de datos anómalos e influyentes
which(abs(ri)>2.5)

Di<-cooks.distance(mod1)
hii<-hatvalues(mod1)
par(mfrow=c(1,2))
plot(hatco$empresa, Di)
plot(hatco$empresa, hii)

influenceIndexPlot(mod1)
#Datos influyentes -> 7 y 100, son también anómalos
#Datos aislados -> 22 y 55


#Eliminación de observaciones anómalas e influyentes
hatco<-hatco[-c(7,100),]
mod2<-lm(fidelidad~velocidad+precio+flexprec+imgfabri+servconj+imgfvent+calidadp,hatco)
summary(mod2)
summary(mod1)
#Ha mejorado considerablemente el ajuste


#Estudio de la multicolinealidad
R<-cor(hatco[,6:12])
ai<-eigen(R)$values # autovalores de R
sqrt(max(ai)/min(ai)) # el índice IC
vif(mod2)
#Existe un problema de multicolinealidad, hay VIF mayores de 10


#Selección de variables explicativas
step(mod2)
step(mod2, direction = 'both')
