#Autor: XuSheng Zheng

#----------------------------------------------------------------------------------
#Apartado 1

A<-matrix(1:9,3,3)
x<-1:3

A%*%x
class(A%*%x)
#x es tratado como una matriz en el producto y el resultado es una matriz.


A%*%t(x)
#Argumentos incompatibles para el producto, la función t devuelve una matriz. En este caso, t(x) es una matriz 1x3.


x%*%A
class(x%*%A)
#A diferencia de la sentencia anterior, x es un vector (no es una matriz 3x1),
#luego R lo convierte en la matriz conveniente para realizar el producto.


t(x)%*%A
class(t(x)%*%A)
#En este caso las dimensiones sí concuerdan.


t(x)%*%x
class(t(x)%*%x)
#El resultado en este caso es una matriz y no un escalar.


#----------------------------------------------------------------------------------
#Apartado 2

solve(2,2)
#2x=2 -> x=1


A<-matrix(c(3,1,4,2),2,2)
b<-c(12,8)
solve(A,b)
# 3x+4y=12
#  x+2y=8
# x=-4, y=6


solve(A,diag(2))
# Puesto que diag(2)=matriz identidad de dimensión 2. Calcula la matriz X tal que AX=I, es decir, X=A^(-1).
solve(A)


#----------------------------------------------------------------------------------
#2.1
A<-matrix(c(10,7,8,7,7,5,6,5,8,6,10,9,7,5,9,10),4,4)
b<-c(32,23,33,31)
solve(A,b)
#El resultado es el vector con todos sus componentes 1


b2<-b+0.05
solve(A,b2)
#Aunque el cambio es pequeño, modifica el resultado


b3<-b+0.1
solve(A,b3)
#El resultado cambia aún más

kappa(A,exact = TRUE)
rcond(A,exact = TRUE)
#Utilizo exact para obtener los resultados exactos. La matriz está muy cerca de ser singualr, el sistema está mal condicionado

eigen(A)
k<-abs(max(eigen(A)$values)/min(eigen(A)$values))
k                      #Coincide con kappa
1/k                    #No coincide del todo, posiblemente por falta de precisión
 

#----------------------------------------------------------------------------------
#Apartado 3

#----------------------------------------------------------------------------------
#3.2
n<-5
set.seed(22)
x<-rnorm(n)

y<-1+x+rnorm(n,0,0.1)     #Creamos y
X<-cbind(1,x)             #Creamos X
X

inv<-solve(crossprod(X))
inv

beta<-inv %*% t(X) %*% y
beta
#El valor del estamador se acerca bastante al valor verdadero

curve(1+x,-3,3)
points(x,y)
curve(beta[1]+beta[2]*x ,add = TRUE, col=2)

#----------------------------------------------------------------------------------
#Repetimos con n=50
n<-50
set.seed(22)
x<-rnorm(n)

y<-1+x+rnorm(n,0,0.1)     #Creamos y
X<-cbind(1,x)             #Creamos X
X

inv<-solve(crossprod(X))
inv

beta<-inv %*% t(X) %*% y
beta
#El valor del estamador se acerca bastante al valor verdadero

curve(1+x,-3,3)
points(x,y)
curve(beta[1]+beta[2]*x ,add = TRUE, col=2)

#----------------------------------------------------------------------------------
#Repetimos con n=500
n<-500
set.seed(22)
x<-rnorm(n)

y<-1+x+rnorm(n,0,0.1)     #Creamos y
X<-cbind(1,x)             #Creamos X
X

inv<-solve(crossprod(X))
inv

beta<-inv %*% t(X) %*% y
beta
#El valor del estamador se acerca bastante al valor verdadero

curve(1+x,-3,3)
points(x,y)
curve(beta[1]+beta[2]*x ,add = TRUE, col=2)

#Como era de esperar, cuando mayor sea n, mejor es la estimación

#El problema que puede haber es que si k es muy grande, 
#el número de observaciones que se requiere para una estimación acertada puede ser muy grande.


#----------------------------------------------------------------------------------
#3.3
n<-5
set.seed(22)
x<-rnorm(n)

y<-1+x+rnorm(n,0,0.1)     #Creamos y
X<-cbind(1,x)             #Creamos X
X

QR<-qr(X)
QR

Q<-qr.Q(QR)
Q

Qt_y<-crossprod(Q, y)
Qt_y

R<-qr.R(QR)
R

backsolve(R,Qt_y)   #Coincide con la implementación directa


#----------------------------------------------------------------------------------
#Ejercicio propuesto
n<-3
A<-matrix(rep(1:n,times=n)^rep(1:n,each=n),n,n)
A

b<-A%*%rep(1,times=n)
b

solve(A,b)
rcond(A)


#----------------------------------------------------------------------------------
#Repetimos con n=12
n<-12
A<-matrix(rep(1:n,times=n)^rep(1:n,each=n),n,n)
A

b<-A%*%rep(1,times=n)
b

solve(A,b)            #No es capaz de resolverlo pues A está muy cerca de ser singular
cA<-rcond(A)
solve(A,b,tol=cA)     #Pero podemos usar solve con tolerancia para calcular   

