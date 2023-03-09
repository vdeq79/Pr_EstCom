#Autor: XuSheng Zheng

#----------------------------------------------------------------------------------
#Ejercicio propuesto
n<-3
A<-matrix(rep(1:n,times=n)^rep(1:n,each=n),n,n)
A

b<-A%*%rep(1,times=n)
b

x<-solve(A,b)
x
rcond(A)
max(abs(x-1))

#----------------------------------------------------------------------------------
#Repetimos con n=4
n<-4
A<-matrix(rep(1:n,times=n)^rep(1:n,each=n),n,n)
A

b<-A%*%rep(1,times=n)
b

x<-solve(A,b)
x
rcond(A)
max(abs(x-1))

#----------------------------------------------------------------------------------
#Repetimos con n=5
n<-5
A<-matrix(rep(1:n,times=n)^rep(1:n,each=n),n,n)
A

b<-A%*%rep(1,times=n)
b

x<-solve(A,b)
x
rcond(A)
max(abs(x-1))

#----------------------------------------------------------------------------------
#Repetimos con n=6
n<-6
A<-matrix(rep(1:n,times=n)^rep(1:n,each=n),n,n)
A

b<-A%*%rep(1,times=n)
b

x<-solve(A,b)
x
rcond(A) 
max(abs(x-1))

#----------------------------------------------------------------------------------
#Repetimos con n=7
n<-7
A<-matrix(rep(1:n,times=n)^rep(1:n,each=n),n,n)
A

b<-A%*%rep(1,times=n)
b

x<-solve(A,b)
x
rcond(A)  
max(abs(x-1))

#----------------------------------------------------------------------------------
#Repetimos con n=8
n<-8
A<-matrix(rep(1:n,times=n)^rep(1:n,each=n),n,n)
A

b<-A%*%rep(1,times=n)
b

x<-solve(A,b)
x
rcond(A) 
max(abs(x-1))

#----------------------------------------------------------------------------------
#Repetimos con n=9
n<-9
A<-matrix(rep(1:n,times=n)^rep(1:n,each=n),n,n)
A

b<-A%*%rep(1,times=n)
b
x<-solve(A,b)
x
rcond(A)  
max(abs(x-1))

#----------------------------------------------------------------------------------
#Repetimos con n=10
n<-10
A<-matrix(rep(1:n,times=n)^rep(1:n,each=n),n,n)
A

b<-A%*%rep(1,times=n)
b

x<-solve(A,b)
x
rcond(A)
max(abs(x-1))

#----------------------------------------------------------------------------------
#Repetimos con n=11
n<-11
A<-matrix(rep(1:n,times=n)^rep(1:n,each=n),n,n)
A

b<-A%*%rep(1,times=n)
b

x<-solve(A,b)
x
rcond(A)
max(abs(x-1))

#----------------------------------------------------------------------------------
#Repetimos con n=12
n<-12
A<-matrix(rep(1:n,times=n)^rep(1:n,each=n),n,n)
A

b<-A%*%rep(1,times=n)
b

#solve(A,b)              #No es capaz de resolverlo pues A está muy cerca de ser singular
cA<-rcond(A)
x<-solve(A,b,tol=cA)     #Pero podemos usar solve con tolerancia para calcular   
x
max(abs(x-1))
