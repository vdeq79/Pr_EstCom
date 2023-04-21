#Autor: XuSheng Zheng

#----------------------------------------------------------------------------------
#Cuestiones teóricas
#----------------------------------------------------------------------------------
#1.
#La diferencia principal es que una lista puede almacener datos de diferentes
#tipos, mientras que un array sólo permite almacenar datos del mismo tipo. 

#Se debe utilizar factor para tareas de agrupaciones. Respecto a objetos de tipo
#character su ventaja principal es que las cadenas se encuentran agrupadas.

#2.
#El operador [i] devuelve el objeto contenido en la posición i-ésima de la lista
#mientra que [[i]] devuelve el contenido de dicho objeto.

#3.
#En el primer caso, al usar & se evalúa ambas condiciones y puesto que el vector
#introducido es de tipo carácter, no permite la suma. En el segundo caso, al 
#utilizar && y se evalúa falso is.numeric(x), ya no se evalúa sum(x) y por lo
#tanto no se produce el error.


#----------------------------------------------------------------------------------
#Ejercicios prácticos
#----------------------------------------------------------------------------------
#Ejercicio 1
x<-c(1, seq(from=3, to=99, by=3), 100)
x

#1.
xm<-mean(x)
xm
sprintf("%.2f",xm)

#2.
xprod<-prod(x[1:3])
xprod

#3.
length(x[x>xprod & x<xm])

#4.
x[seq(1,length(x),by=2)]<-0
x

#5.
names(x)<-paste0("x_", 1:length(x) )
x

#Ejercicio 2
#1.
nba<-read.csv(file="nba2.csv", sep = ";",header = TRUE, as.is=c(2) )
str(nba)

#2.
levels(nba$Posicion)<-c("Alero", "Ala-Pivot", "Base", "Escolta", "Pivot")
nba$Posicion<-factor(nba$Posicion, levels = c( "Base", "Escolta", "Alero", "Ala-Pivot", "Pivot"))
str(nba)

#3.
tapply(nba$Edad, nba$Posicion, mean)
tapply(nba$Temporadas_NBA, nba$Posicion, mean)

#4.
nrow(nba[!complete.cases(nba),])

#5.
valor<-sapply(nba$Temporadas_NBA, function(x) if(x==0) 1 else if (x>=10) 2 else 3 )
nba<-transform(nba, Experiencia=factor(valor, labels = c("Rookie", "Veterano", "Experto")) )

#6.
nba2<-nba[nba$Ano=="2017-2018" & nba$Edad>25,]
nrow(nba2)
nba2[1:6,]

#Ejercicio 3
funcion.examen<-function(vec, num){
  
  #Comprobar si el vector es de números enteros
  if(length(x[x%%1!=0])!=0)
    stop("vector debe ser un vector de números enteros")
  
  if(num%%1!=0)
    stop("el número debe ser entero")
  
  sum<-sum(vec)
  mul<-sum(sapply(vec, function(x) x%%num==0 ))
  cont<-0
  
  for(i in 1:length(vec))
    if(vec[i]==num)
      cont<-cont+1
  
  return(list(suma=sum,múltiplos=mul,conteo=cont ))
}

set.seed(1)
vector<-sample(1:10,50,replace = T)
num<-2
funcion.examen(vector,num)

set.seed(1)
vector<-sample(1:10,50,replace = T)
num<-2.5
funcion.examen(vector,num)
