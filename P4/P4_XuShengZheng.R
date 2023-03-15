#Autor: XuSheng Zheng

#----------------------------------------------------------------------------------
#Ejercicio 1

#----------------------------------------------------------------------------------
#a)
censo<-read.csv(file = 'census.csv', header = TRUE, as.is = NA)

#----------------------------------------------------------------------------------
#b)
str(censo)

#----------------------------------------------------------------------------------
#c)
censoNA<-lapply(censo, is.na)
lapply(censoNA, sum)

#----------------------------------------------------------------------------------
#d)
sum(complete.cases(censo))

#----------------------------------------------------------------------------------
#e)
censo2<-censo[complete.cases(censo),]
censo2

censo2<-na.omit(censo)
censo2

#----------------------------------------------------------------------------------
#f)
write.table(censo2, file = 'censo2.txt', sep = '\t', row.names = FALSE)

#----------------------------------------------------------------------------------
#g)
censo3<-read.table(file = 'censo2.txt', sep='\t', header = TRUE, as.is=NA )
str(censo2)
str(censo3)

#----------------------------------------------------------------------------------
#Ejercicio 2
matriz<-matrix(rnorm(50), 10, 5) 
matriz

#----------------------------------------------------------------------------------
#a)
colnames(matriz)<-paste0('col', 1:5)

#----------------------------------------------------------------------------------
#b)
write(paste0('col', 1:5), file = 'matriz.txt', ncol=5, sep=',')
write(t(matriz), file = 'matriz.txt', ncol=5, sep=',', append = TRUE )

#----------------------------------------------------------------------------------
#c)
matriz2<-read.table(file = 'matriz.txt', sep=',', header = TRUE)
matriz2

#----------------------------------------------------------------------------------
#Ejercicio 3

#----------------------------------------------------------------------------------
#a)
olimpics<-read.csv(file = 'Olympics100m.csv', header = TRUE, as.is = c(2))
str(olimpics)

#----------------------------------------------------------------------------------
#b)
sum(is.na(olimpics))

#----------------------------------------------------------------------------------
#c)
resumen<-summary(olimpics)
is.matrix(resumen)
typeof(resumen)
resumen

write.table(resumen, file = 'resumen.txt', row.names = FALSE, sep = '\t', na='')
resumen2<-read.table(file = 'resumen.txt', sep='\t', header = TRUE, check.names=FALSE)
resumen2

#----------------------------------------------------------------------------------
#d)
resumen3 <- aggregate(olimpics$TIME, by=list(olimpics$Gender), summary)
resumen3

write.table(resumen3, file = 'resumen2.txt', row.names = FALSE, sep = '\t')
resume4 <- read.csv(file = 'resumen2.txt', sep = '\t', header = TRUE)
resume4
