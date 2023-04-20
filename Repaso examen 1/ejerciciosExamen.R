# Ejer 1
x <- seq(1, by=2, length.out=50)
x
mx <- mean(x)
mx
sx <- sd(x)
sx

sum(abs(x-mx)>sx)

x[abs(x-mx)>sx] <- NA
x

sum(x[!is.na(x)]%%3 == 0)
# Los NA no son multiplos de 3

# Ejer 2
aire <- airquality
str(aire)

lapply(lapply(aire, is.na), sum)

nrow(aire)-sum(complete.cases(aire)) # elimino 42 filas
nrow(aire[!complete.cases(aire),])
aire <- na.omit(aire)

meses <- month.name[5:9] # Cojo de Mayo a Septiembre
meses
aire$Month <- factor(aire$Month, levels=5:9, labels=meses)
#aire

tapply(aire$Wind, aire$Month, median)
tapply(aire$Ozone, aire$Month, median)


aire.mayo <- aire[aire$Month == meses[1],] # meses[1] es Mayo
aire.mayo

# Ejer 3

proggeo <- function(n, a1, r){
    if(missing(n) || missing(a1) || missing(r) || !is.numeric(n) || !is.numeric(a1) || !is.numeric(r)){
        stop("Se deben dar 3 argumentos y deben ser numéricos")
    }

    v <- a1*r^(1:n -1)

    suma1 <- sum(v)
    suma2 <- a1*(1-r^n)/(1-r)

    prod1 <- prod(v)

    # comprobamos que a1 y r son positivos
    if(a1>0 && r>0){
        prod2 <- (sqrt( a1^2 * r^(n-1) ))^n
    }
    else{
        prod2 <- NA
        warning("Se ha introducido un valor negativo de r o de a1")
    }

    return(list(v=v, suma1=suma1, suma2=suma2, prod1=prod1, prod2=prod2))
}

proggeo(20,2,-0.5)
proggeo(20,2,0.5)
