datos <- read.csv('../Datos/companies79.csv', header = T)
str(datos[3:8])


#Ejercicio 3
xf <- datos[3:8]

win.graph()

par(mfrow = c(2,3))

sapply(seq(1,6),function(j)boxplot(xf[,j], main = colnames(xf)[j], xlab ="", col = "red"))


#♥Ejercicio 5

win.graph()

par(mfrow = c(2,3))

sapply(seq(1,6),function(j)boxplot(xf[,j]~datos[,9], main = colnames(xf)[j], xlab ="", col = "red", las =2))


#Ejercicio 7

win.graph()

par(mfrow = c(2,3))

sapply(seq(1,6),function(j)plot(density(xf[,j], kernel = "gaussian"),
                                
                                main = colnames(xf)[j], xlab ="", col = "red"))

#Ejercicio 9

win.graph()

pairs(xf[,1:6],pch = 19, col = rainbow(6,alpha = 1))[datos[,9]]
