library(MASS)

bivn <- mvrnorm(5000, mu=c(0,0), Sigma = matrix(data = c(1,.5,.5,1), nrow = 2, ncol = 2))

bivn2 <- mvrnorm(1000, mu=c(0,5), Sigma = matrix(c(1,0,0,1),2))

bivn3 <- mvrnorm(1000, mu = c(0,5), Sigma = matrix(c(1,.9,.9,1),2))

#Plots
plot(bivn[,1], bivn[,2])
plot(bivn2[,1], bivn2[,2])
plot(bivn3[,1], bivn3[,2])

#Densidades kernel 
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50) #El parametro n lo podemos quitar
bivn2.kde <- kde2d(bivn2[,1], bivn2[,2], n = 50)
bivn3.kde <- kde2d(bivn3[,1], bivn3[,2], n = 50)

#COntour plot
image(bivn.kde) #base graphics package
contour(bivn.kde, add = TRUE) #graphics package

#plot(bivn[,1], bivn[,2])
#contour(bivn.kde, add = TRUE) 

### Superficie en tres dimensiones
#Basic perspective plot
persp(bivn.kde, phi = 45, theta = 30, shade = 0.1, border = NA)


#RGL interactive plot
library(rgl)
col1 <- heat.colors(length(bivn.kde$z))[rank(bivn.kde$z)]
persp3d(x = bivn.kde, col = col1)

col2 <- rainbow(length(bivn2.kde$z))[rank(bivn2.kde$z)]
persp3d(x = bivn2.kde, col = col2)

col3 <- rainbow(length(bivn3.kde$z))[rank(bivn3.kde$z)]
persp3d(x = bivn3.kde, col = col3)


#threejs Javasctipr plot
install.packages('threejs')
library(threejs)

#Desagrupar los datos obtenidos con kde
x <- bivn.kde$x; y <- bivn.kde$y; z <- bivn.kde$z
##Construir coordenadas x,y,z
xx <- rep(x, times=length(y))
yy <- rep(y, each = length(x))
zz <- z; dim(zz) <- NULL
# Rango de 
ra <- ceiling(16 * zz/max(zz))
col <- rainbow(13, 2/3)
#Scatter interactivo 3D
scatterplot3js(x = xx, y =yy, z =zz, size = 0.2, color = col[ra], bg= 'black')


#Dibujar la matriz de correlacion
#install.packages('clusterGeneration')
library(corrplot)
library(clusterGeneration)

mu <- rep(0,10)
pdMat <- genPositiveDefMat(10, lambdaLow = 10) #Esto es para asegurarnos que la matriz generada sea positiva
Sigma <- pdMat$Sigma
dim(Sigma)
mvn <- mvrnorm(5000, mu = mu, Sigma = Sigma) #Generamos los datos

corrplot(cor(mvn),
         method = 'ellipse',
         tl.pos = 'n')

