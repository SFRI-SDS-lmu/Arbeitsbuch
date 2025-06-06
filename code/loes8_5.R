#################################################
# Lösung Aufgabe 8.5
#################################################

# Abbildungen 8.3 bis 8.5. sind Darstellungen der Dichten von bivariaten 
# Normalverteilungen

# Dichte der bivariaten NV, siehe Buch, Kapitel 8.6

bivdichte.nv <- function(x,y,mu1,mu2,sigma1,sigma2,rho){
  
  if (!(sigma1>0) || !(sigma2>0) || ! ((rho>-1) && (rho<1)) )
    print("Fehler bei den Eingabeparametern!")
  
  normierung <- 1/(2*pi*sigma1*sigma2*sqrt(1-rho^2)) 
  q1 <- (x-mu1)/sigma1
  q2 <- (y-mu2)/sigma2
  exponent <- -1/(2*(1-rho^2)) * ( q1^2 + q2^2 - 2*rho*q1*q2 )
  d <- normierung*exp(exponent)
  return(d)
}


# Abb. 8.3, Grid/Raster jeweils (-3,+3)
mu1 <- 0
mu2 <- 0
sigma1 <- 1
sigma2 <- 1
rho <- 0

x <- seq(from=-3,to=3, by=0.2)
y <- seq(from=-3,to=3, by=0.2)
z <- matrix(nrow=length(x), ncol=length(y), data=0)
for (i in 1:length(x)) {
  for (j in 1:length(y)){
    z[i,j] <- bivdichte.nv(x[i], y[j], mu1, mu2, sigma1, sigma2, rho)
  }
}

persp(x,y,z, ticktype="detailed", 
      theta=30, phi=15, shade=0.1, expand=0.5, zlim=c(0,0.2), r=10)
pdf("loes_8_5_abb83.pdf")
par(cex.axis=1.2)
persp(x,y,z, ticktype="detailed", 
      theta=30, phi=15, shade=0.1, expand=0.5, zlim=c(0,0.2), r=10)
dev.off()
contour(x,y,z, zlim=c(0,0.2))
pdf("loes_8_5_abb83_2.pdf")
par(cex=1.2)
contour(x,y,z, zlim=c(0,0.2))
dev.off()
image(x,y,z)
pdf("loes_8_5_abb83_3.pdf")
par(cex=1.2)
image(x,y,z)
dev.off()


# Abb. 8.4, Grid/Raster jeweils (-3,+3)
mu1 <- 0
mu2 <- 0
sigma1 <- 1.5
sigma2 <- 1
rho <- 0

# Gleiches Raster
# x <- seq(from=-3,to=3, by=0.2)
# y <- seq(from=-3,to=3, by=0.2)


# Der outer - Befehl vereinfacht die Auswertung der bivariaten Dichte an jeder
# Kombination von x und y Koordinaten un dspart die Schleife

z <- outer(x, y, bivdichte.nv, mu1, mu2, sigma1, sigma2, rho )
persp(x,y,z, ticktype="detailed", 
      theta=30, phi=15, shade=0.1, expand=0.5, zlim=c(0,0.2), r=10)
pdf("loes_8_5_abb84.pdf")
par(cex=1.2)
persp(x,y,z, ticktype="detailed", 
      theta=30, phi=15, shade=0.1, expand=0.5, zlim=c(0,0.2), r=10)
dev.off()
contour(x,y,z, zlim=c(0,0.2))
pdf("loes_8_5_abb84_2.pdf")
par(cex=1.2)
contour(x,y,z, zlim=c(0,0.2))
dev.off()
image(x,y,z)
pdf("loes_8_5_abb84_3.pdf")
par(cex=1.2)
image(x,y,z)
dev.off()



# Abb. 8.5, Grid/Raster jeweils (-3,+3)
mu1 <- 0
mu2 <- 0
sigma1 <- 1
sigma2 <- 1
rho <- 0.8

z <- outer(x, y, bivdichte.nv, mu1, mu2, sigma1, sigma2, rho )
persp(x,y,z, ticktype="detailed", 
      theta=30, phi=15, shade=0.1, expand=0.5, zlim=c(0,0.3), r=10)
pdf("loes_8_5_abb85.pdf")
par(cex=1.2)
persp(x,y,z, ticktype="detailed", 
      theta=30, phi=15, shade=0.1, expand=0.5, zlim=c(0,0.3), r=10)
dev.off()
contour(x,y,z, zlim=c(0,0.3))
pdf("loes_8_5_abb85_2.pdf")
par(cex=1.2)
contour(x,y,z, zlim=c(0,0.3))
dev.off()
image(x,y,z)
pdf("loes_8_5_abb85_3.pdf")
par(cex=1.2)
image(x,y,z)
dev.off()


# Abb. 8.6, Grid/Raster jeweils (-3,+3)
mu1 <- 0
mu2 <- 0
sigma1 <- 1
sigma2 <- 1
rho <- -0.8

z <- outer(x, y, bivdichte.nv, mu1, mu2, sigma1, sigma2, rho )
persp(x,y,z, ticktype="detailed", 
      theta=30, phi=15, shade=0.1, expand=0.5, zlim=c(0,0.3), r=10)
pdf("loes_8_5_abb86.pdf")
par(cex=1.2)
persp(x,y,z, ticktype="detailed", 
      theta=30, phi=15, shade=0.1, expand=0.5, zlim=c(0,0.3), r=10)
dev.off()
contour(x,y,z, zlim=c(0,0.3))
pdf("loes_8_5_abb86_2.pdf")
par(cex=1.2)
contour(x,y,z, zlim=c(0,0.3))
dev.off()
image(x,y,z)
pdf("loes_8_5_abb86_3.pdf")
par(cex=1.2)
image(x,y,z)
dev.off()




# 2. je ein Beispiel für einen contour und einen image plot
# Abb. 8.5, Grid/Raster jeweils (-3,+3)
#mu1 <- 0
#mu2 <- 0
#sigma1 <- 1
#sigma2 <- 1
#rho <- 0.8

#z <- outer(x, y, bivdichte.nv, mu1, mu2, sigma1, sigma2, rho )
#contour(x,y,z, zlim=c(0,0.3))
#pdf("loes_8_5_abb85_2.pdf")
#par(cex=1.2)
#contour(x,y,z, zlim=c(0,0.3))
#dev.off()


# Abb. 8.6, Grid/Raster jeweils (-3,+3)
#mu1 <- 0
#mu2 <- 0
#sigma1 <- 1
#sigma2 <- 1
#rho <- -0.8

#z <- outer(x, y, bivdichte.nv, mu1, mu2, sigma1, sigma2, rho )
#image(x,y,z)
#pdf("loes_8_5_abb86_2.pdf")
#par(cex=1.2)
#image(x,y,z)
#dev.off()


# check
mu1 <- 1
mu2 <- 4
sigma1 <- 2
sigma2 <- 3
rho <- 0.6
Sigma <- matrix(nrow=2,ncol=2,byrow=T,
                data=c(sigma1^2,rho*sigma1*sigma2,rho*sigma1*sigma2,sigma2^2))
x <- c(2,4)

dmvnorm(x, mean=c(mu1,mu2), sigma=Sigma)

bivdichte.nv(x[1], x[2], mu1, mu2, sigma1, sigma2, rho)




