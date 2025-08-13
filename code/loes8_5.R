#################################################
# Lösung Aufgabe 8.5
#################################################

# Funktion bivdichte.nv
# Implmentiere zunächst eine allgemeine Funktion zur Berechnung der 
# Dichte der bivariaten NV, 
# siehe Fahrmeir et al. (2024), Kapitel 8.6

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

# Verwende als Grid/Raster jeweils (-3,+3)
# In der Matrix z werden später die Werte der Dichte gespeichert
x <- seq(from=-3,to=3, by=0.2)
y <- seq(from=-3,to=3, by=0.2)
z <- matrix(nrow=length(x), ncol=length(y), data=0)


# Aufgabe a) und b)
# Visualisiere die vier Dichten mit Hilfe der R Funktionen
# persp, contour und image


# ERSTE DICHTE
mu1 <- 0
mu2 <- 0
sigma1 <- 1
sigma2 <- 1
rho <- 0

# Verwende eine Doppelschleife zur Berechnung der Werte der Dichte 
for (i in 1:length(x)) {
  for (j in 1:length(y)){
    z[i,j] <- bivdichte.nv(x[i], y[j], mu1, mu2, sigma1, sigma2, rho)
  }
}
# Verwende alternativ die outer Funktion zur Vermeidung der Doppelschleife
z <- outer(x, y, bivdichte.nv, mu1, mu2, sigma1, sigma2, rho)



# Visualisiere die Dichte
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur die persp, contour und image Befehle ausführen

# Zu den Parametern theta, phi, shade usw. konsultiere man die ensprechende
# Hilfe, z.B. mit help(persp)

pdf("loes_8_5_persp_1.pdf")
par(cex.axis=1.2)
persp(x,y,z, ticktype="detailed", 
      theta=30, phi=15, shade=0.1, expand=0.5, zlim=c(0,0.2), r=10)
dev.off()

pdf("loes_8_5_contour_1.pdf")
par(cex=1.2)
contour(x,y,z, zlim=c(0,0.2))
dev.off()

pdf("loes_8_5_image_1.pdf")
par(cex=1.2)
image(x,y,z)
dev.off()


# ZWEITE DICHTE  
mu1 <- 0
mu2 <- 0
sigma1 <- 1.5
sigma2 <- 1
rho <- 0

z <- outer(x, y, bivdichte.nv, mu1, mu2, sigma1, sigma2, rho)

# Visualisiere die Dichte
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur die persp, contour und image Befehle ausführen

pdf("loes_8_5_persp_2.pdf")
par(cex.axis=1.2)
persp(x,y,z, ticktype="detailed", 
      theta=30, phi=15, shade=0.1, expand=0.5, zlim=c(0,0.2), r=10)
dev.off()

pdf("loes_8_5_contour_2.pdf")
par(cex=1.2)
contour(x,y,z, zlim=c(0,0.2))
dev.off()

pdf("loes_8_5_image_2.pdf")
par(cex=1.2)
image(x,y,z)
dev.off()


# DRITTE DICHTE
mu1 <- 0
mu2 <- 0
sigma1 <- 1
sigma2 <- 1
rho <- 0.8

z <- outer(x, y, bivdichte.nv, mu1, mu2, sigma1, sigma2, rho)

# Visualisiere die Dichte
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur die persp, contour und image Befehle ausführen

pdf("loes_8_5_persp_3.pdf")
par(cex.axis=1.2)
persp(x,y,z, ticktype="detailed", 
      theta=30, phi=15, shade=0.1, expand=0.5, zlim=c(0,0.2), r=10)
dev.off()

pdf("loes_8_5_contour_3.pdf")
par(cex=1.2)
contour(x,y,z, zlim=c(0,0.2))
dev.off()

pdf("loes_8_5_image_3.pdf")
par(cex=1.2)
image(x,y,z)
dev.off()


# VIERTE DICHTE
mu1 <- 0
mu2 <- 0
sigma1 <- 1
sigma2 <- 1
rho <- -0.8

z <- outer(x, y, bivdichte.nv, mu1, mu2, sigma1, sigma2, rho)

# Visualisiere die Dichte
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur die persp, contour und image Befehle ausführen

pdf("loes_8_5_persp_4.pdf")
par(cex.axis=1.2)
persp(x,y,z, ticktype="detailed", 
      theta=30, phi=15, shade=0.1, expand=0.5, zlim=c(0,0.2), r=10)
dev.off()

pdf("loes_8_5_contour_4.pdf")
par(cex=1.2)
contour(x,y,z, zlim=c(0,0.2))
dev.off()

pdf("loes_8_5_image_4.pdf")
par(cex=1.2)
image(x,y,z)
dev.off()

