##############################################################
# Lösung Aufgabe 7.9 im Lehrbuch
##############################################################

set.seed(5671345)

n <- c(10,20,50,100,1000,10000)

# Plot der Dichten. Als x-Achse wählen wir den Bereich (-3, +3), für die y-Achse 
# den Bereich (0,1), auch wenn Dichten größer 1 werden können.

plot.dichte <- function(n){
  
  s <- rnorm(n=n)
  
  # Raster auf der x-Achse
  x <- seq(-3, 3, length = 30)
  
  # Dichte der Standardnormalverteilung am Raster auswerten
  z.dichte <- dnorm(x)
  
  plot(density(s), xlim=c(-3,3), ylim=c(0,1), 
       main="Kerndichte plus Verteilungsdichte der N(0,1)",
       lty=1)
  lines(x, z.dichte, lwd = 1, lty=2)
  legend(-2.4, 0.8, legend=c("Kerndichte","Dichte der N(0,1)"),
         lty=1:2, cex=0.8)
}

plot.dichte(n[1])
plot.dichte(n[2])
plot.dichte(n[3])
plot.dichte(n[4])
plot.dichte(n[5])
plot.dichte(n[6])

# in einer Schleife
set.seed(5671345)
for (j in 1:length(n)){
  pdf(paste("loes7_9_dichten",as.character(n[j]),".pdf",sep=""))
  par(cex=1.5)
  plot.dichte(n[j])
  dev.off()
}
