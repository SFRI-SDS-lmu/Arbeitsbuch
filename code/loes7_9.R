##############################################################
# Lösung Aufgabe 7.9 
##############################################################

# Lösche alle möglicherweise vorhandenen Objekte im Arbeitsbereich
rm(list = ls())

# Für die Reproduzierbarkeit setzen wir einen seed
set.seed(5671345)

# Berechne und speichere in z.dichte 30 Werte der Dichte der 
# N(0,1) Verteilung im Bereich -3 bis 3
x <- seq(-3, 3, length = 30)
z.dichte <- dnorm(x)

# Zunächst exemplarisch für n=100

n <- 100
# Ziehe n=100 Zufallszahlen aus N(0,1) Verteilung 
s <- rnorm(n)

# Zeichne Kerndichteschätzer und Dichte in einer Grafik
plot(density(s), xlim=c(-3,3), ylim=c(0,1), lty=1,  
main="Kerndichte plus Verteilungsdichte der N(0,1)")
lines(x, z.dichte,lwd = 1,lty=2)
legend(-2.4, 0.8, legend=c("Kerndichte","Dichte der N(0,1)"),
       lty=1:2, cex=0.8)


# Jetzt allgemeinere Funktion für beliebigen Stichprobenumfang n

plot.dichte <- function(n){
  
  s <- rnorm(n=n)
  
  plot(density(s), xlim=c(-3,3), ylim=c(0,1), 
       main="Kerndichte plus Verteilungsdichte der N(0,1)",
       lty=1)
  lines(x, z.dichte, lwd = 1, lty=2)
  legend(-2.4, 0.8, legend=c("Kerndichte","Dichte der N(0,1)"),
         lty=1:2, cex=0.8)
}


# Anwendung der Funktion für verschiedene Stichprobenumfänge
plot.dichte(10)
plot.dichte(20)
plot.dichte(50)
plot.dichte(100)
plot.dichte(1000)
plot.dichte(10000)


# Speichern der Grafiken im pdf Format anstatt Ausgabe am Bildschirm

pdf("loes7_9_n10.pdf")
par(cex=1.5)
plot.dichte(10)
dev.off()

pdf("loes7_9_n20.pdf")
par(cex=1.5)
plot.dichte(20)
dev.off()

pdf("loes7_9_n50.pdf")
par(cex=1.5)
plot.dichte(50)
dev.off()

pdf("loes7_9_n100.pdf")
par(cex=1.5)
plot.dichte(100)
dev.off()

pdf("loes7_9_n1000.pdf")
par(cex=1.5)
plot.dichte(10)
dev.off()

pdf("loes7_9_n10000.pdf")
par(cex=1.5)
plot.dichte(10000)
dev.off()


