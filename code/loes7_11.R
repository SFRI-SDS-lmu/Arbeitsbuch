##############################################################
# Lösung Aufgabe 7.11 im Lehrbuch
##############################################################

set.seed(5671345)

max.n <- 1000

xquer <- numeric(max.n)

# rnorm erwartet die Standardabweichung sd, nicht die Varianz. Deshalb wird
# die Funktion sqrt angewendet.

for (n in 1:max.n){
  xquer[n] <- mean(rnorm(n=n, mean=1, sd=sqrt(5)))
}

plot(xquer, type="l")
abline(a=1,b=0)

pdf("loes7_11.pdf")
par(cex=1.5)
plot(xquer, type="l")
abline(a=1,b=0)
dev.off()

