##############################################################
# Lösung Aufgabe 7.11 
##############################################################

# Lösche alle möglicherweise vorhandenen Objekte im Arbeitsbereich
rm(list = ls())

# Für die Reproduzierbarkeit setzen wir einen Seed
set.seed(5671345)

# Setze maximales n gleich 1000
# Erzeuge einen Vektor xquer, in dem später die Mittelwerte 
# in Abhängigkeit von n gespeichert werden.
max.n <- 1000
xquer <- numeric(max.n)

# Berechne in einer  Schleife die Mittelwerte
# rnorm erwartet die Standardabweichung als Argument, 
# nicht die Varianz. Deshalb wird die Funktion sqrt angewendet.

for (n in 1:max.n){
  xquer[n] <- mean(rnorm(n=n, mean=1, sd=sqrt(5)))
}

# Zeichnen des arithmetischen Mittels in Abhängigkeit von n
plot(xquer, type="l", ylab="arithm. Mittel", xlab="n")
abline(a=1,b=0)


# Alternativ die Grafik im pdf Format abspeichern
pdf("loes7_11.pdf")
par(cex=1.5)
plot(xquer, type="l", ylab="arithm. Mittel", xlab="n")
abline(a=1,b=0)
dev.off()

