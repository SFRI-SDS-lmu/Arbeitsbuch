##############################################################
# Lösung Aufgabe 7.13 im Lehrbuch
##############################################################

set.seed(3134561)

n <- 1000
# erzeuge S Mittelwerte mit dem gewählten n
S <- 1000

# n Zeilen, S Spalten
rv <- matrix(nrow=n, ncol=S,
               data=rcauchy(n=S*n))
# colMeans liefert S Mittelwerte gebildet aus je n Werten.
means <- colMeans(rv)
plot(density(means, adjust=1.5), type="l", main="")
curve(dcauchy, n=100, from=-50, to=50, add=TRUE, lty=2)


# speichere die Grafiken als pdf ab
pdf(paste("loes7_13_hist_cauchy",".pdf",sep=""))
par(cex=1.5)
plot(density(means, adjust=1.5), type="l", main="")
curve(dcauchy, n=100, from=-50, to=50, add=TRUE, lty=2)
dev.off()

