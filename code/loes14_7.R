####################################################
# Lösung Aufgabe 14.7
####################################################

# Daten einlesen
daten <- read.table(file="munichre.txt", header=TRUE, dec=".")
kurs.mr <- ts(daten$AdjClose)

plot(kurs.mr, type="l", lwd=1, lty=2, col="darkgrey",
     xlab="Tage", ylab="Kurs der MunichRe Aktie")

q <- 31
f1 <- filter(kurs.mr, filter=rep(1/(2*q+1), (2*q+1)), method="convolution", sides=2)
lines(f1, lty=1, lwd=2, col="black")                

pdf("loes14_7_1.pdf")
par(cex=1.5)
plot(kurs.mr, type="l", lwd=1, lty=2, col="darkgrey",
     xlab="Tage", ylab="Kurs der MunichRe Aktie")
lines(f1, lty=1, lwd=2, col="black")
dev.off()

plot(kurs.mr, type="l", lwd=1, lty=2, col="darkgrey",
     xlab="Tage", ylab="Kurs der MunichRe Aktie")
q <- 91
f1 <- filter(kurs.mr, filter=rep(1/(2*q+1), (2*q+1)), method="convolution", sides=2)
lines(f1, lty=1, lwd=2, col="black")  

pdf("loes14_7_2.pdf")
par(cex=1.5)
plot(kurs.mr, type="l", lwd=1, lty=2, col="darkgrey",
     xlab="Tage", ylab="Kurs der MunichRe Aktie")
lines(f1, lty=1, lwd=2, col="black")
dev.off()