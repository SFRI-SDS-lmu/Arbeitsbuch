################################################################################
# Lösung Aufgabe 14.7
################################################################################

# Lösche alle möglicherweise vorhandenen Objekte im Arbeitsbereich
rm(list = ls())

# Daten einlesen und im data.frame 'daten' speichern
daten <- read.table(file="munichre.txt", header=TRUE, dec=".")

# Zeitreihenobjekt 'kurs.mr' definieren
kurs.mr <- ts(daten$AdjClose)

# Zeitreihe glätten mit q=31
q <- 31
g_q31 <- filter(kurs.mr, filter=rep(1/(2*q+1), (2*q+1)), 
                method="convolution", sides=2)

# Zeitreihe glätten mit q=91
q <- 91
g_q91 <- filter(kurs.mr, filter=rep(1/(2*q+1), (2*q+1)), 
                method="convolution", sides=2)


# Zeitreihe und deren Glättung zeichnen bei q=31
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'plot' und 'lines' Befehl ausführen
pdf("loes14_7_1.pdf")
par(cex=1.5)
plot(kurs.mr, type="l", lwd=1, lty=2, col="darkgrey",
     xlab="Tage", ylab="Kurs der MunichRe Aktie")
lines(g_q31, lty=1, lwd=2, col="black")
par(cex=1)
dev.off()


# Zeitreihe und deren Glättung zeichnen bei q=31
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'plot' und 'lines' Befehl ausführen
pdf("loes14_7_2.pdf")
par(cex=1.5)
plot(kurs.mr, type="l", lwd=1, lty=2, col="darkgrey",
     xlab="Tage", ylab="Kurs der MunichRe Aktie")
lines(g_q91, lty=1, lwd=2, col="black")
dev.off()

