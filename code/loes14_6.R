################################################################################
# Lösung Aufgabe 14.6
################################################################################


# Lösche alle möglicherweise vorhandenen Objekte im Arbeitsbereich
rm(list = ls())

# Daten einlesen und im data.frame 'daten' speichern
daten <- read.table(file="ifo_zeitreihen.txt", header=TRUE, dec=".")


# ########################### Bauhauptgewerbe ##################################

# Umwandlung in ein Zeitreihenobjekt
bau.series <- ts(daten$Bauhauptgewerbe_Kapazitaetsauslastung, 
                 start = c(1991, 1), frequency=12)

# Starre Saisonfigur
zerlegung.ifo.bau <- stl(bau.series, s.window="periodic")
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den plot Befehl ausführen
pdf("loes14_6_bau1.pdf")
plot(zerlegung.ifo.bau)
dev.off()


# Flexible Saisonfigur
zerlegung.ifo.bau.2 <- stl(bau.series, s.window=7)
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den plot Befehl ausführen
pdf("loes14_6_bau2.pdf")
plot(zerlegung.ifo.bau.2)
dev.off()


# Vergleich der Trendkomponenten
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den plot, lines und legend Befehl ausführen
pdf("loes14_6_trendvergleich_bau.pdf")
par(cex=1.5)
plot(zerlegung.ifo.bau$time.series[,2], lty=1,
     ylab="Trendkomponente Bau", xlab="Zeit")
lines(zerlegung.ifo.bau.2$time.series[,2], lty=2)
legend(1992, 74, legend=c("starre Saison", "flexible Saison"), lty=1:2)
par(cex=1)
dev.off()


# ########################## Geschäftsklimaindex ###############################

# Umwandlung in ein Zeitreihenobjekt
gki.series <- ts(daten$Gewerbliche_Wirtschaft_Gki, 
                 start = c(1991, 1), frequency=12)

# Starre Saisonfigur
zerlegung.ifo.gki <- stl(gki.series, s.window="periodic")
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den plot Befehl ausführen
pdf("loes14_6_gki1.pdf")
plot(zerlegung.ifo.gki)
dev.off()


# Flexible Saisonfigur
zerlegung.ifo.gki.2 <- stl(gki.series, s.window=7)
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den plot Befehl ausführen
pdf("loes14_6_gki2.pdf")
plot(zerlegung.ifo.gki.2)
dev.off()


# Vergleich der Trendkomponenten 
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den plot, lines und legend Befehl ausführen
pdf("loes14_6_trendvergleich.pdf")
par(cex=1.5)
plot(zerlegung.ifo.gki$time.series[,2], lty=1,
     ylab="Trendkomponente GKI", xlab="Zeit")
lines(zerlegung.ifo.gki.2$time.series[,2], lty=2)
legend(1992, 112, legend=c("starre Saison", "flexible Saison"), lty=1:2)
par(cex=1)
dev.off()





