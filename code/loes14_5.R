################################################################################
# Lösung Aufgabe 14.5
################################################################################


############################### VORBEREITUNG ###################################

# Lösche alle möglicherweise vorhandenen Objekte im Arbeitsbereich
rm(list = ls())

# Lade benötigte R Pakete
library(graphics)
require(stlplus)

# Daten einlesen und im data.frame 'daten' speichern
daten <- read.table(file="luftschad.txt", header=TRUE, dec=".")

# März 1996, 63. Beoachtung
print(daten[63,])
# Februar 2016, 302. Beobachtung
print(daten[302,])

# Erzeuge Teildatensatz März 1996 bis Februar 2016
teildaten <- daten[63:302,]

# Stickoxid-Zeitreihe als ts-Datenobjekt speichern in 'nox'
nox <- ts(teildaten$Stickoxide,start=c(1996,3),end=c(2016,2),frequency = 12 )


############################### Datenanalyse ###################################

# Starre Saisonfigur, flexibler Trend 
# Starre Saisonfigur durch 's.window="periodic"
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den plot Befehl ausführen
starr_flexibel<-stlplus(nox, t=time(nox), s.window="periodic")
pdf("loes14_5_stlplus1")
par(cex=1.5)
plot(starr_flexibel)
par(cex=1)
dev.off()


# flexible Saisonfigur, flexibler Trend 
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den plot Befehl ausführen
flexibel_flexibel<-stlplus(nox, t=time(nox), s.window=5, t.window=5)
pdf("loes14_5_stlplus2")
par(cex=1.5)
plot(flexibel_flexibel)
par(cex=1)
dev.off()


# starre Saisonfigur, glatter Trends
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den plot Befehl ausführen
starr_glatt<-stlplus(nox, t=time(nox), s.window="per", t.window=101)
pdf("loes14_5_stlplus3")
par(cex=1.5)
plot(starr_glatt)
par(cex=1)
dev.off()


# flexible Saisonfigur, glatter Trends
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den plot Befehl ausführen
flexibel_glatt<-stlplus(nox, t=time(nox), s.window=5, t.window=101)
pdf("loes14_5_stlplus4")
par(cex=1.5)
plot(flexibel_glatt)
par(cex=1)
dev.off()

# Bestimme die Varianzen der Restkomponenten

# Starre Saisonfigur, flexibler Trend 
var(starr_flexibel$data$remainder)
# flexible Saisonfigur, flexibler Trend 
var(flexibel_flexibel$data$remainder)
# starre Saisonfigur, glatter Trend
var(starr_glatt$data$remainder)
# flexible Saisonfigur, glatter Trend
var(flexibel_glatt$data$remainder)


