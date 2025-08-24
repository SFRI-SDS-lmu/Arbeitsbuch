################################################################################
# Lösung Aufgabe 14.5
################################################################################


############################### VORBEREITUNG ###################################

# Lösche alle möglicherweise vorhandenen Objekte im Arbeitsbereich
rm(list = ls())

# Daten einlesen und im data.frame 'daten' speichern
daten <- read.table(file="luftschad.txt", header=TRUE, dec=".")

# März 1996, 63. Beoachtung
print(daten[63,])
# Februar 2016, 302. Beobachtung
print(daten[302,])

# Erzeuge Teildatensatz März 1996 bis Februar 2016
teildaten <- daten[63:302,]

# Stickoxid-Zeitreihe als ts-Datenobjekt speichern in 'nox'
nox <- ts(teildaten$Stickoxide,start=c(1996,3),end=c(2016,2),frequency = 12)


############################### Datenanalyse ###################################

# flexibler Trend, Starre Saisonfigur 
# Starre Saisonfigur durch 's.window="periodic"
starr_flexibel<-stl(nox, s.window="periodic")
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den plot Befehl ausführen
pdf("loes14_5_stl1.pdf")
plot(starr_flexibel)
dev.off()


# flexibler Trend, flexible Saisonfigur 
flexibel_flexibel<-stl(nox, s.window=5, t.window=5)
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den plot Befehl ausführen
pdf("loes14_5_stl2.pdf")
plot(flexibel_flexibel)
dev.off()


# glatter Trends, starre Saisonfigur
starr_glatt<-stl(nox, s.window="per", t.window=101)
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den plot Befehl ausführen
pdf("loes14_5_stl3.pdf")
plot(starr_glatt)
dev.off()


# glatter Trends, flexible Saisonfigur 
flexibel_glatt<-stl(nox, s.window=5, t.window=101)
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den plot Befehl ausführen
# flexibel_glatt<-stlplus(nox, t=time(nox), s.window=5, t.window=101)
pdf("loes14_5_stl4.pdf")
plot(flexibel_glatt)
dev.off()


# Bestimme die Varianzen der Restkomponenten

# Starre Saisonfigur, flexibler Trend 
var(starr_flexibel$time.series[,"remainder"])
# flexible Saisonfigur, flexibler Trend 
var(flexibel_flexibel$time.series[,"remainder"])
# starre Saisonfigur, glatter Trend
var(starr_glatt$time.series[,"remainder"])
# flexible Saisonfigur, glatter Trend
var(flexibel_glatt$time.series[,"remainder"])


