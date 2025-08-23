################################################################################
# Lösung Aufgabe 2.8
################################################################################


############################### VORBEREITUNG ###################################

# Lösche alle möglicherweise vorhandenen Objekte im Arbeitsbereich
rm(list = ls())

# Lade benötigtes R Paket 'moments' für Berechnung von Schiefe und Wölbung
# in Aufgabe e)
library(moments)

# Daten einlesen und im data.frame 'daten' speichern
daten <- read.table(file="mietspiegel2015.txt", header=TRUE, dec=".")


################################ AUFGABE a) ####################################

# Häufigkeitstabelle für die Variable rooms
h <- table(daten$rooms)
print(h)

# Säulendiagramm für die Variable rooms
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'barplot' Befehl ausführen
pdf("loes2_8_barplot.pdf")
barplot(h, ylim=c(0,1200), main="Säulendiagramm für die Variable rooms", 
        ylab="absolute Häufigkeit")
dev.off()


################################ AUFGABE b) ####################################

# Fünf-Punkte-Zusammenfassung des Baujahrs 'bj'
summary(daten$bj)

# Binarisiertes Baujahr erstellen 
# kleiner Korrektheitscheck mit table
daten$bj.cat <- ifelse(daten$bj<1958, 1, 2)
table(daten$bj.cat)

# Erstelle Teildatensätze/data.frames in denen nur die Beobachtungen mit
# 'bj.cat=1' (data.frame 'daten_bj1') bzw. 
# 'bj.cat=2' (data.frame 'daten_bj2') enthalten sind.
# erweist sich später als nützlich
daten_bj1 <- subset(daten, bj.cat==1)
daten_bj2 <- subset(daten, bj.cat==2)


################################ AUFGABE c) ####################################

# Verwende zur einfachen Lösung die vorher erstellten Teildatensätze
# 'daten_bj1' und 'daten_bj2'

# Variable NETTOMIETE: 'nm'

# Fünf Punkte Zusammenfassung
summary(daten_bj1$nm)
summary(daten_bj2$nm)

# Standardabweichung 
sd(daten_bj1$nm)
sd(daten_bj2$nm)

# Varianz
var(daten_bj1$nm)
var(daten_bj2$nm)

# Interquartilsabstand
IQR(daten_bj1$nm)
IQR(daten_bj2$nm)

# ALTERNATIVE BERECHNUNGSWEISEN
# am Beispiel der Fünf Punkte Zusammenfassung

# Alternative 1
# Wende 'summary' nur auf Beobachtungen an, die eine Bedingung erfüllen. 
# Der logische Ausdruck 'daten$bj.cat==1' innerhalb der eckigen Klammern 
# bewirkt, dass durch diejenigen Beobachtungen in 'daten' verwendet werden, 
# welche die Bedingung erfüllen
summary(daten$nm[daten$bj.cat==1])
summary(daten$nm[daten$bj.cat==2])

# Alternative 2
# Verwende die Funktion 'tapply'
# tapply führt 'summary' für jeden verschiedenen Wert (hier 1 und 2) in
# 'bj.cat' aus
# Vorteil: Funktioniert sehr elegant wenn bezüglich mehr als 2 Werten 
# geschichtet werden soll (vergleiche auch Aufgabe d) )
tapply(daten$nm,daten$bj.cat,summary) 


# Histogramme nebeneinander zeichnen am Bildschirm
# Falls separate Plots gewünscht dann 'par Befehle nicht ausführen 
par(mfrow = c(1, 2))
hist(daten_bj1$nm, main="Baujahr < 1958", xlab="Nettomiete", ylab="Dichte",
     xlim=c(0,6000), freq=FALSE)
hist(daten_bj2$nm, main="Baujahr 1958 und jünger", xlab="Nettomiete", ylab="Dichte",
     freq=FALSE)
par(mfrow = c(1, 1))

# Histogramme nochmal im pdf Format erzeugen und speichern 
pdf("loes2_8_hist_nm_by_bj_cat1.pdf")
par(cex=1.5)
hist(daten_bj1$nm, main="Baujahr < 1958", xlab="Nettomiete", ylab="Dichte",
     xlim=c(0,6000), freq=FALSE)
par(cex=1)
dev.off()

pdf("loes2_8_hist_nm_by_bj_cat2.pdf")
par(cex=1.5)
hist(daten_bj2$nm, main="Baujahr 1958 und jünger", xlab="Nettomiete", ylab="Dichte",
     freq=FALSE)
par(cex=1)
dev.off()


# Boxplots zeichnen
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'boxplot' Befehl ausführen
pdf("loes2_8_boxplot_nm_by_bj_cat.pdf")
par(cex=1.5)
boxplot(daten$nm~daten$bj.cat, xlab=" ", main="Boxplots Nettomiete", ylab="Nettomiete")
par(cex=1)
dev.off()


# Variable NETTOMIETE PRO QUADRATMETER: 'nmqm'

# Fünf Punkte Zusammenfassung
summary(daten_bj1$nmqm)
summary(daten_bj2$nmqm)

# Standardabweichung 
sd(daten_bj1$nmqm)
sd(daten_bj2$nmqm)

# Varianz
var(daten_bj1$nmqm)
var(daten_bj2$nmqm)

# Interquartilsabstand
IQR(daten_bj1$nmqm)
IQR(daten_bj2$nmqm)


# Histogramme nebeneinander zeichnen am Bildschirm
# Falls separate Plots gewünscht dann 'par Befehle nicht ausführen 
par(mfrow = c(1, 2))
hist(daten_bj1$nmqm, main="Baujahr < 1958", xlab="Nettomiete pro qm", 
     ylab="Dichte", xlim=c(0,25), freq=FALSE)
hist(daten_bj2$nmqm, main="Baujahr 1958 und jünger", xlab="Nettomiete pro qm", 
     ylab="Dichte", xlim=c(0,25), freq=FALSE)
par(mfrow = c(1, 1))

# Histogramme nochmal im pdf Format erzeugen und speichern 
pdf("loes2_8_hist_nmqm_by_bj_cat1.pdf")
par(cex=1.5)
hist(daten_bj1$nmqm, main="Baujahr < 1958", xlab="Nettomiete pro qm", 
     ylab="Dichte", xlim=c(0,25), freq=FALSE)
par(cex=1)
dev.off()

pdf("loes2_8_hist_nmqm_by_bj_cat2.pdf")
par(cex=1.5)
hist(daten_bj2$nmqm, main="Baujahr 1958 und jünger", xlab="Nettomiete pro qm", 
     ylab="Dichte", xlim=c(0,25), freq=FALSE)
par(cex=1)
dev.off()


# Boxplots zeichnen
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'boxplot' Befehl ausführen
pdf("loes2_8_boxplot_nmqm_by_bj_cat.pdf")
par(cex=1.5)
boxplot(daten$nmqm~daten$bj.cat, xlab=" ", main="Boxplots Nettomiete pro qm",
        ylab="Nettomiete pro qm")
par(cex=1)
dev.off()


################################ AUFGABE d) ####################################

# Verwende für Fünf Punkte Zusammenfassung, Standardabweichung, Varianz, und
# Interquartilsabstand 'tapply' Funktion wegen der vielen verschiedenen Werte
# der Schichtungsvariabe 'rooms'.


# Variable NETTOMIETE 'nm'

# Fünf Punkte Zusammenfassung
tapply(daten$nm, daten$rooms, summary)

# Standardabweichung 
tapply(daten$nm, daten$rooms, sd)

# Varianz
tapply(daten$nm, daten$rooms, var)

# Interquartilsabstand
tapply(daten$nm, daten$rooms, IQR)


# Histogramme nebeneinander zeichnen
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur die 'par' und 'hist' Befehle ausführen
pdf("loes2_8_hist_nm_by_rooms.pdf")
par(mfrow = c(3, 3))
hist(daten$nm[daten$rooms==1], main="1 Zimmer", xlab="Nettomiete", ylab="Dichte",
     freq=FALSE)
hist(daten$nm[daten$rooms==2], main="2 Zimmer", xlab="Nettomiete", ylab="Dichte",
     freq=FALSE)
hist(daten$nm[daten$rooms==3], main="3 Zimmer", xlab="Nettomiete", ylab="Dichte",
     freq=FALSE)
hist(daten$nm[daten$rooms==4], main="4 Zimmer", xlab="Nettomiete", ylab="Dichte",
     freq=FALSE)
hist(daten$nm[daten$rooms==5], main="5 Zimmer", xlab="Nettomiete", ylab="Dichte",
     freq=FALSE)
hist(daten$nm[daten$rooms==6], main="6 Zimmer", xlab="Nettomiete", ylab="Dichte",
     freq=FALSE)
hist(daten$nm[daten$rooms==8], main="8 Zimmer", xlab="Nettomiete", ylab="Dichte",
     freq=FALSE)
par(mfrow = c(1, 1))
dev.off()


# Boxplots zeichnen
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'boxplot' Befehl ausführen
pdf("loes2_8_boxplot_nm_by_rooms.pdf")
par(cex=1.5)
boxplot(daten$nm~daten$rooms, main="Boxplots Nettomiete", 
        xlab="Anzahl Zimmer", ylab="Nettomiete")
par(cex=1)
dev.off()


# Variable NETTOMIETE 'nmqm'

# Fünf Punkte Zusammenfassung
tapply(daten$nmqm, daten$rooms, summary)

# Standardabweichung
tapply(daten$nmqm, daten$rooms, sd)

# Varianz
tapply(daten$nmqm, daten$rooms, var)

# Interquartilsabstand
tapply(daten$nmqm, daten$rooms, IQR)


# Histogramme nebeneinander zeichnen
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur die 'par' und 'hist' Befehle ausführen
pdf("loes2_8_hist_nmqm_by_rooms.pdf")
par(mfrow = c(3, 3))
hist(daten$nmqm[daten$rooms==1], main="1 Zimmer", xlab="Nettomiete", ylab="Dichte",
     freq=FALSE)
hist(daten$nmqm[daten$rooms==2], main="2 Zimmer", xlab="Nettomiete", ylab="Dichte",
     freq=FALSE)
hist(daten$nmqm[daten$rooms==3], main="3 Zimmer", xlab="Nettomiete", ylab="Dichte",
     freq=FALSE)
hist(daten$nmqm[daten$rooms==4], main="4 Zimmer", xlab="Nettomiete", ylab="Dichte",
     freq=FALSE)
hist(daten$nmqm[daten$rooms==5], main="5 Zimmer", xlab="Nettomiete", ylab="Dichte",
     freq=FALSE)
hist(daten$nmqm[daten$rooms==6], main="6 Zimmer", xlab="Nettomiete", ylab="Dichte",
     freq=FALSE)
hist(daten$nmqm[daten$rooms==8], main="8 Zimmer", xlab="Nettomiete", ylab="Dichte",
     freq=FALSE)
par(mfrow = c(1, 1))
dev.off()


# Boxplots zeichnen
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'boxplot' Befehl ausführen
pdf("loes2_8_boxplot_nmqm_by_rooms.pdf")
par(cex=1.5)
boxplot(daten$nmqm~daten$rooms, main="Boxplots Nettomiete pro qm", 
        xlab="Anzahl Zimmer", ylab="Nettomiete")
par(cex=1)
dev.off()


################################ AUFGABE e) ####################################

# Zeichne QQ- Plot inklusive Winkelhalbierende mit qqline
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur die 'qqnorm' und 'qqline' Befehle ausführen
pdf("loes2_8_hist_nmqm_qqplot.pdf")
par(cex=1.5)
qqnorm(daten$nmqm, xlab="theoretische Quantile", ylab="Stichprobenquantile")
qqline(daten$nmqm)
par(cex=1)
dev.off()

# Histogramm und Kerndichteschätzer
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur die 'hist' und 'lines' Befehle ausführen
pdf("loes2_8_hist_nmqm_kdens.pdf")
par(cex=1.5)
hist(daten$nmqm, main=" ", xlab="Nettomiete", ylab="Dichte", freq=FALSE)
lines(density(daten$nmqm),col="black")
par(cex=1)
dev.off()

# Histogramm und Normalverteilungsdichte
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann den 'pdf' und 'dev.off()' Befehl weglassen.
pdf("loes2_8_hist_nmqm_gauss.pdf")
par(cex=1.5)
hist(daten$nmqm, main=" ", xlab="Nettomiete", ylab="Dichte", freq=FALSE)
mu <- mean(daten$nmqm)
sigma <- sd(daten$nmqm)
curve(dnorm(x,mean=mu,sd=sigma),from=mu - 4*sigma,to=mu + 4*sigma,add=TRUE)
par(cex=1)
dev.off()


################################ AUFGABE f) ####################################

# Momentenkoeffizient der Schiefe und Wölbungsmaß nach Fisher 

# Nettomiete
skewness(daten$nm)
kurtosis(daten$nm)-3

# Nettomiete pro qm
skewness(daten$nmqm)
kurtosis(daten$nmqm)-3

