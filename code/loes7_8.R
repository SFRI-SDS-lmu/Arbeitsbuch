################################################################################
# Lösung Aufgabe 7.8
################################################################################


############################### VORBEREITUNG ###################################

# Lösche alle möglicherweise vorhandenen Objekte im Arbeitsbereich
rm(list = ls())

# Für die Reproduzierbarkeit setzen wir einen seed
set.seed(123)  


################################ AUFGABE a) ####################################

# Zunächst exemplarisch für n=100

# Ziehe n=100 Zufallszahlen 'x' aus einer B(100,0.5) Verteilung
n <- 100
p <- 0.5
x <- rbinom(n=n, size = n, prob = p)

# relative Häufigkeiten erzeugen
relh<-table(x) / n

# Grafische Darstellung mit barplot Funktion
# da für z.B. weniger als 40 Erfolge keine Beobachungen vorliegen,
# wird für diese und andere Werte in der Grafik keine Häufigkeit (von 0)
# ausgegeben.

# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'barplot' Befehl ausführen
pdf("loes7_8_barplot_bsp1.pdf")
par(cex=1.5)
barplot(relh, xlab="Anzahl Erfolge", ylab="Relative Häufigkeit")
par(cex=1)
dev.off()


# Alternative Darstellung des Säulendiagramms mit allen möglichen Werten
# von 0 bis n=100

alle_werte <- as.character(0:n)
relh_gesamt <- setNames(rep(0, length(alle_werte)), alle_werte)
relh_gesamt[names(relh)] <- relh

# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'barplot' Befehl ausführen
pdf("loes7_8_barplot_bsp2.pdf")
par(cex=1.5)
barplot(relh_gesamt, xlab="Anzahl Erfolge", 
ylab="Relative Häufigkeit",xlim=c(0,n))
par(cex=1)
dev.off()


# Vergleich mit der Wahrscheinlichkeitsfunktion
werte <- 0:n
wkeiten<- dbinom(werte, size = n, prob = p)

# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'barplot' Befehl ausführen
pdf("loes7_8_barplot_bsp2_2.pdf")
par(cex=1.5)
barplot(wkeiten, names.arg=werte, xlab="Anzahl Erfolge", 
ylab="Wahrscheinlichkeit")
par(cex=1)
dev.off()


# Alternative 1: beide Säulendiagramme nebeneinander zeichnen
par(mfrow = c(1, 2))
barplot(relh_gesamt, xlab="Anzahl Erfolge", ylab="Relative Häufigkeit",xlim=c(0,n))
barplot(wkeiten, names.arg=werte,  xlab="Anzahl Erfolge", ylab="Wahrscheinlichkeit")
par(mfrow = c(1, 1)) # Standardeinstellung für Grafiken wiederherstellen

# Alternative 2: 
# Relative Häufigkeiten und Wahrscheinlichkeiten in einer Grafik darstellen

# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur jeweils den 'plot' und 'points' Befehl ausführen
pdf("loes7_8_barplot_bsp3.pdf")
par(cex=1.5)
plot(alle_werte,relh_gesamt,col="black",xlab="Anzahl Erfolge", 
ylab="Relative Häufigkeit / W.keit")
points(alle_werte,wkeiten,col="grey",xlab="Anzahl Erfolge")
par(cex=1)
dev.off()


# Wir schreiben jetzt eine allgemeine Funktion plot.vergleich(),
# so dass die Erzeugung und Darstellung für beliebiges n funktioniert
# Achtung: 
# Wir vergleichen immer mit der Wahrscheinlichkeitskeitsverteilung 
# mit p=0.5, so dass wir die Funktion dann auch für Aufgabe b) anwenden können.
plot.vergleich <- function(n,p) {
  x <- rbinom(n=n, size = n, prob = p)
  relh<-table(x) / n
  alle_werte <- as.character(0:n)
  relh_gesamt <- setNames(rep(0, length(alle_werte)), alle_werte)
  relh_gesamt[names(relh)] <- relh
  plot(alle_werte,relh_gesamt,col="black", 
       xlab="Anzahl Erfolge", ylab="Relative Häufigkeit / W.keit")
  
  werte <- 0:n
  wkeiten<- dbinom(werte, size = n, prob = 0.5)
  points(alle_werte,wkeiten,col="grey",xlab="Anzahl Erfolge", ylab="Wahrscheinlichkeit")
}  


# Verwende die Funktion für n=10,100,1000,10000

# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur jeweils den 'plot' Befehl ausführen
pdf("loes7_8_vergleich_n10.pdf")
par(cex=1.5)
plot.vergleich(10,0.5)
par(cex=1)
dev.off()

# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur jeweils den 'plot' Befehl ausführen
pdf("loes7_8_vergleich_n100.pdf")
par(cex=1.5)
plot.vergleich(100,0.5)
par(cex=1)
dev.off()

# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur jeweils den 'plot' Befehl ausführen
pdf("loes7_8_vergleich_n1000.pdf")
par(cex=1.5)
plot.vergleich(1000,0.5)
par(cex=1)
dev.off()

# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur jeweils den 'plot' Befehl ausführen
pdf("loes7_8_vergleich_n10000.pdf")
par(cex=1.5)
plot.vergleich(10000,0.5)
par(cex=1)
dev.off()


################################ AUFGABE b) ####################################

# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur jeweils den 'plot' Befehl ausführen
pdf("loes7_8_vergleich2_n10.pdf")
par(cex=1.5)
plot.vergleich(10,0.51)
par(cex=1)
dev.off()

# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur jeweils den 'plot' Befehl ausführen
pdf("loes7_8_vergleich2_n100.pdf")
par(cex=1.5)
plot.vergleich(100,0.51)
par(cex=1)
dev.off()

# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur jeweils den 'plot' Befehl ausführen
pdf("loes7_8_vergleich2_n1000.pdf")
par(cex=1.5)
plot.vergleich(1000,0.51)
par(cex=1)
dev.off()

# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur jeweils den 'plot' Befehl ausführen
pdf("loes7_8_vergleich2_n10000.pdf")
par(cex=1.5)
plot.vergleich(10000,0.51)
par(cex=1)
dev.off()


################################ AUFGABE c) ####################################

# Simuliere Poisson verteilte Zufallszahlen
n <- 1000
y1 <- rpois(n, lambda=2)
y2 <- rpois(n, lambda=5)

# Berechne Summe, Differenz und Produkt
summe <- y1+y2
differenz <- y1-y2
produkt <- y1*y2

# Bestimme Differenz arithmetisches Mittel und Erwartungswert
2+5-mean(summe)
2-5-mean(differenz)
2*5-mean(produkt)

# Zum Vergleich noch Kerndichteschätzer bzw. Histogramme von Summe, Differenz
# und Produkt
plot(density(summe))
hist(summe)
plot(density(differenz))
hist(differenz)
plot(density(produkt))
hist(produkt)

