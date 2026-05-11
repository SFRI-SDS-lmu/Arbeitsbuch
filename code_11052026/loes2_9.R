################################################################################
# Aufgabe 2.9 
################################################################################


############################### VORBEREITUNG ###################################

# Lösche alle möglicherweise vorhandenen Objekte im Arbeitsbereich
rm(list = ls())

library("tseries")
library("moments")


################################# Aufgabe a) ################################### 

# Einlesen der Daten 

# Zeitraum 1.8.2007 bis 31.7.2008
bmw_vor <- get.hist.quote(start="2007-08-01", end="2008-07-31", 
                      instrument="bmw.de", quote="AdjClose")
r.bmw_vor <- diff(log(bmw_vor$Adjusted))

# Zeitraum August 2008 bis 31.12.2009
bmw_nach <- get.hist.quote(start="2008-08-01", end="2009-12-31", 
                      instrument="bmw.de", quote="AdjClose")
r.bmw_nach <- diff(log(bmw_nach$Adjusted))


################################# Aufgabe b) ################################### 

# Arithmetisches Mittel
print(mean(r.bmw_vor))   # vorher
print(mean(r.bmw_nach))  # nachher

# Median 
print(median(r.bmw_vor))  # vorher
print(median(r.bmw_nach)) # nachher

# Standardabweichung
print(sd(r.bmw_vor))      # vorher
print(sd(r.bmw_nach))     # nachher


################################# Aufgabe c) ################################### 


###################### Histogramme vor Kurseinbruch ############################

# Histogramm mit Standardeinstellungen 'breaks=14'
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'hist' Befehl ausführen
pdf("loes2_9_vor_hist.pdf")
par(cex=1.5)
h<-hist(r.bmw_vor,main="breaks=14 (Standard)", ylab="Dichte",
     xlab="log. Rendite BMW Aktie", freq=FALSE)             
dev.off()

summary(h)                        


# Histogramm mit weniger Breaks als Standard
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'hist' Befehl ausführen
pdf("loes2_9_vor_hist2.pdf")
par(cex=1.5)
hist(r.bmw_vor, breaks=10,main="breaks=10", ylab="Dichte",
     xlab="log. Rendite BMW Aktie", freq=FALSE) 
dev.off()


# Histogramm mit mehr Breaks als Standard
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'hist' Befehl ausführen
pdf("loes2_9_vor_hist3.pdf")
par(cex=1.5)
hist(r.bmw_vor, breaks=20,main="breaks=20", ylab="Dichte",
     xlab="log. Rendite BMW Aktie", freq=FALSE)
dev.off()


####################### Histogramme nach Kurseinbruch ##########################


# Histogramm mit Standardeinstellungen 'breaks=16'
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'hist' Befehl ausführen
pdf("loes2_9_nach_hist.pdf")
par(cex=1.5)
h<-hist(r.bmw_nach,main="breaks=16 (Standard)", ylab="Dichte",
        xlab="log. Rendite BMW Aktie", freq=FALSE)     
dev.off()

summary(h)          


# Histogramm mit weniger Breaks als Standard
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'hist' Befehl ausführen
pdf("loes2_9_nach_hist2.pdf")
par(cex=1.5)
hist(r.bmw_nach, breaks=10,main="breaks=10", ylab="Dichte",
     xlab="log. Rendite BMW Aktie", freq=FALSE) 
dev.off()


# Histogramm mit mehr Breaks als Standard
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'hist' Befehl ausführen
pdf("loes2_9_nach_hist3.pdf")
par(cex=1.5)
hist(r.bmw_nach, breaks=20,main="breaks=20", ylab="Dichte",
     xlab="log. Rendite BMW Aktie", freq=FALSE) 
dev.off()


############ Histogramme vorher nachher in einer Grafik vergleichen ###########

# zur besseren Vergleichbarkeit verwende gleiche Skala auf der vertikalen Achse

# Vor dem Kurseinbruch
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'hist' Befehl ausführen
pdf("loes2_9_vor_hist_vergl.pdf")
par(cex=1.5)
hist(r.bmw_vor,main="Vor dem Kurseinbruch", ylab="Dichte",
     xlab="log. Rendite BMW Aktie", freq=FALSE, xlim=c(-0.15,0.12),
     ylim=c(0,25))             
dev.off()

# Nach dem Kurseinbruch
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'hist' Befehl ausführen
pdf("loes2_9_nach_hist_vergl.pdf")
par(cex=1.5)
hist(r.bmw_nach,main="Nach dem Kurseinbruch", ylab="Dichte",
     xlab="log. Rendite BMW Aktie", freq=FALSE, xlim=c(-0.15,0.12),
     ylim=c(0,25))
dev.off()


#################### Alternative Darstellung: Boxplots #########################

# zur besseren Vergleichbarkeit verwende gleiche Skala auf der 
# horizontalen Achse

# Vor dem Kurseinbruch
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'hist' Befehl ausführen
pdf("loes2_9_vor_box.pdf")
par(cex=1.5)
boxplot(r.bmw_vor,main="vor dem Kurseinbruch",ylim=c(-0.15,0.12))    
dev.off()

# Nach dem Kurseinbruch
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'hist' Befehl ausführen
pdf("loes2_9_nach_box.pdf")
par(cex=1.5)
boxplot(r.bmw_nach,main="nach dem Kurseinbruch",ylim=c(-0.15,0.12)) 
dev.off()


################################# Aufgabe d) ################################### 

# Schiefe (Quartilskoeffizient) und Wölbung

# vorher
qc.bmw_vor <- ( (quantile(r.bmw_vor,prob=0.75)-median(r.bmw_vor) ) -
              (median(r.bmw_vor) - quantile(r.bmw_vor,prob=0.25)) ) /
  (quantile(r.bmw_vor,prob=0.75)-quantile(r.bmw_vor,prob=0.25) )
cbmw_vor <- kurtosis(r.bmw_vor) -3
print(qc.bmw_vor)
print(cbmw_vor)

# nachher
qc.bmw_nach <- ( (quantile(r.bmw_nach,prob=0.75)-median(r.bmw_nach) ) -
                  (median(r.bmw_nach) - quantile(r.bmw_nach,prob=0.25)) ) /
  (quantile(r.bmw_nach,prob=0.75)-quantile(r.bmw_nach,prob=0.25) )
cbmw_nach <- kurtosis(r.bmw_nach) -3
print(qc.bmw_nach)
print(cbmw_nach)

