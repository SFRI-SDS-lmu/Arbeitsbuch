##############################################################
# Lösung Aufgabe 2.9 
##############################################################

library("tseries")
library("moments")

# Aufgabe a) 
# Einlesen der Daten 

# Zeitraum 1.8.2007 bis 31.7.2008
bmw_vor <- get.hist.quote(start="2007-08-01", end="2008-07-31", 
                      instrument="bmw.de", quote="AdjClose")
r.bmw_vor <- diff(log(bmw_vor$Adjusted))

# Zeitraum August 2008 bis 31.12.2009
bmw_nach <- get.hist.quote(start="2008-08-01", end="2009-12-31", 
                      instrument="bmw.de", quote="AdjClose")
r.bmw_nach <- diff(log(bmw_nach$Adjusted))


# Aufgabe b) 
# Arithmetisches Mittel
print(mean(r.bmw_vor))   # vorher
print(mean(r.bmw_nach))  # nachher

# Median 
print(median(r.bmw_vor))  # vorher
print(median(r.bmw_nach)) # nachher

# Standardabweichung
print(sd(r.bmw_vor))      # vorher
print(sd(r.bmw_nach))     # nachher

# Aufgabe c)
# Histogramme vorher
par(mfrow=c(2,2))
# Histogramm mit Standardeinstellungen
# Standard breaks=14   
hist(r.bmw_vor,main="breaks=14 (Standard)")             
summary(h)                        
# Histogramm mit weniger Breaks als Standard
hist(r.bmw_vor, breaks=10,main="breaks=10") 
# Histogramm mit mehr Breaks als Standard
hist(r.bmw_vor, breaks=20,main="breaks=20") 

# Histogramme nachher
par(mfrow=c(2,2))
# Histogramm mit Standardeinstellungen
# Standard breaks=16
hist(r.bmw_nach,main="breaks=16 (Standard)")     
summary(h)          
# Histogramm mit weniger Breaks als Standard
hist(r.bmw_nach, breaks=10,main="breaks=10") 
# Histogramm mit mehr Breaks als Standard
hist(r.bmw_nach, breaks=20,main="breaks=20") 

# Histogramme vorher nachher in einer Grafik vergleichen
# zur besseren Vergleichbarkeit verwende gleiche Skala auf der vertikalen Achse
par(mfrow=c(1,2))
hist(r.bmw_vor,xlim=c(-0.15,0.12))
hist(r.bmw_nach,xlim=c(-0.15,0.12))

# Alternative Darstellung: Boxplots 
# zur besseren Vergleichbarkeit verwende gleiche Skala auf der horizontalen Achse
par(mfrow=c(1,2))
boxplot(r.bmw_vor,main="vorher",ylim=c(-0.15,0.12))    # vorher
boxplot(r.bmw_nach,main="nachher",ylim=c(-0.15,0.12)) # nachher


# Aufgabe d)
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

