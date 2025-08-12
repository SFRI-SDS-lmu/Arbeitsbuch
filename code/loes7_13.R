##############################################################
# Lösung Aufgabe 7.13 
##############################################################

# Lösche alle möglicherweise vorhandenen Objekte im Arbeitsbereich
rm(list = ls())

# Für die Reproduzierbarkeit setzen wir einen Seed
set.seed(3134561)

# Zeige zunächst, dass der Erwartungswert bei einer 
# Cauchy Verteilung nicht existiert.
# Zeichne den Mittelwert in Abhängigkeit vom Stichprobenumfang n  

n<-10000
# Simuliere Cauchy verteilte Zufallszahlen
zcauchy <-rcauchy(n)

# Berechne Xquer_n
mean_n<-rep(0,n) 
for (j in 1:n){
  mean_n[j] <- mean(zcauchy[1:j])
}

# Zeichne Xquer_n gegen n
t<-1:n
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den plot Befehl ausführen
pdf(paste("loes7_13_xquer_n",".pdf",sep=""))
par(cex=1.5)
plot(t,mean_n,xlab="n",ylab="arithmetisches Mittel",type="l")
dev.off()


# Simuliere in einem zweiten Schritt insgesamt S=1000 Mittelwerte von jeweils
# n = 1000 simulierten Cauchy verteilten Zufallszahlen und visualisiere 
# die Wahrscheinlichkeitsdichte in Form eines Kerndichteschätzers.
n <- 1000
S <- 1000

# Definiere eine Matrix mit n Zeilen und S Spalten
# in jeder Spalte werden n Cauchy verteilte Zufallszahlen gespeichert
rv <- matrix(nrow=n, ncol=S, data=rcauchy(n=S*n))

# colMeans liefert die S Spaltenmittelwerte aus der Matrix rv 
means <- colMeans(rv)

# Jetzt zeichnen wir noch einen Kerndichteschätzer der in 'means' 
# gespeicherten Mittelwerte 
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den plot Befehl ausführen
pdf(paste("loes7_13_hist_cauchy",".pdf",sep=""))
par(cex=1.5)
plot(density(means, adjust=1.5), type="l", main="", ylab="Dichte", 
xlab="arithmetisches Mittel")
dev.off()

# Zeichne die Verteilung des Mittelwerts nur im Wertebereich -50 bis 50
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den plot Befehl ausführen
pdf(paste("loes7_13_hist_cauchy_2",".pdf",sep=""))
par(cex=1.5)
plot(density(means, adjust=1.5), type="l", main="", xlim=c(-50,50), 
ylab="Dichte", xlab="arithmetisches Mittel")
dev.off()

# Bestimme in einem dritten und letzten Schritt noch deskriptive 
# Kennzahlen der simulierten Mittelwerte
summary(means)

