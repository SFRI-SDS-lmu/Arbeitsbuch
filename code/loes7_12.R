##############################################################
# Lösung Aufgabe 7.12
##############################################################


# Lösche alle möglicherweise vorhandenen Objekte im Arbeitsbereich
rm(list = ls())

# Für die Reproduzierbarkeit setzen wir einen Seed
set.seed(3134561)

# Zunächst Illustration im Fall n=100, pi=0.4
pi <- 0.4
n<-100

# Erzeuge 1000 Zufallszahlen aus einer B(n=100,pi=0.4) Verteilung
Hn<-rbinom(1000, size = n, prob = pi)
# Bilde die standardisierten Summen
stand_Hn<-(Hn-n*pi)/sqrt(n*pi*(1-pi))

# Zeichne das Histogramm der 1000 standardisierten Summen
# freq=FALSE zeichnet das Histogramm mit relativen Häufigkeiten anstatt mit
# absoluten Häufigkeiten
# Mit Hilfe von paste0 können numerische Variablen im Titel verwendet werden
hist(stand_Hn,freq=FALSE,main=paste0("n=", n, ", pi=", pi),
xlab="standardisierte Summe", ylab="Dichte",xlim=c(-4,4),ylim=c(0,0.5))
# Füge der Grafik die Dichte der Standardnormalverteilung hinzu im Bereich
# -4 bis 4
curve( dnorm(x), from=-4, to=4, add=T, col="red")


# Jetzt schreiben wir eine kleine Funktion 'demoivre' für beliebiges pi und n
demoivre <- function(n,pi){
 
  Hn<-rbinom(1000, size = n, prob = pi)
  stand_Hn<-(Hn-n*pi)/sqrt(n*pi*(1-pi))
  hist(stand_Hn,freq=FALSE, main=paste0("n=", n, ", pi=", pi),
  xlab="standardisierte Summe", ylab="Dichte",xlim=c(-4,4), ylim=c(0,0.5))
  curve( dnorm(x), from=-4, to=4, add=T, col="red")
  
}


# Anwendung der Funktion für verschiedene n und pi
demoivre(5,0.4)
demoivre(20,0.4)
demoivre(30,0.4)
demoivre(50,0.4)
demoivre(100,0.4)
demoivre(1000,0.4)

demoivre(5,0.04)
demoivre(20,0.04)
demoivre(30,0.04)
demoivre(50,0.04)
demoivre(100,0.04)
demoivre(1000,0.04)


# Verwende alternativ Schleifen
n<-c(5,20,30,50,100,1000)
for (j in 1:length(n)){
  demoivre(n[j],0.4)
}

for (j in 1:length(n)){
  demoivre(n[j],0.04)
}


# Speichere alternativ die Grafiken im pdf Format statt Ausgabe am Bildschirm
for (j in 1:length(n)){
  pdf(paste("loes7_12_hist_",as.character(n[j]),"_04.pdf",sep=""))
  par(cex=1.5)
  demoivre(n[j],0.4)
  dev.off()
}

for (j in 1:length(n)){
  pdf(paste("loes7_12_hist_",as.character(n[j]),"_004.pdf",sep=""))
  par(cex=1.5)
  demoivre(n[j],0.04)
  dev.off()
}



