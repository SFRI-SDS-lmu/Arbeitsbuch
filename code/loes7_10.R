##############################################################
# Lösung Aufgabe 7.10 
##############################################################

# Lösche alle möglicherweise vorhandenen Objekte im Arbeitsbereich
rm(list = ls())

# Für die Reproduzierbarkeit setzen wir einen Seed
set.seed(5671345)


# Funktion plot.dichten
# Zeichnet die Dichten. 
# Als x-Achse wählen wir den Bereich (-5, +5), 
# für die y-Achse den Bereich (0,1), auch wenn Dichten größer 1 werden können.

plot.dichten <- function(n){

# Simuliere N(0,1) verteilte Zufallszahlen
x <- rnorm(n, mean=0, sd=1)

# Simuliere Chi2 verteilte Zufallszahlen mit n Freiheitsgraden
z <- rchisq(n, df=n)

# Erzeuge aus x und z t-verteilte Zufallszahlen 
t <- x/sqrt(z/n)

# Dichte der t-Verteilung mit n Freiheitsgraden an 30 verschiedenen Werten
# im Bereich -5 bis 5 auswerten und speichern  
werte <- seq(-5, 5, length = 30)
t.dichte <- dt(werte, df=n)
  
# Zeichne die Kerndichteschätzung für t
plot(density(t), xlim=c(-5,5), ylim=c(0,1), 
     main="Kerndichte plus t-Verteilungsdichte", lty=1)
# Füge der Grafik Dichte der t- Verteilung hinzu
lines(werte, t.dichte, lwd = 1, lty=2)
legend(-3, 0.8, legend=c("Kerndichte","t-Verteilung"),
       lty=1:2, cex=0.8)

# arithmetisches Mittel und Varianzen ausgeben, 
# theoretische Varianz n/(n-2) der t-Verteilung mit n Freiheitsgraden in 
# Klammern ausgeben
cat("n: ", n, ", Mittelwert: ", mean(t), ", Varianz: ", var(t), "(",n/(n-2), ") \n")                            
}


# Anwendung der Funktion für verschiedene n
plot.dichten(5)
plot.dichten(8)
plot.dichten(15)
plot.dichten(20)
plot.dichten(25)
plot.dichten(30)
plot.dichten(50)
plot.dichten(100)


# Alternativ in einer Schleife
n<-c(5,8,15,20,25,30,50,100)
for (j in 1:length(n)){
  plot.dichten(n[j])
}


# Alternativ in einer Schleife und speichern im pdf Format
n<-c(5,8,15,20,25,30,50,100)
for (j in 1:length(n)){
  pdf(paste("loes7_10_dichten",as.character(n[j]),".pdf",sep=""))
  par(cex=1.5)
  plot.dichten(n[j])
  dev.off()
}




# wahre Varianzen
n/(n-2)

