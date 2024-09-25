##############################################################
# Lösung Aufgabe 7.13 im Arbeitsbuch, Aufgabe 7.10 im Lehrbuch
##############################################################

set.seed(5671345)

n <- c(5,8,15,20,25,30,50,100)
# Hier speichern wir die Sequenzen der Zufallszahlen
# für die unterschiedlichen n's
t <- list()

# Simuliere n t-verteilte Zufallszahlen mit n Freiheitsgraden
simulate.t <- function(n) {
  x <- rnorm(n, mean=0, sd=1)
  z <- rchisq(n, df=n)
  t <- x/sqrt(z/n)
  return(t)
}

# Plot der Dichten. Als x-Achse wählen wir den Bereich (-3, +3), für die y-Achse 
# den Bereich (0,1), auch wenn Dichten größer 1 werden können.

plot.dichten <- function(t){

  n.akt <- length(t)
  s <- simulate.t(n.akt)

  # Raster auf der x-Achse
  x <- seq(-3, 3, length = 30)

  # Dichte der zentralen t-Verteilung am Raster auswerten
  t.dichte <- dt(x, df=n.akt)
  
  plot(density(s), xlim=c(-3,3), ylim=c(0,1), 
       main="Kerndichte plus t-Verteilungsdichte",
       lty=1)
  lines(x, t.dichte, lwd = 1, lty=2)
  legend(-3, 0.8, legend=c("Kerndichte","t-Vert."),
         lty=1:2, cex=0.8)
}

# Alle Fälle nacheinander
for (j in 1:length(n)){
  t[[j]] <- simulate.t( n[j]) # Achtung: [[]] für Listen
}
  

plot.dichten(t[[1]])
plot.dichten(t[[2]])
plot.dichten(t[[3]])
plot.dichten(t[[4]])
plot.dichten(t[[5]])
plot.dichten(t[[6]])
plot.dichten(t[[7]])
plot.dichten(t[[8]])


# in einer Schleife
for (j in 1:length(n)){
  pdf(paste("loes7_10_dichten",as.character(n[j]),".pdf",sep=""))
  par(cex=1.5)
  plot.dichten(t[[j]])
  dev.off()
}

# arithmetische Mittel und Varianzen
for (j in 1:length(n)) {
  cat("n: ", n[j], ", Mittelwert: ", mean(t[[j]]), 
      ", Varianz: ", 1/length(t[[j]])*sum( (t[[j]]-mean(t[[j]]))^2 ),
      "\n")                            
}
# wahre Varianzen
n/(n-2)

