################################################################################
# Lösung Aufgabe 9.7 
################################################################################


################################ AUFGABE a) ####################################

# Realisierte Werte im Vektor 'x' zusammenfassen
x <- c(2, 4, 6, 3)

# Funktion zur Berechnung der Likelihood
# Achtung: nur für sehr kleine n so berechnen, da das Multiplizieren 
# vieler Wahrscheinlichkeiten zu einem underflow führt -> Likelihood wird 0.
# Für große n immer erst die log-likelihood berechnen und anschließend 
# die Exponentialfunktion anwenden.

# "zu Fuss"
poisson.likelihood <- function(x, lambda){
  l <- prod( lambda^x/factorial(x)*exp(-lambda) )
  l
}

# Alternativ über die Funktion dpois
poisson.likelihood2 <- function(x, lambda){
  l <- prod( dpois(x, lambda))
  l
}

# Log-likelihood
poisson.loglikelihood <- function(x, lambda){
  l <- sum( dpois(x, lambda, log=TRUE))
  l
}

print("Likelihood und Log-Likelihood, beispielsweise für lambda=2: ")
poisson.likelihood(x,2)
poisson.likelihood2(x,2)
poisson.loglikelihood(x,2)

# Berechne Likelihood und Log-Likelihood für äquidistante lambda Werte
# im Bereich 1-7 mit Schrittweite 0.05
# Speichere die Werte der Likelihood im Vektor 'f' und der Log-Likelihood 
# im Vektor 'log.f'
lambda <- seq(from=1, to=7, by=0.05)
f <- numeric(length(lambda))
log.f <- numeric(length(lambda))

for (i in 1:length(lambda)){
  f[i] <- poisson.likelihood(x, lambda[i])
  log.f[i] <- poisson.loglikelihood(x, lambda[i])
}

# Visualisiere die Likelihood und markiere den ML Schätzer (Wert 3.75)  
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den plot und abline Befehl ausführen
pdf("loes9_7_lik.pdf")
par(cex=1.5)
plot(lambda, f, type="l",ylab="Likelihood",xlab="lambda")
abline(v=3.75)
par(cex=1)
dev.off()

# Visualisiere die Log-Likelihood und markiere den ML Schätzer bei 3.75  
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den plot und abline Befehl ausführen
pdf("loes9_7_loglik.pdf")
par(cex=1.5)
plot(lambda, log.f, type="l",ylab="Log-Likelihood",xlab="lambda")
abline(v=3.75)
par(cex=1)
dev.off()

 
# Numerische Verifizierung des Maximums bei 3.75

# Bestimme Maximum von Likelihood und Loglikelihood
maxlik <- max(f)
maxloglik <- max(log.f)

# Bestimme den jeweiligen Index des Maximums in den Vektoren 'f' und 'log.f'
ind.maxlik <- which.max(f)
ind.maxloglik <- which.max(log.f)

# Bestimme den lambda Wert, bei dem das Maximum auftritt
lambda.max <- lambda[ind.maxlik]
lambda.max

lambda.maxlog <- lambda[ind.maxloglik]
lambda.maxlog


################################ AUFGABE b) ####################################

# MAP Schätzer in Abhängigkeit von 'a' zeichnen

# Definiere Funktion map zur Berechnung des MAP für beliebige Werte von 'a'
map <- function(a){
  15/(4+a)
}

# Visualisiere den MAP in Abhängigkeit vom Parameter 'a'
# Verwende den 'curve' Befehl
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den curve Befehl ausführen
pdf("loes9_7_map.pdf")
par(cex=1.5)
curve(map,from=0.1, to=4, xlab="a", ylab="MAP(a)")
par(cex=1)
dev.off()

# Funktion posterior.propto 
# zur Bestimmung der posterior Dichte ohne Normierungskontante
posterior.propto <- function(lambda, a){
  post <- exp(-(4+a)*lambda) * lambda^15
  post
} 

# Funktion posterior 
# zur Bestimmung der vollständigen posteriori Dichte mit Normierungskonstante 
# Die Normierungskonstante wird mit Hilfe der integrate Funktion berechnet 
# indem das Integral der unnormierten posteriori Dichte im Bereich 0-100
# berrechnet wird.
posterior <- function(lambda,a){
  konst <- integrate(posterior.propto, lower=0, upper=100, a=a)$value
  posterior.propto(lambda,a) / konst
}


# Berechne die posteriori Dichte für äquidistante lambda Werte
# im Bereich 0.1-5 mit Schrittweite 0.001
# Speichere die berechneten Werte in den Vektoren 
# dichte_a4 (wenn a=4), dichte_a2 (wenn a=2), dichte_a1 (wenn a=1) 

lambda <- seq(from=0.1, to=5, by=0.001)
dichte_a4 <- vector(mode="numeric", length=length(lambda))
dichte_a2 <- vector(mode="numeric", length=length(lambda))
dichte_a1 <- vector(mode="numeric", length=length(lambda))

a <- 4
for ( i in 1:length(lambda)){
  dichte_a4[i] <- posterior(lambda[i], a)
}

a <- 2
for ( i in 1:length(lambda)){
  dichte_a2[i] <- posterior(lambda[i], a)
}

a <- 1
for ( i in 1:length(lambda)){
  dichte_a1[i] <- posterior(lambda[i], a)
}


# Visualisiere die berechneten Dichten 
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den plot, points und legend Befehl ausführen
pdf("loes9_7_post.pdf")
par(cex=1.5)
plot(lambda, dichte_a4, type="l",ylab="posteriori Dichte")
points(lambda,dichte_a2,type="l",lty=2)
points(lambda,dichte_a1,type="l",lty=3)
legend(3.6, 0.8, legend=c("a=4", "a=2", "a=1"), lty=1:3)
par(cex=1)
dev.off()

