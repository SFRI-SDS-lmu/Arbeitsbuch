##############################################################
# Lösung Aufgabe 9.19 im Arbeitsbuch, Aufgabe 9.7 im Lehrbuch
##############################################################

# 1.
# Beobachtete Werte aus einer Poisson-Verteilung
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

# Zeichnen, Maximum am ML-Schätzer = mean(x)
lambda <- seq(from=1, to=7, by=0.05)
f <- numeric(length(lambda))
log.f <- numeric(length(lambda))
for (i in 1:length(lambda)){
  f[i] <- poisson.likelihood(x, lambda[i])
  log.f[i] <- poisson.loglikelihood(x, lambda[i])
}

# Maximum
maxlik <- max(f)
maxloglik <- max(log.f)

# Index des Maximums
ind.maxlik <- which.max(f)
ind.maxloglik <- which.max(log.f)

# lambda Wert, bei dem das Maximum ist
lambda.max <- lambda[ind.maxlik]
lambda.max

lambda.maxlog <- lambda[ind.maxloglik]
lambda.maxlog

# Beide Plots in eine Grafik
par(mfrow=c(2,1))
plot(lambda, f, type="l")
abline(v=lambda.max)
text(4, 0.0001, paste("Maximum bei:", as.character(lambda.max)), pos=4)

plot(lambda, log.f, type="l")
abline(v=lambda.maxlog)
text(4, -15, paste("Maximum bei:", as.character(lambda.max)), pos=4)

pdf("loes9_7_lik.pdf")
#par(mfrow=c(2,1))
par(cex=1.5)
par(pty="s")
plot(lambda, f, type="l")
abline(v=lambda.max)
text(1.8, 0.00001, paste("Maximum bei:", as.character(lambda.max)), pos=4)

#plot(lambda, log.f, type="l")
#abline(v=lambda.maxlog)
#text(4, -15, paste("Maximum bei:", as.character(lambda.max)), pos=4)
dev.off()

pdf("loes9_7_loglik.pdf")
#par(mfrow=c(2,1))
par(cex=1.5)
par(pty="s")
#plot(lambda, f, type="l")
#abline(v=lambda.max)
#text(4, 0.0001, paste("Maximum bei:", as.character(lambda.max)), pos=4)

plot(lambda, log.f, type="l")
abline(v=lambda.maxlog)
text(1.8, -15, paste("Maximum bei:", as.character(lambda.max)), pos=4)
dev.off()


# 2.
# MAP Schätzer in Abhängigkeit von a zeichnen:
map <- function(a){
  15/(4+a)
}
curve(map,from=0.1, to=4, xlab="a", ylab="MAP(a)")
pdf("loes9_7_map.pdf")
par(cex=1.5)
curve(map,from=0.1, to=4, xlab="a", ylab="MAP(a)")
dev.off()

# Bestimmung der posteriori Dichte mit integrate
# Likelihood * priori ohne Normierungskonstante
posterior.propto <- function(lambda, a){
  post <- exp(-(4+a)*lambda) * lambda^15
  post
} 
# Dichte 
posterior <- function(lambda,a){
  # Wir müssen die Normalisierungskonstante berechnen
  konst <- integrate(posterior.propto, lower=0, upper=100, a=a)$value
  posterior.propto(lambda,a) / konst
}

# Dichte zeichnen
# Sequenz von lambda's
lambda <- seq(from=0.1, to=5, by=0.001)
dichte <- vector(mode="numeric", length=length(lambda))

# z.B. a=4
a <- 4
for ( i in 1:length(lambda)){
  dichte[i] <- posterior(lambda[i], a)
}

plot(lambda, dichte, type="l")

# MAP
print(lambda[which.max(dichte)])

# a=2
a <- 2 
for ( i in 1:length(lambda)){
  dichte[i] <- posterior(lambda[i], a)
}

# Füge zum Plot hinzu
points(lambda,dichte,type="l",lty=2)

# a=1
a <- 1 
for ( i in 1:length(lambda)){
  dichte[i] <- posterior(lambda[i], a)
}

# Füge zum Plot hinzu
points(lambda,dichte,type="l",lty=3)

legend(4, 0.7, legend=c("a=4", "a=2", "a=1"), lty=1:3)

# alternativ für posteriori-Dichte:
# dgamma(lambda, 16, 4+a)


pdf("loes9_7_post.pdf")
par(cex=1.5)
# z.B. a=4
a <- 4
for ( i in 1:length(lambda)){
  dichte[i] <- posterior(lambda[i], a)
}

plot(lambda, dichte, type="l")

# MAP
print(lambda[which.max(dichte)])

# a=2
a <- 2 
for ( i in 1:length(lambda)){
  dichte[i] <- posterior(lambda[i], a)
}

# Füge zum Plot hinzu
points(lambda,dichte,type="l",lty=2)

# a=1
a <- 1 
for ( i in 1:length(lambda)){
  dichte[i] <- posterior(lambda[i], a)
}

# Füge zum Plot hinzu
points(lambda,dichte,type="l",lty=3)

legend(3.5, 0.7, legend=c("a=4", "a=2", "a=1"), lty=1:3)
dev.off()






