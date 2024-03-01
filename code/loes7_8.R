#####################################################
# Lösung 7.12 im Arbeitsbuch, Aufgabe 7.8 im Lehrbuch
#####################################################

require(ggplot2)

set.seed(5671345)

#1. Hier ist jeweils die Anzahl der gezogenen Zufallszahlen n gleich dem Parameter
# size der Binomialverteilung
p <- 0.5

# Wir schreiben der Einfachheit halber eine Funktion
# n: Anzahl der simulierten Zufallszahlen und n der Binomialverteilung
# p: Wahrscheinlichkeit der Binomialverteilung
# byarg: für die Skalierung der x-Achse
# Achtung: wir vergleichen immer mit der W.keitsvert. mit p=0.5, wir können die 
# Funktion dann auch für 2. anwenden.
plot.vergleich <- function(n,p, byarg) {
  
  x <- rbinom(n=n, size=n, prob=p)

  # Grafische Darstellung:
  # Komplette Wahrscheinklichkeitsfunktion der Binomialverteilung p=0.5
  parray <- dbinom(x=0:n, size=n, prob=0.5)
  # Relative H. der Ausprägungen für die Zufallszahlen
  rel.x <- table(x) / n
  # Wir müssen jetzt alles auf Länge n+1 bringen um es in eine Grafik zu 
  # zeichnen.
  # beobachtete Ausprägungen
  hx<-as.integer(row.names(as.matrix(rel.x)))
  # rel. H.
  hy<-as.matrix(rel.x)
  # matrix of length n+1
  h <- matrix(nrow=(n+1), ncol=2, data=NA)
  # erste Spalte enthält alle möglichen Ausprägungen
  h[,1] <- 0:n 
  # zweite Spalte: rel. H. für die Ausprägungen, die überhaupt beobachtet wurden
  h[hx, 2] <- hy 
  # alles in einen data.frame packen
  df <- data.frame(x=0:n, wahrsch=parray, relh=h[,2])
  ggplot(df, aes(x=x))+
  geom_point(aes(y=wahrsch), color="grey") +
  geom_point(aes(y=relh), color="black")+
  scale_x_continuous(breaks=seq(from=0,to=n, by=byarg)) +
  labs(title = paste("Vergleich Wahrsch.fkt (grau,p=0.5) mit 
                      rel. H. (schwarz, p=", as.character(p), 
                      ")", sep=""), 
                x = "Ausprägung (0,...,n)",
                y = "Wahrsch./rel. H.")
}

plot.vergleich(n=10, p=0.5, byarg=1)
ggsave("loes7_8_1_10.pdf")
plot.vergleich(n=100, p=0.5, byarg=10)
ggsave("loes7_8_1_100.pdf")
plot.vergleich(n=1000, p=0.5, byarg=50)
ggsave("loes7_8_1_1000.pdf")
plot.vergleich(n=10000, p=0.5, byarg=500)
ggsave("loes7_8_1_10000.pdf")



# Ergebnis: sehr viele Werte werden gar nicht beobachtet, da ihre W.keit sehr klein ist.
# Beispiele: 

dbinom(4000,10000,0.5)
dbinom(4500,10000,0.5)
dbinom(4900,10000,0.5)

# 2. Jetzt p=0.51
plot.vergleich(n=10, p=0.51, byarg=1)
ggsave("loes7_8_2_10.pdf")
plot.vergleich(n=100, p=0.51, byarg=10)
ggsave("loes7_8_2_100.pdf")
plot.vergleich(n=1000, p=0.51, byarg=50)
ggsave("loes7_8_2_1000.pdf")
plot.vergleich(n=10000, p=0.51, byarg=500)
ggsave("loes7_8_2_10000.pdf")


#3.
n <- 1000
y1 <- rpois(n, lambda=2)
y2 <- rpois(n, lambda=5)

summe <- y1+y2
differenz <- y1-y2
produkt <- y1*y2

plot(density(summe))
hist(summe)
plot(density(differenz))
hist(differenz)
plot(density(produkt))
hist(produkt)

pdf("loes7_8_3_summe.pdf")
hist(summe)
dev.off()

pdf("loes7_8_3_differenz.pdf")
hist(differenz)
dev.off()

pdf("loes7_8_3_produkt.pdf")
hist(produkt)
dev.off()
# theoretisch erwartet: y1+y2 ~ Poisson(lambda=2+5=7)
mean( summe )







