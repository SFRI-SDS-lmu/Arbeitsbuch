##############################################################
# Lösung Aufgabe 11.7 im Lehrbuch
##############################################################

# Daten einlesen
daten <- read.table(file="mietspiegel2015.txt", header=TRUE, dec=".")
n <- nrow(daten)

# 5-Punkte Zusammenfassung, um den Bereich zu bestimmen
summary(daten$nmqm)

# Bestimmung der Parameter der Normalverteilung (verändert Freiheitsgrade)
mittel <- mean(daten$nmqm)
stdabw <- sd(daten$nmqm)

# Intervalleinteilung 1. Mit dem cut-Befehl erhält man eine Faktorvariable
grenzen.1 <- c(-Inf, 7, 11, 15, 17, Inf)
nmqm.1 <- cut(daten$nmqm, breaks=grenzen.1)

# beobachtete Häufigkeiten
table.1 <- table(nmqm.1)
anzahl.intervalle.1 <- length(table.1)

# erwartete Häufigkeiten
erwartet.1 <- numeric(anzahl.intervalle.1)
for (i in 1:(anzahl.intervalle.1))
  erwartet.1[i] <- pnorm(grenzen.1[i+1], mean=mittel, sd=stdabw) - 
                   pnorm(grenzen.1[i], mean=mittel, sd=stdabw)
#erwartet.1 <- erwartet.1*n

erg.1 <- chisq.test(table.1, p=erwartet.1)
erg.1$statistic
# p-value, Freiheitsgrade korrigieren
1-pchisq(erg.1$statistic, df=anzahl.intervalle.1-1-2)


# Intervalleinteilung 2. Mit dem cut-Befehl erhält man eine Faktorvariable
grenzen.2 <- c(-Inf, seq(from=5, to=18, by=0.5),Inf)
nmqm.2 <- cut(daten$nmqm, breaks=grenzen.2)

# beobachtete Häufigkeiten
table.2 <- table(nmqm.2)
anzahl.intervalle.2 <- length(table.2)

# erwartete Häufigkeiten
erwartet.2 <- numeric(anzahl.intervalle.2)
for (i in 1:(anzahl.intervalle.2))
  erwartet.2[i] <- pnorm(grenzen.2[i+1], mean=mittel, sd=stdabw) - 
  pnorm(grenzen.2[i], mean=mittel, sd=stdabw)

# direkte Berechnung der Teststatistik
stat.2 <- sum( (table.2-n*erwartet.2)^2 / (n*erwartet.2) )
stat.2
# p-value, Freiheitsgrade korrigieren
1-pchisq(stat.2, df=anzahl.intervalle.2-1-2)


