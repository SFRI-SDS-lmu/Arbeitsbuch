##############################################################
# Lösung Aufgabe 11.7 
##############################################################

# Lösche alle möglicherweise vorhandenen Objekte im Arbeitsbereich
rm(list = ls())

# Daten einlesen
# Definiere n = Anzahl der Beobachtungen
daten <- read.table(file="mietspiegel2015.txt", header=TRUE, dec=".")
n <- nrow(daten)

# Bestimme und speichere Mittelwert und Standardabweichung 
# der Nettomiete pro Quadratmeter (wird später benötigt)
mittel <- mean(daten$nmqm)
stdabw <- sd(daten$nmqm)


# Chi Quadrat Test mit 5 Intervallen

# Definiere Intervalle 
# mit Hilfe des cut-Befehls definieren wir die Faktorvariable nmqm.1
grenzen.1 <- c(-Inf, 7, 11, 15, 17, Inf)
nmqm.1 <- cut(daten$nmqm, breaks=grenzen.1)

# Bestimme beobachtete Häufigkeiten der Intervalle
table.1 <- table(nmqm.1)
anzahl.intervalle.1 <- length(table.1)

# Bestimme erwartete Häufigkeiten der Intervalle gemäß Normalverteilung
erwartet.1 <- numeric(anzahl.intervalle.1)
for (i in 1:(anzahl.intervalle.1)) 
  erwartet.1[i] <- pnorm(grenzen.1[i+1], mean=mittel, sd=stdabw) - 
                   pnorm(grenzen.1[i], mean=mittel, sd=stdabw)
  
# Chi Quadrat Teststatistik berechnen mit Hilfe von chisq.test
erg.1 <- chisq.test(table.1, p=erwartet.1)
erg.1$statistic
# Alternative direkte Berechnung der Teststatistik
stat.1 <- sum( (table.1-n*erwartet.1)^2 / (n*erwartet.1) )
stat.1


# Bestimme den  p-Wert unter Beachtung der Freiheitsgrade Korrektur
1-pchisq(erg.1$statistic, df=anzahl.intervalle.1-1-2)


# Chi Quadrat Test mit 28 Intervallen

# Definiere Intervalle 
# mit Hilfe des cut-Befehls definieren wir die Faktorvariable nmqm.2
grenzen.2 <- c(-Inf, seq(from=5, to=18, by=0.5),Inf)
nmqm.2 <- cut(daten$nmqm, breaks=grenzen.2)

# Bestimme beobachtete Häufigkeiten der Intervalle
table.2 <- table(nmqm.2)
anzahl.intervalle.2 <- length(table.2)

# Bestimme erwartete Häufigkeiten der Intervalle gemäß Normalverteilung
erwartet.2 <- numeric(anzahl.intervalle.2)
for (i in 1:(anzahl.intervalle.2))
  erwartet.2[i] <- pnorm(grenzen.2[i+1], mean=mittel, sd=stdabw) - 
  pnorm(grenzen.2[i], mean=mittel, sd=stdabw)

# Chi Quadrat Teststatistik berechnen mit Hilfe von chisq.test
erg.2 <- chisq.test(table.2, p=erwartet.2)
erg.2$statistic
# Alternative direkte Berechnung der Teststatistik
stat.2 <- sum( (table.2-n*erwartet.2)^2 / (n*erwartet.2) )
stat.2

# Bestimme den  p-Wert unter Beachtung der Freiheitsgrade Korrektur
1-pchisq(erg.2$statistic, df=anzahl.intervalle.2-1-2)




