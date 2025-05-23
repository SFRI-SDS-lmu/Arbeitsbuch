##################################################
# Aufgabe 3.8
##################################################

# Wir gehen davon aus, dass die Daten im aktuellen Working directory 
# gespeichert sind. Dieses wird durch getwd() ausgegeben.
# Ändern des aktuellen Working directory mit setwd()

daten <- read.table(file="mietspiegel2015.txt", header=TRUE, dec=".")

# Aufgabe (a) 
# Streudiagramm Nettomiete pro qm versus Baujahr

# mit plot Befehl
# Die pdf's werden ins aktuelle Working directory gespeichert.
# Dieses wird durch getwd() ausgegeben
# Ändern des aktuellen Working directory mit setwd()
# Den pdf Befehl nur ausführen wenn Grafik als pdf gespeichert werden soll,
# ansonsten diesen Befehl auslassen!
pdf("loes3_8_streu1.pdf")
plot(daten$bj, daten$nmqm,ylab = "Nettomiete pro qm", xlab = "Baujahr")
dev.off()


# Aufgabe (b) und (c)
# Lineare Einfachregression Nettomiete por qm versus Baujahr

lm.einfach <- lm(nmqm ~ bj, data=daten)
summary(lm.einfach)

# Aufgabe (d)
# Den pdf Befehl nur ausführen wenn Grafik als pdf gespeichert werden soll,
# ansonsten diesen Befehl auslassen!
pdf("loes3_8_regrgerade1.pdf")
plot(daten$bj, daten$nmqm,ylab = "Nettomiete pro qm", xlab = "Baujahr")
abline(coefficients(lm.einfach))
dev.off()


# Aufgabe (e)
# Speichern der Residuen in eps
eps <- resid(lm.einfach)

# Den pdf Befehl nur ausführen wenn Grafik als pdf gespeichert werden soll,
# ansonsten diesen Befehl auslassen!
pdf("loes3_8_qq.pdf")

# QQ-Plot erstellen, zum besseren Vergleich wird mit Hilfe von qqline 
# in die Grafik noch eine Winkelhalbierende eingezeichnet
qqnorm(eps,ylab="Stichprobenquantile",xlab="theoretische Quantile")
qqline(eps)

dev.off()

