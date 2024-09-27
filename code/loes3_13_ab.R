#########################################################
# Arbeitsbuch: Aufgabe 3.13, Lehrbuch: Lösung Aufgabe 3.7
#########################################################

# Daten einlesen
daten <- read.table(file="luftschad.txt", header=TRUE, dec=".")

# 1.

# Teildatensatz Mai 1999 bis (ausschliesslich) Februar 2016
teildaten <- daten[101:301,]
n <- nrow(teildaten)

# check Datumsbereich, keine fehlenden Werte?
teildaten[1,]
teildaten[n,]
anzahl.fehlend <- n - sum( complete.cases(teildaten) )
anzahl.fehlend

# Korrelation innerhalb der Zeitreihen (y_t, y_(t+1) ). 

# Variante 1, "zu Fuss":
cor(teildaten$Stickoxide[1:(n-1)], teildaten$Stickoxide[2:n], method="pearson")
# mit der acf() Funktion
acf(teildaten$Stickoxide, plot=FALSE, lag.max=1, type="correlation")

# Analog für alle anderen Variablen
cor(teildaten$CO[1:(n-1)], teildaten$CO[2:n], method="pearson")
cor(teildaten$Feinstaub[1:(n-1)], teildaten$Feinstaub[2:n], method="pearson")
cor(teildaten$Lufttemperatur[1:(n-1)], teildaten$Lufttemperatur[2:n], method="pearson")
cor(teildaten$Ozon[1:(n-1)], teildaten$Ozon[2:n], method="pearson")
cor(teildaten$SO2[1:(n-1)], teildaten$SO2[2:n], method="pearson")

# Stärkste Korrelation bei SO2 (dann Lufttemperatur), geringste bei Ozon (dann Feinstaub)

# 2. Jetzt alle Daten verwenden. Die Option use="pairwise.complete.obs" 
#    verwendet jeweils alle möglichen Beobachtungspaare. Wir berechnen gleich die ganze 
#    Korrelationsmatrix ohne die Variable Datum
cor(daten[,2:7], use="pairwise.complete.obs")
# Zum Vergleich mit der Option use="complete.obs", bei der Beobachtungen verwendet werden, 
# die bei keiner der 6 Variablen einen fehlenden Wert haben.
cor(daten[,2:7], use="complete.obs")
sum(complete.cases(daten))
# Option complete.obs ist sicherer, die andere Option kann zu Problemen führen ohne das 
# hier genauer zu besprechen.
