####################################################
# Lösung Aufgabe 11.6
####################################################

# Lösche alle möglicherweise vorhandenen Objekte im Arbeitsbereich
rm(list = ls())

# Daten einlesen
daten <- read.table(file="mietspiegel2015.txt", header=TRUE, dec=".")

# Aufgabe a)

# Binarisiertes Baujahr erstellen 
# kleiner Korrektheitscheck mit tabulate  
daten$bj.cat <- ifelse(daten$bj<1958, 1, 2)
tabulate(daten$bj.cat)

# Einseitiger t-Test 
# Standardeinstellung: alpha=0.05
t.test(daten$nmqm[daten$bj.cat==2], daten$nmqm[daten$bj.cat==1],
       alternative = "greater", paired=FALSE, var.equal=FALSE)

# Aufgabe b

# Wilcoxon-Rangsummen-Test
wilcox.test(daten$nmqm[daten$bj.cat==2], 
            daten$nmqm[daten$bj.cat==1],
            alternative = "greater", paired=FALSE)

# Aufgabe c

# Zweiseitiger t-Test
t.test(daten$wfl[daten$bj.cat==2], daten$wfl[daten$bj.cat==1],
       alternative = "two.sided", paired=FALSE, var.equal=FALSE)

# Zweiseitiger Wilcoxon Test
wilcox.test(daten$wfl[daten$bj.cat==2], daten$wfl[daten$bj.cat==1],
            alternative = "two.sided", paired=FALSE)

