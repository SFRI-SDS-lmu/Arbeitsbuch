####################################################
# Arbeitsbuch: Aufgabe 11.17, Lehrbuch: Aufgabe 11.6
####################################################

# Daten einlesen
daten <- read.table(file="mietspiegel2015.txt", header=TRUE, dec=".")

# 1.
# Binarisiertes Baujahr erstellen
bj.cat <- ifelse(bj<1958, 1, 2)
# bj.cat zum data frame hinzufügen
daten <- data.frame(daten,bj.cat)

# Einseitiger t-Test (Standardeinstellung: alpha=0.05)
t.test(daten$nmqm[daten$bj.cat==2], daten$nmqm[daten$bj.cat==1],
       alternative = "greater", paired=FALSE, var.equal=FALSE)

# 2. Wilcoxon-Rangsummen-Test
wilcox.test(daten$nmqm[daten$bj.cat==2], daten$nmqm[daten$bj.cat==1],
            alternative = "greater", paired=FALSE)

# 3.
t.test(daten$wfl[daten$bj.cat==2], daten$wfl[daten$bj.cat==1],
       alternative = "two.sided", paired=FALSE, var.equal=FALSE)
wilcox.test(daten$wfl[daten$bj.cat==2], daten$wfl[daten$bj.cat==1],
            alternative = "two.sided", paired=FALSE)
