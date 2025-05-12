###########################################
#Aufgabe 2.?
###########################################

# Aufgabe a)

# Achtung: 
# Die pdf's werden ins aktuelle Working directory gespeichert.
# Dieses wird durch getwd() ausgegeben
# Ändern des aktuellen Working directory mit setwd()
# den pdf Befehl nur ausführen wenn Grafik als pdf gespeichert werden soll,
# ansonsten diesen Befehl auslassen!
pdf("loes2_haushalte_relh.pdf")

# relative Häufigkeiten
relh <- c(0.526,0.253,0.121,0.072,0.028)
#Kategorien
hgr  <- c(1,2,3,4,5)

# Erstellen des Säulendiagramms
bp<-barplot(relh,
        names.arg = hgr,
        ylab = "Relative Häufigkeit",
        xlab = "Haushaltsgröße",
        ylim = c(0,0.6))

# realtive Häufigkeiten hinzufügen
text(x = bp,
     y = relh,
     labels = paste0(relh, " "),
     pos = 3, cex = 0.8)

dev.off()

# Aufgabe b)

# Achtung: 
# Die pdf's werden ins aktuelle Working directory gespeichert.
# Dieses wird durch getwd() ausgegeben
# Ändern des aktuellen Working directory mit setwd()
# den pdf Befehl nur ausführen wenn Grafik als pdf gespeichert werden soll,
# ansonsten diesen Befehl auslassen!
pdf("loes2_haushalte_relhpers.pdf")

# relative Häufigkeiten
relh <- c(0.2885,0.2776,0.1991,0.158,0.0768)
#Kategorien
hgr  <- c(1,2,3,4,5)

# Erstellen des Säulendiagramms
bp<-barplot(relh,
            names.arg = hgr,
            ylab = "Relative Häufigkeit",
            xlab = "Anzahl der Personen",
            ylim = c(0,0.35))

# realtive Häufigkeiten hinzufügen
text(x = bp,
     y = relh,
     labels = paste0(relh, " "),
     pos = 3, cex = 0.8)

dev.off()


