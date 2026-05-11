################################################################################
# Aufgabe 2.13
################################################################################


# Lösche alle möglicherweise vorhandenen Objekte im Arbeitsbereich
rm(list = ls())


############################## Aufgabe a) ###################################### 

# relative Häufigkeiten
relh <- c(0.526,0.253,0.121,0.072,0.028)

#Kategorien
hgr  <- c(1,2,3,4,5)

# Erstellen des Säulendiagramms
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'barplot' und 'text' Befehl ausführen
pdf("loes2_haushalte_relh.pdf")
par(cex=1.5)
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


############################## Aufgabe b) ###################################### 


# relative Häufigkeiten
relh <- c(0.2885,0.2776,0.1991,0.158,0.0768)

#Kategorien
hgr  <- c(1,2,3,4,5)

# Erstellen des Säulendiagramms

pdf("loes2_haushalte_relhpers.pdf")
# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'barplot' und 'text' Befehl ausführen
par(cex=1.5)
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


