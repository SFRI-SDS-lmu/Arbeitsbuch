################################################################################
# Lösung Aufgabe 2.12 
################################################################################


############################### VORBEREITUNG ###################################

# Lösche alle möglicherweise vorhandenen Objekte im Arbeitsbereich
rm(list = ls())

# Einlesen der Daten
# Wir gehen davon aus, dass die Daten im aktuellen Working directory 
# gespeichert sind. Dieses wird durch getwd() ausgegeben.
# Ändern des aktuellen Working directory mit setwd()

daten.roh <- read.table(file="sozio.txt", skip=1)

################################################################################


############################# Mermal Note ######################################

daten1 <- daten.roh[1:18, 1:6]
colnames(daten1) <- c("Person","Geschlecht", 
                      "Studiendauer", "Engagement", 
                      "Ausrichtung", "Note")
daten2 <- daten.roh[1:18,7:12]
colnames(daten2) <- c("Person","Geschlecht", 
                      "Studiendauer", "Engagement", 
                      "Ausrichtung", "Note")

daten <- rbind(daten1,daten2)
daten$Note <- factor(daten$Note, levels=c(1,2,3,4,5))

abs.h <- table(daten$Note)
rel.h <- abs.h / sum(abs.h) 
rel.h

# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'barplot' Befehl ausführen
pdf("loes2_absolventen_bp_note.pdf")
par(cex=1.5)
barplot(rel.h, main="Säulendiagramm des Merkmals Note", 
        ylab="relative Häufigkeit")
dev.off()


############################## Merkmal Studiendauer ############################

# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'boxplot' Befehl ausführen
pdf("loes2_absolventen_box_dauer.pdf")
par(cex=1.5)
boxplot(daten$Studiendauer, main="Box-Plot der Studiendauer")
dev.off()


praed <- subset(daten, Note %in% c("1","2") )
nonpraed <- subset(daten, Note %in% c("3","4","5") )

praed$Studiendauer <- factor(praed$Studiendauer, levels=7:18)
nonpraed$Studiendauer <- factor(nonpraed$Studiendauer, levels=7:18)

rel.h.sd.praed <- table(praed$Studiendauer)
rel.h.sd.praed <- rel.h.sd.praed / sum(rel.h.sd.praed)

# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'barplot' Befehl ausführen
pdf("loes2_absolventen_sd_praed.pdf")
par(cex=1.5)
barplot(rel.h.sd.praed, main="Studiendauer, mit Prädikatsexamen", ylab="relative H.",
        ylim=c(0.0, 0.3))
dev.off()

rel.h.sd.nonpraed <- table(nonpraed$Studiendauer)
rel.h.sd.nonpraed <- rel.h.sd.nonpraed / sum(rel.h.sd.nonpraed)

# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'barplot' Befehl ausführen
pdf("loes2_absolventen_sd_nonpraed.pdf")
par(cex=1.5)
barplot(rel.h.sd.nonpraed, main="Studiendauer, ohne Prädikatsexamen", ylab="relative H.",
        ylim=c(0.0, 0.3))
dev.off()

daten$Studiendauer <- factor(daten$Studiendauer, levels=7:18)
rel.h.sd.alle <- table(daten$Studiendauer)
rel.h.sd.alle <- rel.h.sd.alle / sum(rel.h.sd.alle)

# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'barplot' Befehl ausführen
pdf("loes2_absolventen_sd_alle.pdf")
par(cex=1.5)
barplot(rel.h.sd.alle, main="Studiendauer, alle", ylab="relative H.",
        ylim=c(0.0, 0.3))
dev.off()


praed$Studiendauer <- as.numeric(praed$Studiendauer)+6
emp.vert.praed <- ecdf(praed$Studiendauer)

# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'barplot' Befehl ausführen
pdf("loes2_absolventen_emp_praed.pdf")
par(cex=1.5)
plot(emp.vert.praed, main=" mit Prädikatsexamen", xlab="Studiendauer", 
     ylab="F(x)")
dev.off()


nonpraed$Studiendauer <- as.numeric(nonpraed$Studiendauer)+6
emp.vert.nonpraed <- ecdf(nonpraed$Studiendauer)

# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'barplot' Befehl ausführen
pdf("loes2_absolventen_emp_nonpraed.pdf")
par(cex=1.5)
plot(emp.vert.nonpraed, main=" ohne Prädikatsexamen",  xlab="Studiendauer", 
     xlim=c(6,18), ylab="F(x)")
dev.off()



daten$Studiendauer <- as.numeric(daten$Studiendauer)+6
emp.vert.alle <- ecdf(daten$Studiendauer)

# ACHTUNG: 
# Falls Ausgabe auf Bildschirm gewünscht (anstelle Grafik im pdf Format),
# dann nur den 'barplot' Befehl ausführen
pdf("loes2_absolventen_emp_alle.pdf")
par(cex=1.5)
plot(emp.vert.alle, main="alle Daten", xlab="Studiendauer", ylab="F(x)")
dev.off()


