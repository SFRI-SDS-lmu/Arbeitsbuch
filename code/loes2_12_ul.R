
daten.roh <- read.table(file="sozio.txt", skip=1)

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


barplot(rel.h, xlab="Säulendiagramm des Merkmals Note", ylab="relative H.")

rel.perc <- c("5.6","61.1","30.6", "0.0", "2.8" )

pie(rel.h, col="white")
   
boxplot(daten$Studiendauer, xlab="Box-Plot der Studiendauer")

praed <- subset(daten, Note %in% c("1","2") )
nonpraed <- subset(daten, Note %in% c("3","4","5") )

praed$Studiendauer <- factor(praed$Studiendauer, levels=7:18)
nonpraed$Studiendauer <- factor(nonpraed$Studiendauer, levels=7:18)

rel.h.sd.praed <- table(praed$Studiendauer)
rel.h.sd.praed <- rel.h.sd.praed / sum(rel.h.sd.praed)

barplot(rel.h.sd.praed, xlab="Studiendauer, mit Prädikatsexamen", ylab="relative H.",
        ylim=c(0.0, 0.3))

rel.h.sd.nonpraed <- table(nonpraed$Studiendauer)
rel.h.sd.nonpraed <- rel.h.sd.nonpraed / sum(rel.h.sd.nonpraed)

barplot(rel.h.sd.nonpraed, xlab="Studiendauer, ohne Prädikatsexamen", ylab="relative H.",
        ylim=c(0.0, 0.3))

daten$Studiendauer <- factor(daten$Studiendauer, levels=7:18)
rel.h.sd.alle <- table(daten$Studiendauer)
rel.h.sd.alle <- rel.h.sd.alle / sum(rel.h.sd.alle)


barplot(rel.h.sd.alle, xlab="Studiendauer, alle", ylab="relative H.",
        ylim=c(0.0, 0.3))

praed$Studiendauer <- as.numeric(praed$Studiendauer)+6
emp.vert.praed <- ecdf(praed$Studiendauer)
plot(emp.vert.praed, xlab="Studiendauer, mit Prädikatsexamen", main="")


nonpraed$Studiendauer <- as.numeric(nonpraed$Studiendauer)+6
emp.vert.nonpraed <- ecdf(nonpraed$Studiendauer)

plot(emp.vert.nonpraed, xlab="Studiendauer, ohne Prädikatsexamen", main="")

daten$Studiendauer <- as.numeric(daten$Studiendauer)+6
emp.vert.alle <- ecdf(daten$Studiendauer)

plot(emp.vert.alle, xlab="Studiendauer, alle", main="")


