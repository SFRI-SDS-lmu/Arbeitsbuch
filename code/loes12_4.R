################################################################################
# Lösung Aufgabe 12.4
################################################################################


############################### VORBEREITUNG ###################################

# Lösche alle möglicherweise vorhandenen Objekte im Arbeitsbereich
rm(list = ls())

# Daten einlesen
daten <- read.table(file="mietspiegel2015.txt", header=TRUE, dec=".")

# Logarithmierte Nettomiete und Nettomiete/qm erzeugen
daten$lognm <- log(daten$nm)
daten$lognmqm <- log(daten$nmqm)

# Datensatz ohne die 20 bzgl. der Nettomiete teuersten Wohnungen erzeugen
daten <- daten[order(daten$nm, decreasing = TRUE), ] 
daten.20 <- daten[21:nrow(daten),]


################################ AUFGABE a) ####################################

# Regression mit allen Daten, log(Nettomiete)
lm.1 <- lm(lognm ~ wfl+bj+wohngut+wohnbest+badkach0+kueche,
           data=daten)
print(summary(lm.1))

# Speichere die Prognosen ab (wird später in Aufgabe e) benötigt
save.1 <- predict(lm.1)

# Prognose für die in der Aufgabe speziell angegebene Wohnung
prog.1 <- predict(lm.1, newdata=data.frame(wfl=80,bj=1990,wohngut=0,
                                  wohnbest=1,badkach0=1,kueche=0))
prog.1 <- exp(prog.1)
prog.1

# Ohne die 20 bzgl. der Nettomiete teuersten Wohnungen
lm.2 <- lm(lognm ~ wfl+bj+wohngut+wohnbest+badkach0+kueche,
           data=daten.20)
print(summary(lm.2))

# Speichere die Prognosen ab (wird später in Aufgabe e) benötigt
save.2 <- predict(lm.2)

# Prognose für die in der Aufgabe speziell angegebene Wohnung
prog.2 <- predict(lm.2, newdata=data.frame(wfl=80,bj=1990,wohngut=0,
                                           wohnbest=1,badkach0=1,kueche=0))
prog.2 <- exp(prog.2)
prog.2


################################ AUFGABE b) ####################################

# Regression mit allen Daten, Nettomiete
lm.3 <- lm(nm ~ wfl+bj+wohngut+wohnbest+badkach0+kueche,
           data=daten)
print(summary(lm.3))

# Speichere die Prognosen ab (wird später in Aufgabe e) benötigt
save.3 <- predict(lm.3)

# Prognose für die in der Aufgabe speziell angegebene Wohnung
prog.3 <- predict(lm.3, newdata=data.frame(wfl=80,bj=1990,wohngut=0,
                                           wohnbest=1,badkach0=1,kueche=0))
prog.3

# Ohne die bzgl. der Nettomiete 20 teuersten Wohnungen
lm.4 <- lm(nm ~ wfl+bj+wohngut+wohnbest+badkach0+kueche,
           data=daten.20)
print(summary(lm.4))

# Speichere die Prognosen ab (wird später in Aufgabe e) benötigt
save.4 <- predict(lm.4)

# Prognose für die in der Aufgabe speziell angegebene Wohnung
prog.4 <- predict(lm.4, newdata=data.frame(wfl=80,bj=1990,wohngut=0,
                                           wohnbest=1,badkach0=1,kueche=0))
prog.4


################################ AUFGABE c) ####################################

# Unterscheide vier Fälle: lognm, lognm ohne 20, nm, nm ohne

# Fall 1: lognm
lm.5 <- lm(lognm ~ wfl+bj+wohngut+wohnbest+badkach0+kueche+I(bj^2),
           data=daten)
print(summary(lm.5))

# Speichere die Prognosen ab (wird später in Aufgabe e) benötigt
save.5 <- predict(lm.5)

# Prognose für die in der Aufgabe speziell angegebene Wohnung
prog.5 <- predict(lm.5, newdata=data.frame(wfl=80,bj=1990,wohngut=0,
                                           wohnbest=1,badkach0=1,kueche=0))
prog.5 <- exp(prog.5)
prog.5

# Fall 2: lognm ohne die 20 teuersten Wohnungen
lm.6 <- lm(lognm ~ wfl+bj+wohngut+wohnbest+badkach0+kueche+I(bj^2),
           data=daten.20)
print(summary(lm.6))

# Speichere die Prognosen ab (wird später in Aufgabe e) benötigt
save.6 <- predict(lm.6)

# Prognose für die in der Aufgabe speziell angegebene Wohnung
prog.6 <- predict(lm.6, newdata=data.frame(wfl=80,bj=1990,wohngut=0,
                                           wohnbest=1,badkach0=1,kueche=0))
prog.6 <- exp(prog.6)
prog.6

# Fall 3: nm 
lm.7 <- lm(nm ~ wfl+bj+wohngut+wohnbest+badkach0+kueche+I(bj^2),
           data=daten)
print(summary(lm.7))

# Speichere die Prognosen ab (wird später in Aufgabe e) benötigt
save.7 <- predict(lm.7)

# Prognose für die in der Aufgabe speziell angegebene Wohnung
prog.7 <- predict(lm.7, newdata=data.frame(wfl=80,bj=1990,wohngut=0,
                                           wohnbest=1,badkach0=1,kueche=0))
prog.7

# Fall 4: nm ohne die 20 teuersten Wohnungen
lm.8 <- lm(nm ~ wfl+bj+wohngut+wohnbest+badkach0+kueche+I(bj^2),
           data=daten.20)
print(summary(lm.8))

# Speichere die Prognosen ab (wird später in Aufgabe e) benötigt
save.8 <- predict(lm.8)

# Prognose für die in der Aufgabe speziell angegebene Wohnung
prog.8 <- predict(lm.8, newdata=data.frame(wfl=80,bj=1990,wohngut=0,
                                           wohnbest=1,badkach0=1,kueche=0))
prog.8


################################ AUFGABE d) ####################################

# Alles nochmal mit nmqm bzw. log(nmqm)


# Regression mit allen Daten, log(Nettomiete/qm)
lm.11 <- lm(lognmqm ~ wfl+bj+wohngut+wohnbest+badkach0+kueche,
           data=daten)
print(summary(lm.11))

# Speichere die Prognosen ab (wird später in Aufgabe e) benötigt
save.11 <- predict(lm.11)

# Prognose für die in der Aufgabe speziell angegebene Wohnung
prog.11 <- predict(lm.11, newdata=data.frame(wfl=80,bj=1990,wohngut=0,
                                           wohnbest=1,badkach0=1,kueche=0))
prog.11 <- exp(prog.11)
prog.11


# Ohne die 20 bzgl. der Nettomiete teuersten Wohnungen, log(Nettomiete/qm)
lm.12 <- lm(lognmqm ~ wfl+bj+wohngut+wohnbest+badkach0+kueche,
           data=daten.20)
print(summary(lm.12))

# Speichere die Prognosen ab (wird später in Aufgabe e) benötigt
save.12 <- predict(lm.12)

# Prognose für die in der Aufgabe speziell angegebene Wohnung
prog.12 <- predict(lm.12, newdata=data.frame(wfl=80,bj=1990,wohngut=0,
                                           wohnbest=1,badkach0=1,kueche=0))
prog.12 <- exp(prog.12)
prog.12


# Regression mit allen Daten, Nettomiete/qm
lm.13 <- lm(nmqm ~ wfl+bj+wohngut+wohnbest+badkach0+kueche,
           data=daten)
print(summary(lm.13))

# Speichere die Prognosen ab (wird später in Aufgabe e) benötigt
save.13 <- predict(lm.13)

# Prognose für die in der Aufgabe speziell angegebene Wohnung
prog.13 <- predict(lm.13, newdata=data.frame(wfl=80,bj=1990,wohngut=0,
                                           wohnbest=1,badkach0=1,kueche=0))
prog.13


# Ohne die bzgl. der Nettomiete 20 teuersten Wohnungen, Nettomiete/qm
lm.14 <- lm(nmqm ~ wfl+bj+wohngut+wohnbest+badkach0+kueche,
           data=daten.20)
print(summary(lm.14))

# Speichere die Prognosen ab (wird später in Aufgabe e) benötigt
save.14 <- predict(lm.14)

# Prognose für die in der Aufgabe speziell angegebene Wohnung
prog.14 <- predict(lm.14, newdata=data.frame(wfl=80,bj=1990,wohngut=0,
                                           wohnbest=1,badkach0=1,kueche=0))
prog.14


# Regression mit allen Daten, log(Nettomiete/qm) , Baujahr quadratisch
lm.15 <- lm(lognmqm ~ wfl+bj+wohngut+wohnbest+badkach0+kueche+I(bj^2),
           data=daten)
print(summary(lm.15))

# Speichere die Prognosen ab (wird später in Aufgabe e) benötigt
save.15 <- predict(lm.15)

# Prognose für die in der Aufgabe speziell angegebene Wohnung
prog.15 <- predict(lm.15, newdata=data.frame(wfl=80,bj=1990,wohngut=0,
                                           wohnbest=1,badkach0=1,kueche=0))
prog.15 <- exp(prog.15)
prog.15


# Ohne die 20 bzgl. der Nettomiete teuersten Wohnungen, log(Nettomiete/qm) ,
# Baujahr quadratisch
lm.16 <- lm(lognmqm ~ wfl+bj+wohngut+wohnbest+badkach0+kueche+I(bj^2),
           data=daten.20)
print(summary(lm.16))

# Speichere die Prognosen ab (wird später in Aufgabe e) benötigt
save.16 <- predict(lm.16)

# Prognose für die in der Aufgabe speziell angegebene Wohnung
prog.16 <- predict(lm.16, newdata=data.frame(wfl=80,bj=1990,wohngut=0,
                                           wohnbest=1,badkach0=1,kueche=0))
prog.16 <- exp(prog.16)
prog.16


# Regression mit allen Daten, Nettomiete/qm, # Baujahr quadratisch  
lm.17 <- lm(nmqm ~ wfl+bj+wohngut+wohnbest+badkach0+kueche+I(bj^2),
           data=daten)
print(summary(lm.17))

# Speichere die Prognosen ab (wird später in Aufgabe e) benötigt
save.17 <- predict(lm.17)

# Prognose für die in der Aufgabe speziell angegebene Wohnung
prog.17 <- predict(lm.17, newdata=data.frame(wfl=80,bj=1990,wohngut=0,
                                           wohnbest=1,badkach0=1,kueche=0))
prog.17

# Ohne die bzgl. der Nettomiete 20 teuersten Wohnungen, Nettomiete/qm , 
# Baujahr quadratisch
lm.18 <- lm(nmqm ~ wfl+bj+wohngut+wohnbest+badkach0+kueche+I(bj^2),
           data=daten.20)
print(summary(lm.18))

# Speichere die Prognosen ab (wird später in Aufgabe e) benötigt
save.18 <- predict(lm.18)

# Prognose für die in der Aufgabe speziell angegebene Wohnung
prog.18 <- predict(lm.18, newdata=data.frame(wfl=80,bj=1990,wohngut=0,
                                           wohnbest=1,badkach0=1,kueche=0))
prog.18


################################ AUFGABE e) ####################################

# Prognosen mit allen Wohnungen
prognosen <- data.frame(save.1, save.3, save.5, save.7,
                        save.11, save.13, save.15, save.17)
              

# ohne die 20
prognosen.20 <- data.frame(save.2, save.4, save.6, save.8,
                           save.12, save.14, save.16, save.18)

print(cor(prognosen), digits=1)
print(cor(prognosen.20), digits=1)

# grafisch mit Scatterplot
plot(prognosen)
plot(prognosen.20)

pdf("loes12_4_scatter_gesamt.pdf")
par(cex=1.5)
plot(prognosen)
dev.off()

pdf("loes12_4_scatter_teil.pdf")
par(cex=1.5)
plot(prognosen.20)
dev.off()



