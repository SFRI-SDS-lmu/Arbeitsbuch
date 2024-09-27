###################################################
# Arbeitsbuch: Aufgabe 14.5, Lehrbuch: Aufgabe 14.6
###################################################


# Daten einlesen
daten <- read.table(file="ifo_zeitreihen.txt", header=TRUE, dec=".")
bau.series <- ts(daten$Bauhauptgewerbe_KapazitÃ.tsauslastung, 
                 start = c(1991, 1), frequency=12)

zerlegung.ifo.bau <- stl(bau.series, s.window="periodic")
plot(zerlegung.ifo.bau)
zerlegung.ifo.bau.2 <- stl(bau.series, s.window=7)
plot(zerlegung.ifo.bau.2)

plot(zerlegung.ifo.bau$time.series[,2], lty=1)
lines(zerlegung.ifo.bau.2$time.series[,2], lty=2)

gki.series <- ts(daten$Gewerbliche_Wirtschaft_Gki, 
                 start = c(1991, 1), frequency=12)

zerlegung.ifo.gki <- stl(gki.series, s.window="periodic")
plot(zerlegung.ifo.gki)
zerlegung.ifo.gki.2 <- stl(gki.series, s.window=7)
plot(zerlegung.ifo.gki.2)

# Vergleich der Trendkomponenten beim GKI
plot(zerlegung.ifo.gki$time.series[,2], lty=1,
     ylab="Trendkomponente GKI", xlab="Zeit")
lines(zerlegung.ifo.gki.2$time.series[,2], lty=2)
legend(1992, 112, legend=c("starre Saison", "flexible Saison"), lty=1:2)


pdf("loes14_6_bau1.pdf")
par(cex=1.5)
plot(zerlegung.ifo.bau)
dev.off()
pdf("loes14_6_bau2.pdf")
par(cex=1.5)
plot(zerlegung.ifo.bau.2)
dev.off()
pdf("loes14_6_gki1.pdf")
par(cex=1.5)
plot(zerlegung.ifo.gki)
dev.off()
pdf("loes14_6_gki2.pdf")
par(cex=1.5)
plot(zerlegung.ifo.gki.2)
dev.off()

pdf("loes14_6_trendvergleich.pdf")
par(cex=1.5)
plot(zerlegung.ifo.gki$time.series[,2], lty=1,
     ylab="Trendkomponente GKI", xlab="Zeit")
lines(zerlegung.ifo.gki.2$time.series[,2], lty=2)
legend(1992, 112, legend=c("starre Saison", "flexible Saison"), lty=1:2)
dev.off()





