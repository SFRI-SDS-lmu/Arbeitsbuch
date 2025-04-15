###########################################
#  Aufgabe 2.1 Lehrbuch und Arbeitsbuch
###########################################

# X1 ... laufendes Konto

X1.Anteile <- matrix(nrow=3, ncol=2, data=c(0.45, 0.397, 0.153, 0.199, 0.302, 0.497), 
                     byrow=FALSE)
X1        <- cbind(X1.Anteile, c("nein","mittel","gut"))
X1.Y      <- c("Y=1 (schlechte Bonität)","Y=0 (gute Bonität)")

barplot(X1.Anteile~X1[,3], beside=TRUE, ylim=c(0,0.5), xlab="Laufendes Konto", 
        ylab="Anteile", legend.text=X1.Y, args.legend=c(x=6.5, y=0.5))
pdf("loes2_1_lk1.pdf")
barplot(X1.Anteile~X1[,3], beside=TRUE, ylim=c(0,0.5), xlab="Laufendes Konto", 
        ylab="Anteile", legend.text=X1.Y, args.legend=c(x=6.5, y=0.5))
dev.off()

# X4 ... Rückzahlung früherer Kredite

X4.Anteile <- matrix(nrow=2, ncol=2, data=c(0.8233,0.1766,0.9485,0.0515), byrow=FALSE)
X4 <- cbind(X4.Anteile, c("gut","schlecht"))
barplot(X4.Anteile~X4[,3], beside=TRUE, ylim=c(0,1), xlab="Frühere Kredite", 
        ylab="Anteile", legend.text=X1.Y, args.legend="topright")
pdf("loes2_1_fk1.pdf")
barplot(X4.Anteile~X4[,3], beside=TRUE, ylim=c(0,1), xlab="Frühere Kredite", 
        ylab="Anteile", legend.text=X1.Y, args.legend="topright")
dev.off()

# X5 ... Verwendungszweck

X5.Anteile <- matrix(nrow=2, ncol=2, data=c(0.5753,0.4247,0.6929,0.3071), byrow=FALSE)
X5 <- cbind(X5.Anteile, c("privat","beruflich"))
barplot(X5.Anteile~X5[,3], beside=TRUE, ylim=c(0,1), xlab="Verwendungszweck", 
        ylab="Anteile", legend.text=X1.Y, args.legend=c(x=6, y=1))
pdf("loes2_1_vz1.pdf")
barplot(X5.Anteile~X5[,3], beside=TRUE, ylim=c(0,1), xlab="Verwendungszweck", 
        ylab="Anteile", legend.text=X1.Y, args.legend=c(x=6, y=1))
dev.off()

# X3 ... Kredithöhe in Euro

# Erstellen der Tabelle für die Histogramme (getrennt für Y=0 und Y=1)
# Rel. Häufigkeit = Klassenbreite * Höhe, damit 
# Höhe = rel. Häufigkeit / Klassenbreite

intervalle <- c(0,500,1000,1500,2500,5000,7500,10000,15000,20000)
klassenbreiten <- c(500,500,500,1000,2500,2500,2500,5000,5000)
relH.Y1 <- c(1.00,11.33,17,19.67,25,11.33,6.67,7,1)/100
h.Y1 <- relH.Y1 / klassenbreiten * 10000

barplot(h.Y1,klassenbreiten, space=0,col="white",
        ylim=c(0,3.5), axes=F, 
        xlab="Kreditwürdigkeit", ylab="Höhe*10000", main="Y = 1")
axis(1, at=intervalle)
axis(2, at=c(0,0.5,1,1.5,2,2.5,3,3.5))

pdf("loes2_1_kwy1.pdf")
barplot(h.Y1,klassenbreiten, space=0,col="white",
        ylim=c(0,3.5), axes=F, 
        xlab="Kreditwürdigkeit", ylab="Höhe*10000", main="Y = 1")
axis(1, at=intervalle)
axis(2, at=c(0,0.5,1,1.5,2,2.5,3,3.5))
dev.off()

relH.Y0 <- c(2.14,9.14,19.86,24.57,28.57,9.71,3.71,2.0,0.29)/100
h.Y0 <- relH.Y0 / klassenbreiten * 10000

barplot(h.Y0,klassenbreiten, space=0,col="white",
        ylim=c(0,4.0), axes=F, 
        xlab="Kreditwürdigkeit", ylab="Höhe*10000", main="Y = 0")
axis(1, at=intervalle)
axis(2, at=c(0,0.5,1,1.5,2,2.5,3,3.5,4.0))

pdf("loes2_1_kwy0.pdf")
barplot(h.Y0,klassenbreiten, space=0,col="white",
        ylim=c(0,4.0), axes=F, 
        xlab="Kreditwürdigkeit", ylab="Höhe*10000", main="Y = 0")
axis(1, at=intervalle)
axis(2, at=c(0,0.5,1,1.5,2,2.5,3,3.5,4.0))
dev.off()
