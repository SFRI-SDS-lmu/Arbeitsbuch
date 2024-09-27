#################################################
# Arbeitsbuch: Aufgabe 2.28, Lehrbuch Aufgabe 2.8
#################################################

require(moments)
require(ggplot2)

# Daten einlesen
daten <- read.table(file="mietspiegel2015.txt", header=TRUE, dec=".")
attach(daten)

# 1. Tabelle und Säulendiagramm für die Variable rooms
h <- table(rooms)
print(h)
barplot(h, ylim=c(0,1200), main="Säulendiagramm für die Variable rooms")

pdf("loes2_8_barplot.pdf")
barplot(h, ylim=c(0,1200), main="Säulendiagramm für die Variable rooms")
dev.off()

# 2.
summary(bj)
# besser:
print( summary(bj), digits=5)

bj.cat <- ifelse(bj<1958, 1, 2)
# alternativ:
bj.cat <- cut(bj, breaks=c(0,1958,2013),
  labels=c("1", "2"),
  right=FALSE,dig.lab=3)
# check: table(bj,cat) sollte in beiden Fällen die gleiche Ausgabe liefern
table(bj.cat)

# 3.
# Nettomiete:
tapply(nm, bj.cat, summary )
print("Standardabweichung:")
tapply(nm, bj.cat, sd )
print("Varianz:")
tapply(nm, bj.cat, var )
print("Interquartilsabstand:")
print("IQ-Abstand für Gebäude älter als 1958:")
q1 <- quantile( subset(nm, bj.cat==1), probs=c(0.25, 0.75), names=F )
q1[2]-q1[1]
print("IQ-Abstand für Gebäude mit BJ >= 1958:")
q2 <- quantile( subset(nm, bj.cat==2), probs=c(0.25, 0.75), names=F ) 
q2[2]-q2[1]

require(lattice)
hist <- histogram(~nm | bj.cat, nint=20, 
  xlab=list(label="Nettomiete in Euro",cex=1))
print(hist)

pdf("loes2_8_hist_nm_by_bj_cat.pdf")
print(hist)
dev.off()

# Standard Boxplot mit Formel
boxplot(nm~bj.cat)

pdf("loes2_8_boxplot_nm_by_bj_cat1.pdf")
boxplot(nm~bj.cat)
dev.off()

# Boxplot aus lattice
bp <- bwplot(nm ~ bj.cat,
             xlab=list(label="Nettomiete in Euro",cex=1))
print(bp)

pdf("loes2_8_boxplot_nm_by_bj_cat2.pdf")
print(bp)
dev.off()


# Nettomiete pro Quadratmeter
tapply(nmqm, bj.cat, summary )
print("Standardabweichung:")
tapply(nmqm, bj.cat, sd )
print("Varianz:")
tapply(nmqm, bj.cat, var )
print("Interquartilsabstand:")
print("IQ-Abstand für Gebäude älter als 1958:")
q1 <- quantile( subset(nmqm, bj.cat==1), probs=c(0.25, 0.75), names=F )
q1[2]-q1[1]
print("IQ-Abstand für Gebäude mit BJ >= 1958:")
q2 <- quantile( subset(nmqm, bj.cat==2), probs=c(0.25, 0.75), names=F ) 
q2[2]-q2[1]

require(lattice)
hist <- histogram(~nmqm | bj.cat, nint=20, xlab=list(label="Nettomiete in Euro/qm",cex=1))
print(hist)

pdf("loes2_8_hist_nmqm_by_bj_cat.pdf")
print(hist)
dev.off()

boxplot(nmqm~bj.cat)

pdf("loes2_8_boxplot_nmqm_by_bj_cat1.pdf")
boxplot(nmqm~bj.cat)
dev.off()

bp <- bwplot(nmqm ~ bj.cat,
              xlab=list(label="Nettomiete in Euro/qm",cex=1))
print(bp)

pdf("loes2_8_boxplot_nmqm_by_bj_cat2.pdf")
print(bp)
dev.off()

require(ggplot2)
ggplot(
  daten, aes(x=factor(bj.cat), y=nmqm)
) +
  labs(title="Boxplots für Nettomiete/qm nach Anzahl der Räume",
       x="Baujahr kategorisert", y="Nettomiete/qm") +
  geom_boxplot() +
  theme(text = element_text(size = 24),            # Beschriftungen & Titel
      axis.text = element_text(size = 18)) 

ggsave("loes2_8_boxplot_nmqm_by_bj_cat3.pdf")

# 4. Analyse nach Anzahl der Zimmer "rooms"

# Nettomiete:
tapply(nm, rooms, summary )
print("Standardabweichung:")
tapply(nm, rooms, sd )
print("Varianz:")
tapply(nm, rooms, var )
print("Interquartilsabstand:")
IQR <- function(x){
  h <- quantile(x, probs=c(0.25, 0.75), names=F )
  h[2]-h[1]
}
tapply(nm,rooms,IQR)

require(lattice)
hist <- histogram(~nm | as.factor(rooms), nint=20, 
    xlab=list(label="Nettomiete in Euro nach Anzahl der Zimmer",cex=1))
print(hist)

pdf("loes2_8_hist_nm_by_rooms.pdf")
print(hist)
dev.off()

boxplot(nm~rooms)
bp <- bwplot(nm ~ as.factor(rooms),
             xlab=list(label="Nettomiete in Euro",cex=1))
print(bp)

pdf("loes2_8_boxplot_nm_by_rooms.pdf")
print(bp)
dev.off()


# Nettomiete pro Quadratmeter
tapply(nmqm, rooms, summary )
print("Standardabweichung:")
tapply(nmqm, rooms, sd )
print("Varianz:")
tapply(nmqm, rooms, var )
print("Interquartilsabstand:")
tapply(nmqm,rooms,IQR)

require(lattice)
hist <- histogram(~nmqm | as.factor(rooms), nint=20, 
    xlab=list(label="Nettomiete in Euro/qm nach Anzahl der Zimmer",cex=1))
print(hist)

pdf("loes2_8_hist_nmqm_by_rooms.pdf")
print(hist)
dev.off()


boxplot(nmqm~rooms)

bp <- bwplot(nmqm ~ as.factor(rooms),
             xlab=list(label="Nettomiete in Euro/qm",cex=1))
print(bp)

require(ggplot2)
ggplot(
  daten, aes(x=factor(rooms), y=nmqm)
) +
  labs(title="Boxplots für Nettomiete/qm nach Anzahl der Räume",
       x="Anzahl der Zimmer", y="Nettomiete/qm") +
  geom_boxplot()+
  theme(text = element_text(size = 24),            # Beschriftungen & Titel
        axis.text = element_text(size = 18)) 
  
ggsave("loes2_8_boxplot_nmqm_by_rooms.pdf")

# 5. 
qqnorm(nmqm)
qqline(nmqm)

pdf("loes2_8_hist_nmqm_qqplot.pdf")
qqnorm(nmqm)
qqline(nmqm)
dev.off()

hist <- histogram(nmqm,
                  xlab=list("Nettomiete in Euro/qm",cex=1.5),
                  col="grey90",
                  ylab=list("Dichte",cex=1.5),
                  type="density",
                  scales=list(cex=1.5),
                  breaks=seq(from=0,to=25,by=1),
                  panel = function(x, ...) {
                    panel.histogram(x, ...)
                    panel.mathdensity(dmath = dnorm, col="black", lty=2,
                                      args = list(mean=
                                                    mean(nmqm),
                                                  sd=sd(nmqm)),n=200)
                    panel.densityplot(x, lty=1, col="black",plot.points=F)
                  }
)
print(hist)

pdf("loes2_8_hist_nmqm_dens_gauss.pdf")
print(hist)
dev.off()

ggplot(daten, aes(x=nmqm)) +
  geom_histogram(aes(y=after_stat(density)), alpha=0.3) +
  stat_function(fun = dnorm, aes(colour="Dichte der NV"),
    args = list(mean = mean(nmqm), sd = sd(nmqm))) +
  stat_density(geom = "line", aes(colour = "Kerndichte")) +
  theme(text = element_text(size = 24),            # Beschriftungen & Titel
        axis.text = element_text(size = 18))
ggsave("loes2_8_hist_nmqm_dens_gauss2.pdf")

# 6.
require(moments)

# Nettomiete
# Schiefe
# Quartilskoeffitzient
g.nm  <- ( ( quantile(nm,prob=0.75,names=F)-median(nm) ) -
              (median(nm) - quantile(nm,prob=0.25,names=F)) ) /
  (quantile(nm,prob=0.75,names=F)-quantile(nm,prob=0.25,names=F) )

g.nm
# Momentenkoeffizient "zu Fuss" und unter Benutzung der Funktion
n <- length(nm)
snm1 <- sum( (nm-mean(nm))^3 )/n  / (sum((nm - mean(nm))^2)/n)^(3/2)
snm1
snm2 <- skewness(nm) #, method="moment")
snm2
# Wölbung
cnm1 <- sum( (nm-mean(nm))^4 )/n  / (sum((nm - mean(nm))^2)/n)^(4/2) -3
cnm1
cnm2 <- kurtosis(nm) -3
cnm2

# Nettomiete pro Quadratmeter
# Schiefe
# Quartilskoeffitzient
g.nmq  <- ( ( quantile(nmqm,prob=0.75,names=F)-median(nmqm) ) -
             (median(nmqm) - quantile(nmqm,prob=0.25,names=F)) ) /
  (quantile(nmqm,prob=0.75,names=F)-quantile(nmqm,prob=0.25,names=F) )

g.nmq
# Momentenkoeffizient "zu Fuss" und unter Benutzung der Funktion
n <- length(nm)
snmq1 <- sum( (nmqm-mean(nmqm))^3 )/n  / (sum((nmqm - mean(nmqm))^2)/n)^(3/2)
snmq1
snmq2 <- skewness(nmqm) #, method="moment")
snmq2

# Wölbung
cnmq1 <- sum( (nmqm-mean(nmqm))^4 )/n  / (sum((nmqm - mean(nmqm))^2)/n)^(4/2) -3
cnmq1
cnmq2 <- kurtosis(nmqm) -3
cnmq2

detach(daten)

