##############################################################
# Lösung Aufgabe 2.9 im Lehrbuch
##############################################################

library("tseries")
library("moments")

# Einlesen der Daten ab 1.8.2007 bis 31.7.2008 ("Mitte 2008")
bmw <- get.hist.quote(start="2007-08-01", end="2008-07-31", 
                      instrument="bmw.de", quote="AdjClose")
r.bmw <- diff(log(bmw$Adjusted))

#
# 1.Arithmetisches Mittel, Median, Standardabweichung
print(mean(r.bmw))
print(median(r.bmw))
print(sd(r.bmw))

# 2. Histogramme, Boxplot
hist(r.bmw, breaks=10)
hist(r.bmw, breaks=20)
boxplot(r.bmw)

# 3. 
# Schiefe (Quantilskoeffizient) und Wölbung

qc.bmw <- ( (quantile(r.bmw,prob=0.75)-median(r.bmw) ) -
              (median(r.bmw) - quantile(r.bmw,prob=0.25)) ) /
  (quantile(r.bmw,prob=0.75)-quantile(r.bmw,prob=0.25) )
cbmw <- kurtosis(r.bmw) -3

print(qc.bmw)
print(cbmw)

pdf("loes2_9_vor_hist1.pdf")
par(cex=1.5)
hist(r.bmw, breaks=10)
dev.off()
pdf("loes2_9_vor_hist2.pdf")
par(cex=1.5)
hist(r.bmw, breaks=20)
dev.off()
pdf("loes2_9_vor_box.pdf")
par(cex=1.5)
boxplot(r.bmw)
dev.off()

# 4. Daten ab August 2008 bis 31.12.2009
bmw <- get.hist.quote(start="2008-08-01", end="2009-12-31", 
                      instrument="bmw.de", quote="AdjClose")
r.bmw <- diff(log(bmw$Adjusted))

#
# 1b.Arithmetisches Mittel, Median, Standardabweichung
print(mean(r.bmw))
print(median(r.bmw))
print(sd(r.bmw))

# 2b. Histogramme, Boxplot
hist(r.bmw, breaks=10)
hist(r.bmw, breaks=20)
boxplot(r.bmw)

# 3b. 
# Schiefe (Quantilskoeffizient) und Wölbung

qc.bmw <- ( (quantile(r.bmw,prob=0.75)-median(r.bmw) ) -
              (median(r.bmw) - quantile(r.bmw,prob=0.25)) ) /
  (quantile(r.bmw,prob=0.75)-quantile(r.bmw,prob=0.25) )
cbmw <- kurtosis(r.bmw) -3

print(qc.bmw)
print(cbmw)

pdf("loes2_9_nach_hist1.pdf")
par(cex=1.5)
hist(r.bmw, breaks=10)
dev.off()
pdf("loes2_9_nach_hist2.pdf")
par(cex=1.5)
hist(r.bmw, breaks=20)
dev.off()
pdf("loes2_9_nach_box.pdf")
par(cex=1.5)
boxplot(r.bmw)
dev.off()




