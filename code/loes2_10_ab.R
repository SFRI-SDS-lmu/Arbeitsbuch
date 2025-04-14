###################################################
# Aufgabe 2.10
###################################################
set.seed(5178123)
n1 <- 10
x1 <- rnorm(n1)
n2 <- 100
x2 <- rnorm(n2)
n3 <- 1000
x3 <- rnorm(n3)

# Mittelwert
sum(x1)/n1
# oder
mean(x1)

# Median
median(x1)

# Varianz
sum( (x1-mean(x1))^2 )/n1
# oder
var(x1)*(n1-1)/n1

hist <- histogram(x1,
                  xlab=list("Stichprobe aus einer N(0,1)-Verteilung",cex=1.5),
                  col="grey90",
                  ylab=list("Dichte",cex=1.5),
                  type="density",
                  scales=list(cex=1.5),
                  breaks=seq(from=-5,to=5,by=0.5),
                  panel = function(x, ...) {
                    panel.histogram(x, ...)
                    panel.mathdensity(dmath = dnorm, col="black", lty=2,
                                      args = list(mean=
                                                    mean(x1),
                                                  sd=sd(x1)),n=200)
#                    panel.densityplot(x1, lty=1, col="black",plot.points=F)
                  }
)
print(hist)

pdf("loes2_10_hist_n10.pdf")
print(hist)
dev.off()


qqnorm(x1)
qqline(x1)

pdf("loes2_10_qq_n10.pdf")
qqnorm(x1, xlab="Theoretische Quantile", ylab="Stichprobenquantile" )
qqline(x1)
dev.off()

##########################
# Wiederhole für x2 und x3
##########################

# Mittelwerte
sum(x2)/n2
# oder
mean(x2)

# Median
median(x2)

# Varianz
sum( (x2-mean(x2))^2 )/n2
# oder
var(x2)*(n2-1)/n2

hist <- histogram(x2,
                  xlab=list("Stichprobe aus einer N(0,1)-Verteilung",cex=1.5),
                  col="grey90",
                  ylab=list("Dichte",cex=1.5),
                  type="density",
                  scales=list(cex=1.5),
                  breaks=seq(from=-5,to=5,by=0.5),
                  panel = function(x, ...) {
                    panel.histogram(x, ...)
                    panel.mathdensity(dmath = dnorm, col="black", lty=2,
                                      args = list(mean=
                                                    mean(x2),
                                                  sd=sd(x2)),n=200)
#                    panel.densityplot(x2, lty=1, col="black",plot.points=F)
                  }
)
print(hist)

pdf("loes2_10_hist_n100.pdf")
print(hist)
dev.off()


qqnorm(x2)
qqline(x2)

pdf("loes2_10_qq_n100.pdf")
qqnorm(x2, xlab="Theoretische Quantile", ylab="Stichprobenquantile" )
qqline(x2)
dev.off()


# Mittelwerte
sum(x3)/n3
# oder
mean(x3)

# Median
median(x3)

# Varianz
sum( (x3-mean(x3))^2 )/n3
# oder
var(x3)*(n3-1)/n3

hist <- histogram(x3,
                  xlab=list("Stichprobe aus einer N(0,1)-Verteilung",cex=1.5),
                  col="grey90",
                  ylab=list("Dichte",cex=1.5),
                  type="density",
                  scales=list(cex=1.5),
                  breaks=seq(from=-5,to=5,by=0.5),
                  panel = function(x, ...) {
                    panel.histogram(x, ...)
                    panel.mathdensity(dmath = dnorm, col="black", lty=2,
                                      args = list(mean=
                                                    mean(x3),
                                                  sd=sd(x3)),n=200)
#                    panel.densityplot(x3, lty=1, col="black",plot.points=F)
                  }
)
print(hist)

pdf("loes2_10_hist_n1000.pdf")
print(hist)
dev.off()


qqnorm(x3)
qqline(x3)

pdf("loes2_10_qq_n1000.pdf")
qqnorm(x3, xlab="Theoretische Quantile", ylab="Stichprobenquantile" )
qqline(x3)
dev.off()


