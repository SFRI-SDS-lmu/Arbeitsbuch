####################################################
# Lehrbuch Aufgabe 14.5
####################################################
library(graphics)
require(stlplus)

daten <- read.table(file="luftschad.txt", header=TRUE, dec=".")

# März 1996
print(daten[63,])
# Februar 2016
print(daten[302,])

# Teildatensatz
teildaten <- daten[63:302,]

# Stickoxid-Daten als ts-Datenobjekt
nox <- ts( teildaten$Stickoxide, start=c(1996,3), end=c(2016,2), frequency = 12 )

# starre Saisonfigur, Trend variiert
plot(stlplus(nox, t=time(nox), s.window="per"))
# flexible Saisonfigur mit stark variierendem Trend 
plot(stlplus(nox, t=time(nox), s.window=5), t.window=5)
# starre Saisonfigur mit starker Glättung des Trends
plot(stlplus(nox, t=time(nox), s.window="per", t.window=101))
# flexible Saisonfigur mit starker Glättung des Trends
plot(stlplus(nox, t=time(nox), s.window=5, t.window=101))

var( stlplus(nox, t=time(nox), s.window="per")$data$remainder)
var( stlplus(nox, t=time(nox), s.window=5, t.window=5)$data$remainder)
var( stlplus(nox, t=time(nox), s.window="per",t.window=101)$data$remainder)
var( stlplus(nox, t=time(nox), s.window=5, t.window=101)$data$remainder)


