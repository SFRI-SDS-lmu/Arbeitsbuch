################################################################################
# Löhsung Aufgabe 12.2
################################################################################

# Lösche alle möglicherweise vorhandenen Objekte im Arbeitsbereich
rm(list = ls())


################################ AUFGABE a) ####################################

# Prognostizierte Nettomiete
linearer.praediktor <- 5.6727 + 0.0114*80 + 
                       0.0683*0 + 0.1627*0 +
                       0.1647*1 + 0.0482*1
nm.predicted <- exp(linearer.praediktor)
print(nm.predicted)


################################ AUFGABE b) ####################################

# t- und p- Werte
t <- c(5.6727/0.0650, 0.0114/0.0003, 0.0683/0.0175,
       0.1627/0.0502, 0.1647/0.0609, 0.0482/0.0169)
print(t)
print("p-Werte:")
2*(1-pt(t, df=625))


################################ AUFGABE c) ####################################

# F-Wert aus R^2 berechnen:
R2 <- 0.6798
n <- 631
p <- 5
Fwert <- R2/(1-R2) * ((n-p-1)/p)
Fwert
# kritischer Wert zu alpha=0.01
Fcrit <- qf(0.99, p, n-p-1)
Fcrit
# damit: H_0 ablehnen

