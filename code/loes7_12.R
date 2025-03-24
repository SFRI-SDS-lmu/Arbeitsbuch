##############################################################
# Lösung Aufgabe 7.12 im Lehrbuch
##############################################################

set.seed(3134561)

pi <- 0.4
N <- c(5, 20, 30, 50, 100)
# erzeuge jeweils S standardisierte Summen je gewähltem n
S <- 1000
for ( n in N ){
  # n Zeilen, S Spalten
  rv <- matrix(nrow=n, ncol=S,
               data=rbinom(n=S*n,size=1, prob=pi ))
  # standardsierte Summe, colSums liefert S Summen gebildet aus je n Werten
  zvalues <- ( colSums(rv)-n*pi) / ( sqrt(n*pi*(1-pi)) )
  hist(zvalues, freq=FALSE)
  curve( dnorm(x), from=-4, to=4, add=T, col="red")
  mtext(bquote(n== .(n)),3)
  Sys.sleep(3)
  
  # speichere die Grafiken als pdf ab
  pdf(paste("loes7_12_hist",as.character(n),"_",
            as.character(pi),".pdf",sep=""))
  par(cex=1.5)
  hist(zvalues, freq=FALSE)
  curve( dnorm(x), from=-4, to=4, add=T, col="red")
  mtext(bquote(n== .(n)),3)
  dev.off()
}





