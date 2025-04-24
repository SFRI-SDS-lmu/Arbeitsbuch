##################################################
# Aufgabe 3.8
##################################################

daten <- read.table(file="mietspiegel2015.txt", header=TRUE, dec=".")

# 1. Streudiagramm
plot(daten$bj, daten$nmqm)
pdf("loes3_8_streu1.pdf")
plot(daten$bj, daten$nmqm)
dev.off()

# mit ggplot
require(ggplot2)
ggplot(daten, aes(x=bj, y=nmqm)) + 
  geom_point() +
  theme(text = element_text(size = 24),
        axis.text = element_text(size = 18)) 
ggsave("loes3_8_streu2.pdf")

# 2. Lineare Einfachregression

lm.einfach <- lm(nmqm ~ bj, data=daten)
summary(lm.einfach)
# Schätzungen: alpha = (Intercept)=-32.93163295, beta=0.02222939
coefficients(lm.einfach)

# 3. R^2=0.04853. Eher klein, bj hat keinen großen Erklärungswert
# 4.
plot(daten$bj, daten$nmqm)
abline(coefficients(lm.einfach))

pdf("loes3_8_regrgerade1.pdf")
plot(daten$bj, daten$nmqm)
abline(coefficients(lm.einfach))
dev.off()

# oder ggplot:
ggplot(daten, aes(x=bj, y=nmqm)) + 
  geom_point() + 
  geom_smooth(method = "lm", se=FALSE)
ggsave("loes3_8_regrgerade2.pdf")

# 5.
eps <- resid(lm.einfach)
qqnorm(eps)
qqline(eps)

pdf("loes3_8_qq.pdf")
qqnorm(eps)
qqline(eps)
dev.off()

