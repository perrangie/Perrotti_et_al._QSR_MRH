wd <- "~/Dropbox/Postdoc/Late Glacial Project/Silver"
setwd(wd)

#pct.conc <- read.csv("SheeFungi_AllTotals_NewestAges.csv")
#conc <- read.csv("Cup_FungiConc_NewestAges.csv")
#accum <- read.csv("Shee_FungiAccum.csv")

compare <- read.csv("Silv_FungiCompare.csv")


par(mfrow=c(1,2)) #puts all graphs in 1 row, 3 columms
plot(compare$StronglyCoproConc, compare$Age, type = "l", ylim = rev(range(compare$Age)), lwd = 2, col = "black", xlab = "Spores/cc", ylab = "Age", main="Silv Strongly Copro Conc")
plot(compare$TotalFungiConc, compare$Age, type = "l", ylim = rev(range(compare$Age)), lwd = 2, col = "black", xlab = "Spores/cc", ylab = "Age", main="Silv Total Fungi Conc")
plot(compare$StronglyCoproAccum, compare$Age, type = "l", ylim = rev(range(compare$Age)), lwd = 2, col = "black", xlab = "Spores/cm/year", ylab = "Age", main="Silv Strongly Copro Accum")
plot(compare$TotalFungiAccum, compare$Age, type = "l", ylim = rev(range(compare$Age)), lwd = 2, col = "black", xlab = "Spores/cm/year", ylab = "Age", main="Silv Total Fungi Accum")
plot(compare$StronglyCoproPct, compare$Age, type = "l", ylim = rev(range(compare$Age)), lwd = 2, col = "black", xlab = "Percent Total Pollen", ylab = "Age", main="Silv Strongly Copro Pct")
plot(compare$TotalFungiPct, compare$Age, type = "l", ylim = rev(range(compare$Age)), lwd = 2, col = "black", xlab = "Percent Total Pollen", ylab = "Age", main="Silv Total Fungi Pct")
