wd <- "~/Dropbox/Postdoc/Late Glacial Project/Silver"
setwd(wd)
getwd()

# ONLY WORKS IF THE DEPTHS SAMPLED ARE IN FILE WITH THE CORRECT AGES
accum.rates <-read.csv("SILV_accumrates.csv") 

#load accum rate file with blank cm/year column
fungi.conc <- read.csv("Silv_FungiConc.csv")

#merge fungi accum file with accumrates to deposit accum rates from sheelar accum rates into blank cm/year colum
fungi.conc$cm.year <-accum.rates$cm.year[accum.rates$depth %in% fungi.conc$Depth]
head(fungi.conc)

#Export
write.csv(fungi.conc, "Silv_FungiAccum.csv", row.names = FALSE)
