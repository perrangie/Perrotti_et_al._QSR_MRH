wd <- "~/Dropbox/Postdoc/Late Glacial Project/"
setwd(wd)
getwd()

# ONLY WORKS IF THE DEPTHS SAMPLED ARE IN FILE WITH THE CORRECT AGES

#load updated ages (age estimates from the minSCD data produced by David Fastovich, sent to AGP in June 2020)
updated.ages <-read.csv("AgeDepth/Ages_Sept2020Models/SILV_ages.csv") 
head(updated.ages)

#load "old" ages
old.ages.fungi <- read.csv("Silver/SilvFungi_AllTotals_NewAges.csv")
old.ages.pollen <- read.csv("Silver/Silv_PollPct_NewAges.csv")
old.ages.minscd <-  read.csv("MinSCD/Silv_minscd.csv")
#old.ages.charcoal read.csv

#Update ages in the data frame/CSV with the wrong ages - the one line that accomplishes the merge
old.ages.fungi$Age <-updated.ages$age[updated.ages$depth %in%  old.ages.fungi$Depth]
old.ages.pollen$Age <-updated.ages$age[updated.ages$depth %in%  old.ages.pollen$Depth]
old.ages.minscd$age <-updated.ages$age[updated.ages$depth %in%  old.ages.minscd$depth]
#old.ages.char$Age <-updated.ages$age[updated.ages$depth %in%  old.ages.char$Depth]

# compare to make sure all went well
head(updated.ages$age)
head(old.ages.fungi$Age)
head(old.ages.pollen$Age)
head(old.ages.minscd$age)
#head(old.ages.char$Age)


#Export
write.csv(old.ages.fungi,"Silver/SilvFungi_AllTotals_NewestAges.csv", row.names = FALSE)
write.csv(old.ages.pollen,"Silver/Silv_PollPct_NewestAges.csv", row.names = FALSE)
write.csv(old.ages.minscd,"MinSCD/Silv_minscd_NewestAges.csv", row.names = FALSE)
#write.csv(old.ages.char,"Silver/Silv_Char.csv", row.names = FALSE)
