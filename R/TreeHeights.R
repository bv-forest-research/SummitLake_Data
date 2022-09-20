# April 2022
# By: Leah Walker

# An R script to compare actual vs. predicted tree heights
# Not all of our field data has measured heights, so we needed to use a function to calculate tree heights, we used this same function to calculate tree heights for the sortie data as well

library(dplyr)
library(data.table)
source("../Carbon/AllometryFunctions.R")

summit.lk.dat <- read.csv("./Inputs/Data/SummitLakeData.csv")

# Select needed columns
summit.lk.dat <- summit.lk.dat %>%
  dplyr::select(Plot, Species, DBH_19_live, HGT_19)

summit.lk.dat$DBH_19_live <- as.numeric(summit.lk.dat$DBH_19_live)
summit.lk.dat$HGT_19 <- as.numeric(summit.lk.dat$HGT_19)
summit.lk.dat <- subset(summit.lk.dat, DBH_19_live != "NA")
summit.lk.dat <- subset(summit.lk.dat, HGT_19 != "NA")

summit.lk.dat <- as.data.table(summit.lk.dat)

HT <- vector()
for(i in 1:nrow(summit.lk.dat)){
  HT[i] <- DiamHgtFN(Species = summit.lk.dat[i, Species], DBH = summit.lk.dat[i, DBH_19_live])
}

summit.lk.dat[, ':='(HT_predicted = HT)]

# Plot predicted vs observed
plot(summit.lk.dat$HGT_19, summit.lk.dat$HT, xlim = c(0,40), ylim = c(0,40)) + abline(0,1)

dev.off()
