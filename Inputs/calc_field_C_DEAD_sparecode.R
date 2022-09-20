# April 2022
# By: Leah Walker 

### keeping spare code here


summit.lk.dat_C_field <- summarySE(summit.lk.dat_C,
                                   measurevar="C_unit",
                                   groupvars=c("treatment", "timestep"))


##### keeping at tree level ####


summit.lk.dat_C$TreeID <- NULL
summit.lk.dat_C$Species <- NULL
summit.lk.dat_C$DBH_c_92_live <- NULL
summit.lk.dat_C$DBH_c_94_live<- NULL
summit.lk.dat_C$DBH_c_97_live<- NULL
summit.lk.dat_C$DBH_09_live<- NULL
summit.lk.dat_C$DBH_19_live<- NULL
summit.lk.dat_C$HT_92<- NULL
summit.lk.dat_C$HT_94<- NULL
summit.lk.dat_C$HT_97<- NULL
summit.lk.dat_C$HT_09<- NULL
summit.lk.dat_C$HT_19<- NULL
summit.lk.dat_C$Class<- NULL

summit.lk.dat_C <- melt(summit.lk.dat_C, id.vars = c("unit", "treatment"), 
                        measure.vars = c("C_92", "C_94", "C_97", "C_09", "C_19"))

names(summit.lk.dat_C)[names(summit.lk.dat_C) == "variable"] <- "timestep"
names(summit.lk.dat_C)[names(summit.lk.dat_C) == "value"] <- "C_tree"

summit.lk.dat_C$timestep <- as.character(summit.lk.dat_C$timestep)

summit.lk.dat_C$timestep[summit.lk.dat_C$timestep == "C_92"] <- 0
summit.lk.dat_C$timestep[summit.lk.dat_C$timestep == "C_94"] <- 2
summit.lk.dat_C$timestep[summit.lk.dat_C$timestep == "C_97"] <- 5
summit.lk.dat_C$timestep[summit.lk.dat_C$timestep == "C_09"] <- 17
summit.lk.dat_C$timestep[summit.lk.dat_C$timestep == "C_19"] <- 27

summit.lk.dat_C$timestep <- as.numeric(summit.lk.dat_C$timestep)

summit.lk.dat_C <- subset(summit.lk.dat_C, C_tree != "NA")

write.csv(summit.lk.dat_C, "./Outputs/csv/SummitLake_C_field_L.csv", row.names = FALSE)


summit.lk.dat$Sector <- NULL
summit.lk.dat$DBH_u_92 <- NULL
summit.lk.dat$DBH_u_94 <- NULL
summit.lk.dat$DBH_u_97 <- NULL
summit.lk.dat$FOUND_09 <- NULL
summit.lk.dat$Tag_hgt <- NULL
summit.lk.dat$DBH_Tag_Hgt_09 <- NULL
summit.lk.dat$DBH_BH_09 <- NULL
summit.lk.dat$DBH_Corr_Factor <- NULL
summit.lk.dat$DBH_c_92_live <- NULL
summit.lk.dat$DBH_c_94_live <- NULL
summit.lk.dat$DBH_c_97_live <- NULL
summit.lk.dat$DBH_09_live <- NULL
summit.lk.dat$DBH_19_live <- NULL
summit.lk.dat$X <- NULL
summit.lk.dat$HGT_92 <- NULL
summit.lk.dat$HGT_94 <- NULL
summit.lk.dat$HGT_97 <- NULL
summit.lk.dat$HGT_19 <- NULL
summit.lk.dat$COMMENTS_1992 <- NULL
summit.lk.dat$COMMENTS_1994 <- NULL
summit.lk.dat$COMMENTS_1997 <- NULL
summit.lk.dat$COMMENTS_2009 <- NULL
summit.lk.dat$COMMENTS_2019_May <- NULL
summit.lk.dat$COMMENTS_2019_Oct <- NULL
summit.lk.dat$Comments_2020_data <- NULL
summit.lk.dat$Conk <- NULL
summit.lk.dat$Blind_Conk <- NULL
summit.lk.dat$Scar <- NULL
summit.lk.dat$Fork_Crook <- NULL
summit.lk.dat$Frost_Crack <- NULL
summit.lk.dat$Mistletoe <- NULL
summit.lk.dat$R_Branch <- NULL
summit.lk.dat$DB_Top <- NULL
summit.lk.dat$Crown_Class <- NULL
summit.lk.dat$L_Crown <- NULL



# Clean species codes
unique(summit.lk.dat$Species)
# I am assuming "l" is for Larch
summit.lk.dat$Species[which(summit.lk.dat$Species == "l")] <- "Lw"



# Repeat dead DBH's for following years
summit.lk.dat <- summit.lk.dat %>% dplyr::mutate(DBH_09_dead = ifelse(is.na(DBH_97_dead), DBH_09_dead, DBH_97_dead))
summit.lk.dat <- summit.lk.dat %>% dplyr::mutate(DBH_19_dead = ifelse(is.na(DBH_09_dead), DBH_19_dead, DBH_09_dead))


summit.lk.dat <- as.data.table(summit.lk.dat)

summit.lk.dat_C_D <- summit.lk.dat


# rename plot to unit and add treatments
names(summit.lk.dat_C_D)[names(summit.lk.dat_C_D) == "Plot"] <- "unit"
treatment <- data.table(unit = c(3, 6, 9, 10, 16, 17, 20, 4, 8, 11, 12, 15, 18, 5, 7, 13, 14, 19, 24),
                        treatment = c(rep("ctrl", times = 7), rep("med", times = 6), rep("low", times = 6)))
summit.lk.dat_C_D <- merge(summit.lk.dat_C_D, treatment, by = "unit")



# Create a column for tree class, all deadso given a class of 3
summit.lk.dat_C_D[, Class := 3]


# Calculate height per tree
HT92_D <- vector()
for(i in 1:nrow(summit.lk.dat_C_D)){
  HT92_D[i] <- DiamHgtFN(Species = summit.lk.dat_C_D[i, Species], DBH = summit.lk.dat_C_D[i, DBH_92_dead])
}

HT94_D <- vector()
for(i in 1:nrow(summit.lk.dat_C_D)){
  HT94_D[i] <- DiamHgtFN(Species = summit.lk.dat_C_D[i, Species], DBH = summit.lk.dat_C_D[i, DBH_94_dead])
}

HT97_D <- vector()
for(i in 1:nrow(summit.lk.dat_C_D)){
  HT97_D[i] <- DiamHgtFN(Species = summit.lk.dat_C_D[i, Species], DBH = summit.lk.dat_C_D[i, DBH_97_dead])
}

HT09_D <- vector()
for(i in 1:nrow(summit.lk.dat_C_D)){
  HT09_D[i] <- DiamHgtFN(Species = summit.lk.dat_C_D[i, Species], DBH = summit.lk.dat_C_D[i, DBH_09_dead])
}

HT19_D <- vector()
for(i in 1:nrow(summit.lk.dat_C_D)){
  HT19_D[i] <- DiamHgtFN(Species = summit.lk.dat_C_D[i, Species], DBH = summit.lk.dat_C_D[i, DBH_19_dead])
}

summit.lk.dat_C_D[, ':='(HT_92_D = HT92_D)]
summit.lk.dat_C_D[, ':='(HT_94_D = HT94_D)]
summit.lk.dat_C_D[, ':='(HT_97_D = HT97_D)]
summit.lk.dat_C_D[, ':='(HT_09_D = HT09_D)]
summit.lk.dat_C_D[, ':='(HT_19_D = HT19_D)]



# Calculate carbon per tree
C92_D <- vector()
for(ii in 1:nrow(summit.lk.dat_C_D)){
  C92_D[ii] <- TreeCarbonFN(Species = summit.lk.dat_C_D[ii, Species], DBH = summit.lk.dat_C_D[ii, DBH_92_dead], 
                          HT = summit.lk.dat_C_D[ii, HT_92_D], Tree_class = summit.lk.dat_C_D[ii, Class])
}

C94_D <- vector()
for(ii in 1:nrow(summit.lk.dat_C_D)){
  C94_D[ii] <- TreeCarbonFN(Species = summit.lk.dat_C_D[ii, Species], DBH = summit.lk.dat_C_D[ii, DBH_94_dead], 
                          HT = summit.lk.dat_C_D[ii, HT_94_D], Tree_class = summit.lk.dat_C_D[ii, Class])
}

C97_D <- vector()
for(ii in 1:nrow(summit.lk.dat_C_D)){
  C97_D[ii] <- TreeCarbonFN(Species = summit.lk.dat_C_D[ii, Species], DBH = summit.lk.dat_C_D[ii, DBH_97_dead], 
                          HT = summit.lk.dat_C_D[ii, HT_97_D], Tree_class = summit.lk.dat_C_D[ii, Class])
}

C09_D <- vector()
for(ii in 1:nrow(summit.lk.dat_C_D)){
  C09_D[ii] <- TreeCarbonFN(Species = summit.lk.dat_C_D[ii, Species], DBH = summit.lk.dat_C_D[ii, DBH_09_dead], 
                          HT = summit.lk.dat_C_D[ii, HT_09_D], Tree_class = summit.lk.dat_C_D[ii, Class])
}

C19_D <- vector()
for(ii in 1:nrow(summit.lk.dat_C_D)){
  C19_D[ii] <- TreeCarbonFN(Species = summit.lk.dat_C_D[ii, Species], DBH = summit.lk.dat_C_D[ii, DBH_19_dead], 
                          HT = summit.lk.dat_C_D[ii, HT_19_D], Tree_class = summit.lk.dat_C_D[ii, Class])
}


summit.lk.dat_C_D[, ':='(C_92_D = C92_D/1000)]
summit.lk.dat_C_D[, ':='(C_94_D = C94_D/1000)]
summit.lk.dat_C_D[, ':='(C_97_D = C97_D/1000)]
summit.lk.dat_C_D[, ':='(C_09_D = C09_D/1000)]
summit.lk.dat_C_D[, ':='(C_19_D = C19_D/1000)]



# Calculate C per plot
summit.lk.dat_C_D <- summit.lk.dat_C_D %>% group_by(unit) %>% mutate(C_92_D_unit = sum(C_92_D, na.rm = TRUE)*20)
summit.lk.dat_C_D <- summit.lk.dat_C_D %>% group_by(unit) %>% mutate(C_94_D_unit = sum(C_94_D, na.rm = TRUE)*20)
summit.lk.dat_C_D <- summit.lk.dat_C_D %>% group_by(unit) %>% mutate(C_97_D_unit = sum(C_97_D, na.rm = TRUE)*20)
summit.lk.dat_C_D <- summit.lk.dat_C_D %>% group_by(unit) %>% mutate(C_09_D_unit = sum(C_09_D, na.rm = TRUE)*20)
summit.lk.dat_C_D <- summit.lk.dat_C_D %>% group_by(unit) %>% mutate(C_19_D_unit = sum(C_19_D, na.rm = TRUE)*20)



# Clean up columns
summit.lk.dat_C_D <- as.data.table(summit.lk.dat_C_D)


summit.lk.dat_C_D$TreeID <- NULL
summit.lk.dat_C_D$TreeID_new <- NULL
summit.lk.dat_C_D$Species <- NULL
summit.lk.dat_C_D$DBH_92_dead <- NULL
summit.lk.dat_C_D$DBH_94_dead<- NULL
summit.lk.dat_C_D$DBH_97_dead<- NULL
summit.lk.dat_C_D$DBH_09_dead<- NULL
summit.lk.dat_C_D$DBH_19_dead<- NULL
summit.lk.dat_C_D$HT_92_D<- NULL
summit.lk.dat_C_D$HT_94_D<- NULL
summit.lk.dat_C_D$HT_97_D<- NULL
summit.lk.dat_C_D$HT_09_D<- NULL
summit.lk.dat_C_D$HT_19_D<- NULL
summit.lk.dat_C_D$Class<- NULL
summit.lk.dat_C_D$C_92_D<- NULL
summit.lk.dat_C_D$C_94_D<- NULL
summit.lk.dat_C_D$C_97_D<- NULL
summit.lk.dat_C_D$C_09_D<- NULL
summit.lk.dat_C_D$C_19_D<- NULL


summit.lk.dat_C_D <- unique(summit.lk.dat_C_D)

summit.lk.dat_C_D <- melt(summit.lk.dat_C_D, id.vars = c("unit", "treatment"), 
                        measure.vars = c("C_92_D_unit", "C_94_D_unit", "C_97_D_unit", "C_09_D_unit", "C_19_D_unit"))



names(summit.lk.dat_C_D)[names(summit.lk.dat_C_D) == "variable"] <- "timestep"
names(summit.lk.dat_C_D)[names(summit.lk.dat_C_D) == "value"] <- "C_unit"



summit.lk.dat_C_D$timestep <- as.character(summit.lk.dat_C_D$timestep)

summit.lk.dat_C_D$timestep[summit.lk.dat_C_D$timestep == "C_92_D_unit"] <- 0
summit.lk.dat_C_D$timestep[summit.lk.dat_C_D$timestep == "C_94_D_unit"] <- 2
summit.lk.dat_C_D$timestep[summit.lk.dat_C_D$timestep == "C_97_D_unit"] <- 5
summit.lk.dat_C_D$timestep[summit.lk.dat_C_D$timestep == "C_09_D_unit"] <- 17
summit.lk.dat_C_D$timestep[summit.lk.dat_C_D$timestep == "C_19_D_unit"] <- 27

summit.lk.dat_C_D$timestep <- as.numeric(summit.lk.dat_C_D$timestep)





write.csv(summit.lk.dat_C_D, "./Outputs/csv/SummitLake_carbon_field_D.csv", row.names = FALSE)
summit.lk.dat_C_D <- read.csv("./Outputs/csv/SummitLake_carbon_field_D.csv")


summit.lk.dat_C_D_field <- summarySE(summit.lk.dat_C_D,
                                   measurevar="C_unit",
                                   groupvars=c("treatment", "timestep"))





