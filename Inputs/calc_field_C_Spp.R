# 2022-03-25
# By: Leah Walker

# An R script to calculate plot level carbon for spruce and fir independently 

raw_data <- summit.lk.dat

raw_data <- raw_data %>%
  dplyr::select(Plot, Species, DBH_c_92_live, DBH_c_94_live, DBH_c_97_live, DBH_09_live, DBH_19_live)



# Clean species codes
# I am assuming "l" is for Larch
raw_data <- raw_data %>%
  dplyr::mutate(Species = replace(Species, Species == "l", "Lw"))


raw_data_C <- as.data.table(raw_data)


# rename plot to unit and add treatments
names(raw_data_C)[names(raw_data_C) == "Plot"] <- "unit"

treatment <- data.table(unit = c(3, 6, 9, 10, 16, 17, 20, 4, 8, 11, 12, 15, 18, 5, 7, 13, 14, 19, 24),
                        treatment = c(rep("ctrl", times = 7), rep("med", times = 6), rep("low", times = 6)))

raw_data_C <- merge(raw_data_C, treatment, by = "unit")


# Create a column for tree class, all living so given a class of 1
raw_data_C[, Class := 1]


# Calculate height per tree
HT92 <- vector()
for(i in 1:nrow(raw_data_C)){
  HT92[i] <- DiamHgtFN(Species = raw_data_C[i, Species], DBH = raw_data_C[i, DBH_c_92_live])
}

HT94 <- vector()
for(i in 1:nrow(raw_data_C)){
  HT94[i] <- DiamHgtFN(Species = raw_data_C[i, Species], DBH = raw_data_C[i, DBH_c_94_live])
}

HT97 <- vector()
for(i in 1:nrow(raw_data_C)){
  HT97[i] <- DiamHgtFN(Species = raw_data_C[i, Species], DBH = raw_data_C[i, DBH_c_97_live])
}

HT09 <- vector()
for(i in 1:nrow(raw_data_C)){
  HT09[i] <- DiamHgtFN(Species = raw_data_C[i, Species], DBH = raw_data_C[i, DBH_09_live])
}

HT19 <- vector()
for(i in 1:nrow(raw_data_C)){
  HT19[i] <- DiamHgtFN(Species = raw_data_C[i, Species], DBH = raw_data_C[i, DBH_19_live])
}

raw_data_C[, ':='(HT_92 = HT92)]
raw_data_C[, ':='(HT_94 = HT94)]
raw_data_C[, ':='(HT_97 = HT97)]
raw_data_C[, ':='(HT_09 = HT09)]
raw_data_C[, ':='(HT_19 = HT19)]



# Calculate carbon per tree
C92 <- vector()
for(ii in 1:nrow(raw_data_C)){
  C92[ii] <- TreeCarbonFN(Species = raw_data_C[ii, Species], DBH = raw_data_C[ii, DBH_c_92_live], 
                          HT = raw_data_C[ii, HT_92], Tree_class = raw_data_C[ii, Class])
}

C94 <- vector()
for(ii in 1:nrow(raw_data_C)){
  C94[ii] <- TreeCarbonFN(Species = raw_data_C[ii, Species], DBH = raw_data_C[ii, DBH_c_94_live], 
                          HT = raw_data_C[ii, HT_94], Tree_class = raw_data_C[ii, Class])
}

C97 <- vector()
for(ii in 1:nrow(raw_data_C)){
  C97[ii] <- TreeCarbonFN(Species = raw_data_C[ii, Species], DBH = raw_data_C[ii, DBH_c_97_live], 
                          HT = raw_data_C[ii, HT_97], Tree_class = raw_data_C[ii, Class])
}

C09 <- vector()
for(ii in 1:nrow(raw_data_C)){
  C09[ii] <- TreeCarbonFN(Species = raw_data_C[ii, Species], DBH = raw_data_C[ii, DBH_09_live], 
                          HT = raw_data_C[ii, HT_09], Tree_class = raw_data_C[ii, Class])
}

C19 <- vector()
for(ii in 1:nrow(raw_data_C)){
  C19[ii] <- TreeCarbonFN(Species = raw_data_C[ii, Species], DBH = raw_data_C[ii, DBH_19_live], 
                          HT = raw_data_C[ii, HT_19], Tree_class = raw_data_C[ii, Class])
}

raw_data_C[, ':='(C_92 = C92/1000)]
raw_data_C[, ':='(C_94 = C94/1000)]
raw_data_C[, ':='(C_97 = C97/1000)]
raw_data_C[, ':='(C_09 = C09/1000)]
raw_data_C[, ':='(C_19 = C19/1000)]




########################
###  INTERIOR SPRUCE ###
########################

raw_data_C_Sx <- subset(raw_data_C, raw_data_C$Species == "Sx")


# Calculate C per plot
raw_data_C_Sx <- raw_data_C_Sx %>% group_by(unit) %>% mutate(C_92_unit = sum(C_92, na.rm = TRUE)*20)
raw_data_C_Sx <- raw_data_C_Sx %>% group_by(unit) %>% mutate(C_94_unit = sum(C_94, na.rm = TRUE)*20)
raw_data_C_Sx <- raw_data_C_Sx %>% group_by(unit) %>% mutate(C_97_unit = sum(C_97, na.rm = TRUE)*20)
raw_data_C_Sx <- raw_data_C_Sx %>% group_by(unit) %>% mutate(C_09_unit = sum(C_09, na.rm = TRUE)*20)
raw_data_C_Sx <- raw_data_C_Sx %>% group_by(unit) %>% mutate(C_19_unit = sum(C_19, na.rm = TRUE)*20)


raw_data_C_Sx <- as.data.table(raw_data_C_Sx)

raw_data_C_Sx <- raw_data_C_Sx %>%
  dplyr::select(unit, treatment, C_92_unit, C_94_unit, C_97_unit, C_09_unit, C_19_unit)



raw_data_C_Sx <- unique(raw_data_C_Sx)



raw_data_C_Sx <- melt(raw_data_C_Sx, id.vars = c("unit", "treatment"), 
                   measure.vars = c("C_92_unit", "C_94_unit", "C_97_unit", "C_09_unit", "C_19_unit"))

names(raw_data_C_Sx)[names(raw_data_C_Sx) == "variable"] <- "timestep"
names(raw_data_C_Sx)[names(raw_data_C_Sx) == "value"] <- "C_unit"

raw_data_C_Sx <- raw_data_C_Sx %>%
  dplyr::mutate(timestep = as.character(timestep))

raw_data_C_Sx <- raw_data_C_Sx %>%
  dplyr::mutate(timestep = replace(timestep, timestep == "C_92_unit", 0),
                timestep = replace(timestep, timestep == "C_94_unit", 2),
                timestep = replace(timestep, timestep == "C_97_unit", 5),
                timestep = replace(timestep, timestep == "C_09_unit", 17),
                timestep = replace(timestep, timestep == "C_19_unit", 27))

raw_data_C_Sx <- raw_data_C_Sx %>%
  dplyr::mutate(timestep = as.numeric(timestep))

#raw_data_C_Sx <- subset(raw_data_C_Sx, C_unit != 0.0)

#raw_data_C_Sx <- unique(raw_data_C_Sx)

write.csv(raw_data_C_Sx, "./Outputs/csv/SummitLake_C_field_L_Sx.csv", row.names = FALSE)



######################
###  SUBALPINE FIR ###
######################

raw_data_C_Bl <- subset(raw_data_C, raw_data_C$Species == "Bl")


# Calculate C per plot
raw_data_C_Bl <- raw_data_C_Bl %>% group_by(unit) %>% mutate(C_92_unit = sum(C_92, na.rm = TRUE)*20)
raw_data_C_Bl <- raw_data_C_Bl %>% group_by(unit) %>% mutate(C_94_unit = sum(C_94, na.rm = TRUE)*20)
raw_data_C_Bl <- raw_data_C_Bl %>% group_by(unit) %>% mutate(C_97_unit = sum(C_97, na.rm = TRUE)*20)
raw_data_C_Bl <- raw_data_C_Bl %>% group_by(unit) %>% mutate(C_09_unit = sum(C_09, na.rm = TRUE)*20)
raw_data_C_Bl <- raw_data_C_Bl %>% group_by(unit) %>% mutate(C_19_unit = sum(C_19, na.rm = TRUE)*20)


raw_data_C_Bl <- as.data.table(raw_data_C_Bl)

raw_data_C_Bl <- raw_data_C_Bl %>%
  dplyr::select(unit, treatment, C_92_unit, C_94_unit, C_97_unit, C_09_unit, C_19_unit)



raw_data_C_Bl <- unique(raw_data_C_Bl)



raw_data_C_Bl <- melt(raw_data_C_Bl, id.vars = c("unit", "treatment"), 
                      measure.vars = c("C_92_unit", "C_94_unit", "C_97_unit", "C_09_unit", "C_19_unit"))

names(raw_data_C_Bl)[names(raw_data_C_Bl) == "variable"] <- "timestep"
names(raw_data_C_Bl)[names(raw_data_C_Bl) == "value"] <- "C_unit"

raw_data_C_Bl <- raw_data_C_Bl %>%
  dplyr::mutate(timestep = as.character(timestep))

raw_data_C_Bl <- raw_data_C_Bl %>%
  dplyr::mutate(timestep = replace(timestep, timestep == "C_92_unit", 0),
                timestep = replace(timestep, timestep == "C_94_unit", 2),
                timestep = replace(timestep, timestep == "C_97_unit", 5),
                timestep = replace(timestep, timestep == "C_09_unit", 17),
                timestep = replace(timestep, timestep == "C_19_unit", 27))

raw_data_C_Bl <- raw_data_C_Bl %>%
  dplyr::mutate(timestep = as.numeric(timestep))

#raw_data_C_Bl <- subset(raw_data_C_Bl, C_unit != 0.0)

#raw_data_C_Bl <- unique(raw_data_C_Bl)

write.csv(raw_data_C_Bl, "./Outputs/csv/SummitLake_C_field_L_Bl.csv", row.names = FALSE)


