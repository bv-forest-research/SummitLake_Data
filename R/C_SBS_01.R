# 2022-08-22
# By: Leah Walker

# A script to calculate and plot live and dead standing carbon for SBS-01


##################
### FIELD Data ###
##################

# Field C LIVE - can skip if csv saved
#calc_field_C(raw_data = summit.lk.dat, live = TRUE) # You need to run lines 34-37 prior to this line (read in Summit Lake field data)

# Read in LIVE field C data
field_C_L <- read.csv("./Outputs/csv/SummitLake_C_field_L.csv") # Once you've run the calc_field_C function, you can just read in the data here

# Field C DEAD - can skip if csv saved
#calc_field_C(raw_data = summit.lk.dat, live = FALSE) # You need to run lines 34-37 prior to this line (read in Summit Lake field data)

# Read in DEAD field C data
field_C_D <- read.csv("./Outputs/csv/SummitLake_C_field_D.csv") # Once you've run the calc_field_BA function, you can just read in the data here


###################
### SORTIE DATA ###
###################

# Read in the csv file - created in the Data_prep_validation RMD
out_01 <- read.csv("./Outputs/csv/SummitLake_output_01.csv")
out_01 <- as.data.table(out_01)

# Data collection for Unit 4 and 15 started in 1994, not 1992 - DO NOT run these two lines if testing initiation
out_01$timestep[out_01$unit == 4] <- out_01$timestep[out_01$unit == 4] + 2
out_01$timestep[out_01$unit == 15] <- out_01$timestep[out_01$unit == 15] + 2

# Subset living and dead
out_01_L <- subset(out_01, out_01$Type != "Snag")
out_01_D <- subset(out_01, out_01$Type == "Snag")



##############
### SBS 01 ### 
##############

### LIVE ###

# Calculate Carbon per plot per timestep from SORTIE output

###
### You can skip below if the csv files are already created ###
###

out_01_L_C <- out_01_L # Read in SORTIE outputs in section 1

out_01_L_C[, Class := 1]

out_01_L_C$Species[out_01_L_C$Species == "Subalpine_Fir"] <- "Bl"
out_01_L_C$Species[out_01_L_C$Species == "Interior_Spruce"] <- "Sx"
out_01_L_C$Species[out_01_L_C$Species == "Paper_Birch"] <- "Ep"
out_01_L_C$Species[out_01_L_C$Species == "Western_Larch"] <- "Lw"
out_01_L_C$Species[out_01_L_C$Species == "Black_Cottonwood"] <- "Ac"
out_01_L_C$Species[out_01_L_C$Species == "Douglas_Fir"] <- "Fd"
out_01_L_C$Species[out_01_L_C$Species == "Trembling_Aspen"] <- "At"
out_01_L_C$Species[out_01_L_C$Species == "Lodgepole_Pine" ] <- "Pl"


Carbon <- vector()
for(i in 1:nrow(out_01_L_C)){
  Carbon[i] <- TreeCarbonFN(Species = out_01_L_C[i, Species], DBH = out_01_L_C[i, DBH], 
                            HT = out_01_L_C[i, Height], Tree_class = out_01_L_C[i, Class])
}

out_01_L_C[, ':='(C_tree = Carbon/1000)]

out_01_L_C <- out_01_L_C %>% group_by(timestep, unit) %>% mutate(C_unit = sum(C_tree, na.rm = TRUE))


#write.csv(out_01_L_C, paste0(path_outputs, "csv/SummitLake_C_01_L.csv"), row.names = FALSE)

####################
### SKIP TO HERE ###
####################

out_01_L_C <- read.csv(paste0(path_outputs, "csv/SummitLake_C_01_L.csv"))

out_01_L_C <- out_01_L_C %>%
  dplyr::select(unit, timestep, C_unit, treatment)

out_01_L_C <- unique(out_01_L_C)

out_01_L_C <- as.data.table(out_01_L_C)



# Compare final year of simulation C to final year of field data C - LIVE ONLY
final_C_01_L <- subset(out_01_L_C, timestep == 27)
#final_C_01_L[, ':='(DataSource = "sortie")]

final_C_01_L <- as.data.table(final_C_01_L)

final_field_C_L <- subset(field_C_L, timestep == 27)


final_C_01_L <- merge(final_C_01_L, final_field_C_L, by = c("unit", "treatment", "timestep"))
names(final_C_01_L)[names(final_C_01_L) == "C_unit.x"] <- "C_unit"
names(final_C_01_L)[names(final_C_01_L) == "C_unit.y"] <- "field_C"



lm_C_finalyr01 <-lm(C_unit ~ field_C - 1, final_C_01_L)
summary(lm_C_finalyr01)

rss01 <- with(final_C_01_L, sum(C_unit - field_C)^2)
rmse01 <- with(final_C_01_L, sqrt(mean(C_unit - field_C)^2))



# Plot observed versus predicted for the final year (27) of the trial
png("./Figures/Final_SBS01_L_C.png", width=700, height=500, units="px", pointsize=23, antialias="default")

ggplot(final_C_01_L, aes(x = field_C, y = C_unit, shape = treatment, color = treatment, label = unit)) +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12)) +
  geom_point(aes(colour = factor(treatment), shape = factor(treatment)), size = 2) +
  geom_text(hjust=-0.5, vjust=-0.5) +
  geom_abline() +
  scale_x_continuous(breaks = seq(30, 120, by = 10), limits = c(30, 120)) +
  scale_y_continuous(breaks = seq(30, 120, by = 10), limits = c(30, 120)) +
  labs(x = "Observed Carbon (Mg/ha)", y = "Predicted Carbon (Mg/ha)", col = "Treatment", shape = "Treatment") +
  scale_shape_manual(values = c(16, 15, 17),
                     breaks = c("ctrl", "med", "low"), 
                     labels = c("Control", "Medium", "Low")) +
  scale_colour_manual(values = c("#F8766D", "#619CFF", "#00BA38"), 
                      breaks = c("ctrl", "med", "low"), 
                      labels = c("Control", "Medium", "Low")) +
  geom_abline(slope = coef(lm_C_finalyr01)[["field_C"]], linetype = 3)

dev.off()


# Plot live carbon over time by treatment
out_01_L_C_T <- out_01_L_C 

out_01_L_C_T <- summarySE(out_01_L_C_T,
                          measurevar="C_unit",
                          groupvars=c("treatment", "timestep"))

out_01_L_C_T$N <- NULL

png("./Figures/C_01_L_T.png", width=1600, height=800, units="px",
    pointsize=23, antialias="default")

ggplot(NULL, aes(x = timestep,  y = C_unit, fill = treatment, group = treatment)) + 
  geom_Line(data = out_01_L_C_T, aes(col = treatment), size = 1.2) + 
  theme(axis.line = element_Line(colour = "black"), panel.background = element_blank(), legend.position = "top")+
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  ylab("Live carbon Mg/ha") + 
  xlab('Years post-treatment') +
  coord_cartesian(ylim = c(0, 115)) +
  geom_ribbon(data = out_01_L_C_T, aes(ymin = C_unit - ci, ymax = C_unit + ci),  alpha=0.1, linetype="dashed",  color="grey") +
  geom_point(data = summit.lk.dat_C_field, aes(shape = treatment), size = 3 , position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c(24, 21, 23, 22)) +
  geom_errorbar(data = summit.lk.dat_C_field, aes(ymin = C_unit - ci, ymax = C_unit + ci), position = position_dodge(width = 1)) 

dev.off()



##############
### SBS 01 ###
##############


### DEAD ### 


# Calculate Carbon per plot per timestep from SORTIE output

out_01_D_C <- out_01_D

out_01_D_C[, Class := 3]

out_01_D_C$Species[out_01_D_C$Species == "Subalpine_Fir"] <- "Bl"
out_01_D_C$Species[out_01_D_C$Species == "Interior_Spruce"] <- "Sx"
out_01_D_C$Species[out_01_D_C$Species == "Paper_Birch"] <- "Ep"
out_01_D_C$Species[out_01_D_C$Species == "Western_Darch"] <- "Lw"
out_01_D_C$Species[out_01_D_C$Species == "Black_Cottonwood"] <- "Ac"
out_01_D_C$Species[out_01_D_C$Species == "Douglas_Fir"] <- "Fd"
out_01_D_C$Species[out_01_D_C$Species == "Trembling_Aspen"] <- "At"
out_01_D_C$Species[out_01_D_C$Species == "Lodgepole_Pine" ] <- "Pl"




Carbon <- vector()
for(i in 1:nrow(out_01_D_C)){
  Carbon[i] <- TreeCarbonFN(Species = out_01_D_C[i, Species], DBH = out_01_D_C[i, DBH], 
                            HT = out_01_D_C[i, Height], Tree_class = out_01_D_C[i, Class])
}
out_01_D_C[, ':='(C_tree = Carbon/1000)]

out_01_D_C <- out_01_D_C %>% group_by(timestep, unit) %>% mutate(C_unit = sum(C_tree, na.rm = TRUE))

#write.csv(out_01_D_C, paste0(path_outputs, "csv/SummitLake_C_01_D.csv"), row.names = FALSE)
out_01_D_C <- read.csv(paste0(path_outputs, "csv/SummitLake_C_01_D.csv"))



c_vars <- c("unit", "timestep", "C_unit", "treatment")

out_01_D_C <- out_01_D_C[c_vars]
out_01_D_C <- unique(out_01_D_C)

out_01_D_C <- as.data.table(out_01_D_C)

# Compare final year of simulation C to final year of field data C - DEAD ONLY
u92 <- c(3,5,6,7,8,9,10,11,12,13,14,16,17,18,19,20,24)
u94 <- c(4,15)

final92_D <- subset(out_01_D_C, timestep == 27 & unit %in% u92)
final94_D <- subset(out_01_D_C, timestep == 25 & unit %in% u94)
final_C_01_D <- rbindlist(list(final92_D, final94_D))


final_C_01_D <-final_C_01_D[, field_C := c(30.7338663, 0.0, 10.2989469,  0.4441273, 12.3311521, 10.7635679, 15.6987466,  3.82775443, 0.0,  3.9516680, 16.839392,  1.5402020, 7.5052757, 31.5613052,  0.2057371,  2.8282707,  0.2722016, 19.2266906, 10.6543192)]



# Different colour by treatment
png("./Figures/Final_SBS01_D_C.png", width=700, height=500, units="px", pointsize=23, antialias="default")

ggplot(final_C_01_D, aes(x = field_C, y = C_unit, shape = treatment, color = treatment, label = unit)) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line =     
          element_Dine(colour = "black")) +
  geom_point(size = 2) +
  geom_text(hjust=-0.5, vjust=-0.5) +
  geom_abline() +
  scale_x_continuous(breaks = seq(0, 35, by = 5), limits = c(0, 35)) +
  scale_y_continuous(breaks = seq(0, 35, by = 15), limits = c(0, 35)) +
  xlab("Field Carbon (Mg/ha)") +
  ylab("SORTIE Carbon (Mg/ha")

dev.off()


# Carbon over time by treatment

out_01_D_C_T <- out_01_D_C 

out_01_D_C_T <- summarySE(out_01_D_C_T,
                          measurevar="C_unit",
                          groupvars=c("treatment", "timestep"))

out_01_D_C_T$N <- NULL

png("./Figures/C_01_D_T.png", width=1600, height=800, units="px",
    pointsize=23, antialias="default")

ggplot(NULL, aes(x = timestep,  y = C_unit, fill = treatment, group = treatment)) + 
  geom_Dine(data = out_01_D_C_T, aes(col = treatment), size = 1.2) + 
  theme(axis.line = element_Dine(colour = "black"), panel.background = element_blank(), legend.position = "top")+
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  ylab("Live carbon Mg/ha") + 
  xlab('Years post-treatment') +
  coord_cartesian(ylim = c(0, 30)) +
  geom_ribbon(data = out_01_D_C_T, aes(ymin = C_unit - ci, ymax = C_unit + ci),  alpha=0.1, linetype="dashed",  color="grey") +
  geom_point(data = summit.lk.dat_C_D_field, aes(shape = treatment), size = 3 , position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c(24, 21, 23, 22)) +
  geom_errorbar(data = summit.lk.dat_C_D_field, aes(ymin = C_unit - ci, ymax = C_unit + ci), position = position_dodge(width = 1)) 

dev.off()





