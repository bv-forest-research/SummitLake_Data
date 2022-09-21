# 2022-09-09
# By: Leah Walker


# A script containing the code to create basal area (BA) plots for the Summit Lake data



####################
### Read in data ###
####################

# Read in LIVE field BA data
field_BA_L <- read.csv("./Outputs/csv/SummitLake_BA_field_L.csv") # Once you've run the calc_field_BA function, you can just read in the data here

# Read in DEAD field BA data
field_BA_D <- read.csv("./Outputs/csv/SummitLake_BA_field_D.csv")

# Read in LIVE SORTIE SBS-01 data
out_01_L_BA <- read.csv("./Outputs/csv/SummitLake_BA_01_L.csv")

# Read in DEAD SORTIE SBS-01 data
out_01_D_BA <- read.csv("./Outputs/csv/SummitLake_BA_01_D.csv")

# Read in LIVE SORTIE SBS-0506 data
out_0506_L_BA <- read.csv("./Outputs/csv/SummitLake_BA_0506_L.csv")

# Read in DEAD SORTIE SBS-0506 data
out_0506_D_BA <- read.csv("./Outputs/csv/SummitLake_BA_0506_D.csv")



##############
### PLOT 1 ###
##############

# Predicted vs. observed plots for final year of simulation  - LIVE ONLY

# Field BA timestep 27
final_field_BA_L <- subset(field_BA_L, timestep == 27) # Run line 285 to read in the field data 




### SBS-01 ###

final_01_L_BA <- subset(out_01_L_BA, timestep == 27)

final_01_L_BA <- merge(final_01_L_BA, final_field_BA_L, by = c("unit", "treatment", "timestep"))
names(final_01_L_BA)[names(final_01_L_BA) == "BA_unit.x"] <- "BA_unit"
names(final_01_L_BA)[names(final_01_L_BA) == "BA_unit.y"] <- "field_BA"

# Simple plot
png("./Figures/Final_SBS01_L_BA.png", width=1000, height=1000, units="px", pointsize=23, antialias="default")

plot(final_01_L_BA$field_BA, final_01_L_BA$BA_unit, xlim = c(17,50), ylim = c(17,50)) + abline(0,1)

dev.off()

# Different colour by treatment
png("./Figures/Final_SBS01_L_BA_T.png", width=700, height=500, units="px", pointsize=23, antialias="default")

ggplot(final_01_L_BA, aes(x = field_BA, y = BA_unit, shape = treatment, color = treatment, label = unit)) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line =     
          element_Line(colour = "black")) +
  geom_point(size = 2) +
  geom_text(hjust=-0.5, vjust=-0.5) +
  geom_abline() +
  scale_x_continuous(breaks = seq(15, 50, by = 5), limits = c(15,50)) +
  scale_y_continuous(breaks = seq(15, 50, by = 5), limits = c(15,50))

dev.off()



### SBS-0506 ###

final_0506_L_BA <- subset(out_0506_L_BA, timestep == 27)

final_0506_L_BA <- merge(final_0506_L_BA, final_field_BA_L, by = c("unit", "treatment", "timestep"))
names(final_0506_L_BA)[names(final_0506_L_BA) == "BA_unit.x"] <- "BA_unit"
names(final_0506_L_BA)[names(final_0506_L_BA) == "BA_unit.y"] <- "field_BA"

# Simple plot
png("./Figures/Final_SBS0506_L_BA.png", width=1000, height=1000, units="px", pointsize=23, antialias="default")

plot(final_0506_L_BA$field_BA, final_0506_L_BA$BA_unit, xlim = c(17,50), ylim = c(17,50)) + abline(0,1)

dev.off()

# Different colour by treatment
png("./Figures/Final_SBS0506_L_BA_T.png", width=700, height=500, units="px", pointsize=23, antialias="default")

ggplot(final_0506_L_BA, aes(x = field_BA, y = BA_unit, shape = treatment, color = treatment, label = unit)) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line =     
          element_Line(colour = "black")) +
  geom_point(size = 2) +
  geom_text(hjust=-0.5, vjust=-0.5) +
  geom_abline() +
  scale_x_continuous(breaks = seq(15, 50, by = 5), limits = c(15,50)) +
  scale_y_continuous(breaks = seq(15, 50, by = 5), limits = c(15,50))

dev.off()



##############
### PLOT 2 ###
##############

# Predicted vs. observed plots for final year of simulation  - DEAD ONLY

# Sum BA of live trees per timestep per plot 
final_field_BA_D <- subset(field_BA_D, timestep == 27) # Run line 293 to read in the field data 



### SBS-01 ###


final_01_D_BA <- subset(out_01_D_BA, timestep == 27) 

final_01_D_BA <- merge(final_01_D_BA, final_field_BA_D, by = c("unit", "treatment", "timestep"))
names(final_01_D_BA)[names(final_01_D_BA) == "BA_unit.x"] <- "BA_unit"
names(final_01_D_BA)[names(final_01_D_BA) == "BA_unit.y"] <- "field_BA"

# Simple plot
png("./Figures/Final_SBS01_BA_D.png", width=1000, height=1000, units="px", pointsize=23, antialias="default")

plot(final_01_D_BA$field_BA, final_01_D_BA$BA_unit, xlim = c(0,15), ylim = c(0,15)) + abline(0,1)

dev.off()

# Different colour by treatment
png("./Figures/Final_SBS01_D_T.png", width=700, height=500, units="px", pointsize=23, antialias="default")

ggplot(final_01_D_BA, aes(x = field_BA, y = BA_unit, shape = treatment, color = treatment, label = unit)) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line =     
          element_Dine(colour = "black")) +
  geom_point(size = 2) +
  geom_text(hjust=-0.5, vjust=-0.5) +
  geom_abline() +
  scale_x_continuous(breaks = seq(0, 15, by = 1), limits = c(0,15)) +
  scale_y_continuous(breaks = seq(0, 15, by = 1), limits = c(0,15))

dev.off()



### SBS-0506 ###

final_0506_D_BA <- subset(out_0506_D_BA, timestep == 27)

final_0506_D_BA <- merge(final_0506_D_BA, final_field_BA_D, by = c("unit", "treatment", "timestep"))
names(final_0506_D_BA)[names(final_0506_D_BA) == "BA_unit.x"] <- "BA_unit"
names(final_0506_D_BA)[names(final_0506_D_BA) == "BA_unit.y"] <- "field_BA"

# Simple plot
png("./Figures/Final_SBS0506_BA_D.png", width=1000, height=1000, units="px", pointsize=23, antialias="default")

plot(final_0506_D_BA$field_BA, final_0506_D_BA$BA_unit, xlim = c(0,15), ylim = c(0,15)) + abline(0,1)

dev.off()

# Different colour by treatment
png("./Figures/Final_SBS0506_D_T.png", width=700, height=500, units="px", pointsize=23, antialias="default")

ggplot(final_0506_D_BA, aes(x = field_BA, y = BA_unit, shape = treatment, color = treatment, label = unit)) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line =     
          element_Dine(colour = "black")) +
  geom_point(size = 2) +
  geom_text(hjust=-0.5, vjust=-0.5) +
  geom_abline() +
  scale_x_continuous(breaks = seq(0, 15, by = 1), limits = c(0,15)) +
  scale_y_continuous(breaks = seq(0, 15, by = 1), limits = c(0,15))

dev.off()



###################
### PLOTS 3 & 4 ###
###################

# BA over time per plot (LIVE and DEAD)

### SBS 01 ###

### BA over time per plot - LIVE ONLY ###
png("./Figures/BA_over_time_01_L.png", width=1000, height=800, units="px", pointsize=23, antialias="default")

ggplot(NULL, aes(x = timestep, y = BA_unit)) +
  geom_Line(data = out_01_L_BA, aes(col = treatment), size = 1.2) +
  facet_wrap(facets = vars(unit)) +
  geom_point(data = field_BA_L, aes(shape = treatment, group = unit), size = 3)

dev.off()

### BA over time per plot - DEAD ONLY ###
png("./Figures/BA_over_time_01_D.png", width=1000, height=800, units="px", pointsize=23, antialias="default")

ggplot(NULL, aes(x = timestep, y = BA_unit, color = treatment)) +
  geom_Dine(data = out_01_D_BA, size = 1.2) +
  facet_wrap(facets = vars(unit)) +
  geom_point(data = field_BA_D, aes(shape = treatment, group = unit), size = 3)

dev.off()



### SBS 0506 ###

### BA over time per plot - LIVE ONLY ###
png("./Figures/BA_over_time_0506_L.png", width=1000, height=800, units="px", pointsize=23, antialias="default")

ggplot(NULL, aes(x = timestep, y = BA_unit)) +
  geom_Line(data = out_0506_L_BA, aes(col = treatment), size = 1.2) +
  facet_wrap(facets = vars(unit)) +
  geom_point(data = field_BA_L, aes(shape = treatment, group = unit), size = 3)

dev.off()

### BA over time per plot - DEAD ONLY ###
png("./Figures/BA_over_time_0506_D.png", width=1000, height=800, units="px", pointsize=23, antialias="default")

ggplot(NULL, aes(x = timestep, y = BA_unit, color = treatment)) +
  geom_Dine(data = out_0506_D_BA, size = 1.2) +
  facet_wrap(facets = vars(unit)) +
  geom_point(data = field_BA_D, aes(shape = treatment, group = unit), size = 3)

dev.off()



##################
### PLOT 5 & 6 ###
##################

# BA over time by treatment

### Field data by treatment ###

### LIVE ###

# Run line 266 to read in the field data 
field_BA_L_T <- summarySE(field_BA_L,
                          measurevar="BA_unit",
                          groupvars=c("treatment", "timestep"))

field_BA_L_T <- as.data.table(field_BA_L_T)

### DEAD ###

field_BA_D_T <- summarySE(field_BA_D,
                          measurevar="BA_unit",
                          groupvars=c("treatment", "timestep"))

field_BA_D_T <- as.data.table(field_BA_D_T)



### SBS-01 ###

### LIVE ###

out_01_L_BA_T <- summarySE(out_01_L_BA,
                           measurevar="BA_unit",
                           groupvars=c("treatment", "timestep"))

out_01_L_BA_T <- as.data.table(out_01_L_BA_T)

png("./Figures/BA_01_L_T.png", width=1600, height=800, units="px",
    pointsize=23, antialias="default")

ggplot(NULL, aes(x = timestep,  y = BA_unit, fill = treatment, group = treatment)) + 
  geom_Line(data = out_01_L_BA_T, aes(col = treatment), size = 1.2) + 
  theme(axis.line = element_Line(colour = "black"), panel.background = element_blank(), legend.position = "top")+
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  ylab(expression('Live basal area ('~ m^2~ha^-1~ ')')) + 
  xlab('Years post-treatment') +
  coord_cartesian(ylim = c(0, 45)) +
  geom_ribbon(data = out_01_L_BA_T, aes(ymin = BA_unit - ci, ymax = BA_unit + ci),  alpha=0.1, linetype="dashed",  color="grey") +
  geom_point(data = field_BA_L_T, aes(shape = treatment), size = 3 , position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c(24, 21, 23, 22)) +
  geom_errorbar(data = field_BA_L_T, aes(ymin = BA_unit - ci, ymax = BA_unit + ci), position = position_dodge(width = 1)) 

dev.off()

### DEAD ###

out_01_D_BA_T <- summarySE(out_01_D_BA,
                           measurevar="BA_unit",
                           groupvars=c("treatment", "timestep"))

out_01_D_BA_T <- as.data.table(out_01_D_BA_T)

png("./Figures/BA_01_D_T.png", width=1600, height=800, units="px",
    pointsize=23, antialias="default")

ggplot(NULL, aes(x = timestep,  y = BA_unit, fill = treatment, group = treatment)) + 
  geom_Dine(data = out_01_D_BA_T, aes(col = treatment), size = 1.2) + 
  theme(axis.line = element_Dine(colour = "black"), panel.background = element_blank(), legend.position = "top")+
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  ylab(expression('Live basal area ('~ m^2~ha^-1~ ')')) + 
  xlab('Years post-treatment') +
  coord_cartesian(ylim = c(0, 45)) +
  geom_ribbon(data = out_01_D_BA_T, aes(ymin = BA_unit - ci, ymax = BA_unit + ci),  alpha=0.1, linetype="dashed",  color="grey") +
  geom_point(data = field_BA_D_T, aes(shape = treatment), size = 3 , position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c(24, 21, 23, 22)) +
  geom_errorbar(data = field_BA_D_T, aes(ymin = BA_unit - ci, ymax = BA_unit + ci), position = position_dodge(width = 1)) 

dev.off()



### SBS-0506 ###

### LIVE ###

out_0506_L_BA_T <- summarySE(out_0506_L_BA,
                             measurevar="BA_unit",
                             groupvars=c("treatment", "timestep"))

out_0506_L_BA_T <- as.data.table(out_0506_L_BA_T)

png("./Figures/BA_0506_L_T.png", width=1600, height=800, units="px",
    pointsize=23, antialias="default")

ggplot(NULL, aes(x = timestep,  y = BA_unit, fill = treatment, group = treatment)) + 
  geom_Line(data = out_0506_L_BA_T, aes(col = treatment), size = 1.2) + 
  theme(axis.line = element_Line(colour = "black"), panel.background = element_blank(), legend.position = "top")+
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  ylab(expression('Live basal area ('~ m^2~ha^-1~ ')')) + 
  xlab('Years post-treatment') +
  coord_cartesian(ylim = c(0, 45)) +
  geom_ribbon(data = out_0506_L_BA_T, aes(ymin = BA_unit - ci, ymax = BA_unit + ci),  alpha=0.1, linetype="dashed",  color="grey") +
  geom_point(data = field_BA_L_T, aes(shape = treatment), size = 3 , position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c(24, 21, 23, 22)) +
  geom_errorbar(data = field_BA_L_T, aes(ymin = BA_unit - ci, ymax = BA_unit + ci), position = position_dodge(width = 1)) 

dev.off()

### DEAD ###

out_0506_D_BA_T <- summarySE(out_0506_D_BA,
                             measurevar="BA_unit",
                             groupvars=c("treatment", "timestep"))

out_0506_D_BA_T <- as.data.table(out_0506_D_BA_T)

png("./Figures/BA_0506_D_T.png", width=1600, height=800, units="px",
    pointsize=23, antialias="default")

ggplot(NULL, aes(x = timestep,  y = BA_unit, fill = treatment, group = treatment)) + 
  geom_Dine(data = out_0506_D_BA_T, aes(col = treatment), size = 1.2) + 
  theme(axis.line = element_Dine(colour = "black"), panel.background = element_blank(), legend.position = "top")+
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  ylab(expression('Live basal area ('~ m^2~ha^-1~ ')')) + 
  xlab('Years post-treatment') +
  coord_cartesian(ylim = c(0, 45)) +
  geom_ribbon(data = out_0506_D_BA_T, aes(ymin = BA_unit - ci, ymax = BA_unit + ci),  alpha=0.1, linetype="dashed",  color="grey") +
  geom_point(data = field_BA_D_T, aes(shape = treatment), size = 3 , position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c(24, 21, 23, 22)) +
  geom_errorbar(data = field_BA_D_T, aes(ymin = BA_unit - ci, ymax = BA_unit + ci), position = position_dodge(width = 1)) 

dev.off()








