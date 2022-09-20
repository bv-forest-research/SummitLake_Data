# 2022-03-25
# By: Leah Walker

### A script containing the functions to calculate field metrics for the Summit Lake silvicultural trial

# A function to calculate the standing live and dead field basal area (BA) per plot from the field data

SL_calc_field_BA <- function(raw_data, live) {
  
  BA.function <- function(DBH) {pi*(DBH/200)^2}
  
  if(live == TRUE){
    
    raw_data <- raw_data %>%
      dplyr::select(Plot, Species, DBH_c_92_live, DBH_c_94_live, DBH_c_97_live, DBH_09_live, DBH_19_live)
    
    
    
    # Clean species codes
    # I am assuming "l" is for Larch
    raw_data <- raw_data %>%
      dplyr::mutate(Species = replace(Species, Species == "l", "Lw"))
    
    raw_data_BA <- raw_data
    
    # Calculate BA per tree
    raw_data_BA[, ':='(BA_92 = BA.function(raw_data_BA$DBH_c_92_live))]
    raw_data_BA[, ':='(BA_94 = BA.function(raw_data_BA$DBH_c_94_live))]
    raw_data_BA[, ':='(BA_97 = BA.function(raw_data_BA$DBH_c_97_live))]
    raw_data_BA[, ':='(BA_09 = BA.function(raw_data_BA$DBH_09_live))]
    raw_data_BA[, ':='(BA_19 = BA.function(raw_data_BA$DBH_19_live))]
    
    # Calculate BA per plot
    raw_data_BA <- raw_data_BA %>% group_by(Plot) %>% mutate(BA_92_plot = sum(BA_92, na.rm = TRUE)*20)
    raw_data_BA <- raw_data_BA %>% group_by(Plot) %>% mutate(BA_94_plot = sum(BA_94, na.rm = TRUE)*20)
    raw_data_BA <- raw_data_BA %>% group_by(Plot) %>% mutate(BA_97_plot = sum(BA_97, na.rm = TRUE)*20)
    raw_data_BA <- raw_data_BA %>% group_by(Plot) %>% mutate(BA_09_plot = sum(BA_09, na.rm = TRUE)*20)
    raw_data_BA <- raw_data_BA %>% group_by(Plot) %>% mutate(BA_19_plot = sum(BA_19, na.rm = TRUE)*20)
  
    raw_data_BA <- as.data.table(raw_data_BA)
    
    treatment <- data.table(Plot = c(3, 6, 9, 10, 16, 17, 20, 4, 8, 11, 12, 15, 18, 5, 7, 13, 14, 19, 24),
                            treatment = c(rep("ctrl", times = 7), rep("med", times = 6), rep("low", times = 6)))
    
    raw_data_BA <- merge(raw_data_BA, treatment, by = "Plot")
    
    raw_data_BA <- raw_data_BA %>%
      dplyr::select(Plot, treatment, BA_92_plot, BA_94_plot, BA_97_plot, BA_09_plot, BA_19_plot)
    
    raw_data_BA <- unique(raw_data_BA)
  
    raw_data_BA<- melt(raw_data_BA, id.vars = c("Plot", "treatment"), 
                       measure.vars = c("BA_92_plot", "BA_94_plot", "BA_97_plot", "BA_09_plot", "BA_19_plot"))
    
    names(raw_data_BA)[names(raw_data_BA) == "variable"] <- "timestep"
    names(raw_data_BA)[names(raw_data_BA) == "value"] <- "BA_unit"
    names(raw_data_BA)[names(raw_data_BA) == "Plot"] <- "unit"
    
    raw_data_BA <- raw_data_BA %>%
      dplyr::mutate(timestep = as.character(timestep))
    
    raw_data_BA <- raw_data_BA %>%
      dplyr::mutate(timestep = replace(timestep, timestep == "BA_92_plot", 0),
                    timestep = replace(timestep, timestep == "BA_94_plot", 2),
                    timestep = replace(timestep, timestep == "BA_97_plot", 5),
                    timestep = replace(timestep, timestep == "BA_09_plot", 17),
                    timestep = replace(timestep, timestep == "BA_19_plot", 27))
    
    raw_data_BA <- raw_data_BA %>%
      dplyr::mutate(timestep = as.numeric(timestep))
    
    raw_data_BA <- subset(raw_data_BA, BA_unit != 0.0)
    
    write.csv(raw_data_BA, paste0("./Outputs/csv/SummitLake_BA_field_L.csv"), row.names = FALSE)
    
  } else if (live == FALSE){
    
    raw_data <- raw_data %>%
      dplyr::select(Plot, Species, DBH_92_dead, DBH_94_dead, DBH_97_dead, DBH_09_dead, DBH_19_dead)
    
    # Clean species codes
    # I am assuming "l" is for Larch
    raw_data <- raw_data %>%
      dplyr::mutate(Species = replace(Species, Species == "l", "Lw"))
    
    
    # Repeat dead DBH's for following years
    raw_data <- raw_data %>% 
      dplyr::mutate(DBH_09_dead = ifelse(is.na(DBH_97_dead), DBH_09_dead, DBH_97_dead))
    raw_data <- raw_data %>% 
      dplyr::mutate(DBH_19_dead = ifelse(is.na(DBH_09_dead), DBH_19_dead, DBH_09_dead))
    
    raw_data_BA <- as.data.table(raw_data)
    
    # Calculate BA per dead tree
    raw_data_BA[, ':='(BA_92_d = BA.function(raw_data_BA$DBH_92_dead))]
    raw_data_BA[, ':='(BA_94_d = BA.function(raw_data_BA$DBH_94_dead))]
    raw_data_BA[, ':='(BA_97_d = BA.function(raw_data_BA$DBH_97_dead))]
    raw_data_BA[, ':='(BA_09_d = BA.function(raw_data_BA$DBH_09_dead))]
    raw_data_BA[, ':='(BA_19_d = BA.function(raw_data_BA$DBH_19_dead))]
  
    # Calculate dead BA per plot
    raw_data_BA <- raw_data_BA %>% group_by(Plot) %>% mutate(BA_92_d_plot = sum(BA_92_d, na.rm = TRUE)*20)
    raw_data_BA <- raw_data_BA %>% group_by(Plot) %>% mutate(BA_94_d_plot = sum(BA_94_d, na.rm = TRUE)*20)
    raw_data_BA <- raw_data_BA %>% group_by(Plot) %>% mutate(BA_97_d_plot = sum(BA_97_d, na.rm = TRUE)*20)
    raw_data_BA <- raw_data_BA %>% group_by(Plot) %>% mutate(BA_09_d_plot = sum(BA_09_d, na.rm = TRUE)*20)
    raw_data_BA <- raw_data_BA %>% group_by(Plot) %>% mutate(BA_19_d_plot = sum(BA_19_d, na.rm = TRUE)*20)
    
    raw_data_BA <- as.data.table(raw_data_BA)
    
    treatment <- data.table(Plot = c(3, 6, 9, 10, 16, 17, 20, 4, 8, 11, 12, 15, 18, 5, 7, 13, 14, 19, 24),
                            treatment = c(rep("ctrl", times = 7), rep("med", times = 6), rep("low", times = 6)))
    
    raw_data_BA <- merge(raw_data_BA, treatment, by = "Plot")
    
    raw_data_BA <- raw_data_BA %>%
      dplyr::select(Plot, treatment, BA_92_d_plot, BA_94_d_plot, BA_97_d_plot, BA_09_d_plot, BA_19_d_plot)
    
    raw_data_BA <- unique(raw_data_BA)
    
    # Wide to long
    raw_data_BA <- as.data.table(raw_data_BA)
    raw_data_BA <- melt(raw_data_BA, id.vars = c("Plot", "treatment"), 
                        measure.vars = c("BA_92_d_plot", "BA_94_d_plot", "BA_97_d_plot", "BA_09_d_plot", "BA_19_d_plot"))
    
    names(raw_data_BA)[names(raw_data_BA) == "variable"] <- "timestep"
    names(raw_data_BA)[names(raw_data_BA) == "value"] <- "BA_unit"
    names(raw_data_BA)[names(raw_data_BA) == "Plot"] <- "unit"
    
    raw_data_BA <- raw_data_BA %>%
      dplyr::mutate(timestep = as.character(timestep))
    
    raw_data_BA <- raw_data_BA %>%
      dplyr::mutate(timestep = replace(timestep, timestep == "BA_92_d_plot", 0),
                    timestep = replace(timestep, timestep == "BA_94_d_plot", 2),
                    timestep = replace(timestep, timestep == "BA_97_d_plot", 5),
                    timestep = replace(timestep, timestep == "BA_09_d_plot", 17),
                    timestep = replace(timestep, timestep == "BA_19_d_plot", 27))
    
    raw_data_BA <- raw_data_BA %>%
      dplyr::mutate(timestep = as.numeric(timestep))    
    
    raw_data_BA <- subset(raw_data_BA, BA_unit != 0.0)
    
    write.csv(raw_data_BA, paste0("./Outputs/csv/SummitLake_BA_field_D.csv"), row.names = FALSE)
    
  } else {
    
    print(paste("Specify whether live or dead"))
    
  }
}




# A function to calculate standing live and dead carbon for all species (and Sx and Bl separately) per plot from the field data

SL_calc_field_C <- function(raw_data, live, spp = NULL) {
  
  if(live == TRUE){
    
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
    
    
    
    if(is.null(spp)){ 
      
      # Calculate C per plot
      raw_data_C <- raw_data_C %>% group_by(unit) %>% mutate(C_92_unit = sum(C_92, na.rm = TRUE)*20)
      raw_data_C <- raw_data_C %>% group_by(unit) %>% mutate(C_94_unit = sum(C_94, na.rm = TRUE)*20)
      raw_data_C <- raw_data_C %>% group_by(unit) %>% mutate(C_97_unit = sum(C_97, na.rm = TRUE)*20)
      raw_data_C <- raw_data_C %>% group_by(unit) %>% mutate(C_09_unit = sum(C_09, na.rm = TRUE)*20)
      raw_data_C <- raw_data_C %>% group_by(unit) %>% mutate(C_19_unit = sum(C_19, na.rm = TRUE)*20)
      
      
      raw_data_C <- as.data.table(raw_data_C)
      
      raw_data_C <- raw_data_C %>%
        dplyr::select(unit, treatment, C_92_unit, C_94_unit, C_97_unit, C_09_unit, C_19_unit)
      
      raw_data_C <- unique(raw_data_C)
      
      raw_data_C <- melt(raw_data_C, id.vars = c("unit", "treatment"), 
                         measure.vars = c("C_92_unit", "C_94_unit", "C_97_unit", "C_09_unit", "C_19_unit"))
      
      names(raw_data_C)[names(raw_data_C) == "variable"] <- "timestep"
      names(raw_data_C)[names(raw_data_C) == "value"] <- "C_unit"
      
      raw_data_C <- raw_data_C %>%
        dplyr::mutate(timestep = as.character(timestep))
      
      raw_data_C <- raw_data_C %>%
        dplyr::mutate(timestep = replace(timestep, timestep == "C_92_unit", 0),
                      timestep = replace(timestep, timestep == "C_94_unit", 2),
                      timestep = replace(timestep, timestep == "C_97_unit", 5),
                      timestep = replace(timestep, timestep == "C_09_unit", 17),
                      timestep = replace(timestep, timestep == "C_19_unit", 27))
      
      raw_data_C <- raw_data_C %>%
        dplyr::mutate(timestep = as.numeric(timestep))
      
      raw_data_C <- subset(raw_data_C, C_unit != 0.0)
      
      raw_data_C <- unique(raw_data_C)
      
      write.csv(raw_data_C, "./Outputs/csv/SummitLake_C_field_L.csv", row.names = FALSE)
      
    } else if (spp == "Sx"){
      
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
      
    } else if (spp == "Bl"){
      
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
      
    }
    
  } else if (live == FALSE){
    
    raw_data <- raw_data %>%
      dplyr::select(Plot, Species, DBH_92_dead, DBH_94_dead, DBH_97_dead, DBH_09_dead, DBH_19_dead)
    
    # Clean species codes
    # I am assuming "l" is for Larch
    raw_data <- raw_data %>%
      dplyr::mutate(Species = replace(Species, Species == "l", "Lw"))
    
    # Repeat dead DBH's for following years
    raw_data <- raw_data %>% dplyr::mutate(DBH_09_dead = ifelse(is.na(DBH_97_dead), DBH_09_dead, DBH_97_dead))
    raw_data <- raw_data %>% dplyr::mutate(DBH_19_dead = ifelse(is.na(DBH_09_dead), DBH_19_dead, DBH_09_dead))
    
    raw_data_C <- as.data.table(raw_data)
    
    
    # rename plot to unit and add treatments
    names(raw_data_C)[names(raw_data_C) == "Plot"] <- "unit"
    
    treatment <- data.table(unit = c(3, 6, 9, 10, 16, 17, 20, 4, 8, 11, 12, 15, 18, 5, 7, 13, 14, 19, 24),
                            treatment = c(rep("ctrl", times = 7), rep("med", times = 6), rep("low", times = 6)))
    
    raw_data_C <- merge(raw_data_C, treatment, by = "unit")
    
    
    
    # Create a column for tree class, all deadso given a class of 3
    raw_data_C[, Class := 3]
    
    
    # Calculate height per tree
    HT92_D <- vector()
    for(i in 1:nrow(raw_data_C)){
      HT92_D[i] <- DiamHgtFN(Species = raw_data_C[i, Species], DBH = raw_data_C[i, DBH_92_dead])
    }
    
    HT94_D <- vector()
    for(i in 1:nrow(raw_data_C)){
      HT94_D[i] <- DiamHgtFN(Species = raw_data_C[i, Species], DBH = raw_data_C[i, DBH_94_dead])
    }
    
    HT97_D <- vector()
    for(i in 1:nrow(raw_data_C)){
      HT97_D[i] <- DiamHgtFN(Species = raw_data_C[i, Species], DBH = raw_data_C[i, DBH_97_dead])
    }
    
    HT09_D <- vector()
    for(i in 1:nrow(raw_data_C)){
      HT09_D[i] <- DiamHgtFN(Species = raw_data_C[i, Species], DBH = raw_data_C[i, DBH_09_dead])
    }
    
    HT19_D <- vector()
    for(i in 1:nrow(raw_data_C)){
      HT19_D[i] <- DiamHgtFN(Species = raw_data_C[i, Species], DBH = raw_data_C[i, DBH_19_dead])
    }
    
    raw_data_C[, ':='(HT_92_D = HT92_D)]
    raw_data_C[, ':='(HT_94_D = HT94_D)]
    raw_data_C[, ':='(HT_97_D = HT97_D)]
    raw_data_C[, ':='(HT_09_D = HT09_D)]
    raw_data_C[, ':='(HT_19_D = HT19_D)]
    
    
    
    # Calculate carbon per tree
    C92_D <- vector()
    for(ii in 1:nrow(raw_data_C)){
      C92_D[ii] <- TreeCarbonFN(Species = raw_data_C[ii, Species], DBH = raw_data_C[ii, DBH_92_dead], 
                                HT = raw_data_C[ii, HT_92_D], Tree_class = raw_data_C[ii, Class])
    }
    
    C94_D <- vector()
    for(ii in 1:nrow(raw_data_C)){
      C94_D[ii] <- TreeCarbonFN(Species = raw_data_C[ii, Species], DBH = raw_data_C[ii, DBH_94_dead], 
                                HT = raw_data_C[ii, HT_94_D], Tree_class = raw_data_C[ii, Class])
    }
    
    C97_D <- vector()
    for(ii in 1:nrow(raw_data_C)){
      C97_D[ii] <- TreeCarbonFN(Species = raw_data_C[ii, Species], DBH = raw_data_C[ii, DBH_97_dead], 
                                HT = raw_data_C[ii, HT_97_D], Tree_class = raw_data_C[ii, Class])
    }
    
    C09_D <- vector()
    for(ii in 1:nrow(raw_data_C)){
      C09_D[ii] <- TreeCarbonFN(Species = raw_data_C[ii, Species], DBH = raw_data_C[ii, DBH_09_dead], 
                                HT = raw_data_C[ii, HT_09_D], Tree_class = raw_data_C[ii, Class])
    }
    
    C19_D <- vector()
    for(ii in 1:nrow(raw_data_C)){
      C19_D[ii] <- TreeCarbonFN(Species = raw_data_C[ii, Species], DBH = raw_data_C[ii, DBH_19_dead], 
                                HT = raw_data_C[ii, HT_19_D], Tree_class = raw_data_C[ii, Class])
    }
    
    
    raw_data_C[, ':='(C_92_D = C92_D/1000)]
    raw_data_C[, ':='(C_94_D = C94_D/1000)]
    raw_data_C[, ':='(C_97_D = C97_D/1000)]
    raw_data_C[, ':='(C_09_D = C09_D/1000)]
    raw_data_C[, ':='(C_19_D = C19_D/1000)]
    
    
    
    # Calculate C per plot
    raw_data_C <- raw_data_C %>% group_by(unit) %>% mutate(C_92_D_unit = sum(C_92_D, na.rm = TRUE)*20)
    raw_data_C <- raw_data_C %>% group_by(unit) %>% mutate(C_94_D_unit = sum(C_94_D, na.rm = TRUE)*20)
    raw_data_C <- raw_data_C %>% group_by(unit) %>% mutate(C_97_D_unit = sum(C_97_D, na.rm = TRUE)*20)
    raw_data_C <- raw_data_C %>% group_by(unit) %>% mutate(C_09_D_unit = sum(C_09_D, na.rm = TRUE)*20)
    raw_data_C <- raw_data_C %>% group_by(unit) %>% mutate(C_19_D_unit = sum(C_19_D, na.rm = TRUE)*20)
    
    
    
    # Clean up columns
    raw_data_C <- raw_data_C %>%
      dplyr::select(unit, treatment, C_92_D_unit, C_94_D_unit, C_97_D_unit, C_09_D_unit, C_19_D_unit)
    
    
    raw_data_C <- as.data.table(unique(raw_data_C))
    
    raw_data_C <- melt(raw_data_C, id.vars = c("unit", "treatment"), 
                       measure.vars = c("C_92_D_unit", "C_94_D_unit", "C_97_D_unit", "C_09_D_unit", "C_19_D_unit"))
    
    
    
    names(raw_data_C)[names(raw_data_C) == "variable"] <- "timestep"
    names(raw_data_C)[names(raw_data_C) == "value"] <- "C_unit"
    
    
    
    raw_data_C <- raw_data_C %>%
      dplyr::mutate(timestep = as.character(timestep))
    
    raw_data_C <- raw_data_C %>%
      dplyr::mutate(timestep = replace(timestep, timestep == "C_92_D_unit", 0),
                    timestep = replace(timestep, timestep == "C_94_D_unit", 2),
                    timestep = replace(timestep, timestep == "C_97_D_unit", 5),
                    timestep = replace(timestep, timestep == "C_09_D_unit", 17),
                    timestep = replace(timestep, timestep == "C_19_D_unit", 27))
    
    raw_data_C <- raw_data_C %>%
      dplyr::mutate(timestep = as.numeric(timestep))
    
    raw_data_C <- subset(raw_data_C, C_unit != 0.0)
    
    raw_data_C <- unique(raw_data_C)
    
    write.csv(raw_data_C, "./Outputs/csv/SummitLake_C_field_D.csv", row.names = FALSE)
    
  }
  
}


