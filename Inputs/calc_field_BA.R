# 2022-03-25
# By: Leah Walker

# A function to calculate BA from the summit lake data

library(data.table)

calc_field_BA <- function(raw_data, live) {
  
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
    
    write.csv(summit.lk.dat_BA, paste0("./Outputs/csv/SummitLake_BA_field_D.csv"), row.names = FALSE)
    
  } else {
    
    print(paste("Specify whether live or dead"))
    
  }
}


