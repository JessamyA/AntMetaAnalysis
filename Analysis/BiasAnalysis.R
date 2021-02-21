#Set Working Directory
setwd("C:/Users/jessa/Documents/AntMetaAnalysis")

#Load in Packages
library(dplyr)
library(plyr)
library(ggplot2)
library(tidyverse)
library(Rtools)
library(rgdal)
library(RColorBrewer)
library(sf)

#Import data 
Raw <- read.table("Data/Raw Data.txt", header = TRUE, sep = "\t")
SpeciesData <- read.table("Data/Species Level.txt", header = TRUE, sep = "\t")
TotalSpeciesDataV1 <- read.table("Data/SubfamilySpeciesV1.txt", header = TRUE, sep = "\t")
TotalSpeciesDataV2 <- read.table("Data/SubfamilySpeciesV2.txt", header = TRUE, sep = "\t")
TotalSpeciesDataV3 <- read.table("Data/SubfamilySpeciesV3.txt", header = TRUE, sep = "\t")
Locations <- read.table("Data/Locations.txt", header = TRUE, sep = "\t")

#Blank theme for ggplot
blank_theme <- theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(size = 14, face = "bold")
  )

############################################################################################## Subfamily

#Piechart for Subfamily occurrence in Meta-Analysis
ggplot(data = SpeciesData, aes(x = factor(1), fill = Subfamily)) +
  geom_bar(color = "black") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#ff6b6b",
                               "#ffb76b",
                               "#ffff6b",
                               "#7dff6b",
                               "#6befff",
                               "#6b6eff",
                               "#cc6bff")) +
  blank_theme
  
#Piechart for Subfamily occurrence in Total - VERSION 1
ggplot(data = TotalSpeciesDataV1, aes(x = "", y = ExtantSpecies, fill = Subfamily)) +
  geom_bar(color = "black", stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("Grey",
                               "#ff6b6b",
                               "Grey",
                               "Grey",
                               "#ffb76b",
                               "#ffff6b",
                               "#7dff6b",
                               "#6befff",
                               "Grey",
                               "Grey",
                               "Grey",
                               "Grey",
                               "#6b6eff",
                               "Grey",
                               "#cc6bff",
                               "Grey",
                               "Grey")) +
  blank_theme

#Piechart for Subfamily occurrence in Total - VERSION 2
str(TotalSpeciesDataV2$Subfamily)
TotalSpeciesDataV2$Subfamily <- as.factor(TotalSpeciesDataV2$Subfamily)
str(TotalSpeciesDataV2$Subfamily)
levels(TotalSpeciesDataV2$Subfamily)
TotalSpeciesDataV2$Subfamily <- factor(TotalSpeciesDataV2$Subfamily,
                                       levels = c("Amblyoponinae",
                                                  "Dolichoderinae",
                                                  "Dorylinae",
                                                  "Ectatomminae",
                                                  "Formicinae",
                                                  "Myrmicinae",
                                                  "Ponerinae",
                                                  "Not Identified in Meta-Analysis"))
levels(TotalSpeciesDataV2$Subfamily)

ggplot(data = TotalSpeciesDataV2,
       aes(x = "", y = ExtantSpecies, fill = Subfamily)) +
  geom_bar(color = "black", stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#ff6b6b",
                               "#ffb76b",
                               "#ffff6b",
                               "#7dff6b",
                               "#6befff",
                               "#6b6eff",
                               "#cc6bff",
                               "Grey")) +
  blank_theme

#Piechart for Subfamily occurrence in Total - VERSION 3
ggplot(data = TotalSpeciesDataV3, aes(x = "", y = ExtantSpecies, fill = Subfamily)) +
  geom_bar(color = "black", stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#ff6b6b",
                               "#ffb76b",
                               "#ffff6b",
                               "#7dff6b",
                               "#6befff",
                               "#6b6eff",
                               "#cc6bff")) +
  blank_theme

#Statistical Analysis for Subfamily
#Merging Counts into the same dataframe
SubfamStats <- TotalSpeciesDataV3[,c("Subfamily", "ExtantSpecies")]
str(SpeciesData$Subfamily)
SpeciesData$Subfamily <- as.factor(SpeciesData$Subfamily)
str(SpeciesData$Subfamily)
levels(SpeciesData$Subfamily)
MetaSubfam <- count(SpeciesData, "Subfamily")
ComboSubfam <- merge(SubfamStats, MetaSubfam, by = "Subfamily")
ComboSubfam <- rbind(ComboSubfam, c("Other", 623, 0))
colnames(ComboSubfam)[colnames(ComboSubfam) %in% c("ExtantSpecies", "freq")] <- c("Expected", "Observed")
str(ComboSubfam)
ComboSubfam$Subfamily <- as.factor(ComboSubfam$Subfamily)
ComboSubfam$Expected <- as.numeric(ComboSubfam$Expected)
ComboSubfam$Observed <- as.numeric(ComboSubfam$Observed)
str(ComboSubfam)

#Transform to percentages for comparison
ES <- sum(ComboSubfam$Expected)
ComboSubfam["Expected"] = ComboSubfam["Expected"]/ES
ComboSubfam["Expected"] = ComboSubfam["Expected"]*100
sum(ComboSubfam$Expected)

OS <- sum(ComboSubfam$Observed)
ComboSubfam["Observed"] = ComboSubfam["Observed"]/OS
ComboSubfam["Observed"] = ComboSubfam["Observed"]*100
sum(ComboSubfam$Observed)

ComboSubfam <- ComboSubfam %>% mutate_at(vars(Observed, Expected), funs(round(., 7)))
sum(ComboSubfam$Expected)
sum(ComboSubfam$Observed)

#OR multiply Observed up to same value
ComboSubfam["Observed"] = ComboSubfam["Observed"]*(ES/OS)
sum(ComboSubfam$Observed)
ComboSubfam <- ComboSubfam %>% mutate_at(vars(Observed, Expected), funs(round(., 0)))
sum(ComboSubfam$Expected)
sum(ComboSubfam$Observed)

#Stat test - Chi-squared
Chidata <- ComboSubfam[,!(colnames(ComboSubfam) %in% "Expected")]
ExRatio <- (ComboSubfam$Expected)/ES
Chi <- chisq.test(Chidata, p = ExRatio)

############################################################################################## Lab vs. Field

#Counts
str(Raw$LorF)
Raw$LorF <- as.factor(Raw$LorF)
str(Raw$LorF)
levels(Raw$LorF)
LabField <- count(Raw, "LorF")
sum(LabField$freq)

#Piechart for Lab vs. Field studies
ggplot(data = Raw, aes(x = factor(1), fill = LorF)) +
  geom_bar(color = "black") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("Light Green", "Light Blue")) +
  blank_theme

############################################################################################## Map
#https://www.naturalearthdata.com/about/
MapLandBoundaries <- st_read("Analysis/Maps/ne_50m_admin_0_boundary_lines_land/ne_50m_admin_0_boundary_lines_land.shp")
MapCoastline <- st_read("Analysis/Maps/ne_50m_coastline/ne_50m_coastline.shp")

ggplot() + 
  geom_sf(data = MapLandBoundaries, size = 1, color = "black") +
  geom_sf(data = MapCoastline, size = 1, color = "black") + 
  ggtitle("Map") + 
  coord_sf()
  


























