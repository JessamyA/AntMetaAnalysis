#Set Working Directory
setwd("C:/Users/jessa/Documents/AntMetaAnalysis")

#Load in Packages
library(dplyr)
library(ggplot2)
library(tidyverse)

#Import data 
Raw <- read.table("Data/Raw Data.txt", header = TRUE, sep = "\t")
SpeciesData <- read.table("Data/Species Level.txt", header = TRUE, sep = "\t")
TotalSpeciesDataV1 <- read.table("Data/SubfamilySpeciesV1.txt", header = TRUE, sep = "\t")
TotalSpeciesDataV2 <- read.table("Data/SubfamilySpeciesV2.txt", header = TRUE, sep = "\t")
TotalSpeciesDataV3 <- read.table("Data/SubfamilySpeciesV3.txt", header = TRUE, sep = "\t")

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

############################################################################################## Lab vs. Field

#Piechart for Lab vs. Field studies
ggplot(data = Raw, aes(x = factor(1), fill = LorF)) +
  geom_bar(color = "black") +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/100)), size=5) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("Light Green", "Light Blue")) +
  blank_theme
