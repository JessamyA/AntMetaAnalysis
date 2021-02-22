#Set Working Directory
setwd("C:/Users/jessa/Documents/AntMetaAnalysis")

#Load in Packages
library(dplyr)
library(plyr)
library(ggplot2)

#Import data 
Rank <- read.table("Data/Ranking.txt", header = TRUE, sep = "\t")
SpeciesData <- read.table("Data/SpeciesLevel.txt", header = TRUE, sep = "\t") #NO RANK ADJUSTMENT

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

############################################################################################## Rank Plot

Rank <- Rank %>% mutate_at(vars(AveResults), funs(round(., 1)))

#Full data histogram
ggplot(Rank, aes(x = AveResults)) +
  geom_histogram(aes(y = ..density..), binwidth = 25, colour = "black", fill = "white") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(breaks = round(seq(min(Rank$AveResults),
                                        max(Rank$AveResults),
                                        by = 100), 1)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))

# Zoomed in 
ggplot(Rank, aes(x = AveResults)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, colour = "black", fill = "white") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(breaks = round(seq(min(Rank$AveResults), max(Rank$AveResults), by = 50), 1)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) +
  xlim(0, 250)

############################################################################################## Species

#Subsets for Mobility and Storage data with NO RANK ADJUSTMENT
MobSpecies <- SpeciesData[!is.na(SpeciesData$MobilityYN),]
StoSpecies <- SpeciesData[!is.na(SpeciesData$StorageYN),]

#Occurence of mobility data split among subfamilies - as pie chart
ggplot(MobSpecies, aes(x = Subfamily, y = factor(1), fill = MobilityYN)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = factor(1), vjust = 1.6, color = "white", size = 3.5))

ggplot(data = MobSpecies, aes(x = factor(1), fill = Subfamily)) +
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

#Occurence of storage data split among subfamilies - as pie chart
ggplot(data = StoSpecies, aes(x = factor(1), fill = Subfamily)) +
  geom_bar(color = "black") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#6befff",
                               "#6b6eff",
                               "#cc6bff")) +
  blank_theme






