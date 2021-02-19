#Set Working Directory
setwd("C:/Users/jessa/Documents/AntMetaAnalysis")

#Load in Packages
library(ggplot2)

#Import data 
Raw <- read.table("Data/Raw Data.txt", header = TRUE, sep = "\t")
SpeciesData <- read.table("Data/Species Level.txt", header = TRUE, sep = "\t")
TotalSpeciesData <- read.table("Data/Subfamily Species.txt", header = TRUE, sep = "\t")

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
  
#Piechart for Subfamily occurrence in Total
ggplot(data = TotalSpeciesData, aes(x = "", y = ExtantSpecies, fill = Subfamily)) +
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

#Piechart for Lab vs. Field studies
ggplot(data = Raw, aes(x = factor(1), fill = LorF)) +
  geom_bar(color = "black") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("Light Green", "Light Blue")) +
  blank_theme
