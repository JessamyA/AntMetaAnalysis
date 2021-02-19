#Set Working Directory
setwd("C:/Users/jessa/Documents/AntMetaAnalysis")

#Load in Packages
library(ggplot2)

#Import data 
Raw <- read.table("Data/Raw Data.txt", header = TRUE, sep = "\t")
SpeciesData <- read.table("Data/Species Level.txt", header = TRUE, sep = "\t")

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

#Piechart for Subfamily occurrence
ggplot(data = SpeciesData, aes(x = factor(1), fill = Subfamily)) +
  geom_bar() +
  coord_polar("y", start = 0) +
  blank_theme
  
#Piechart for Lab vs. Field studies
ggplot(data = Raw, aes(x = factor(1), fill = LorF)) +
  geom_bar(color = "black") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("Light Green", "Light Blue")) +
  blank_theme
