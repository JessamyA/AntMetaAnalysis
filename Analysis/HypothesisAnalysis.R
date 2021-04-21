#Set Working Directory
setwd("C:/Users/jessa/Documents/AntMetaAnalysis")

#Load in Packages
library(dplyr)
library(plyr)
library(ggplot2)
library(ape)
library(gplots)

#Import data 
Rank <- read.table("Data/Ranking.txt", header = TRUE, sep = "\t")
SpeciesData <- read.table("Data/SpeciesLevel.txt", header = TRUE, sep = "\t") #NO RANK ADJUSTMENT
MobType <- read.table("Data/SubFamMobType.txt", header = TRUE, sep = "\t")
StoType <- read.table("Data/SubFamStoType.txt", header = TRUE, sep = "\t")
MobCount <- read.table("Data/MobTypeCount.txt", header = TRUE, sep = "\t")
StoCount <- read.table("Data/StoTypeCount.txt", header = TRUE, sep = "\t")
AMobType <- read.table("Data/SubFamMobType-ADJUSTED.txt", header = TRUE, sep = "\t")
AStoType <- read.table("Data/SubFamStoType-ADJUSTED.txt", header = TRUE, sep = "\t")

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

############################################################################################## Subfamily data Occurrence
str(SpeciesData)
SpeciesData$Subfamily <- as.factor(SpeciesData$Subfamily)
str(SpeciesData$Subfamily)
levels(SpeciesData$Subfamily)
SpeciesData$StorageYN <- as.factor(SpeciesData$StorageYN)
str(SpeciesData$StorageYN)
levels(SpeciesData$StorageYN)
SpeciesData$StorageType <- as.factor(SpeciesData$StorageType)
str(SpeciesData$StorageType)
levels(SpeciesData$StorageType)
SpeciesData$MobilityType <- as.factor(SpeciesData$MobilityType)
str(SpeciesData$MobilityType)
levels(SpeciesData$MobilityType)
SpeciesData$MobilityYN <- as.factor(SpeciesData$MobilityYN)
str(SpeciesData$MobilityYN)
levels(SpeciesData$MobilityYN)
str(SpeciesData)

#Subsets for Mobility and Storage data with NO RANK ADJUSTMENT
MobSpecies <- SpeciesData[!is.na(SpeciesData$MobilityYN),] #74 obs.
StoSpecies <- SpeciesData[!is.na(SpeciesData$StorageYN),] #42 obs.

MobSpeciesY <- MobSpecies[!(MobSpecies$MobilityYN == "N"),] #74 obs.
MobSpeciesN <- MobSpecies[!(MobSpecies$MobilityYN == "Y"),] #3 obs.

StoSpeciesY <- StoSpecies[!(StoSpecies$StorageYN == "N"),] #38 obs.
StoSpeciesN <- StoSpecies[!(StoSpecies$StorageYN == "Y"),] #4 obs.

#Occurrence of mobility data split among subfamilies - as pie chart
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

#Occurrence of storage data split among subfamilies - as pie chart
ggplot(data = StoSpecies, aes(x = factor(1), fill = Subfamily)) +
  geom_bar(color = "black") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#6befff",
                               "#6b6eff",
                               "#cc6bff")) +
  blank_theme

############################################################################################## Type Counts

#Mobility
MobSpeciesType <- MobSpeciesY[!is.na(MobSpeciesY$MobilityType),]
MoTypeCount <- count(MobSpeciesType, "MobilityType")
str(MoTypeCount)

ggplot(MoTypeCount, aes(x = MobilityType, y = freq)) +
  geom_bar(stat = "identity", fill = "Light Blue", color = "black") +
  geom_text(aes(label = freq), vjust = 1.6, color = "black", size = 3.5) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black",
                                    fill = NA,)) +
  labs(x = "Mobility Type", y = "Frequency")

ggplot(MobCount, aes(x = Type, y = Count)) +
  geom_bar(stat = "identity", color = "black", fill = (values = c("pink",
                                                                  "light green",
                                                                  "light blue",
                                                                  "violet"))) +
  geom_text(aes(label = Count), vjust = 1.6, color = "black", size = 3.5) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black",
                                    fill = NA,))

#Storage
StoSpeciesType <- StoSpeciesY[!is.na(StoSpeciesY$StorageType),]
StTypeCount <- count(StoSpeciesType, "StorageType")
str(StTypeCount)

ggplot(StTypeCount, aes(x = StorageType, y = freq)) +
  geom_bar(stat = "identity", fill = "Light Blue", color = "black") +
  geom_text(aes(label = freq), vjust = 1.6, color = "black", size = 3.5) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black",
                                    fill = NA,)) +
  labs(x = "Storage Type", y = "Frequency")

ggplot(StoCount, aes(x = Type, y = Count)) +
  geom_bar(stat = "identity", color = "black", fill = (values = c("pink",
                                                                  "light blue",
                                                                  "light green"))) +
  geom_text(aes(label = Count), vjust = 1.6, color = "black", size = 3.5) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black",
                                    fill = NA,))

############################################################################################## Subfamily Type Graphs

#STORAGE
#Normal
ggplot(StoType, aes(x = Subfamily, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_text(aes(label = Count), position = position_dodge(width = .9), vjust = 1.6, color = "black", size = 3.5) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black",
                                    fill = NA,)) +
  scale_fill_manual(values = c("pink",
                               "light blue",
                               "light green")) +
  scale_x_discrete(limits = c("Amblyoponinae",
                              "Ponerinae",
                              "Dorylinae",
                              "Dolichoderinae",
                              "Formicinae",
                              "Myrmicinae",
                              "Ectatomminae"))

#Adjusted v1
ggplot(AStoType, aes(x = Subfamily, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black",
                                    fill = NA,)) +
  scale_fill_manual(values = c("pink",
                               "light blue",
                               "light green")) +
  scale_x_discrete(limits = c("Amblyoponinae",
                              "Ponerinae",
                              "Dorylinae",
                              "Dolichoderinae",
                              "Formicinae",
                              "Myrmicinae",
                              "Ectatomminae"))

#Adjusted v2
ggplot(AStoType, aes(x = Subfamily, y = Count, fill = Type)) +
  geom_bar(stat = "identity", color = "black") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black",
                                    fill = NA,)) +
  scale_fill_manual(values = c("pink",
                               "light blue",
                               "light green")) +
  scale_x_discrete(limits = c("Amblyoponinae",
                              "Ponerinae",
                              "Dorylinae",
                              "Dolichoderinae",
                              "Formicinae",
                              "Myrmicinae",
                              "Ectatomminae"))

#geom_text(aes(label = Count), position = position_dodge(width = .9), vjust = 1.6, color = "black", size = 3.5)

#MOBILITY
#Normal
ggplot(MobType, aes(x = Subfamily, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_text(aes(label = Count), position = position_dodge(width = .9), vjust = 1.6, color = "black", size = 3.5) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black",
                                    fill = NA,)) +
  scale_fill_manual(values = c("pink",
                               "light green",
                               "light blue",
                               "violet")) +
  scale_x_discrete(limits = c("Amblyoponinae",
                              "Ponerinae",
                              "Dorylinae",
                              "Dolichoderinae",
                              "Formicinae",
                              "Myrmicinae",
                              "Ectatomminae"))

#Adjusted v1
ggplot(AMobType, aes(x = Subfamily, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black",
                                    fill = NA,)) +
  scale_fill_manual(values = c("pink",
                               "light green",
                               "light blue",
                               "violet")) +
  scale_x_discrete(limits = c("Amblyoponinae",
                              "Ponerinae",
                              "Dorylinae",
                              "Dolichoderinae",
                              "Formicinae",
                              "Myrmicinae",
                              "Ectatomminae"))

#Adjusted v2
ggplot(AMobType, aes(x = Subfamily, y = Count, fill = Type)) +
  geom_bar(stat = "identity", color = "black") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black",
                                    fill = NA,)) +
  scale_fill_manual(values = c("pink",
                               "light green",
                               "light blue",
                               "violet")) +
  scale_x_discrete(limits = c("Amblyoponinae",
                              "Ponerinae",
                              "Dorylinae",
                              "Dolichoderinae",
                              "Formicinae",
                              "Myrmicinae",
                              "Ectatomminae"))

# geom_text(aes(label = Count), position = position_dodge(width = .9), vjust = 1.6, color = "black", size = 3.5)

############################################################################################## Phylogenetic Tree

#All subfamilies
SubfamTree <- ape::read.tree(text = '(((((((Heteroponerinae, Ectatomminae), Myrmicinae), Formicinae), ((Aneuretinae, Dolichoderinae),(Myrmeciinae, Pseudomyrmecinae))), Dorylinae), ((((Agroecomyrmecinae, Paraponerinae), Ponerinae), Proceratiinae), (Amblyoponinae, Apomyrminae))), (Leptanillinae, Martialinae));')
plot(SubfamTree)

#Subfamiles found in meta-analysis
SubfamTreeSMALL <- ape::read.tree(text = '(((((Ectatomminae, Myrmicinae), Formicinae), Dolichoderinae), Dorylinae), (Ponerinae, Amblyoponinae));')
plot(SubfamTreeSMALL)

############################################################################################## Contingency tables

#BALLOON PLOTS
#No Adjustments
FisherNOAdj <- read.delim("Data/FishersNo.txt", header = TRUE, row.names = 1, sep = "\t")
TABFisherNOAdj <- as.table(as.matrix(FisherNOAdj))

balloonplot(t(TABFisherNOAdj), main = "", xlab = "Storage", ylab = "Mobility",
            label = FALSE, show.margins = FALSE)

#Including High
FisherAdjH <- read.delim("Data/Fishers+H.txt", header = TRUE, row.names = 1, sep = "\t")
TABFisherAdjH <- as.table(as.matrix(FisherAdjH))

balloonplot(t(TABFisherAdjH), main = "", xlab = "Storage", ylab = "Mobility",
            label = FALSE, show.margins = FALSE)

#Including High & Medium
FisherAdjHM <- read.delim("Data/Fishers+H+M.txt", header = TRUE, row.names = 1, sep = "\t")
TABFisherAdjHM <- as.table(as.matrix(FisherAdjHM))

balloonplot(t(TABFisherAdjHM), main = "", xlab = "Storage", ylab = "Mobility",
            label = FALSE, show.margins = FALSE)




















