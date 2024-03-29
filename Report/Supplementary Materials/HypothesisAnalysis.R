#WARNING: Working directory and file-paths will need to be changed if replicated from the sup materials

#Set Working Directory
setwd("C:/Users/jessa/Documents/AntMetaAnalysis")

#Load in Packages
library(dplyr)
library(plyr)
library(ggplot2)
library(ape)
library(gplots)
library(ggpubr)
library(rcompanion)
library(directlabels)

#Import data 
Rank <- read.table("Data/Ranking.txt", header = TRUE, sep = "\t")
SpeciesData <- read.table("Data/SpeciesLevel.txt", header = TRUE, sep = "\t") #NO RANK ADJUSTMENT
MobType <- read.table("Data/MobilityTypes/SubFamMobType.txt", header = TRUE, sep = "\t")
StoType <- read.table("Data/StorageTypes/SubFamStoType.txt", header = TRUE, sep = "\t")
MobCount <- read.table("Data/MobilityTypes/MobTypeCount.txt", header = TRUE, sep = "\t")
StoCount <- read.table("Data/StorageTypes/StoTypeCount.txt", header = TRUE, sep = "\t")
AMobType <- read.table("Data/MobilityTypes/SubFamMobType-ADJUSTED.txt", header = TRUE, sep = "\t")
AStoType <- read.table("Data/StorageTypes/SubFamStoType-ADJUSTED.txt", header = TRUE, sep = "\t")

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
        panel.border = element_rect(colour = "black", fill = NA)) +
  labs(x = "Mean Search Results", y = "Density")

# Zoomed in 
ggplot(Rank, aes(x = AveResults)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, colour = "black", fill = "white") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(breaks = round(seq(min(Rank$AveResults), max(Rank$AveResults), by = 50), 1)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) +
  labs(x = "Mean Search Results", y = "Density") +
  xlim(0, 200)

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
  scale_fill_manual(values = c("#ff6b6b",
                               "#ffff6b",
                               "#6b6eff",
                               "#cc6bff")) +
  scale_x_discrete(limits = c("Amblyoponinae",
                              "Ponerinae",
                              "Dorylinae",
                              "Dolichoderinae",
                              "Formicinae",
                              "Myrmicinae",
                              "Ectatomminae"))

#Adjusted v1
ggplot(AStoType, aes(x = Subfamily, y = X., fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black",
                                    fill = NA,)) +
  scale_fill_manual(values = c("#ff6b6b",
                               "#ffff6b",
                               "#6b6eff",
                               "#cc6bff"),
                    name = "Storage Type",
                    labels = c("External", "Internal", "Larval")) +
  scale_x_discrete(limits = c("Amblyoponinae",
                              "Ponerinae",
                              "Dorylinae",
                              "Dolichoderinae",
                              "Formicinae",
                              "Myrmicinae",
                              "Ectatomminae")) +
  labs(y = "Percentage")

#Adjusted v2
ggplot(AStoType, aes(x = Subfamily, y = X., fill = Type)) +
  geom_bar(stat = "identity", color = "black") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black",
                                    fill = NA,)) +
  scale_fill_manual(values = c("#ff6b6b",
                               "#ffff6b",
                               "#6b6eff",
                               "#cc6bff"),
                    name = "Storage Type",
                    labels = c("External", "Internal", "Larval")) +
  scale_x_discrete(limits = c("Amblyoponinae",
                              "Ponerinae",
                              "Dorylinae",
                              "Dolichoderinae",
                              "Formicinae",
                              "Myrmicinae",
                              "Ectatomminae"))

#geom_text(aes(label = Count), position = position_dodge(width = .9), vjust = 1.6, color = "black", size = 3.5)

StoTypeAnalysis <- read.delim("Data/StorageTypes/StoTypeTable.txt", header = TRUE, row.names = 1, sep = "\t")
TABStoTypeAnalysis <- as.table(as.matrix(StoTypeAnalysis))
fisher.test(StoTypeAnalysis)
pairwiseNominalIndependence(TABStoTypeAnalysis,
                            fisher = TRUE,
                            gtest  = FALSE,
                            chisq  = FALSE,
                            digits = 3)

#                Comparison p.Fisher p.adj.Fisher
#1  Formicinae : Myrmicinae 2.76e-08     2.76e-08
#2  Formicinae : Ponerinae  1.08e-37     3.24e-37
#3  Myrmicinae : Ponerinae  2.49e-16     3.74e-16

TStoTypeAnalysis <- t(StoTypeAnalysis)
TTABStoTypeAnalysis <- as.table(as.matrix(TStoTypeAnalysis))
pairwiseNominalIndependence(TTABStoTypeAnalysis,
                            fisher = TRUE,
                            gtest  = FALSE,
                            chisq  = FALSE,
                            digits = 3)

#  Comparison p.Fisher p.adj.Fisher
#1      E : I 8.67e-36     2.60e-35
#2      E : L 3.94e-06     5.91e-06
#3      I : L 8.94e-02     8.94e-02

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
  scale_fill_manual(values = c("#ff6b6b",
                               "#ffff6b",
                               "#6b6eff",
                               "#cc6bff")) +
  scale_x_discrete(limits = c("Amblyoponinae",
                              "Ponerinae",
                              "Dorylinae",
                              "Dolichoderinae",
                              "Formicinae",
                              "Myrmicinae",
                              "Ectatomminae"))

#Adjusted v1
ggplot(AMobType, aes(x = Subfamily, y = X., fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black",
                                    fill = NA,)) +
  scale_fill_manual(values = c("#ff6b6b",
                               "#ffff6b",
                               "#6b6eff",
                               "#cc6bff"),
                    name = "Storage Type",
                    labels = c("Adventitious", "Intrinsic Relocators", "Nomadism", "Unstable Nesters")) +
  scale_x_discrete(limits = c("Amblyoponinae",
                              "Ponerinae",
                              "Dorylinae",
                              "Dolichoderinae",
                              "Formicinae",
                              "Myrmicinae",
                              "Ectatomminae")) +
  labs(y = "Percentage")

#Adjusted v2
ggplot(AMobType, aes(x = Subfamily, y = X., fill = Type)) +
  geom_bar(stat = "identity", color = "black") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black",
                                    fill = NA,)) +
  scale_fill_manual(values = c("#ff6b6b",
                               "#ffff6b",
                               "#6b6eff",
                               "#cc6bff")) +
  scale_x_discrete(limits = c("Amblyoponinae",
                              "Ponerinae",
                              "Dorylinae",
                              "Dolichoderinae",
                              "Formicinae",
                              "Myrmicinae",
                              "Ectatomminae"))

# geom_text(aes(label = Count), position = position_dodge(width = .9), vjust = 1.6, color = "black", size = 3.5)

MobTypeAnalysis <- read.delim("Data/MobilityTypes/MobTypeTable.txt", header = TRUE, row.names = 1, sep = "\t")
TABMobTypeAnalysis <- as.table(as.matrix(MobTypeAnalysis))
fisher.test(MobTypeAnalysis, simulate.p.value = TRUE)
pairwiseNominalIndependence(TABMobTypeAnalysis,
                            fisher = TRUE,
                            gtest  = FALSE,
                            chisq  = FALSE,
                            digits = 3)

#                       Comparison p.Fisher p.adj.Fisher
#1  Amblyoponinae : Dolichoderinae 9.48e-30     2.21e-29
#2       Amblyoponinae : Dorylinae 4.45e-19     9.35e-19
#3    Amblyoponinae : Ectatomminae 2.03e-35     7.11e-35
#4      Amblyoponinae : Formicinae 2.81e-31     8.43e-31
#5      Amblyoponinae : Myrmicinae 6.93e-30     1.82e-29
#6       Amblyoponinae : Ponerinae 1.33e-13     2.54e-13
#7      Dolichoderinae : Dorylinae 2.21e-59     1.55e-58
#8   Dolichoderinae : Ectatomminae 2.14e-02     2.25e-02
#9     Dolichoderinae : Formicinae 3.74e-06     5.24e-06
#10    Dolichoderinae : Myrmicinae 9.96e-02     9.96e-02
#11     Dolichoderinae : Ponerinae 9.53e-06     1.25e-05
#12       Dorylinae : Ectatomminae 2.21e-59     1.55e-58
#13         Dorylinae : Formicinae 2.13e-51     1.12e-50
#14         Dorylinae : Myrmicinae 5.55e-60     1.17e-58
#15          Dorylinae : Ponerinae 6.91e-42     2.90e-41
#16      Ectatomminae : Formicinae 2.52e-04     3.11e-04
#17      Ectatomminae : Myrmicinae 2.58e-03     2.85e-03
#18       Ectatomminae : Ponerinae 2.35e-09     4.11e-09
#19        Formicinae : Myrmicinae 1.03e-03     1.20e-03
#20         Formicinae : Ponerinae 6.14e-09     9.92e-09
#21         Myrmicinae : Ponerinae 2.20e-06     3.30e-06

TMobTypeAnalysis <- t(MobTypeAnalysis)
TTABMobTypeAnalysis <- as.table(as.matrix(TMobTypeAnalysis))
pairwiseNominalIndependence(TTABMobTypeAnalysis,
                            fisher = TRUE,
                            gtest  = FALSE,
                            chisq  = FALSE,
                            digits = 3,
                            simulate.p.value = TRUE)

#  Comparison p.Fisher p.adj.Fisher
#1      A : I    5e-04        6e-04
#2      A : N    5e-04        6e-04
#3      A : U    5e-04        6e-04
#4      I : N    5e-04        6e-04
#5      I : U    1e-03        1e-03
#6      N : U    5e-04        6e-04

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
FisherNOAdj <- read.delim("Data/Cooccurance/FishersNo.txt", header = TRUE, row.names = 1, sep = "\t")
TABFisherNOAdj <- as.table(as.matrix(FisherNOAdj))

balloonplot(t(TABFisherNOAdj),
            main = "Real Data",
            xlab = "Mobility",
            ylab = "Storage",
            rowmar = .75,
            colmar = .75,
            label = TRUE,
            show.margins = FALSE,
            cum.margins = FALSE,
            dotcolor = "#ff6b6b")

#Including High
FisherAdjH <- read.delim("Data/Cooccurance/Fishers+H.txt", header = TRUE, row.names = 1, sep = "\t")
TABFisherAdjH <- as.table(as.matrix(FisherAdjH))

balloonplot(t(TABFisherAdjH), main = "Real Data + High-studied",
            xlab = "Mobility",
            ylab = "Storage",
            rowmar = .75,
            colmar = .75,
            label = TRUE,
            show.margins = FALSE,
            cum.margins = FALSE,
            dotcolor = "#6b6eff")

#Including High & Medium
FisherAdjHM <- read.delim("Data/Cooccurance/Fishers+H+M.txt", header = TRUE, row.names = 1, sep = "\t")
TABFisherAdjHM <- as.table(as.matrix(FisherAdjHM))

balloonplot(t(TABFisherAdjHM), main = "Real Data + High-studied + Medium-studied",
            xlab = "Mobility",
            ylab = "Storage",
            rowmar = .75,
            colmar = .75,
            label = TRUE,
            show.margins = FALSE,
            cum.margins = FALSE,
            dotcolor = "#cc6bff")

ggballoonplot(TABFisherAdjHM)

#FISHERS TESTS
#NO
fisher.test(FisherNOAdj)
#p-value = 0.3571
pairwiseNominalIndependence(TABFisherNOAdj,
                            fisher = TRUE,
                            gtest  = FALSE,
                            chisq  = FALSE,
                            digits = 3)
#StoY:StoN    p = 0.357
TNOAdj <- t(FisherNOAdj)
TTABNOAdj <- as.table(as.matrix(TNOAdj))
pairwiseNominalIndependence(TTABNOAdj,
                            fisher = TRUE,
                            gtest  = FALSE,
                            chisq  = FALSE,
                            digits = 3)
#MobY:MobN    p = 0.357

#+H
fisher.test(FisherAdjH)
#p-value = 0.2305
pairwiseNominalIndependence(TABFisherAdjH,
                            fisher = TRUE,
                            gtest  = FALSE,
                            chisq  = FALSE,
                            digits = 3)
#StoY:StoN    p = 0.23
TAdjH <- t(FisherAdjH)
TTABAdjH <- as.table(as.matrix(TAdjH))
pairwiseNominalIndependence(TTABAdjH,
                            fisher = TRUE,
                            gtest  = FALSE,
                            chisq  = FALSE,
                            digits = 3)
#MobY:MobN    p = 0.23

#+H+M
fisher.test(FisherAdjHM)
#p-value = 0.006366
pairwiseNominalIndependence(TABFisherAdjHM,
                            fisher = TRUE,
                            gtest  = FALSE,
                            chisq  = FALSE,
                            digits = 3)
#StoY:StoN    p = 0.00637
TAdjHM <- t(FisherAdjHM)
TTABAdjHM <- as.table(as.matrix(TAdjHM))
pairwiseNominalIndependence(TTABAdjHM,
                            fisher = TRUE,
                            gtest  = FALSE,
                            chisq  = FALSE,
                            digits = 3)
#MobY:MobN    p = 0.00637

#PEARSONS TESTS
#No Adjustments
PearsonNOAdj <- read.table("Data/Cooccurance/PearsonsNO.txt", header = TRUE, sep = "\t")

ggplot(PearsonNOAdj, aes(x = Storage, y = Mobility)) +
  geom_point(color = "white") +
  geom_smooth(method=lm, se=FALSE) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black",
                                    fill = NA,))

ggscatter(PearsonNOAdj, x = "Storage", y = "Mobility", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Storage", ylab = "Mobility")
shapiro.test(PearsonNOAdj$Storage)
shapiro.test(PearsonNOAdj$Mobility)
cor.test(PearsonNOAdj$Storage, PearsonNOAdj$Mobility, method = "pearson")

#+H
PearsonAdjH <- read.table("Data/Cooccurance/Pearsons+H.txt", header = TRUE, sep = "\t")

ggplot(PearsonAdjH, aes(x = Storage, y = Mobility)) +
  geom_point(color = "white") +
  geom_smooth(method=lm, se=FALSE) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black",
                                    fill = NA,))

ggscatter(PearsonAdjH, x = "Storage", y = "Mobility", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Storage", ylab = "Mobility")
shapiro.test(PearsonAdjH$Storage)
shapiro.test(PearsonAdjH$Mobility)
cor.test(PearsonAdjH$Storage, PearsonAdjH$Mobility, method = "pearson")

#+H+M
PearsonAdjHM <- read.table("Data/Cooccurance/Pearsons+H+M.txt", header = TRUE, sep = "\t")

ggplot(PearsonAdjHM, aes(x = Storage, y = Mobility)) +
  geom_point(color = "white") +
  geom_smooth(method=lm, se=FALSE) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black",
                                    fill = NA,))

ggscatter(PearsonAdjHM, x = "Storage", y = "Mobility", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Storage", ylab = "Mobility")
shapiro.test(PearsonAdjHM$Storage)
shapiro.test(PearsonAdjHM$Mobility)
cor.test(PearsonAdjHM$Storage, PearsonAdjHM$Mobility, method = "pearson")

#Combined Scatter plot + predicted line
Predicted <- data.frame(Storage = c(1,0),
                        Mobility = c(0,1))

ggplot(PearsonNOAdj, aes(x = Storage, y = Mobility)) +
  geom_point(color = "white") +
  geom_smooth(method = lm, se = FALSE, color = "#ff6b6b") +
  geom_text(aes(x = 0.97, y = 1, label = "Real"), color = "#ff6b6b") +
  geom_smooth(data = PearsonAdjH, method = lm, se = FALSE, color = "#6b6eff") +
  geom_text(aes(x = 0.95, y = 0.59, label = "Real +H"), color = "#6b6eff") + 
  geom_smooth(data = PearsonAdjHM, method = lm, se = FALSE, color = "#cc6bff") +
  geom_text(aes(x = 0.94, y = 0.40, label = "Real +H +M"), color = "#cc6bff") + 
  geom_smooth(data = Predicted, method = lm, se = FALSE, color = "Black") + 
  geom_text(aes(x = 0.95, y = 0.16, label = "Predicted"), color = "Black") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black",
                                    fill = NA,))


















