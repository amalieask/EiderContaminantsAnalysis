##############################
# BAR PLOT EIDER PLASMA 2021 #
##############################

# Code for producing Figure 2 in the article.

# Load libraries
library(tidyverse)
library(readxl)
library(cowplot)

# Create palette
pal <- c("#E69F00", "#56B4E9", "#0072B2", "#F0E442",  "#009E73", "#D55E00", "#CC79A7", "#999999")


# Import dataset (which is created manually from the values provided by the 
# "summaryStatisticsAllObs" function in the Python scipt)
sumStats <- read_excel("data/summaryStatsEiderPlasma2021.xlsx")

# Force the order of the IDs and compounds
sumStats$id <- factor(sumStats$id, unique(sumStats$id))
sumStats$compound <- factor(sumStats$compound, unique(sumStats$compound))

# Filter dataset
sumStats <- filter(sumStats, id != "earlyInc" & id != "lateInc")
BPS <- filter(sumStats, compound == "BPS")
BPA <- filter(sumStats, compound == "BPA")
BzP3 <- filter(sumStats, compound == "BzP3")
mMP <- filter(sumStats, compound == "mMP")
mEP <- filter(sumStats, compound == "mEP")


# Create bar plots
# BPA
bpa <- ggplot(BPA, aes(x = incubation, y = mean, ymin=mean-se, ymax=mean+se, fill=breedingPeriod)) + 
  geom_col(position = "dodge", show.legend=FALSE) +
  geom_hline(aes(yintercept = 6.21), linetype = 'dashed', show.legend = FALSE) +
  geom_errorbar(position = position_dodge(0.90), width = 0.25) +
  scale_fill_manual(values=pal, labels = c("Early breeders", 
                                           "Late breeders")) +
  theme_classic() +
  labs(x = "", y = "Mean ± SEM (ng/g ww)", fill = "", title = "BPA") + 
  scale_x_discrete(labels = c("T1", "T2"))


# BPS
bps <- ggplot(BPS, aes(x = incubation, y = mean, ymin=mean-se, ymax=mean+se, fill=breedingPeriod)) + 
  geom_col(position = "dodge", show.legend=FALSE) +
  geom_hline(aes(yintercept = 0.063), linetype = 'dashed', show.legend = FALSE) +
  geom_errorbar(position = position_dodge(0.90), width = 0.25) +
  scale_fill_manual(values=pal, labels = c("Early breeders", 
                                           "Late breeders")) +
  annotate("segment", x = 1.75, xend = 2.25, y = 0.2, yend = 0.2) +  # T2 samples
  annotate("text", x = 2.0, y = 0.21, label = "*") +
  theme_classic() +
  labs(x = "", y = "", fill = "", title = "BPS") +
  scale_x_discrete(labels = c("T1", "T2"))

# BzP-3
bzp3 <- ggplot(BzP3, aes(x = incubation, y = mean, ymin=mean-se, ymax=mean+se, fill=breedingPeriod)) + 
  geom_col(position = "dodge", show.legend=FALSE) +
  geom_hline(aes(yintercept = 0.873), linetype = 'dashed', show.legend = FALSE) +
  geom_errorbar(position = position_dodge(0.90), width = 0.25) +
  scale_fill_manual(values=pal, labels = c("Early breeders", 
                                           "Late breeders")) +
  annotate("segment", x = 0.75, xend = 1.25, y = 6, yend = 6) +  # T1 samples
  annotate("text", x = 1.0, y = 6.25, label = "*") +
  annotate("segment", x = 1.75, xend = 2.25, y = 7.25, yend = 7.25) +  # T2 samples
  annotate("text", x = 2.0, y = 7.5, label = "*") +
  theme_classic() +
  labs(x = "", y = "", fill = "", title = "BzP-3") +
  scale_x_discrete(labels = c("T1", "T2"))

# mMP
mmp <- ggplot(mMP, aes(x = incubation, y = mean, ymin=mean-se, ymax=mean+se, fill=breedingPeriod)) + 
  geom_col(position = "dodge", show.legend=FALSE) +
  geom_hline(aes(yintercept = 1.794), linetype = 'dashed', show.legend = FALSE) +
  geom_errorbar(position = position_dodge(0.90), width = 0.25) +
  scale_fill_manual(values=pal, labels = c("Early breeders", 
                                           "Late breeders")) +
  annotate("segment", x = 0.75, xend = 1.25, y = 8.5, yend = 8.5) +  # T1 samples
  annotate("text", x = 1.0, y = 8.75, label = "*") +
  annotate("segment", x = 1.75, xend = 2.25, y = 7.5, yend = 7.5) +  # T2 samples
  annotate("text", x = 2.0, y = 7.75, label = "*") +
  theme_classic() +
  labs(x = "", y = "Mean ± SEM (ng/g ww)", fill = "", title = "mMP") +
  scale_x_discrete(labels = c("T1", "T2"))

# mEP
mep <- ggplot(mEP, aes(x = incubation, y = mean, ymin=mean-se, ymax=mean+se, fill=breedingPeriod)) + 
  geom_col(position = "dodge") +
  geom_hline(aes(yintercept = 2.082), linetype = 'dashed', show.legend = FALSE) +
  geom_errorbar(position = position_dodge(0.90), width = 0.25) +
  scale_fill_manual(values=pal, labels = c("Early", 
                                           "Late")) +
  annotate("segment", x = 0.75, xend = 1.25, y = 18.25, yend = 18.25) +  # T1 samples
  annotate("text", x = 1.0, y = 18.5, label = "*") +
  annotate("segment", x = 1.75, xend = 2.25, y = 10, yend = 10) +  # T2 samples
  annotate("text", x = 2.0, y = 10.25, label = "*") +
  theme_classic() +
  labs(x = "", y = "", fill = "Breeders", title = "mEP") +
  scale_x_discrete(labels = c("T1", "T2"))

# Combine the individual plots into one plot
barPlot <- plot_grid(bpa, bps, bzp3, mmp, mep)
barPlot

# Export figure
ggsave("graphs/barPlot.png", plot = barPlot, width = 2571, height = 1865, units = "px")
