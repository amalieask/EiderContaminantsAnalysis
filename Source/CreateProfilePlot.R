###########################################
# MEDIAN PROFILE PLOT - EIDER 2021 PLASMA #
###########################################

# Code for producing Figure 1 in the article, but note that it was further
# annotated using Affinity Publisher 2. 

# Load libraries
library(tidyverse)
library(readxl)
library(cowplot)

# Import dataset (which is created manually by taking the median values calculated
# by the "summaryStatisticsAboveLOD" function in the Python script)
median <- read_excel("data/mediansForProfileEiderPlasma2021.xlsx")

# Create colour palette
pal <- c("#D55E00", "#009E73", "#E69F00", "#56B4E9", "#F0E442", "#0072B2", "#CC79A7", "#999999")

# Filter dataset
BPs <- filter(median, class == "BP")
BzPs <- filter(median, class == "BzP")
PMs <- filter(median, class == "PM")
BTRsBTHs <- filter(median, class == "BTRandBTH")
Paraben <- filter(median, class == "Paraben")

# Re-position compounds
Paraben$compound <- factor(Paraben$compound, levels = c("MetP", "EtP", "PrP", "BuP", "BezP"))
PMs$compound <- factor(PMs$compound, levels = c("mMP", "mEP", "mBP", "mIBP", "mHxP", "mDP", "mEHP"))
BPs$compound <- factor(BPs$compound, levels = c( "BPS", "BPAF", "BPA"))
BTRsBTHs$compound <- factor(BTRsBTHs$compound, levels = c("2SCNMeSBTH", "2SBTH", "2OHBTH", "TTR"))

# Bisphenols
bps <- ggplot(BPs, aes(x = incubation, y = concentration, fill = compound)) + 
  geom_col(position = "stack", width = 0.5, show.legend=FALSE) +
  scale_fill_manual(values = pal) +
  labs(x = "", y = "Concentration (ng/g ww)", title = "BPs") +
  scale_x_discrete(labels = c("T1", "T2")) +
  theme_classic()

# Benzophenones
bzps <- ggplot(BzPs, aes(x = incubation, y = concentration, fill = compound)) + 
  geom_col(position = "stack", width = 0.5, show.legend=FALSE) +
  scale_fill_manual(values = pal) +
  labs(x = "", y = "", title = "BzPs") +
  scale_x_discrete(labels = c("T1", "T2")) +
  theme_classic()

# Phthalate metabolites
pms <- ggplot(PMs, aes(x = incubation, y = concentration, fill = compound)) + 
  geom_col(position = "stack", width = 0.5, show.legend=FALSE) +
  scale_fill_manual(values = pal) +
  labs(x = "", y = "", title = "PMs") +
  scale_x_discrete(labels = c("T1", "T2")) +
  theme_classic()

# Benzotriazoles and benzothiazoles
btrsbths <- ggplot(BTRsBTHs, aes(x = incubation, y = concentration, fill = compound)) + 
  geom_col(position = "stack", width = 0.5, show.legend=FALSE) +
  scale_fill_manual(values = pal) +
  labs(x = "", y = "Concentration (ng/g ww)", title = "BTRs and BTHs") +
  scale_x_discrete(labels = c("T1", "T2")) +
  theme_classic()

# Parabens
paras <- ggplot(Paraben, aes(x = incubation, y = concentration, fill = compound)) + 
  geom_col(position = "stack", width = 0.5, show.legend=FALSE) +
  scale_fill_manual(values = pal) +
  labs(x = "", y = "", title = "Parabens") +
  scale_x_discrete(labels = c("T1", "T2")) +
  theme_classic()

# Combine individual plots into one plot
profilePlot <- plot_grid(bps, bzps, pms, btrsbths, paras)

# Export figure
ggsave("graphs/profilePlot.png", plot = profilePlot, width = 2571, height = 1865, units = "px")

