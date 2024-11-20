##################################
# BCI BOXPLOTS EIDER PLASMA 2021 #
##################################

# Code for producing Figure S2 in the supplementary materials.

# Load libraries:
library(tidyverse)
#library(readxl)
library(patchwork)

# Import datasets:
allCECs <- read.csv("data/plasmaEiderNTNU2021WithLODs.csv")

# Rename 2-OH-BTH, 2-S-BTH, 2-MeS-BTH, and 2-SCNMeS-BTH to remove the initial "2":
allCECs <- rename(allCECs, 
                  OHBTH = X2OHBTH,
                  SBTH = X2SBTH,
                  MeSBTH = X2MeSBTH,
                  SCNMeSBTH = X2SCNMeSBTH)

# Log-transform tarsus and mass:
allCECs <- mutate(allCECs, logTarsus = log(TarsusLong),
                  logMass = log(Mass))

# Create new column with standardized residuals, logBCI:
reg_log_tarsus_mass <- lm(logMass ~ logTarsus, data = allCECs)
allCECs <- mutate(allCECs, logBCI_standardized = rstandard(reg_log_tarsus_mass))


# Split dataframe into early (T1) and late (T2) incubation:
earlyIncubation <- filter(allCECs, Incubation == "early")
lateIncubation <- filter(allCECs, Incubation == "late")

# Create separate plots for the T1 and T2 sampling points:
T1BCI <- ggplot(earlyIncubation, aes(x = BreedingPeriod, y = logBCI_standardized)) + geom_boxplot() +
  labs(title = "First blood sample", x = "Breeding Period", y = "BCI") +
  scale_x_discrete(labels = c("Early", "Late"))
T2BCI <- ggplot(lateIncubation, aes(x = BreedingPeriod, y = logBCI_standardized)) + geom_boxplot() +
  labs(title = "Second blood sample", x = "Breeding Period", y = "BCI") +
  scale_x_discrete(labels = c("Early", "Late"))

# Combine plots using the patchwork library:
BCIPlot <- T1BCI + T2BCI

# Export figure:
ggsave("graphs/BCIPlot.png", plot = BCIPlot, 
       width = 2571, height = 1865, units = "px")