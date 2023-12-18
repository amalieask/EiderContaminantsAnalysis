##########################################################
# RELATIONSHIP SUMCECS AND CLUTCH SIZE EIDER PLASMA 2021 #
##########################################################

# Code for cleaning up the csv file we wrote in the Python script and
# for producing Figure S1 from the article. 

# Load libraries
library(tidyverse)
library(readxl)
library(DHARMa)

# Import dataset
allCECs <- read.csv("data/plasmaEiderNTNU2021WithLODs.csv")

# Rename 2-OH-BTH, 2-S-BTH, 2-MeS-BTH, and 2-SCNMeS-BTH to remove the initial 
# "2", which works fine in Python, but not in R
allCECs <- rename(allCECs, 
                  OHBTH = X2OHBTH,
                  SBTH = X2SBTH,
                  MeSBTH = X2MeSBTH,
                  SCNMeSBTH = X2SCNMeSBTH)


# Replace the NaNs in mMP and mEP with zero for the purposes of adding the concentrations,
# as otherwise the samples containing NaNs will be dropped entirely.
allCECs$mMP <- replace_na(allCECs$mMP, 0)
allCECs$mEP <- replace_na(allCECs$mEP, 0)

# Add new column with sum of BPs, BzPs, PMs, BTRs&BTHs, parabens, and total CECs
allCECs <- mutate(allCECs, sumBPs = BPA + BPS + BPAF,
               sumBzPs = BzP1 + BzP3,
               sumPMs = mDP + mIBP + mBP + mHP + mEHP + mMP + mEP,
               sumBTRsBTHs = TTR + OHBTH + SBTH + MeSBTH + SCNMeSBTH,
               sumParabens = EtP + PrP + BuP + BezP,
               sumCECs = sumBPs + sumBzPs + sumPMs + sumBTRsBTHs + sumParabens)

# Filter dataset
allCECs <- filter(allCECs, BreedingPeriod != "mid") # Remove the "egg" birds
sample1 <- filter(allCECs, PlasmaSample == 1) # Early incubation, both breeders
sample2 <- filter(allCECs, PlasmaSample == 2) # Late incubation, both breeders


# Relationship between sumCECs and eggs laid, T1
clutchCEC <- ggplot(sample1, aes(x = Eggs, y = sumCECs)) + geom_point() +
  labs(x = "Number of eggs", y = "sumCEC (ng/g ww)") +
  scale_x_continuous(breaks = c(3, 5, 7, 9, 11)) +
  geom_smooth(method = "lm", colour = "black") +
  theme_classic()
early <- lm(sumCECs ~ Eggs, data = sample1)
summary(early) # Not significant, 0.45
# No relationship between how many eggs the female laid and her sumCEC at timepoint 1.

# Export figure
ggsave("graphs/clutchCECPlot.png", plot = clutchCEC, width = 2571, height = 1865, units = "px")

# Check diagnostic plots (using DHARMa)
simOutput <- simulateResiduals(fittedModel = early, plot = F)
residuals(simOutput)
plot(simOutput)
