###########################
#### New data analysis ####
###########################

# Load Data and Packages ----
rm(list=ls())
Sys.setenv(LANG = "en")
setwd("Z:/Projects/R Projects/who-intervenes-how/data")
load("Data_04072016.RData")
library(ggplot2) # Create plots and graphics
library(psych) # Especially to use describeBy
library(plyr)
library(gmodels)
library(modeest) # Calculate the mode
library(mhurdle) # Hurdle and Double Hurdle Models (also for metric, not just counts)
library(lmtest)
library(stargazer) # Create tables for statistics and regressions
library(sandwich) # To calculate robust standard errors for regressions
library(MASS) # for ordered logistic regression 
library(sft) # Adjusted Rank Transform test
library(sjPlot) # Data Visualization for statistics in social science (especially interactions)
library(rms) # especially for visualization of interactions
library(mfx) # fr marginal effects in non-linear models
library(compactr) # for interaction plots (mm function)
library(interplot) # for automatic interaction plots
library(lsmeans) # predicted marginal means for specified factors or factor combinations (interactions)
library(effects) # to plot predicted values including interactions

# Distributions ----
# Histogram of Donation and Distance
ggplot(data = df, aes(x = Donation)) +
  geom_histogram(binwidth = .1) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7,8))+
  geom_vline(xintercept = 5, linetype = "dashed") +
  xlab("Contribution (in €)") +
  ylab("Number of observations")

ggplot(data = df, aes(x = Dist)) +
  geom_histogram(binwidth = .1) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7,8))+
  geom_vline(xintercept = 2, linetype = "dashed") +
  xlab("Distance (in €)") +
  ylab("Number of observations")

# with fractions on y axis
ggplot(data = df, aes(x = Donation, y =..count../sum(..count..))) +
  geom_histogram(binwidth = .1) +
#  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7,8))+
  geom_vline(xintercept = 5, linetype = "dashed") +
  xlab("Contribution (in €)") +
  ylab("Number of observations")

ggplot(data = df, aes(x = Dist, y =..count../sum(..count..))) +
  geom_histogram(binwidth = .1) +
#  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7,8))+
  geom_vline(xintercept = 2, linetype = "dashed") +
  xlab("Distance (in €)") +
  ylab("Number of observations")

# Histograms faceted by treatments ----
ggplot(data = df, aes(x = Donation)) +
  facet_wrap(~treatment)+ 
  geom_histogram(binwidth = .1) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7,8))+
  geom_vline(xintercept = 5, linetype = "dashed") +
  xlab("Contribution (in €)") +
  ylab("Number of observations")

# too complicated to include fractions per group and donation value ----
dfpart <- aggregate(df$Donation, list(df$treatment, df$Donation), length)
dfpar1 <- dfpart[dfpart$Group.1 == "Control",]$x 
dfpart1[dfpart$Group.1 == "Control",]$x/31


# count the number of 1.51 - 2.49 in Donations per treatment ----
dfzero <- aggregate(df$Donation == 0, list(df$treatment), sum)
dfzero$freq <- dfzero$x / aggregate(df$Donation, list(df$treatment), length)[2]
dftwo <- aggregate(df$Donation == 2, list(df$treatment), sum)
dftwo$freq <- dftwo$x / aggregate(df$Donation, list(df$treatment), length)[2]
dffive <- aggregate(df$Donation == 5, list(df$treatment), sum)
dffive$freq <- dffive$x / aggregate(df$Donation, list(df$treatment), length)[2]

ggplot() +
#  geom_bar(data = dftwo, aes(x = Group.1, y = freq), stat = "identity") +
#  geom_bar(data = dfzero, aes(x = Group.1, y = freq), stat = "identity") +
  geom_bar(data = dffive, aes(x = Group.1, y = freq), stat = "identity") +
    scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5), limits = c(0, 0.5)) +
  ylab("Fraction of participants contributing 5") +
  theme(axis.text.x = element_text(angle = 45, vjust=.8, size=10)) +
  xlab("Treatments")

