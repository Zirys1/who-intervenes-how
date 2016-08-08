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
library(commarobust) # to include robust se'S in stargazer tables

# Distributions ----
# Histogram of Donation and Distance
ggplot(data = df, aes(x = Donation)) +
  geom_histogram(binwidth = .1) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7,8))+
  geom_vline(xintercept = 5, linetype = "dashed") +
  labs(x = "Contribution [in €]", y='Number of observations') +
  theme(legend.position="none",
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

ggplot(data = df, aes(x = Dist)) +
  geom_histogram(binwidth = .1) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7,8))+
  geom_vline(xintercept = 2, linetype = "dashed") +
  labs(x = "Distance to 5 [in €]", y='Number of observations') +
    theme(legend.position="none",
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

# with fractions on y axis
ggplot(data = df, aes(x = Donation, y =..count../sum(..count..))) +
  geom_histogram(binwidth = .1) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7,8))+
  geom_vline(xintercept = 5, linetype = "dashed") +
  labs(title = "Distribution of contribution values", x = "Contribution [in €]", y='Fraction of participants') +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, vjust=.5, size=10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

# For those that donated
ggplot(data = df, aes(x = Donationno0, y =..count../sum(..count..))) +
  geom_histogram(binwidth = .1) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7,8))+
  geom_vline(xintercept = 5, linetype = "dashed") +
  labs(title = "Distribution of contribution values conditional on contributing", x = "Contribution [in €]", y='Fraction of participants') +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, vjust=.5, size=10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))


ggplot(data = df, aes(x = Dist, y =..count../sum(..count..))) +
  geom_histogram(binwidth = .1) +
#  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7,8))+
  geom_vline(xintercept = 2, linetype = "dashed") +
  labs(x = "Distance to 5 [in €]", y='Fraction of participants') +
    theme(legend.position="none",
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

# Histograms faceted by treatments ----
# Overall Histogram
ggplot(data = df, aes(x = Donation, y = ..count../sum(..count..))) +
  #facet_wrap(~treatment)+ 
  geom_histogram(binwidth = .1) +
  scale_y_continuous(breaks = c(0, .05, .1, .15, .2, .25, .3, .35, .4, .45,.5), limits = c(0, .5)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7), limits = c(-0.5, 7.5))+
  geom_vline(xintercept = 5, linetype = "dashed") +
  labs(x = "Contribution [in €]", y='Number of observations') +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, vjust=.5, size=10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

# Facetted by treatment
ggplot(data = df[df$treatment == "Control",], aes(x = Donation, y = ..count../sum(..count..))) +
  geom_histogram(binwidth = .1) +
  scale_y_continuous(breaks = c(0, .05, .1, .15, .2, .25, .3, .35, .4, .45,.5), limits = c(0, .5)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7), limits = c(-0.5, 7.5))+
  geom_vline(xintercept = 5, linetype = "dashed") +
  labs(x = "Contribution [in €]", y='Fraction') +
  theme(legend.position="none",
        plot.title = element_text(angle = 45, vjust=.5,size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

ggplot(data = df[df$treatment == "RecNos",], aes(x = Donation, y = ..count../sum(..count..))) +
  geom_histogram(binwidth = .1) +
  scale_y_continuous(breaks = c(0, .05, .1, .15, .2, .25, .3, .35, .4, .45,.5), limits = c(0, .5)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7), limits = c(-0.5, 7.5))+
  geom_vline(xintercept = 5, linetype = "dashed") +
  labs(x = "Contribution [in €]", y='Fraction') +
  theme(legend.position="none",
        plot.title = element_text(angle = 45, vjust=.5,size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

ggplot(data = df[df$treatment == "DefNos",], aes(x = Donation, y = ..count../sum(..count..))) +
  geom_histogram(binwidth = .1) +
  scale_y_continuous(breaks = c(0, .05, .1, .15, .2, .25, .3, .35, .4, .45,.5), limits = c(0, .5)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7), limits = c(-0.5, 7.5))+
  geom_vline(xintercept = 5, linetype = "dashed") +
  labs(x = "Contribution [in €]", y='Fraction') +
  theme(legend.position="none",
        plot.title = element_text(angle = 45, vjust=.5,size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

ggplot(data = df[df$treatment == "RecNap",], aes(x = Donation, y = ..count../sum(..count..))) +
  geom_histogram(binwidth = .1) +
  scale_y_continuous(breaks = c(0, .05, .1, .15, .2, .25, .3, .35, .4, .45,.5), limits = c(0, .5)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7), limits = c(-0.5, 7.5))+
  geom_vline(xintercept = 5, linetype = "dashed") +
  labs(x = "Contribution [in €]", y='Fraction') +
  theme(legend.position="none",
        plot.title = element_text(angle = 45, vjust=.5,size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

ggplot(data = df[df$treatment == "DefNap",], aes(x = Donation, y = ..count../sum(..count..))) +
  geom_histogram(binwidth = .1) +
  scale_y_continuous(breaks = c(0, .05, .1, .15, .2, .25, .3, .35, .4, .45,.5), limits = c(0, .5)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7), limits = c(-0.5, 7.5))+
  geom_vline(xintercept = 5, linetype = "dashed") +
  labs(x = "Contribution [in €]", y='Fraction') +
  theme(legend.position="none",
        plot.title = element_text(angle = 45, vjust=.5,size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

ggplot(data = df[df$treatment == "RecPol",], aes(x = Donation, y = ..count../sum(..count..))) +
  geom_histogram(binwidth = .1) +
  scale_y_continuous(breaks = c(0, .05, .1, .15, .2, .25, .3, .35, .4, .45,.5), limits = c(0, .5)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7), limits = c(-0.5, 7.5))+
  geom_vline(xintercept = 5, linetype = "dashed") +
  labs(x = "Contribution [in €]", y='Fraction') +
  theme(legend.position="none",
        plot.title = element_text(angle = 45, vjust=.5,size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

ggplot(data = df[df$treatment == "DefPol",], aes(x = Donation, y = ..count../sum(..count..))) +
  geom_histogram(binwidth = .1) +
  scale_y_continuous(breaks = c(0, .05, .1, .15, .2, .25, .3, .35, .4, .45,.5), limits = c(0, .5)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7), limits = c(-0.5, 7.5))+
  geom_vline(xintercept = 5, linetype = "dashed") +
  labs(x = "Contribution [in €]", y='Fraction') +
  theme(legend.position="none",
        plot.title = element_text(angle = 45, vjust=.5,size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

ggplot(data = df[df$treatment == "RecPar",], aes(x = Donation, y = ..count../sum(..count..))) +
  geom_histogram(binwidth = .1) +
  scale_y_continuous(breaks = c(0, .05, .1, .15, .2, .25, .3, .35, .4, .45,.5), limits = c(0, .5)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7), limits = c(-0.5, 7.5))+
  geom_vline(xintercept = 5, linetype = "dashed") +
  labs(x = "Contribution [in €]", y='Fraction') +
  theme(legend.position="none",
        plot.title = element_text(angle = 45, vjust=.5,size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

ggplot(data = df[df$treatment == "DefPar",], aes(x = Donation, y = ..count../sum(..count..))) +
  geom_histogram(binwidth = .1) +
  scale_y_continuous(breaks = c(0, .05, .1, .15, .2, .25, .3, .35, .4, .45,.5), limits = c(0, .5)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7), limits = c(-0.5, 7.5))+
  geom_vline(xintercept = 5, linetype = "dashed") +
  labs(x = "Contribution [in €]", y='Fraction') +
  theme(legend.position="none",
        plot.title = element_text(angle = 45, vjust=.5,size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

ggplot(data = df[df$treatment == "RecKno",], aes(x = Donation, y = ..count../sum(..count..))) +
  geom_histogram(binwidth = .1) +
  scale_y_continuous(breaks = c(0, .05, .1, .15, .2, .25, .3, .35, .4, .45,.5), limits = c(0, .5)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7), limits = c(-0.5, 7.5))+
  geom_vline(xintercept = 5, linetype = "dashed") +
  labs(x = "Contribution [in €]", y='Fraction') +
  theme(legend.position="none",
        plot.title = element_text(angle = 45, vjust=.5,size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

ggplot(data = df[df$treatment == "DefKno",], aes(x = Donation, y = ..count../sum(..count..))) +
  geom_histogram(binwidth = .1) +
  scale_y_continuous(breaks = c(0, .05, .1, .15, .2, .25, .3, .35, .4, .45,.5), limits = c(0, .5)) +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7), limits = c(-0.5, 7.5))+
  geom_vline(xintercept = 5, linetype = "dashed") +
  labs(x = "Contribution [in €]", y='Fraction') +
  theme(legend.position="none",
        plot.title = element_text(angle = 45, vjust=.5,size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

# count the number of 0's, 2's and 5'S in Donations per treatment ----
dfzero <- aggregate(df$Donation == 0, list(df$treatment), sum)
dfzero$freq <- dfzero$x / aggregate(df$Donation, list(df$treatment), length)[2]
dftwo <- aggregate(df$Donation == 2, list(df$treatment), sum)
dftwo$freq <- dftwo$x / aggregate(df$Donation, list(df$treatment), length)[2]
dfnonzerotwo <- aggregate((df$Donation != 0 & df$Donation != 2), list(df$treatment), sum)
dfnonzerotwo$freq <- dfnonzerotwo$x / aggregate(df$Donation, list(df$treatment), length)[2]
dffive <- aggregate(df$Donation == 5, list(df$treatment), sum)
dffive$freq <- dffive$x / aggregate(df$Donation, list(df$treatment), length)[2]

ggplot() +
#  geom_bar(data = dfzero, aes(x = Group.1, y = freq), stat = "identity") +
#  geom_bar(data = dftwo, aes(x = Group.1, y = freq), stat = "identity") +
#  geom_bar(data = dffive, aes(x = Group.1, y = freq), stat = "identity") +
#  geom_bar(data = dfnonzerotwo, aes(x = Group.1, y = freq), stat = "identity") +
  geom_bar(data = dfpos, aes(x = Group.1, y = x/n*100), stat = "identity") +
  geom_errorbar(data = dfpos, aes(ymin = lower, ymax = upper), colour = "#999999") +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), limits = c(0, 0.9)) +
  labs(x = "Experimental group", y='Fraction of participants contributing something') +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, vjust=.5, size=10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

# Extensive margin with Error bars ----
dfpos <- aggregate(as.numeric(df$Donatedm), list(df$treatment), sum)
dfpos$n <- aggregate(df$Donatedm, list(df$treatment), length)[2]
dfpos$sd <- aggregate(as.numeric(df$Donatedm), list(df$treatment), sd)[2]
dfpos$se <- dfpos$sd/sqrt(dfpos$n)
dfpos$lower <- (dfpos$x / dfpos$n - dfpos$se)*100
dfpos$upper <- (dfpos$x / dfpos$n + dfpos$se)*100
dfpos$up95 <- (dfpos$x / dfpos$n + 1.96 * dfpos$se) * 100
dfpos$low95 <- (dfpos$x / dfpos$n - 1.96 * dfpos$se) * 100

ggplot(data = dfpos, aes(Group.1, x/n*100)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "#999999") +
  scale_y_continuous(breaks = c(0, 10, 20,30,40,50,60,70,80)) +
  labs(title='Figure 1: Percentage donating', x = "Treatment", y='Contributed (in %)')+
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

## for a subset of treatments 
ggplot(data = dfpos[dfpos$Group.1 == "DefPol" | dfpos$Group.1 == "DefKno" | dfpos$Group.1 == "RecPol" | dfpos$Group.1 == "RecKno",], aes(Group.1, x/n*100)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "#999999") +
  scale_y_continuous(breaks = c(0, 10, 20,30,40,50,60,70,80,90,100)) +
  labs(title='Figure 1: Percentage donating', x = "Treatment", y='Contributed (in %)')+
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

## different presentation for all treatments except control
dfpos$RecvsDef <- ifelse(dfpos$Group.1 == "RecNos" |dfpos$Group.1 == "RecNap" |dfpos$Group.1 == "RecPol" |dfpos$Group.1 == "RecPar" |dfpos$Group.1 == "RecKno", 0, ifelse(dfpos$Group.1 != "Control",1,NA))
dfpos$RecvsDef <- factor(dfpos$RecvsDef, levels = c(0,1), labels = c("Rec", "Def"))
dfpos$Sourcetype <- ifelse((dfpos$Group.1 == "DefNos" | dfpos$Group.1 == "RecNos"), 0, 
                           ifelse((dfpos$Group.1 == "DefNap" | dfpos$Group.1 == "RecNap"), 1, 
                                  ifelse((dfpos$Group.1 == "DefKno" | dfpos$Group.1 == "RecKno"), 2,
                                         ifelse((dfpos$Group.1 == "DefPol" | dfpos$Group.1 == "RecPol"), 3,
                                                ifelse((dfpos$Group.1 == "DefPar" | dfpos$Group.1 == "RecPar"), 4, NA)))))
dfpos$Sourcetype <- as.factor(as.character(dfpos$Sourcetype))
dfpos$Sourcetype <- factor(dfpos$Sourcetype, levels = c(0,1,2,3,4), labels = c("NoSource", "NameAndPicture", "Knowledgeable", "Political", "Partisan"))


ggplot(data = subset(dfpos, dfpos$Group.1 =="Control"| dfpos$Sourcetype != "NameAndPicture" & dfpos$Sourcetype != "Partisan"), aes(Sourcetype, x/n*100, fill=RecvsDef)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "#999999", width=.2, position=position_dodge(.9)) +
  scale_y_continuous(breaks = c(0, 10, 20,30,40,50,60,70,80,90,100)) +
  labs(title='Figure 1: Percentage donating', x = "Treatment", y='Contributed (in %)')+
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(angle=45, vjust=.5, size=10))


# same as above for mean donations
ggplot(data = subset(df, df$treatment == "Control" | df$Sourcetype != "NameAndPicture" & df$Sourcetype != "Partisan"), aes(Sourcetype, Donation, fill=RecvsDef)) +
#  stat_boxplot(geom ='errorbar', width = 0.5) +
  geom_boxplot()+
#  geom_bar(position = position_dodge(), stat = "identity") +
#  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "#999999", width=.2, position=position_dodge(.9)) +
  scale_y_continuous(breaks = c(0, 1, 2,3,4,5,6,7)) +
  labs(title='Figure 1: Percentage donating', x = "Treatment", y='Contribution')+
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(angle=45, vjust=.5, size=10))

# Boxplots by treaments ----
ggplot(data = df, aes(x = treatment, y = Donation)) +
 # stat_boxplot(geom ='errorbar', width = 0.5) +
  #geom_boxplot() +
  stat_summary(fun.y = mean, colour="darkred", geom="point", shape=18, size=3) +
  geom_jitter(shape=1)+
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8)) +
  labs(x = "Experimental group", y='Contribution [in €]') +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, vjust=.5, size=10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

describeBy(df$Donation, df$treatment)

# Is there a default-effect (aggregated over all default-treatments) ----
df$ConvsRest <- ifelse(df$treatment == "Control", 0, 1)
wilcox.test(df$Donation ~ df$ConvsRest) # n.s.
pairwise.wilcox.test(df$Donation, df$treatment, p.adjust.method = "none", exact = FALSE)
pairwise.wilcox.test(df$Dist, df$treatment, p.adjust.method = "none", exact = FALSE)
chisq.test(df$Donation, df$treatment) # n.s.
glm <- (glm(Donated ~ treatment, df, family = "binomial")) # RecPol/DefPol sig
coeftest(glm, vcov = vcovHC(glm, "HC1")) # RecPol/DefPol sig
glm <- (glm(Donated ~ RecvsDefD*Sourcetype, df, family = "binomial")) # n.s.
coeftest(glm, vcov = vcovHC(glm, "HC1")) # n.s.

# Are there treatment effects for those that believed we cooperated with Julia Verlinden? ----
describeBy(dfbel$Donation, dfbel$treatment)
kruskal.test(dfbel$Donation ~ dfbel$treatment) # n.s.

summary(lm(Donation ~ RecvsDefD*SourcetypeD*believe2, df)) 
summary(lm(Donation ~ RecvsDefD*SourcetypeD, dfbel)) 

## Boxplots by treatment
ggplot(data = dfbel, aes(x = treatment, y = Donation)) +
  stat_boxplot(geom ='errorbar', width = 0.5) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour="darkred", geom="point", shape=18, size=3) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8)) +
  labs(x = "Experimental group", y='Contribution [in €]') +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, vjust=.5, size=10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

# Pairwise Wilcox-tests ----
# for whole dataset
pairwise.wilcox.test(df$Donation, df$treatment, p.adjust.method = "none", exact = FALSE)
pairwise.wilcox.test(df$Dist, df$treatment, p.adjust.method = "none", exact = FALSE)

# for those that believe we cooperated with Julia Verlinden
pairwise.wilcox.test(dfbel$Donation, dfbel$treatment, p.adjust.method = "none", exact = FALSE)
pairwise.wilcox.test(dfbel$Dist, dfbel$treatment, p.adjust.method = "none", exact = FALSE)

# Proximity of 2 and 5 ----
length(df$Donation[df$Donation <= 3 & df$Donation >= 1])
length(df$Donation[df$Donation <= 6 & df$Donation >= 4])
length(df$Donation[df$Donation <= 5 & df$Donation >= 4])

# excluding all donations >2 
dfred <- df[df$Donation <= 2,]
describeBy(dfred$Donation, dfred$treatment)

# Regression analyses and tables ----
## With treatment variable
lm1 <- (lm(Dist ~ treatment, df))
lm2 <- lm(Donation ~ treatment, df)
lm3 <- lm(Donationno0 ~ treatment, df) # wrong, needs truncreg, currently implemented in Stata
lm4 <- lm(Distno5 ~ treatment, df) # wrong, needs truncreg, currently implemented in Stata
glm1 <- glm(Donated[df$treatment != "Control"] ~ treatment[df$treatment != "Control"] , df, family = "binomial")
glm2 <- glm(Default ~ treatment, df, family = "binomial")

#lm1 <- coeftest(lm1, vcov = vcovHC(lm1, "HC1"))
#lm2 <- coeftest(lm2, vcov = vcovHC(lm2, "HC1"))
#lm3 <- coeftest(lm3, vcov = vcovHC(lm3, "HC1"))
#lm4 <- coeftest(lm4, vcov = vcovHC(lm4, "HC1"))
#glm1 <- coeftest(glm1, vcov = vcovHC(glm1, "HC1"))
#glm2 <- coeftest(glm2, vcov = vcovHC(glm2, "HC1"))

stargazer(glm1, lm3, lm2, lm4, lm1, type = "html", style = "aer",
          se = makerobustseslist(glm1, lm3, lm2, lm4, lm1, type = "HC1"),
          p = makerobustpslist(glm1, lm3, lm2, lm4, lm1, type = "HC1"),
          covariate.labels = c("RecNos", "DefNos", "RecNap", "DefNap", "RecPol", "DefPol",
                               "RecPar", "DefPar", "RecKno", "DefKno"))

## With interaction term instead of treatment 
lm1 <- (lm(Dist ~ RecvsDefD*Sourcetype, df))
lm2 <- lm(Donation ~ RecvsDefD*Sourcetype, df)
lm3 <- lm(Donationno0 ~ RecvsDefD*Sourcetype, df)
lm4 <- lm(Distno5 ~ RecvsDefD*Sourcetype, df)
glm1 <- glm(Donated ~ RecvsDefD*Sourcetype , df, family = "binomial")
glm2 <- glm(Default ~ RecvsDefD*Sourcetype, df, family = "binomial")

#lm1 <- coeftest(lm1, vcov = vcovHC(lm1, "HC1"))
#lm2 <- coeftest(lm2, vcov = vcovHC(lm2, "HC1"))
#lm3 <- coeftest(lm3, vcov = vcovHC(lm3, "HC1"))
#lm4 <- coeftest(lm4, vcov = vcovHC(lm4, "HC1"))
#glm1 <- coeftest(glm1, vcov = vcovHC(glm1, "HC1"))
#glm2 <- coeftest(glm2, vcov = vcovHC(glm2, "HC1"))

stargazer(glm1, lm3, lm2, lm4, lm1, type = "html", style = "aer",
          se = makerobustseslist(glm1, lm3, lm2, lm4, lm1, type = "HC1"),
          p = makerobustpslist(glm1, lm3, lm2, lm4, lm1, type = "HC1"),
          covariate.labels = c("Default", "NameAndPicture", "Knowledgeable", "Political",
                               "Partisan", "Default x NAP", "Default x KNO", "Default x POL",
                               "Default x PAR"))

## With NosvsSome instead of Sourcetype 
lm1 <- (lm(Dist ~ RecvsDefD*NosvsSomeD, df))
lm2 <- lm(Donation ~ RecvsDefD*NosvsSomeD, df)
lm3 <- lm(Donationno0 ~ RecvsDefD*NosvsSomeD, df)
lm4 <- lm(Distno5 ~ RecvsDefD*NosvsSomeD, df)
glm1 <- glm(Donated ~ RecvsDefD*NosvsSomeD , df, family = "binomial")
glm2 <- glm(Default ~ RecvsDefD*NosvsSomeD, df, family = "binomial")

#lm1 <- coeftest(lm1, vcov = vcovHC(lm1, "HC1"))
#lm2 <- coeftest(lm2, vcov = vcovHC(lm2, "HC1"))
#lm3 <- coeftest(lm3, vcov = vcovHC(lm3, "HC1"))
#lm4 <- coeftest(lm4, vcov = vcovHC(lm4, "HC1"))
#glm1 <- coeftest(glm1, vcov = vcovHC(glm1, "HC1"))
#glm2 <- coeftest(glm2, vcov = vcovHC(glm2, "HC1"))

stargazer(glm1, lm3, lm2, lm4, lm1, type = "html", style = "aer",
          se = makerobustseslist(glm1, lm3, lm2, lm4, lm1, type = "HC1"),
          p = makerobustpslist(glm1, lm3, lm2, lm4, lm1, type = "HC1"),
          covariate.labels = c("Default", "Some Source", "Def x SS"))


# Epps-Singleton & Kolmogorov-Smirnov tests of entire distributions ----
ggplot(data = df, aes(x = Donation)) +
  facet_wrap(~treatment) +
  geom_histogram( binwidth = 0.1)
ks.test(df$Donation[df$treatment == "DefNos"], df$Donation[df$treatment == "DefPol"])


df$Don2 <- ifelse(df$Donation == 2, 1, 0)
chisq.test(df$Don2, df$treatment)
table(df$Don2, df$treatment)


chisq.test(table(df$Donated[df$treatment == "DefPol" | df$treatment == "DefKno" | df$treatment == "RecPol" | df$treatment == "RecKno"], df$treatment[df$treatment == "DefPol" | df$treatment == "DefKno"]))
dfff <- df[df$treatment == "DefPol" | df$treatment == "DefKno" | df$treatment == "RecPol" | df$treatment == "RecKno",]
chisq.test(dfff$Donated, dfff$treatment)

summary(glm(Donated ~ relevel(RecvsDefD, "Def") * relevel(Sourcetype, "Knowledgeable"), dfff, family = "binomial"))
