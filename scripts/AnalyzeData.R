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

##### Two-way ANOVA? #####
# define factor variables for two-way anova
df$Source <- ifelse(df$Nos == 1, 0, ifelse(df$Nap == 1, 1, ifelse(df$Pol == 1, 2, ifelse(df$Par == 1, 3, ifelse(df$Kno == 1, 4, NA)))))
df$Source <- as.factor(as.character(df$Source))
df$Source <- factor(df$Source, levels = c(0,1,2,3,4), labels = c("Nos", "Nap", "Pol", "Par", "Kno"))
df$Intervention <- ifelse((df$treatment == "RecNos" | df$treatment == "RecNap" | df$treatment == "RecPol" | df$treatment == "RecPar" | df$treatment == "RecKno"), 0, ifelse((df$treatment == "DefNos" | df$treatment == "DefNap" | df$treatment == "DefPol" | df$treatment == "DefPar" | df$treatment == "DefKno"), 1, NA))
aov <- aov(df$belief ~ df$Intervention*df$Source)
summary(aov)


table(df$Donated, df$treatment)
chisq.test(table(df$Donated, df$treatment))

##### Sum of Donations in respective session ####
sum(df$Donation[df$Date.x == "160412_1019"])
sum(df$Donation[df$Date.x == "160412_1144"])
sum(df$Donation[df$Date.x == "160412_1333"])
sum(df$Donation[df$Date.x == "160412_1510"])

sum(df$Donation[df$Date.x == "160414_1035"])
sum(df$Donation[df$Date.x == "160414_1213"])
sum(df$Donation[df$Date.x == "160414_1333"])
sum(df$Donation[df$Date.x == "160414_1515"])

sum(df$Donation[df$Date.x == "160419_1040"])
sum(df$Donation[df$Date.x == "160419_1215"])
sum(df$Donation[df$Date.x == "160419_1342"])

sum(df$Donation[df$Date.x == "160426_1017" |df$Date.x == "160426_1016"])
sum(df$Donation[df$Date.x == "160426_1157" | df$Date.x == "160426_1158"])





##### Descriptive statistics ####
summary(df$Donation)
sd(df$Donation)

summary(df$Default) # 1 if Default chosen, 0 otherwise
summary(df$Donated) # 1 if Donation > 0, 0 otherwise
summary(df$belief) 

##### Descriptive statistics by treatment ####
# Some important descriptive statistics for each treatment
describeBy(df$Donation, df$treatment) 
describeBy(df$Donated, df$treatment)
describeBy(df$belief, df$treatment)
describeBy(df$Diff, df$treatment)

# Gives out the most frequent values for the variable (mode; Modus; Modalwert).
# Note however, that this treats variable not as numeric, but factor
mfv(df$Donation) # 0
mfv(df$Default) # 0
mfv(df$Donated) # 1
mfv(df$belief) # 2

##### Distribution of Donation variable ####
ggplot(df, aes(x = Donation)) +
geom_histogram() + 
stat_bin(binwidth = .1) +
  ggtitle("Frequency of Donation values") +
  xlab("Donation amount") +
  ylab("Frequency")

##### Distribution of Donation variable by treatment ####
df.n <- data.frame(c(1:11), c(1:11))
colnames(df.n)[1] <- "treatment"
colnames(df.n)[2] <- "V1"
df.n$V1[1] <- paste0("n==", length(df$treatment[df$treatment == "Control"]))
df.n$V1[2] <- paste0("n==", length(df$treatment[df$treatment == "RecNos"]))
df.n$V1[3] <- paste0("n==", length(df$treatment[df$treatment == "DefNos"]))
df.n$V1[4] <- paste0("n==", length(df$treatment[df$treatment == "RecNap"]))
df.n$V1[5] <- paste0("n==", length(df$treatment[df$treatment == "DefNap"]))
df.n$V1[6] <- paste0("n==", length(df$treatment[df$treatment == "RecPol"]))
df.n$V1[7] <- paste0("n==", length(df$treatment[df$treatment == "DefPol"]))
df.n$V1[8] <- paste0("n==", length(df$treatment[df$treatment == "RecPar"]))
df.n$V1[9] <- paste0("n==", length(df$treatment[df$treatment == "DefPar"]))
df.n$V1[10] <- paste0("n==", length(df$treatment[df$treatment == "RecKno"]))
df.n$V1[11] <- paste0("n==", length(df$treatment[df$treatment == "DefKno"]))
df.n$treatment <- as.factor(as.character(df.n$treatment))
df.n$treatment <- factor(df.n$treatment, levels = c(1,2,3,4,5,6,7,8,9,10,11), labels=c("Control", "RecNos", "DefNos", "RecNap", "DefNap", "RecPol", "DefPol", "RecPar", "DefPar", "RecKno", "DefKno")) 

qplot(data=df, x=Donation, color=treatment, facets = ~ treatment) +
  stat_bin(binwidth = .1) +
  geom_text(data=df.n, aes(x=5, y = 10, label=V1), parse=TRUE) +
  ggtitle("Frequency of Donation values by treatment") +
  xlab("Donation amount") +
  ylab("Frequency")

qplot(data=df, x = Reactance, y = Donation, color = treatment, facets = ~treatment) +
  geom_point() +
  geom_text(data=df.n, aes(x=5, y=10, label=V1), parse=TRUE) +
  geom_smooth()

qplot(data=df, x = EAI, y = Donation, color = treatment, facets = ~treatment) +
  geom_point() +
  geom_text(data=df.n, aes(x=5, y=10, label=V1), parse=TRUE) +
  geom_smooth()


##### Coss Tabulation ####
table(df$warmGlow, df$guilt)
CrossTable(df$warmGlow, df$guilt, format = "SPSS")

##### Correlation ####
cor(df$Donation, df$belief, method = "spearman")
cor(df$Donation, df$Reactance, method = "spearman")
cor(df$Donation, df$EAI, method = "spearman")


#### Box Plots of Donations and beliefs by treatment ####
ggplot(df, aes(treatment, Donation)) +
  geom_boxplot()

ggplot(df, aes(treatment, belief)) +
  geom_boxplot()


#### Distribution of Reactance score ####
ggplot(df, aes(Reactance)) +
  geom_histogram() +
  stat_bin(binwidth = 1)

##### Dot Plot of Donations by treatment #### (http://www.statmethods.net/graphs/dot.html)
x <- df[order(df$Donation),] # sort by Donation
dotchart(x$Donation, labels=row.names(x), cex = .7, groups = x$treatment,
         main="Donation of Subjects\ngrouped by treatment",
         xlab="Donation in Euro", gcolor="black", color="black")
rm(x)

#### Dot Plot of Donations by Reactance score ####
x <- df[order(df$Donation),] # sort by donation
dotchart(x$Donation, labels=row.names(x), cex = .7, groups = x$Reactance,
         main="Donation of Subjects\ngrouped by Reactance",
         xlab="Donation in Euro", gcolor="black", color="black")
rm(x)

#### (Randomization) checks of questionnaire data ####
## probably dependent on treatment ##
describeBy(df$belief, df$treatment) # Beliefs are endogenous, highly correlated with donations
kruskal.test(df$belief ~ df$treatment) # significant
table(df$sourceInterest, df$treatment)
table(df$sourceKnow, df$treatment)
table(df$demandEffect, df$treatment)
describeBy(df$payforInf, df$treatment)
kruskal.test(df$payforInf ~ df$treatment)
table(df$effPercept, df$treatment)
table(df$believe2, df$treatment) # think about whether I should exclude those who said "no"

## probably independent of treatment ###
table(df$knowSource, df$treatment)
table(df$pastRetire, df$treatment)
table(df$pastOffset, df$treatment)
table(df$party, df$treatment)
table(df$persKnow, df$treatment)
table(df$persInt, df$treatment)
table(df$warmGlow, df$treatment)

table(df$guilt, df$treatment)
table(df$conformity, df$treatment)
table(df$retireEffic, df$treatment)
table(df$moral, df$treatment)
table(df$trustPol, df$treatment)
describeBy(df$Reactance, df$treatment) # numeric or factor?
kruskal.test(df$Reactance ~ df$treatment)
describeBy(df$EAI, df$treatment) # numeric or factor?
kruskal.test(df$EAI ~ df$treatment)
describeBy(df$age, df$treatment)
table(df$gender, df$treatment)
table(df$faculty, df$treatment)
table(df$degree, df$treatment)
describeBy(df$income, df$treatment)
kruskal.test(df$income ~ df$treatment)

##### Kruskal-Wallis Test ####
# The Kruskal Wallis test is used when you have one independent variable with two or more levels and 
# an ordinal dependent variable. In other words, it is the non-parametric version of ANOVA and a generalized 
# form of the Mann-Whitney test method since it permits 2 or more groups.
kruskal.test(df$Donation, df$treatment) # non-significant
summary(aov(df$Donation ~ df$treatment)) # one-way ANOVA, non-significant

# kruskal.test(df$Diff, df$treatment) # Diff = Difference between belief and Donation

## exclude those that did not believe we collaborated with Julia
df$believe2 <- as.character(df$believe2)
dfallnos <- df[(df$believe2 == "Nein" & !df$Nos == 1 & !is.na(df$believe2)),]
dfallyes <- df[!(df$ID %in% dfallnos$ID),]
kruskal.test(dfallyes$Donation, dfallyes$treatment) # no significant results (Even higher p-value)
chisq.test(table(df$believe2, df$treatment))

tab <- table(df$believe2, df$treatment)
tab <- cbind(tab, Total = rowSums(tab))
tab

##### F-Test for homogeneity of variance ####
var.test(df$Donation ~ df$RecvsDef)

##### OLS-Regression ####
## Some tests (possibly wrong)
summary(lm(data = df, formula = Donation ~ treatment))


##### Logistic-Regression ####
summary(glm(data = df, formula = Default ~ treatment))

summary(glm(data = df, formula = Donated ~ treatment))

##### Hurdle-Model ?####



##### Operationalized hypotheses from Working Paper ####
## H0a:
# Mean and median payments to retire carbon licenses in the control condition are close to zero. 
# We justify this prediction based on evidence that people seem to prefer individual-related caus-es, to 
# environmental ones (Forbes, 2015), most givers in dictator games give zero (Engel, 2011), and median 
# willingness-to-pay for carbon license retirement is close to zero (L?schel et al., 2013; Diederich and Goeschl, 2014).
describeBy(df$Donation, df$treatment)
t.test(df$Donation, mu = 0)
wilcox.test(df$Donation, mu = 0)

## H0b:
# The share of subjects whose payments correspond to the recommended, respectively defaulted payment-value 
# (convergence) is higher than in the control condition. Additionally, we expect that the share of subjects 
# converging to the default is higher than the share converging to the recommendation. This is primarily because 
# of the slight increase in effort needed to deviate from the default, relative to the recommendation.

# default > control
describeBy(df$Donation, df$DefvsC)
t.test(df$Donation ~ df$DefvsC)
wilcox.test(df$Donation ~ df$DefvsC)

# recommendation > control
describeBy(df$Donation, df$RecvsC)
t.test(df$Donation ~ df$RecvsC)
wilcox.test(df$Donation ~ df$RecvsC)

# default > recommendation
describeBy(df$Donation, df$RecvsDef)
t.test(df$Donation ~ df$RecvsDef)
wilcox.test(df$Donation ~ df$RecvsDef)

## H0c:
# The share of subjects converging to the recommended, respectively defaulted payment-values in the name and 
# picture condition is higher than in the neutral source-condition.

# DefNap > DefNos
describeBy(df$Donation, df$DefNapvsDefNos)
t.test(df$Donation ~ df$DefNapvsDefNos)
wilcox.test(df$Donation ~ df$DefNapvsDefNos)

# RecNap > RecNos
describeBy(df$Donation, df$RecNapvsRecNos)
t.test(df$Donation ~ df$RecNapvsRecNos)
wilcox.test(df$Donation ~ df$RecNapvsRecNos)
?t.test
## H1:
# A subject's reaction towards the respective intervention depends on trait reactance. 
# a.	A subject that scores high on trait reactance is less likely to converge to the recommended and defaulted 
# payment-values, than a subject scoring low on trait reactance.
summary(lm(data = df, formula = Donation ~ RecvsC + RecvsC*Reactance)) # unsure!!! # NOT CAUSAL
summary(lm(data = df, formula = Donation ~ DefvsC + DefvsC*Reactance)) # unsure!!! # NOT CAUSAL

# b.	A subject that scores high on trait reactance is less likely to converge to the defaulted than to the 
# recommended payment-value.
summary(lm(data = df, formula = Donation ~ RecvsDef + RecvsDef*Reactance))  # unsure!!! # NOT CAUSAL

## H2:
# The share of subjects converging to the recommended, respectively defaulted payment-values in the condition 
# informing about the academic degree of the source is higher than in the name and picture condition. This is 
# because subjects prefer complying with interventions by sources they perceive as competent.
describeBy(df$Donation, df$RecNapvsRecKno)
t.test(df$Donation ~ df$RecNapvsRecKno)
wilcox.test(df$Donation ~ df$RecNapvsRecKno)

describeBy(df$Donation, df$DefNapvsDefKno)
t.test(df$Donation ~ df$DefNapvsDefKno)
wilcox.test(df$Donation ~ df$DefNapvsDefKno)

## H3: 
# The share of subjects converging to the recommended, respectively defaulted payment-values in the condition 
# informing about the political characteristic of the source is lower than in the name and picture condition. 
describeBy(df$Donation, df$RecNapvsRecPol)
t.test(df$Donation ~ df$RecNapvsRecPol)
wilcox.test(df$Donation ~ df$RecNapvsRecPol)

describeBy(df$Donation, df$DefNapvsDefPol)
t.test(df$Donation ~ df$DefNapvsDefPol)
wilcox.test(df$Donation ~ df$DefNapvsDefPol)

# Additionally, when the source is political the share of subjects converging to the default is lower than the 
# share of subjects converging to the recommendation. 
describeBy(df$Donation, df$RecPolvsDefPol)
t.test(df$Donation ~ df$RecPolvsDefPol)
wilcox.test(df$Donation ~ df$RecPolvsDefPol)

#NOT RIGHT!!! a.	A subject that scores high on trust in politics is more likely to converge to the recommended and defaulted 
# payment-values, than a subject scoring low on trust in politics. // given Source is POL or PAR
qplot(data=df, x = trustPolD, y = Donation) +
  geom_point()+
  geom_smooth(method=lm)

qplot(data=df, x = trustPolD, y = Donation, color = treatment, facets = ~treatment) +
  geom_point() +
  geom_text(data=df.n, aes(x=1, y=10, label=V1), parse=TRUE)+
  geom_smooth(method=lm)

# b.	A subject that values conformity, i.e. doing what the majority does, is more likely to converge to the 
# recommended and defaulted payment values, than a subject that does not value conformity.
qplot(data=df, x = conformityD, y = Donation, color = treatment, facets = ~RecPolvsDefPol) +
  geom_point() +
  geom_text(data=df.n, aes(x=1, y=10, label=V1), parse=TRUE)+
  geom_smooth(method=lm)


## H4:
# The share of subjects converging to the recommended, respectively defaulted payment-values, relative to the 
# politicalcharacteristic condition, is higher for subjects with same party preferences, and lower for subjects 
# with different party preferences.

## So, if trustPol is independent of treatment, I could use it as predictor without treatment.
# otherwise as interaction in regression model. However, there is NO CAUSAL RELATIONSHIP. I did realize that to late



##### Sample size calculation formulas #####
library(pwr)
(sigma / delta)^2 * (zAlpha - zBeta)^2
pwr.norm.test(d=effectSize, sig.level=0.05, power=0.80, alternative="greater")
pwr.t.test(d=effectSize, sig.level=0.05, power=0.80, type="one.sample", alternative = "greater")
?power.anova.test
summary(aov(df$Donation ~ df$treatment))

dat$groupmean <- aggregate(df$Donation, list(df$treatment), mean)
meanvect <- dat$groupmean[[2]]
withinvar <- aggregate(df$Donation, list(df$treatment), var) #ranges from 1.25 to 4.34
meanwithinvar <- mean(withinvar[[2]]) # 2.466682
power.anova.test(groups = length(meanvect), between.var = var(meanvect), within.var = 2.466682, power = .8, sig.level = .05)
# n = 51 (assuming variance homogeneity)



# http://www.statmethods.net/stats/power.html
# pooled sd: sqrt((1.16^2 + 2.04^2)/2) (square root of the average of the sds)
pwr.t.test(d = (1.95 - 1.18)/2.466682, sig.level = .05, power = .8, type = "two.sample", alternative = "two.sided")
# n = 163
pwr.t.test(d = (1.95 - 1.18)/(sqrt((27*1.16^2 + 25*2.04^2)/(28+26-2))), sig.level = .05, power = .8, type = "two.sample", alternative = "two.sided")
# (sqrt((27*1.16^2 + 25*2.04^2)/(28+26-2))) is the pooled sample standard deviation
# n = 73
var.test(df$Donation ~ df$DefParvsC) # no variance homogeneity (H0 rejected)
# Attention: delta = |(mu1 - mu2)/sigma|, Cohen's d: .2, .5, .8 are small, medium, large
# In my case: biggest difference in means (Control vs. DefPar) in my data =.77, 
# mean sd of both groups: 1.6 --> Cohen's d: .48 (medium)
pwr.t.test(d = (0.94 - 1.49)/(sqrt((26*1.71^2 + 24*1.12^2)/(27+25-2))), sig.level = .05, power = .8, type = "two.sample", alternative = "two.sided")



a <- aggregate(df$Donation, list(df$treatment), length ) #number of observations in each treatment
dat <- a$x/313 # pi
dat <- as.data.frame(dat)
colnames(dat)[1] <- "pi"
dat$grandmean <- mean(df$Donation)
dat$groupmean <- aggregate(df$Donation, list(df$treatment), mean)
dat$withinvar <- aggregate(df$Donation, list(df$treatment), var) # here problem because of 
# assumption of variance homogeneity


sum(sqrt((dat$pi * (dat$groupmean[[2]] - dat$grandmean)^2)/dat$withinvar[[2]]))

da <- dat[1:2,]
sum(sqrt((da$pi * (da$groupmean[[2]] - da$grandmean)^2)/da$withinvar[[2]]))



##### Source Knowledge beliefs per treatment ####
table(df$sourceKnow, df$treatment, useNA = "ifany")
chisq.test(table(df$sourceKnow, df$treatment, useNA = "ifany" ))
kruskal.test(df$sourceKnow ~ df$treatment, na.action = "na.omit")



# Compare new and old sessions for respective treatments
table(df$Session, df$treatment)
describeBy(df$Donation, df$Session)
describeBy(df$Dist, df$Session)

ggplot(data = df, aes(x=Session, y= Donation)) +
  geom_boxplot() +
  geom_jitter(shape=1)

wilcox.test(df$Donation ~ df$Session) # < .01
wilcox.test(df$Dist ~ df$Session) # < .01

chisq.test(table(df$Donated[df$RecvsDefD == "Def"], df$Session[df$RecvsDefD == "Def"]))
chisq.test(table(df$Donated[df$RecvsDefD == "Rec"], df$Session[df$RecvsDefD == "Rec"]))
