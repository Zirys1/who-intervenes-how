# Stats and Graphs for Presentation
## Descriptive statistics that give mean, sd's and tests if the respective important variables
# (used in the regression models, at least) are randomly distributed among treatment groups

# To work towards why the interaction of treatment and Reactance might be interesting ------
ggplot(df, aes(x = RecvsDefD, y = Dist))+
  facet_grid(~Sourcetype)+
  geom_boxplot() +
  geom_jitter(shape=1)+
  theme(axis.text.x = element_text(angle = 45, vjust=.5, size=10)) +
  stat_summary(fun.y = mean, colour="blue", geom="point", shape=18, size=3) 

kruskal.test(df$Dist, df$treatment) # no treatment differences
lm <- (lm(Dist ~ RecvsDefD * Sourcetype, df))
coeftest(lm, vcov = vcovHC(lm, "HC1")) # no treatment effects


## Does Reactance predict Distance to 5? ------
ggplot(df, aes(x = Reactance, y = Dist)) +
  geom_jitter(shape=1) +
  geom_smooth(method = "lm")

## Does Distance and REactance interact differently for Interventiontype factor? -------
visreg(m, "Reactance", by="RecvsDefD", overlay = F) # suggests NO difference
# ATTENTION, THIS SHOWS RELATIONSHIP BETWEEN OBSERVED REACTANCE AND FITTED DISTANCE IN MODEL
ggplot(data = df, aes(x = Reactance, y = Dist)) +
  facet_grid(~RecvsDefD) +
  geom_point(shape=1) +
  geom_smooth(method= "lm")
# ATTENTION, THIS SHOWS RELATIONSHIP BETWEEN OBSERVED REACTANCE AND OBSERVED DISTANCE (NO MODEL)


## Does Distance and Reactance interact differently for Sourcetype factor? --------
visreg(m, "Reactance", by="NosvsSomeD", overlay = F)
# ATTENTION, THIS SHOWS RELATIONSHIP BETWEEN OBSERVED REACTANCE AND FITTED DISTANCE IN MODEL
ggplot(data = df, aes(x = Reactance, y = Dist)) +
  facet_grid(~NosvsSomeD) +
  geom_point(shape=1) +
  geom_smooth(method= "lm")
# ATTENTION, THIS SHOWS RELATIONSHIP BETWEEN OBSERVED REACTANCE AND OBSERVED DISTANCE (NO MODEL)

# how can I link the previous and the following step? Would this only make sense if Reactance
# is not randomly distributed among treatments or do I just justify this with the hypothesis
# that reactance and treatment interact?

## Does Reactance interact with treatment on Distance to 5? ------------
ggplot(df, aes(x = Reactance, y = Dist))+
  facet_grid(RecvsDefD~NosvsSomeD)+
  geom_jitter(shape = 1) +
  theme(axis.text.x = element_text(angle = 45, vjust=.5, size=10)) +
  geom_smooth(method = "lm")

lm <- (lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance, df))
coeftest(lm, vcov = vcovHC(lm, "HC1"))

ggplot(df, aes(x = Reactance, y = Dist))+
  facet_grid(RecvsDefD~NosvsSomeD)+
  geom_jitter(shape = 1) +
  theme(axis.text.x = element_text(angle = 45, vjust=.5, size=10)) +
  geom_smooth(method = "lm")

anova(lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance, df))

## Yes, it does. So how exactly does it interact?
### Fitted values plot

### Marginal effects plot ---------


## To investigate Simple main effects ---------
### divide data into 11 different sub-datasets along the treatment variable
### run Reactance on Dist

df1 <- subset(df, treatment == "Control")
df2 <- subset(df, treatment == "RecNos")
df3 <- subset(df, treatment == "DefNos")
df4 <- subset(df, treatment == "RecNap")
df5 <- subset(df, treatment == "DefNap")
df6 <- subset(df, treatment == "RecPol")
df7 <- subset(df, treatment == "DefPol")
df8 <- subset(df, treatment == "RecPar")
df9 <- subset(df, treatment == "DefPar")
df10 <- subset(df, treatment == "RecKno")
df11 <- subset(df, treatment == "DefKno")

anova(lm(Dist ~ Reactance, df1))
summary(lm(Dist ~ Reactance, df1))
anova(lm(Dist ~ Reactance, df2)) #RecNos; sig 0.1
summary(lm(Dist ~ Reactance, df2)) #sig 0.1
anova(lm(Dist ~ Reactance, df3))
summary(lm(Dist ~ Reactance, df3))
anova(lm(Dist ~ Reactance, df4))
summary(lm(Dist ~ Reactance, df4))
anova(lm(Dist ~ Reactance, df5))
summary(lm(Dist ~ Reactance, df5))
anova(lm(Dist ~ Reactance, df6))
summary(lm(Dist ~ Reactance, df6))
anova(lm(Dist ~ Reactance, df7))
summary(lm(Dist ~ Reactance, df7))
anova(lm(Dist ~ Reactance, df8))
summary(lm(Dist ~ Reactance, df8))
anova(lm(Dist ~ Reactance, df9))
summary(lm(Dist ~ Reactance, df9))
anova(lm(Dist ~ Reactance, df10)) #RecKno; sig 0.1
summary(lm(Dist ~ Reactance, df10)) #sig 0.1
anova(lm(Dist ~ Reactance, df11))
summary(lm(Dist ~ Reactance, df11))

stargazer(df, type = "html", style = "aer", title = "Summary statistics")

# RecNos appears to be the main driver of the significant interaction and of course this is 
# dangerous because there are only 30 or so obesrvations in that group. It can be doubted that
# the negative correlation of Reactance and Distance to 5 in that group would also be present
# (and different from this positive correlation in the DefNos group) for more observations.
# This criticism is especially valid, because this relation of negative correlation of Reactance
# and Dist in case of Recommendation and the positive correlation in case of Default, is not 
# necessarily the same for other sourcetypes. Especially, data for "Some Source", suggests
# there is no correlation at all between Reactance and Dist, at all. Since "Some Source" has
# more data, this evidence is more reliable, but only if we assume that Sourcetype has no
# significant influence on the correlation between Reactance and Distance to 5. If we assume that
# sourcetype can significantly change how Reactance predicts the outcome variable (also in
# interaction with the intervention type), then this criticism is not valid.


# New variable to include Control in boxplots --------
df$SourcetypeC <- ifelse((df$treatment == "DefNos" | df$treatment == "RecNos"), 0, 
                        ifelse((df$treatment == "DefNap" | df$treatment == "RecNap"), 1, 
                               ifelse((df$treatment == "DefKno" | df$treatment == "RecKno"), 2,
                                      ifelse((df$treatment == "DefPol" | df$treatment == "RecPol"), 3,
                                             ifelse((df$treatment == "DefPar" | df$treatment == "RecPar"), 4, 5)))))
df$SourcetypeC <- as.factor(as.character(df$SourcetypeC))
df$SourcetypeC <- factor(df$SourcetypeC, levels = c(0,1,2,3,4,5), labels = c("NoSource", "NameAndPicture", "Knowledgeable", "Political", "Partisan", "Control"))

## Boxplot for Dist, factorial ----------
ggplot(data = df, aes(x = RecvsDef, y = Dist)) +
  facet_wrap(~SourcetypeC) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour="darkred", geom="point", shape=18, size=3) +
  labs(title='Figure 2: Distance to 5 for intervention type x source type', x = "Intervention type", y='Distance') +
  theme(plot.title = element_text(size = 10),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(angle = 45, vjust=.5, size=10))

## Fractions of people donating (bar graph, including SEs or Confidence intervals) ---------
m <- aggregate(df$Donatedm, list(df$treatment), sum)
m$n <- aggregate(df$Donatedm, list(df$treatment), length)[2]
m$Sourcetype <- ifelse((m$Group.1 == "DefNos" | m$Group.1 == "RecNos"), 0, 
                       ifelse((m$Group.1 == "DefNap" | m$Group.1 == "RecNap"), 1, 
                              ifelse((m$Group.1 == "DefKno" | m$Group.1 == "RecKno"), 2,
                                     ifelse((m$Group.1 == "DefPol" | m$Group.1 == "RecPol"), 3,
                                            ifelse((m$Group.1 == "DefPar" | m$Group.1 == "RecPar"), 4, 5)))))
m$Sourcetype <- as.factor(as.character(m$Sourcetype))
m$Sourcetype <- factor(m$Sourcetype, levels = c(0,1,2,3,4,5), labels = c("NoSource", "NameAndPicture", "Knowledgeable", "Political", "Partisan", "Control"))
m$RecvsDef <- ifelse((m$Group.1 == "RecNos" | m$Group.1 == "RecNap" | m$Group.1 == "RecKno" | m$Group.1 == "RecPol" | m$Group.1 == "RecPar"), 0,
                     ifelse((m$Group.1 == "DefNos" | m$Group.1 == "DefNap" | m$Group.1 == "DefKno" | m$Group.1 == "DefPol" | m$Group.1 == "DefPar"),1,2))
m$RecvsDef <- as.factor(as.character(m$RecvsDef))
m$RecvsDef <- factor(m$RecvsDef, levels = c(0,1,2), labels = c("Rec", "Def", "Control"))
m$NosvsSomeD <- ifelse((m$Sourcetype == "NoSource" & (m$RecvsDef == "Def" | m$RecvsDef == "Rec")), 0, ifelse((m$Sourcetype != "NoSource" & (m$RecvsDef == "Def" | m$RecvsDef == "Rec")), 1, NA))
m$NosvsSomeD <- factor(m$NosvsSomeD, levels = c(0,1), labels = c("No Source", "Some Source"))

m$sd <- aggregate(df$Donatedm, list(df$treatment), sd)[2]
m$se <- m$sd / sqrt(m$n)
m$lower <- (m$x / m$n - m$se)*100
m$upper <- (m$x / m$n + m$se)*100
m$up90 <- (m$x / m$n + 1.64 * m$se) * 100
m$low90 <- (m$x / m$n - 1.64 * m$se) * 100

ggplot(data = m, aes(RecvsDef, x/n*100)) +
  facet_wrap(~Sourcetype) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "#999999") +
  scale_y_continuous(breaks = c(0, 20,40,60,80)) +
  labs(title='Figure 1: Percentage donating for intervention type x source type', x = "Intervention type", y='Donated (in %)')+
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust=.5, size=10))

# RecvsDef & NosvsSome on ext. margin --------
m <- aggregate(df$Donatedm, list(df$RecvsDefD, df$NosvsSomeD), sum)
m$n <- aggregate(df$Donatedm, list(df$RecvsDefD, df$NosvsSomeD), length)[3]
m$Sourcetype <- ifelse((m$Group.1 == "DefNos" | m$Group.1 == "RecNos"), 0, 
                       ifelse((m$Group.1 == "DefNap" | m$Group.1 == "RecNap"), 1, 
                              ifelse((m$Group.1 == "DefKno" | m$Group.1 == "RecKno"), 2,
                                     ifelse((m$Group.1 == "DefPol" | m$Group.1 == "RecPol"), 3,
                                            ifelse((m$Group.1 == "DefPar" | m$Group.1 == "RecPar"), 4, 5)))))
m$Sourcetype <- as.factor(as.character(m$Sourcetype))
m$Sourcetype <- factor(m$Sourcetype, levels = c(0,1,2,3,4,5), labels = c("NoSource", "NameAndPicture", "Knowledgeable", "Political", "Partisan", "Control"))
m$RecvsDef <- ifelse((m$Group.1 == "RecNos" | m$Group.1 == "RecNap" | m$Group.1 == "RecKno" | m$Group.1 == "RecPol" | m$Group.1 == "RecPar"), 0,
                     ifelse((m$Group.1 == "DefNos" | m$Group.1 == "DefNap" | m$Group.1 == "DefKno" | m$Group.1 == "DefPol" | m$Group.1 == "DefPar"),1,2))
m$RecvsDef <- as.factor(as.character(m$RecvsDef))
m$RecvsDef <- factor(m$RecvsDef, levels = c(0,1,2), labels = c("Rec", "Def", "Control"))
m$NosvsSomeD <- ifelse((m$Sourcetype == "NoSource" & (m$RecvsDef == "Def" | m$RecvsDef == "Rec")), 0, ifelse((m$Sourcetype != "NoSource" & (m$RecvsDef == "Def" | m$RecvsDef == "Rec")), 1, NA))
m$NosvsSomeD <- factor(m$NosvsSomeD, levels = c(0,1), labels = c("No Source", "Some Source"))

m$sd <- aggregate(df$Donatedm, list(df$RecvsDefD, df$NosvsSomeD), sd)[3]
m$se <- m$sd / sqrt(m$n)
m$lower <- (m$x / m$n - m$se)*100
m$upper <- (m$x / m$n + m$se)*100

ggplot(data = m, aes(Group.2, x/n*100)) +
  facet_grid(~Group.1) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "#999999") +
  scale_y_continuous(breaks = c(0, 20,40,60,80)) +
  labs(title='Figure 2: Bar graph of percentage of subjects donating', x = "Intervention type", y='Donated')+
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust=.5, size=10))


## Um fragen für kurze Ergebnisbeschreibung (TheCompensators) zu beantworten ----------
### Extensive margin (TheCompensators) ----------
# Unterscheiden sich NoSource vs Some Source für Defaults, bzw. Rec voneinander?
chisq.test(df$Donated, df$NosvsSomeD) # < .05
chisq.test(df$Donated[df$RecvsDefD == "Def"], df$NosvsSomeD[df$RecvsDefD == "Def"]) # < .05
chisq.test(df$Donated[df$RecvsDefD == "Rec"], df$NosvsSomeD[df$RecvsDefD == "Rec"]) # n.s.

# Unterscheiden sich Rec Def Con voneinander?
chisq.test(df$Donated, df$RecvsDef) # n.s.

# Unterscheiden sich Rec vs Def für No Source bzw. Some Source voneinander?
chisq.test(df$Donated[df$NosvsSomeD == "No Source"], df$RecvsDefD[df$NosvsSomeD == "No Source"]) # n.s.
chisq.test(df$Donated[df$NosvsSomeD == "Some Source"], df$RecvsDefD[df$NosvsSomeD == "Some Source"]) # n.s.

# schauen ob ein Unterschied besteht wenn man Control als NoSource ansieht
df$NosvsSomeC <- ifelse((df$Sourcetype == "NoSource" & (df$Intervention == "Default" | df$Intervention == "Recommendation")), 0, ifelse((df$Sourcetype != "NoSource" & (df$Intervention == "Default" | df$Intervention == "Recommendation")), 1, 0))
df$NosvsSomeC <- factor(df$NosvsSomeC, levels = c(0,1), labels = c("No Source", "Some Source"))
chisq.test(df$Donated[df$NosvsSomeC == "No Source"], df$RecvsDef[df$NosvsSomeC == "No Source"]) # n.s.

# Is there a significant difference between Control and RecSomeS and DefSomeS (is that trivial?)
# interpret Control as Some Source
df$NosvsSomeF <- ifelse((df$Sourcetype == "NoSource" & (df$Intervention == "Default" | df$Intervention == "Recommendation")), 0, ifelse((df$Sourcetype != "NoSource" & (df$Intervention == "Default" | df$Intervention == "Recommendation")), 1, 1))
df$NosvsSomeF <- factor(df$NosvsSomeF, levels = c(0,1), labels = c("No Source", "Some Source"))

chisq.test(df$Donated[df$NosvsSomeF == "Some Source"], df$RecvsDef[df$NosvsSomeF == "Some Source"])

l <- aggregate(df$Donatedm, list(df$NosvsSomeC, df$RecvsDef), sum)
l$n <- aggregate(df$Donatedm, list(df$NosvsSomeC, df$RecvsDef), length)[3]
l$sd <- aggregate(df$Donatedm, list(df$NosvsSomeC, df$RecvsDef), sd)[3]
l$se <- l$sd / sqrt(l$n)
l$lower <- (l$x / l$n - l$se)*100
l$upper <- (l$x / l$n + l$se)*100
l$ratio <- l$x/l$n*100
ggplot(data = l, aes(Group.1, x/n*100)) +
  facet_grid(~Group.2) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "#999999") +
  scale_y_continuous(breaks = c(0, 20,40,60,80)) +
  labs(title='Figure 2: Bar graph of percentage of subjects donating', x = "Intervention type", y='Donated')+
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust=.5, size=10))


glm <- (glm(Donated ~ RecvsDefD * NosvsSome, df, family = "binomial")) # n.s.
coeftest(glm, vcov = vcovHC(glm, "HC1"))
glm <- (glm(Donated ~ relevel(RecvsDefD, "Def") * NosvsSome, df, family = "binomial")) # SomeS < .05
coeftest(glm, vcov = vcovHC(glm, "HC1"))

### Intensive margin (Donation, not Dist) (TheCompensators) ---------
# Unterscheiden sich NoSource vs Some Source für Defaults, bzw. Rec voneinander?
wilcox.test(df$Donation[df$RecvsDefD == "Def"] ~ df$NosvsSomeD[df$RecvsDefD == "Def"]) # n.s.
wilcox.test(df$Donation[df$RecvsDefD == "Rec"] ~ df$NosvsSomeD[df$RecvsDefD == "Rec"]) # n.s.
ggplot(df, aes(x = NosvsSome, y = Donation)) +
  facet_grid(~RecvsDefD) +
  geom_boxplot()

# Unterscheiden sich Rec Def Con voneinander?
wilcox.test(df$Donation ~ df$RecvsDefD) # n.s.
kruskal.test(df$Donation ~ df$RecvsDef) # n.s.
ggplot(df, aes(x = RecvsDef, y = Donation)) +
  geom_boxplot()

# Unterscheidet sich Donation zwischen NoS vs SomeS (vs Kontrollgruppe)
wilcox.test(df$Donation ~ df$NosvsSomeD) # < .1
ggplot(data = df, aes(x = Donation, color = NosvsSomeD)) +
  facet_grid(~NosvsSomeD) +
  geom_density()
  
ggplot(df, aes(x = NosvsSomeD, y = Donation)) +
  facet_grid(~RecvsDefD) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour="darkred", geom="point", shape=18, size=3)

kruskal.test(df$Donation ~ df$NosvsSome) # n.s. (?)

# Unterscheiden sich Rec vs Def für No Source bzw. Some Source voneinander?
wilcox.test(df$Donation[df$NosvsSomeD == "No Source"] ~ df$RecvsDefD[df$NosvsSomeD == "No Source"], exact = FALSE)
# n.s.
wilcox.test(df$Donation[df$NosvsSomeD == "Some Source"] ~ df$RecvsDefD[df$NosvsSomeD == "Some Source"])
# n.s.
ggplot(df, aes(x = RecvsDefD, y = Donation)) +
  facet_grid(~NosvsSome) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour="darkred", geom="point", shape=18, size=3)

lm <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance, df)
plot(lm)
df$Reactance

ggplot() +
  geom_histogram(aes(x = lm$residuals))

### Conditional intensive margin (Donation, not Dist) (TheCompensators) ------
ggplot(df, aes(x = Donationno0)) +
  geom_histogram(na.rm = TRUE, binwidth = .5) # looks more like a (right skewed) normal
# distribution than the non-censored Donation-variable, but still not very good
# maybe makes sense to prepare the variables as I did for the ordered regression

# Unterscheiden sich NoSource vs Some Source für Defaults, bzw. Rec voneinander?

wilcox.test(df$Donationno0[df$RecvsDefD == "Def"] ~ df$NosvsSomeD[df$RecvsDefD == "Def"]) # n.s.
wilcox.test(df$Donationno0[df$RecvsDefD == "Rec"] ~ df$NosvsSomeD[df$RecvsDefD == "Rec"]) # n.s.
ggplot(df, aes(x = NosvsSome, y = Donationno0)) +
  facet_grid(~RecvsDefD) +
  geom_boxplot()

# Unterscheiden sich Rec Def Con voneinander?
wilcox.test(df$Donationno0 ~ df$RecvsDefD) # n.s.
kruskal.test(df$Donationno0 ~ df$RecvsDef) # n.s.
ggplot(df, aes(x = RecvsDef, y = Donationno0)) +
  geom_boxplot(na.rm = T)

# Unterscheidet sich Donation zwischen NoS vs SomeS (vs Kontrollgruppe)
wilcox.test(df$Donationno0 ~ df$NosvsSomeD) # < n.s.
ggplot(df, aes(x = NosvsSome, y = Donationno0)) +
  geom_boxplot(na.rm = T)

kruskal.test(df$Donationno0 ~ df$NosvsSome) # n.s.
ggplot(df, aes(x = RecvsDef, y = Donationno0)) +
  geom_jitter(shape=1)

# Unterscheiden sich Rec vs Def für No Source bzw. Some Source voneinander?
wilcox.test(df$Donationno0[df$NosvsSomeD == "No Source"] ~ df$RecvsDefD[df$NosvsSomeD == "No Source"], exact = FALSE)
# n.s.
wilcox.test(df$Donationno0[df$NosvsSomeD == "Some Source"] ~ df$RecvsDefD[df$NosvsSomeD == "Some Source"])
# n.s.
ggplot(df, aes(x = RecvsDefD, y = Donationno0)) +
  facet_grid(~NosvsSome) +
  geom_boxplot(na.rm=T)
ggplot(df, aes(x = RecvsDefD, y = Donationno0)) +
  facet_grid(~NosvsSome) +
  geom_jitter(na.rm=T, shape = 1)

lm <- lm(Donationno0 ~ RecvsDefD * NosvsSomeD * Reactance, df)
coeftest(lm, vcov = vcovHC(lm, "HC1"))

ggplot(df, aes(x = Reactance, y = Donationno0)) +
  facet_grid(NosvsSomeD~RecvsDefD) +
  geom_jitter(na.rm=T, shape = 1) +
  geom_smooth(method="lm", na.rm=T) # Hier sieht man, dass die signifikante Interaktion in erster Linie
# durch die starke negative Interaktion von Reaktanz * Def * No Source herrührt (das ist auch
# bei Donation der Fall, wobei da die Regressionsgerade für Rec * No Source positiv ist und
# gleichzeitig weniger schwach negative bei Def * No Source)
summary(lm(Donation ~ RecvsDefD * NosvsSomeD * Reactance, df))

## Gruppentests für die conditionals (nicht-parametrisch) ----------

## Kruskal Wallis test on Distance ----
kruskal.test(df$Dist ~ df$treatment)
### Wilcoxon rank sum test on Distance aggregated over Sourcetype
wilcox.test(df$Dist ~ df$RecvsDefD)
### KW test on Distance aggregated over Intervention type
kruskal.test(df$Dist ~ df$Sourcetype)
## Chi² test on Donated
chisq.test(df$Donated, df$treatment)
chisq.test(table(df$Sourcetype, df$Donated))
### Chi² test on Donated aggregated over Sourcetype
chisq.test(df$Donated, df$RecvsDefD)
### Chi² test on Donated aggregated over Intervention type
chisq.test(df$Donated, df$Sourcetype) # < .1

#### Logistic regression on Donated aggregated over Intervention type ------------
summary(glm(Donated ~ Sourcetype, df, family = "binomial")) # Political at < .1, Partisan at < .05
glm <- (glm(Donated ~ relevel(Sourcetype, "Knowledgeable"), df, family = "binomial")) # Pol at < .1
coeftest(glm, vcov = vcovHC(glm, "HC1"))
glm <- (glm(Donated ~ relevel(Sourcetype, "Political"), df, family = "binomial")) # Pol at < .1, NoSource at < .01
coeftest(glm, vcov = vcovHC(glm, "HC1"))
glm <- (glm(Donated ~ relevel(Sourcetype, "Partisan"), df, family = "binomial")) # NoSource at < .05
coeftest(glm, vcov = vcovHC(glm, "HC1"))

#### Logistic regression on Donated with aggregated over Intervention type ---------
glm <- glm(Donated ~ NosvsSomeD, df, family = "binomial") # significant < .05
coeftest(glm, vcov = vcovHC(glm, "HC1"))



## Logistic regression on Donated (ATTENTION: INTERPRETATION OF INTERACTION EFFECTS IN NON-LINEAR REGRESSIONS) -------
summary(glm(Donated ~ treatment, df, family = "binomial"))
glm <- (glm(Donated ~ relevel(RecvsDefD, "Def") * relevel(Sourcetype, "NoSource"), df, family = "binomial")) # Pol/Par < .05
coeftest(glm, vcov = vcovHC(glm, "HC1"))
glm <- (glm(Donated ~ relevel(RecvsDefD, "Def") * relevel(Sourcetype, "Knowledgeable"), df, family = "binomial")) # n.s.
coeftest(glm, vcov = vcovHC(glm, "HC1"))
glm <- (glm(Donated ~ relevel(RecvsDefD, "Def") * relevel(Sourcetype, "Partisan"), df, family = "binomial")) # NoS < .05
coeftest(glm, vcov = vcovHC(glm, "HC1"))

# incorporates the control group ----------
glm <- (glm(Donated ~ relevel(df$treatment, "DefNos"), df, family = "binomial")) # RecPol < .05, DefPol < .05, DefPar < .05
coeftest(glm, vcov = vcovHC(glm, "HC1"))
glm <- (glm(Donated ~ relevel(df$treatment, "RecNos"), df, family = "binomial")) # n.s.
coeftest(glm, vcov = vcovHC(glm, "HC1"))
glm <- (glm(Donated ~ relevel(df$treatment, "DefNap"), df, family = "binomial")) # n.s.
coeftest(glm, vcov = vcovHC(glm, "HC1"))
glm <- (glm(Donated ~ relevel(df$treatment, "RecNap"), df, family = "binomial")) # n.s.
coeftest(glm, vcov = vcovHC(glm, "HC1"))
glm <- (glm(Donated ~ relevel(df$treatment, "DefKno"), df, family = "binomial")) # n.s.
coeftest(glm, vcov = vcovHC(glm, "HC1"))
glm <- (glm(Donated ~ relevel(df$treatment, "RecKno"), df, family = "binomial")) # n.s.
coeftest(glm, vcov = vcovHC(glm, "HC1"))
glm <- (glm(Donated ~ relevel(df$treatment, "DefPol"), df, family = "binomial")) # Control < .05, DefNos < .05
coeftest(glm, vcov = vcovHC(glm, "HC1"))
glm <- (glm(Donated ~ relevel(df$treatment, "RecPol"), df, family = "binomial")) # Control < .1, DefNos < .05
coeftest(glm, vcov = vcovHC(glm, "HC1"))
glm <- (glm(Donated ~ relevel(df$treatment, "DefPar"), df, family = "binomial")) # DefNos < .05
coeftest(glm, vcov = vcovHC(glm, "HC1"))
glm <- (glm(Donated ~ relevel(df$treatment, "RecPar"), df, family = "binomial")) # n.s.
coeftest(glm, vcov = vcovHC(glm, "HC1"))

# relevel regressions on Dist -----------
lm <- lm(Dist ~ RecvsDefD * Sourcetype, df) # n.s.
coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- lm(Dist ~ RecvsDefD * relevel(Sourcetype,"NameAndPicture"), df) # n.s.
coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- lm(Dist ~ RecvsDefD * relevel(Sourcetype,"Knowledgeable"), df) # Pol < .1
coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- lm(Dist ~ RecvsDefD * relevel(Sourcetype,"Political"), df) # Kno < .1
coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- lm(Dist ~ RecvsDefD * relevel(Sourcetype,"Partisan"), df) # n.s.
coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- lm(Dist ~ relevel(RecvsDefD, "Rec") * relevel(Sourcetype,"NameAndPicture"), df) # n.s.
coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- lm(Dist ~ relevel(RecvsDefD, "Rec") * relevel(Sourcetype,"Knowledgeable"), df) # Pol < .1
coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- lm(Dist ~ relevel(RecvsDefD, "Rec") * relevel(Sourcetype,"Political"), df) # Kno < .1
coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- lm(Dist ~ relevel(RecvsDefD, "Rec") * relevel(Sourcetype,"Partisan"), df)
coeftest(lm, vcov = vcovHC(lm, "HC1"))

# ordinal regression on DistOrd (because there the assumptions are not as strict as in OLS, not much
# but some information on the Dist variable ist lost, and it can deal with clumping at zero) -----
# assumption of normality on residuals very strict for ML
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - mean(df$Reactance)
summary(polr(DistOrd ~ treatment, df, method = "logistic", Hess = TRUE)) # n.s.
summary(polr(DistOrd ~ RecvsDefD * Sourcetype, df, method = "logistic", Hess = TRUE)) # n.s
olr <- (polr(DistOrd ~ RecvsDefD * Sourcetype * Reactance, df, method = "logistic", Hess = TRUE))
summary(olr)
stargazer(olr, type = "html", style = "aer") # CIs instead of t-values
## odds ratios
olr$coefficients <- exp(coef(olr)) # to insert Odds Ratios into coefficients for stargazer
# however, we need to also adjust the CIs, otherwise the significances are wrong. CAREFUL!

# On the difference/similarity between using treatment as IV or RecvsDef * Sourcetype (including control) ------
# The models are the same, but the coefficients are interpreted differently
summary(lm(Dist ~ RecvsDef * SourcetypeC, df))
summary(lm(Dist ~ treatment, df))
summary(glm(Donated ~ RecvsDef * SourcetypeC, df, family = "binomial"))


# Investigate the impact of believe2 on Dist and Donated (also interacted with factors) -----
lm <- (lm(Dist ~ RecvsDefD * SourcetypeD + believe2, df)) # n.s.
coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- (lm(Dist ~ RecvsDefD * SourcetypeD * believe2, df)) # Par < .05, Def*Par < .05, Def*beln < .05
# Parbel < .1, Def*Kno*beln < .01, Def*Par*beln < .05
coeftest(lm, vcov = vcovHC(lm, "HC1"))

glm <- glm(Donated ~ RecvsDefD * SourcetypeD + believe2, df, family = "binomial") # n.s.
coeftest(glm, vcov = vcovHC(glm, "HC1"))
glm <- glm(Donated ~ RecvsDefD * SourcetypeD * believe2, df, family = "binomial") # Par*beln < .1
coeftest(glm, vcov = vcovHC(glm, "HC1"))

# Are the following groups different from one another? -------
# No Source vs. SomeSource & believe2 == 0 vs. SomeSource & believe2 == 1 ----
# Why interesting? Does it make a difference with respect to extensive/ intensive margin whether
# there is no source, whether there is one but subject does not believe there is one, and there is
# one and subject believes there is one?

# set believe2 for DefNos to NA
df$believe2 <- ifelse(df$treatment == "DefNos", NA, df$believe2)
df$believe2 <- factor(df$believe2, levels = c(1,2), labels = c("Yes", "No"))

summary(lm(Dist ~ Sourcetype*believe2, df)) # n.s., but significant effects /w 3-way interaction

ggplot(df, aes(x = believe2, y = Dist)) +
  facet_grid(~NosvsSomeD) +
  geom_boxplot()+
  stat_summary(fun.y = mean, colour="darkred", geom="point", shape=18, size=3) 
  
# plot suggests that
## NoSource = SomeSourceYes < SomeSourceNo

# is the difference significant?
wilcox.test(df$Dist[df$NosvsSomeD == "Some Source"] ~ df$believe2[df$NosvsSomeD == "Some Source"])
# n.s.


n <- aggregate(df$Donatedm, list(df$NosvsSomeD, df$believe2), sum)
n$n <- aggregate(df$Donatedm, list(df$NosvsSomeD, df$believe2), length)[3]
ggplot(n, aes(x = Group.2, y = x/n)) +
  facet_grid(~Group.1) +
  geom_bar(stat = "identity")
# does not suggest that there is a difference in the probability to donate between those that
# believe and those that do not (among those that get a source)

df$believe2 <- ifelse(is.na(df$believe2 == TRUE), 2, ifelse(df$believe2 == "Ja", 1, 0))
df$believe2 <- factor(df$believe2, levels = c(0,1,2), labels = c("No", "Yes", "Miss"))
n <- aggregate(df$Donatedm, list(df$RecvsDef, df$SourcetypeC, df$believe2), sum)
n$n <- aggregate(df$Donatedm, list(df$RecvsDef, df$SourcetypeC, df$believe2), length)[4]
n$sd <- aggregate(df$Donatedm, list(df$RecvsDef, df$SourcetypeC, df$believe2), sd)[4]
n$se <- n$sd / sqrt(n$n)
n$lower <- (n$x/n$n - n$se)*100
n$upper <- (n$x/n$n + n$se)*100

ggplot(n, aes(x = Group.3, y = x/n*100)) +
  facet_grid(Group.1~Group.2) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "#999999")
# suggests a difference in the probability to donate between those that believe and those that do not
# for some treatments, especially RecPar and DefKno

summary(glm(Donated ~ RecvsDefD*Sourcetype*believe2, df, family = "binomial")) # n.s.
chisq.test(df$Donated[df$treatment == "RecPar"], df$believe2[df$treatment == "RecPar"], simulate.p.value = T)
# < .01
chisq.test(df$Donated[df$treatment == "DefKno"], df$believe2[df$treatment == "DefKno"], simulate.p.value = T)
# < .1

# Some other interactions with the treatment variables ---------
## moralD
lm <- (lm(Dist ~ RecvsDefD * Sourcetype * moralD, df)) # Nap < .05, Def*Nap < .05, Nap * moralD < .05
# Def*Nap*moralD < .1
coeftest(lm, vcov = vcovHC(lm, "HC1"))
ggplot(df, aes(x = moralD, y = Dist)) +
  facet_grid(RecvsDefD~Sourcetype) +
  geom_boxplot()

## efficiency
lm <- (lm(Dist ~ RecvsDefD * Sourcetype * retireEffD, df)) # Nap < .1, retireEff < .1, Nap *retireEff < .05
coeftest(lm, vcov = vcovHC(lm, "HC1"))
ggplot(df, aes(x = retireEffD, y = Dist)) +
  facet_grid(RecvsDefD~Sourcetype) +
  geom_boxplot()

## gender
lm <- (lm(Dist ~ RecvsDefD * Sourcetype * gender, dfsub)) # Par * genderW < .05, Def*Par*genderW < .05
coeftest(lm, vcov = vcovHC(lm, "HC1"))
ggplot(dfsub, aes(x = gender, y = Dist)) +
  facet_grid(RecvsDefD~Sourcetype) +
  geom_boxplot()



# How to decide which covariates I include in the model?


# Non-parametric group comparisons only with those that donated ---------
kruskal.test(dfDon$Donation ~ dfDon$RecvsDef) # n.s.
kruskal.test(dfDon$Dist ~ dfDon$RecvsDef) # n.s.
wilcox.test(dfDon$Donation ~ dfDon$RecvsDefD) # n.s.
wilcox.test(dfDon$Dist ~ dfDon$RecvsDefD) # n.s.

wilcox.test(dfDon$Donation ~ dfDon$RecNapvsRecNos, exact = F) # n.s.
wilcox.test(dfDon$Dist ~ dfDon$RecNapvsRecNos, exact = F) # n.s.
wilcox.test(dfDon$Donation ~ dfDon$DefNapvsDefNos, exact = F) # n.s.
wilcox.test(dfDon$Dist ~ dfDon$DefNapvsDefNos, exact = F) # n.s.

#H1
lm <- (lm(Donation ~ RecvsDefD * Sourcetype * ReactanceM, dfDon))
lm1 <- coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- (lm(Donation ~ RecvsDefD * NosvsSomeD * ReactanceM, dfDon))
lm2 <- coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- (lm(Dist ~ RecvsDefD * Sourcetype * ReactanceM, dfDon))
lm3 <- coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- (lm(Dist ~ RecvsDefD * NosvsSomeD * ReactanceM, dfDon))
lm4 <- coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- (lm(Donation ~ RecvsDefD * Sourcetype * ReactanceM, df))
lm5 <- coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- (lm(Donation ~ RecvsDefD * NosvsSomeD * ReactanceM, df))
lm6 <- coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- (lm(Dist ~ RecvsDefD * Sourcetype * ReactanceM, df))
lm7 <- coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- (lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance, df))
lm8 <- coeftest(lm, vcov = vcovHC(lm, "HC1"))
stargazer(lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, style = "aer", type = "latex", column.labels = c("DonDon", "DonDon", "DistDon", "DistDon", "Dondf", "Dondf", "Distdf", "Distdf"))

##including covariates
lm <- (lm(Donation ~ RecvsDefD * Sourcetype * ReactanceM + EAI + moralD + retireEffD + genderB + green, dfDon))
lm1 <- coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- (lm(Donation ~ RecvsDefD * NosvsSomeD * ReactanceM + EAI + moralD + retireEffD + genderB + green, dfDon))
lm2 <- coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- (lm(Dist ~ RecvsDefD * Sourcetype * ReactanceM + EAI + moralD + retireEffD + genderB + green, dfDon))
lm3 <- coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- (lm(Dist ~ RecvsDefD * NosvsSomeD * ReactanceM + EAI + moralD + retireEffD + genderB + green, dfDon))
lm4 <- coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- (lm(Donation ~ RecvsDefD * Sourcetype * ReactanceM + EAI + moralD + retireEffD + genderB + green, df))
lm5 <- coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- (lm(Donation ~ RecvsDefD * NosvsSomeD * ReactanceM + EAI + moralD + retireEffD + genderB + green, df))
lm6 <- coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- (lm(Dist ~ RecvsDefD * Sourcetype * ReactanceM + EAI + moralD + retireEffD + genderB + green, df))
lm7 <- coeftest(lm, vcov = vcovHC(lm, "HC1"))
lm <- (lm(Dist ~ RecvsDefD * NosvsSomeD * ReactanceM + EAI + moralD + retireEffD + genderB + green, df))
lm8 <- coeftest(lm, vcov = vcovHC(lm, "HC1"))
stargazer(lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, style = "aer", type = "html", column.labels = c("DonDon", "DonDon", "DistDon", "DistDon", "Dondf", "Dondf", "Distdf", "Distdf"))

# H1b
wilcox.test(dfDon$Donation[dfDon$ReactD == "Above Med React"] ~ dfDon$RecvsDefD[dfDon$ReactD == "Above Med React"]) # n.s.
# better treat Reactance as metric than as dummy
ggplot(dfDon, aes(x = Reactance, y = Donation)) +
  facet_grid(NosvsSomeD~RecvsDefD) +
  geom_jitter(shape = 1) +
  geom_smooth(method = "lm")
ggplot(df, aes(x = Reactance, y = Donation)) +
  facet_grid(NosvsSomeD~RecvsDefD) +
  geom_jitter(shape = 1) +
  geom_smooth(method = "lm")
ggplot(dfDon, aes(x = Reactance, y = Dist)) +
  facet_grid(NosvsSomeD~RecvsDefD) +
  geom_jitter(shape = 1) +
  geom_smooth(method = "lm")
ggplot(df, aes(x = Reactance, y = Dist)) +
  facet_grid(NosvsSomeD~RecvsDefD) +
  geom_jitter(shape = 1) +
  geom_smooth(method = "lm")

# H2
wilcox.test(dfDon$Donation ~ dfDon$RecNapvsRecKno) # n.s.
wilcox.test(dfDon$Donation ~ dfDon$DefNapvsDefKno) # n.s.

# H3-1
wilcox.test(dfDon$Donation ~ dfDon$RecNapvsRecPol) # n.s.
wilcox.test(dfDon$Donation ~ dfDon$DefNapvsDefPol) # n.s.

# H3-2
wilcox.test(dfDon$Donation ~ dfDon$RecPolvsDefPol) # n.s.

# H3a
# not testable because of not enough observations in trustPol
## with differently coded variable (above and below median)
ggplot(dfDon, aes(x = trustPolM, y = Donation)) +
  facet_grid(~RecPolvsDefPol) +
  geom_boxplot()
ggplot(dfDon, aes(x = trustPolM, y = Dist)) +
  facet_grid(~RecPolvsDefPol) +
  geom_boxplot()

wilcox.test(dfDon$Donation[dfDon$treatment == "RecPol"] ~ dfDon$trustPolM[dfDon$treatment == "RecPol"], conf.int=TRUE)
# n.s.
wilcox.test(dfDon$Donation[dfDon$treatment == "DefPol"] ~ dfDon$trustPolM[dfDon$treatment == "DefPol"], conf.int=TRUE)
# n.s.
wilcox.test(dfDon$Dist[dfDon$treatment == "RecPol"] ~ dfDon$trustPolM[dfDon$treatment == "RecPol"], conf.int=TRUE)
# n.s.
wilcox.test(dfDon$Dist[dfDon$treatment == "DefPol"] ~ dfDon$trustPolM[dfDon$treatment == "DefPol"], conf.int=TRUE)
# n.s.

# H3b
wilcox.test(dfDon$Donation ~ dfDon$conformityD) # n.s.
wilcox.test(dfDon$Dist ~ dfDon$conformityD) # n.s.
## with alternatively coded variable (median deviation)
ggplot(dfDon, aes(x = conformityM, y = Donation)) +
  facet_grid(~RecPolvsDefPol) +
  geom_boxplot()
ggplot(dfDon, aes(x = conformityM, y = Dist)) +
  facet_grid(~RecPolvsDefPol) +
  geom_boxplot()

wilcox.test(df$Donation[df$treatment == "RecPol"] ~ df$conformityM[df$treatment == "RecPol"]) # n.s.
wilcox.test(dfDon$Dist[df$treatment == "RecPol"] ~ dfDon$conformityM[df$treatment == "RecPol"]) # n.s.
wilcox.test(dfDon$Donation[df$treatment == "DefPol"] ~ dfDon$conformityM[df$treatment == "DefPol"]) # n.s.
wilcox.test(dfDon$Dist[df$treatment == "DefPol"] ~ dfDon$conformityM[df$treatment == "DefPol"]) # n.s.

# H4
chisq.test(table(dfDon$green, dfDon$RecParvsDefPar)) # green
wilcox.test(dfDon$Donation[dfDon$treatment == "RecPar"] ~ dfDon$green[dfDon$treatment == "RecPar"])
# n.s.
wilcox.test(dfDon$Donation[dfDon$treatment == "DefPar"] ~ dfDon$green[dfDon$treatment == "DefPar"])
# n.s.
ggplot(dfDon, aes(x = green, y = Donation)) +
  facet_grid(~RecParvsDefPar) +
  geom_boxplot() +
  geom_jitter(shape=1)

# Replicate Figure 5, Altmann 2014, 21
# Attention: I need to know whether I want fractions from that specific treatment or 
# overall fractions (I guess the former)
# would take too long and is probably not that important for me, resp. my research question

# try to to at least a visual comparison with density plots

glm <- glm(Donated ~ RecvsDefD * Sourcetype * ReactanceM, df, family = "binomial")
rglmres <- coeftest(glm, vcov = vcovHC(glm, "HC1"))
glm <- glm(Donated ~ RecvsDefD * Sourcetype * ReactanceM + EAI + moralD + retireEffD + genderB + green, df, family = "binomial")
rglmfull <- coeftest(glm, vcov = vcovHC(glm, "HC1"))
glm <- glm(Donated ~ RecvsDefD * NosvsSomeD * ReactanceM, df, family = "binomial")
rglmresNvS <- coeftest(glm, vcov = vcovHC(glm, "HC1"))
glm <- glm(Donated ~ RecvsDefD * NosvsSomeD * ReactanceM + EAI + moralD + retireEffD + genderB + green, df, family = "binomial")
rglmfullNvS <- coeftest(glm, vcov = vcovHC(glm, "HC1"))
stargazer(rglmres, rglmfull, rglmresNvS, rglmfullNvS, type = "html", style = "aer", column.labels = c("ResLog", "FullLog", "ResLog", "FullLog"))


tt <- aggregate(df$Donatedm, list(df$Reactance, df$RecvsDefD, df$NosvsSomeD), sum)
tt$n <- aggregate(df$Donatedm, list(df$Reactance, df$RecvsDefD, df$NosvsSomeD), length)[4]
tt$sd <- aggregate(df$Donatedm, list(df$Reactance, df$RecvsDefD, df$NosvsSomeD), sd)[4] # macht das hier überhaupt Sinn?
tt$se <- tt$sd/sqrt(tt$n)
tt$upper <- tt$x/tt$n + tt$se
tt$lower <- tt$x/tt$n - tt$se

ggplot(tt, aes(x=Group.1, y = x/n)) +
  facet_grid(Group.2~Group.3) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "#999999") +
  geom_smooth(method="glm")

