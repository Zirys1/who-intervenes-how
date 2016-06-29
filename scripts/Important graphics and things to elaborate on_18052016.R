# Important graphics and things to elaborate on
par(mfrow=c(1,1)) # Change the panel layout to 2 x 2

# Interventiontype x Sourcetype x Reactance interaction----------------------------------------------------------------------------------------------------
#### Note that treating Reactance as metric complicates the interpretation of interactions.
#### Especially since all coefficients become insignificant when including reactance as a median
#### dummy, these results do not seem to be very robust. (http://www.theanalysisfactor.com/interpreting-interactions-in-regression/)

## Treatment x Reactance interaction
Dist <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance, df)
#Dist <- coeftest(Dist, vcov = vcovHC(Dist, "HC1"))
Donation <- lm(Donation ~ RecvsDefD * Sourcetype * Reactance, df)
#Donation <- coeftest(Donation, vcov = vcovHC(Donation, "HC1"))
Donated <- glm(Donated ~ RecvsDefD * Sourcetype * Reactance, df, family = "binomial")
#Donated <- coeftest(Donated, vcov = vcovHC(Donated, "HC1"))
stargazer(Dist, Donation, Donated, type = "html", style = "aer", summary = TRUE, column.labels = c("Distance to 5 (convergence)", "Donation Amount (int. margin)", "Donated (ext. margin)"), covariate.labels = c("Default Treatment", "Name and Picture", "Knowledgeable", "Political", "Partisan", "Reactance Score", "Default Treatment x Name and Picture", "Default Treatment x Knowledgeable", "Default Treatment x Political", "Default Treatment x Partisan", "Default Treatment x Reactance Score", "Name and Picture x Reactance Score", "Knowledgeable x Reactance Score", "Political x Reactance Score", "Partisan x Reactance Score", "Default Treatment x Name and Picture x Reactance score", "Default Treatment x Knowledgeable x Reactance Score", "Default Treatment x Political x Reactance Score", "Default Treatment x Partisan x Reactance Score", "Constant"), title = "Regressions estimates")

### Check assumptions
plot(Dist)
plot(Donation) 

# As above, with mean and median centering of Reactance -----------------------
DistLM <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance, df)
DistRLM <- coeftest(DistLM, vcov = vcovHC(DistLM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
DistLMM <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance, df)
DistRLMM <- coeftest(DistLMM, vcov = vcovHC(DistLMM, "HC1"))
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
DistLMMM <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance, df)
DistRLMMM <- coeftest(DistLMMM, vcov = vcovHC(DistLMMM, "HC1"))
stargazer(DistRLM, DistRLMM, DistRLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DistLM, DistLMM, DistLMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Plot interpretation-------------------- 
# Plot indicates (fewer observations under 0 residual y axis) that I have concentration
# at 0 values and the model under-predicts (Residual = Observed - Fitted) some observations pretty
# strongly. However, the trend (red line, loess) is more towards under-fitting for higher fitted observations
# this can be fixed by (1) transforming the DV, and/or (2) adding more predictors to the model
# http://docs.statwing.com/interpreting-residual-plots-to-improve-your-regression/#y-unbalanced-header

# There also seems to be a slight problem of heteroskedasticity (which leads to too low SEs)
# I'll probably need more variables in the model. Question would be, however, if I also assessed the
# variables I need in order for my model to be better

# Things that can be done
# 1. Delete outliers, if I can theoretically defend that
# 2. Transform variables (square, square root, logarithm, ... e.g. log only positive in 2-step model)
#   a. y^2 y^3 y^4
#   b. sqrt(y)
#   c. log(y)
#   d. 1/sqrt(y), 1/y, 1/y^2, 1/y^3, 1/y^4
# MIND THAT TRANSFORMING CHANGES INTERPRETATION OF THE ESTIMATED COEFFICIENTS!!!
#-------------------
plot(Donated)


# None of these graphs looks as if assumptions are met, which is bad
## Transform Dependent variables (in order to achieve normal distribution for error terms)

## Treatment x Reactance interaction (logit for extensive margin and OLS without 0s)
Donated <- glm(Donated ~ RecvsDefD * Sourcetype * Reactance, df, family = "binomial")
Donated <- coeftest(Donated, vcov = vcovHC(Donated, "HC1"))
Donationno0 <- lm(Donationno0 ~ RecvsDefD * Sourcetype * Reactance, df)
Donationno0 <- coeftest(Donationno0, vcov = vcovHC(Donationno0, "HC1"))
df$Distno5 <- ifelse(df$Dist == 5, NA, df$Dist)
Distno5 <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance, df)
Distno5 <- coeftest(Distno5, vcov = vcovHC(Distno5, "HC1"))
stargazer(Donated, Donationno0, Distno5, type = "html", style = "aer", summary = TRUE, column.labels = c("Donated (ext. margin)", "Donation Amount/wo 0's (int. margin)", "Distance to 5/wo 5's (convergence)"), covariate.labels = c("Default Treatment", "Name and Picture", "Knowledgeable", "Political", "Partisan", "Reactance Score", "Default Treatment x Name and Picture", "Default Treatment x Knowledgeable", "Default Treatment x Political", "Default Treatment x Partisan", "Default Treatment x Reactance Score", "Name and Picture x Reactance Score", "Knowledgeable x Reactance Score", "Political x Reactance Score", "Partisan x Reactance Score", "Default Treatment x Name and Picture x Reactance score", "Default Treatment x Knowledgeable x Reactance Score", "Default Treatment x Political x Reactance Score", "Default Treatment x Partisan x Reactance Score", "Constant"), title = "Two-step Regressions estimates")

### Check assumptions
plot(Distno5)
plot(Donationno0)
# None of these graphs looks as if assumptions are met, which is bad
## Transform Dependent variables (in order to achieve normal distribution for error terms)

# Treatment x Reactance interaction in Ologit
ordered <- polr(as.factor(as.character(Donation)) ~ RecvsDefD * Sourcetype * Reactance, df, Hess = TRUE)
orderedDist <- polr(as.factor(as.character(Dist)) ~ RecvsDefD * Sourcetype * Reactance, df, Hess = TRUE)
stargazer(ordered, orderedDist, type = "html", style = "aer", summary = TRUE, title = "Proportional odds logistic regression")

orderedno0 <- polr(as.factor(as.character(Donationno0)) ~ RecvsDefD * Sourcetype * Reactance, df, Hess = TRUE)
orderedDistno5 <- polr(as.factor(as.character(Distno5)) ~ RecvsDefD * Sourcetype * Reactance, df, Hess = TRUE)
stargazer(orderedno0, orderedDistno5, type = "html", style = "aer", summary = TRUE, title = "Two-step Proportional odds logistic regression")

# Interventiontype x Some Source x Reactance interaction ----------------------------------------------------------------------------------------------------

### The same as above but instead of Sourcetype I'll use NosvsSome dummy
## Treatment x Reactance interaction
Dist <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance, df)
Dist <- coeftest(Dist, vcov = vcovHC(Dist, "HC1"))
Donation <- lm(Donation ~ RecvsDefD * NosvsSomeD * Reactance, df)
Donation <- coeftest(Donation, vcov = vcovHC(Donation, "HC1"))
Donated <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance, df, family = "binomial")
Donated <- coeftest(Donated, vcov = vcovHC(Donated, "HC1"))
stargazer(Dist, Donation, Donated, type = "html", style = "aer", summary = TRUE, column.labels = c("Distance to 5 (convergence)", "Donation Amount (int. margin)", "Donated (ext. margin)"), title = "Regressions estimates", covariate.labels = c("Default Treatment", "Some Source", "Reactance Score", "Default Treatment x Some Source", "Default Treatment x Reactance Score", "Some Source x Reactance Score", "Default Treatment x Some Source x Reactance Score", "Constant"))
### Check assumptions
plot(Dist)
plot(Donation)
plot(Donated)
# None of these graphs looks as if assumptions are met, which is bad
## Transform Dependent variables (in order to achieve normal distribution for error terms)

## Treatment x Reactance interaction (logit for extensive margin and OLS without 0s)
Donated <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance, df, family = "binomial")
#Donated <- coeftest(Donated, vcov = vcovHC(Donated, "HC1"))
Donationno0 <- lm(Donationno0 ~ RecvsDefD * NosvsSomeD * Reactance, df)
#Donationno0 <- coeftest(Donationno0, vcov = vcovHC(Donationno0, "HC1"))
df$Distno5 <- ifelse(df$Dist == 5, NA, df$Dist)
Distno5 <- lm(Distno5 ~ RecvsDefD * NosvsSomeD * Reactance, df)
#Distno5 <- coeftest(Distno5, vcov = vcovHC(Distno5, "HC1"))
stargazer(Distno5, Donationno0, Donated, type = "html", style = "aer", summary = TRUE, column.labels = c("Distance to 5 (convergence)", "Donation Amount (int. margin)", "Donated (ext. margin)"), covariate.labels = c("Default Treatment", "Some Source", "Reactance Score", "Default Treatment x Some Source", "Default Treatment x Reactance Score", "Some Source x Reactance Score", "Default Treatment x Some Source x Reactance Score", "Constant"), title = "Two-step Regressions estimates")

### Check assumptions
plot(Distno5)
plot(Donationno0)
# None of these graphs looks as if assumptions are met, which is bad
## Transform Dependent variables (in order to achieve normal distribution for error terms)

# Treatment x Reactance interaction in Ologit (should do this as in Agresti (2002): Modeling nonnegative data with clumping at zero, p. 16: Ordinal threshold models)
ordered <- polr(as.factor(as.character(Donation)) ~ RecvsDefD * NosvsSomeD * Reactance, df, Hess = TRUE)
orderedDist <- polr(as.factor(as.character(Dist)) ~ RecvsDefD * NosvsSomeD * Reactance, df, Hess = TRUE)
stargazer(ordered, orderedDist, type = "html", style = "aer", summary = TRUE, title = "Proportional odds logistic regression")

orderedno0 <- polr(as.factor(as.character(Donationno0)) ~ RecvsDefD * NosvsSomeD * Reactance, df, Hess = TRUE)
orderedDistno5 <- polr(as.factor(as.character(Distno5)) ~ RecvsDefD * NosvsSomeD * Reactance, df, Hess = TRUE)
stargazer(orderedno0, orderedDistno5, type = "html", style = "aer", summary = TRUE, title = "Two-step Proportional odds logistic regression")

# As above, with mean and median centering of Reactance -----------------------
DistLM <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance, df)
DistRLM <- coeftest(DistLM, vcov = vcovHC(DistLM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
DistLMM <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance, df)
DistRLMM <- coeftest(DistLMM, vcov = vcovHC(DistLMM, "HC1"))
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
DistLMMM <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance, df)
DistRLMMM <- coeftest(DistLMMM, vcov = vcovHC(DistLMMM, "HC1"))
stargazer(DistRLM, DistRLMM, DistRLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DistLM, DistLMM, DistLMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Interventiontype x Some Source x non-linear Reactance interaction ----------------------------------------------------------------------------------------------------

### The same as above but instead of Sourcetype I'll use NosvsSome dummy
## Treatment x Reactance interaction
Dist <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance, df)
Dist <- coeftest(Dist, vcov = vcovHC(Dist, "HC1"))
Donation <- lm(Donation ~ RecvsDefD * NosvsSomeD * Reactance, df)
Donation <- coeftest(Donation, vcov = vcovHC(Donation, "HC1"))
Donated <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance, df, family = "binomial")
Donated <- coeftest(Donated, vcov = vcovHC(Donated, "HC1"))
stargazer(Dist, Donation, Donated, type = "html", style = "aer", summary = TRUE, column.labels = c("Distance to 5 (convergence)", "Donation Amount (int. margin)", "Donated (ext. margin)"), title = "Regressions estimates", covariate.labels = c("Default Treatment", "Some Source", "Reactance Score", "Default Treatment x Some Source", "Default Treatment x Reactance Score", "Some Source x Reactance Score", "Default Treatment x Some Source x Reactance Score", "Constant"))
### Check assumptions
plot(Dist)
plot(Donation)
plot(Donated)
# None of these graphs looks as if assumptions are met, which is bad
## Transform Dependent variables (in order to achieve normal distribution for error terms)

## Treatment x Reactance interaction (logit for extensive margin and OLS without 0s)
Donated <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance, df, family = "binomial")
#Donated <- coeftest(Donated, vcov = vcovHC(Donated, "HC1"))
Donationno0 <- lm(Donationno0 ~ RecvsDefD * NosvsSomeD * Reactance, df)
#Donationno0 <- coeftest(Donationno0, vcov = vcovHC(Donationno0, "HC1"))
df$Distno5 <- ifelse(df$Dist == 5, NA, df$Dist)
Distno5 <- lm(Distno5 ~ RecvsDefD * NosvsSomeD * Reactance, df)
#Distno5 <- coeftest(Distno5, vcov = vcovHC(Distno5, "HC1"))
stargazer(Distno5, Donationno0, Donated, type = "html", style = "aer", summary = TRUE, column.labels = c("Distance to 5 (convergence)", "Donation Amount (int. margin)", "Donated (ext. margin)"), covariate.labels = c("Default Treatment", "Some Source", "Reactance Score", "Default Treatment x Some Source", "Default Treatment x Reactance Score", "Some Source x Reactance Score", "Default Treatment x Some Source x Reactance Score", "Constant"), title = "Two-step Regressions estimates")

### Check assumptions
plot(Distno5)
plot(Donationno0)
# None of these graphs looks as if assumptions are met, which is bad
## Transform Dependent variables (in order to achieve normal distribution for error terms)

# Treatment x Reactance interaction in Ologit (should do this as in Agresti (2002): Modeling nonnegative data with clumping at zero, p. 16: Ordinal threshold models)
ordered <- polr(as.factor(as.character(Donation)) ~ RecvsDefD * NosvsSomeD * Reactance, df, Hess = TRUE)
orderedDist <- polr(as.factor(as.character(Dist)) ~ RecvsDefD * NosvsSomeD * Reactance, df, Hess = TRUE)
stargazer(ordered, orderedDist, type = "html", style = "aer", summary = TRUE, title = "Proportional odds logistic regression")

orderedno0 <- polr(as.factor(as.character(Donationno0)) ~ RecvsDefD * NosvsSomeD * Reactance, df, Hess = TRUE)
orderedDistno5 <- polr(as.factor(as.character(Distno5)) ~ RecvsDefD * NosvsSomeD * Reactance, df, Hess = TRUE)
stargazer(orderedno0, orderedDistno5, type = "html", style = "aer", summary = TRUE, title = "Two-step Proportional odds logistic regression")

# Interventiontype x Political Source x Reactance interaction: no significant results----------------------------------------------------------------------------------------------------

### The same as above but instead of Sourcetype I'll use NonPolvsPol dummy
## Treatment x Reactance interaction


### NO SIGNIFICANT RESULTS; THEREFORE OMITTED!!!

# Interventiontype x Political/Partisan Source x Reactance interaction: no significant results----------------------------------------------------------------------------------------------------

### The same as above but instead of Sourcetype I'll use NonPolvsPolvsPar factor
df$NonPolvsPolvsPar <- ifelse(df$treatment == "RecNap" | df$treatment == "DefNap" | df$treatment == "RecKno" | df$treatment == "DefKno", 0, ifelse(df$treatment == "RecPol" | df$treatment == "DefPol", 1, ifelse(df$treatment == "RecPar" | df$treatment == "DefPar", 2, NA ) ) )
df$NonPolvsPolvsPar <- factor(df$NonPolvsPolvsPar, levels = c(0,1,2), labels = c("Non Political", "Political", "Partisan"))
## Treatment x Reactance interaction
Dist <- lm(Dist ~ RecvsDefD * NonPolvsPolvsPar * Reactance, df)
Dist <- coeftest(Dist, vcov = vcovHC(Dist, "HC1"))
Donation <- lm(Donation ~ RecvsDefD * NonPolvsPolvsPar * Reactance, df)
Donation <- coeftest(Donation, vcov = vcovHC(Donation, "HC1"))
Donated <- glm(Donated ~ RecvsDefD * NonPolvsPolvsPar * Reactance, df, family = "binomial")
Donated <- coeftest(Donated, vcov = vcovHC(Donated, "HC1"))
stargazer(Dist, Donation, Donated, type = "html", style = "aer", summary = TRUE, column.labels = c("Distance to 5 (convergence)", "Donation Amount (int. margin)", "Donated (ext. margin)"), title = "Regressions estimates")
covariate.labels = c("Default Treatment", "Some Source", "Reactance Score", "Default Treatment x Some Source", "Default Treatment x Reactance Score", "Some Source x Reactance Score", "Default Treatment x Some Source x Reactance Score", "Constant")
### Check assumptions
plot(Dist)
plot(Donation)
plot(Donated)
# None of these graphs looks as if assumptions are met, which is bad
## Transform Dependent variables

## Treatment x Reactance interaction (logit for extensive margin and OLS without 0s)
Donated <- glm(Donated ~ RecvsDefD * NonPolvsPolvsPar * Reactance, df, family = "binomial")
Donated <- coeftest(Donated, vcov = vcovHC(Donated, "HC1"))
Donationno0 <- lm(Donationno0 ~ RecvsDefD * NonPolvsPolvsPar * Reactance, df)
Donationno0 <- coeftest(Donationno0, vcov = vcovHC(Donationno0, "HC1"))
df$Distno5 <- ifelse(df$Dist == 5, NA, df$Dist)
Distno5 <- lm(Distno5 ~ RecvsDefD * NonPolvsPolvsPar * Reactance, df)
Distno5 <- coeftest(Distno5, vcov = vcovHC(Distno5, "HC1"))
stargazer(Distno5, Donationno0, Donated, type = "html", style = "aer", summary = TRUE, column.labels = c("Distance to 5 (convergence)", "Donation Amount (int. margin)", "Donated (ext. margin)"), title = "Two-step Regressions estimates")

### Check assumptions
plot(Distno5)
plot(Donationno0)
# None of these graphs looks as if assumptions are met, which is bad
## Transform Dependent variables

# Treatment x Reactance interaction in Ologit
ordered <- polr(as.factor(as.character(Donation)) ~ RecvsDefD * NonPolvsPolvsPar * Reactance, df, Hess = TRUE)
orderedDist <- polr(as.factor(as.character(Dist)) ~ RecvsDefD * NonPolvsPolvsPar * Reactance, df, Hess = TRUE)
stargazer(ordered, orderedDist, type = "html", style = "aer", summary = TRUE, title = "Proportional odds logistic regression")

orderedno0 <- polr(as.factor(as.character(Donationno0)) ~ RecvsDefD * NonPolvsPolvsPar * Reactance, df, Hess = TRUE)
orderedDistno5 <- polr(as.factor(as.character(Distno5)) ~ RecvsDefD * NonPolvsPolvsPar * Reactance, df, Hess = TRUE)
stargazer(orderedno0, orderedDistno5, type = "html", style = "aer", summary = TRUE, title = "Two-step Proportional odds logistic regression")

# Interventiontype x Sourcetype x Reactance interaction with more predictors -------------------------
# Predictor 1: EAI
DistLM <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance + EAI, df)
DistRLM <- coeftest(DistLM, vcov = vcovHC(DistLM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
DistLMM <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance + EAI, df)
DistRLMM <- coeftest(DistLMM, vcov = vcovHC(DistLMM, "HC1"))
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
DistLMMM <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance + EAI, df)
DistRLMMM <- coeftest(DistLMMM, vcov = vcovHC(DistLMMM, "HC1"))
stargazer(DistRLM, DistRLMM, DistRLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DistLM, DistLMM, DistLMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Predictor 2: moralD
DistLM <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD, df)
DistRLM <- coeftest(DistLM, vcov = vcovHC(DistLM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
DistLMM <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD, df)
DistRLMM <- coeftest(DistLMM, vcov = vcovHC(DistLMM, "HC1"))
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
DistLMMM <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD, df)
DistRLMMM <- coeftest(DistLMMM, vcov = vcovHC(DistLMMM, "HC1"))
stargazer(DistRLM, DistRLMM, DistRLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DistLM, DistLMM, DistLMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Predictor 3: retireEffD
DistLM <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD, df)
DistRLM <- coeftest(DistLM, vcov = vcovHC(DistLM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
DistLMM <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD, df)
DistRLMM <- coeftest(DistLMM, vcov = vcovHC(DistLMM, "HC1"))
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
DistLMMM <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD, df)
DistRLMMM <- coeftest(DistLMMM, vcov = vcovHC(DistLMMM, "HC1"))
stargazer(DistRLM, DistRLMM, DistRLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DistLM, DistLMM, DistLMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Predictor 4: demographics
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance
DistLM <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender, dfsub)
DistRLM <- coeftest(DistLM, vcov = vcovHC(DistLM, "HC1"))
# center around mean of the Reactance variable
dfsub$Reactance <- dfsub$Reactance - mean(dfsub$Reactance)
DistLMM <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender, dfsub)
DistRLMM <- coeftest(DistLMM, vcov = vcovHC(DistLMM, "HC1"))
# center around median of the Reactance variable
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv)
dfsub$Reactance <- dfsub$Reactance - median(dfsub$Reactance)
DistLMMM <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender, dfsub)
DistRLMMM <- coeftest(DistLMMM, vcov = vcovHC(DistLMMM, "HC1"))
stargazer(DistRLM, DistRLMM, DistRLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DistLM, DistLMM, DistLMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Predictor 5: beliefs
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance
DistLM <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender + belief, dfsub)
DistRLM <- coeftest(DistLM, vcov = vcovHC(DistLM, "HC1"))
# center around mean of the Reactance variable
dfsub$Reactance <- dfsub$Reactance - mean(dfsub$Reactance)
DistLMM <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender + belief, dfsub)
DistRLMM <- coeftest(DistLMM, vcov = vcovHC(DistLMM, "HC1"))
# center around median of the Reactance variable
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv)
dfsub$Reactance <- dfsub$Reactance - median(dfsub$Reactance)
DistLMMM <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender + belief, dfsub)
DistRLMMM <- coeftest(DistLMMM, vcov = vcovHC(DistLMMM, "HC1"))
stargazer(DistRLM, DistRLMM, DistRLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DistLM, DistLMM, DistLMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance
