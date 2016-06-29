# CONSECUTIVE REGRESSION ANALYSES

# INTENSIVE MARGIN -----------

## Interventiontype x Sourcetype x Reactance interaction, with mean and median centering of Reactance -----------------------
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
# Put into stargazer
stargazer(DistRLM, DistRLMM, DistRLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DistLM, DistLMM, DistLMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

## Interventiontype x NosvsSomeD x Reactance interaction, with mean and median centering of Reactance -----------------------
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance
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
# Put into stargazer
stargazer(DistRLM, DistRLMM, DistRLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DistLM, DistLMM, DistLMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))

interplot(DistLMM, var1 = "NosvsSomeD", var2 = "Reactance")

## Interventiontype x Sourcetype x Reactance interaction with more predictors -------------------------
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
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance

# Predictor 5: beliefs
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance
DistLM <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender + belief, dfsub)
DistRLM <- coeftest(DistLM, vcov = vcovHC(DistLM, "HC1"))
# center around mean of the Reactance variable
dfsub$Reactance <- dfsub$Reactance - mean(dfsub$Reactance)
DistLMM <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance + age+ EAI + moralD + retireEffD + gender + belief, dfsub)
DistRLMM <- coeftest(DistLMM, vcov = vcovHC(DistLMM, "HC1"))
# center around median of the Reactance variable
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv)
dfsub$Reactance <- dfsub$Reactance - median(dfsub$Reactance)
DistLMMM <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender + belief, dfsub)
DistRLMMM <- coeftest(DistLMMM, vcov = vcovHC(DistLMMM, "HC1"))
stargazer(DistRLM, DistRLMM, DistRLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DistLM, DistLMM, DistLMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance

## Interventiontype x NosvsSomeD x Reactance interaction with more predictors -------------------------
# Predictor 1: EAI
DistLM <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance + EAI, df)
DistRLM <- coeftest(DistLM, vcov = vcovHC(DistLM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
DistLMM <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance + EAI, df)
DistRLMM <- coeftest(DistLMM, vcov = vcovHC(DistLMM, "HC1"))
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
DistLMMM <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance + EAI, df)
DistRLMMM <- coeftest(DistLMMM, vcov = vcovHC(DistLMMM, "HC1"))
stargazer(DistRLM, DistRLMM, DistRLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DistLM, DistLMM, DistLMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Predictor 2: moralD
DistLM <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD, df)
DistRLM <- coeftest(DistLM, vcov = vcovHC(DistLM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
DistLMM <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD, df)
DistRLMM <- coeftest(DistLMM, vcov = vcovHC(DistLMM, "HC1"))
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
DistLMMM <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD, df)
DistRLMMM <- coeftest(DistLMMM, vcov = vcovHC(DistLMMM, "HC1"))
stargazer(DistRLM, DistRLMM, DistRLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DistLM, DistLMM, DistLMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Predictor 3: retireEffD
DistLM <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD, df)
DistRLM <- coeftest(DistLM, vcov = vcovHC(DistLM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
DistLMM <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD, df)
DistRLMM <- coeftest(DistLMM, vcov = vcovHC(DistLMM, "HC1"))
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
DistLMMM <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD, df)
DistRLMMM <- coeftest(DistLMMM, vcov = vcovHC(DistLMMM, "HC1"))
stargazer(DistRLM, DistRLMM, DistRLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DistLM, DistLMM, DistLMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Predictor 4: demographics
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance
DistLM <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD + gender, dfsub)
DistRLM <- coeftest(DistLM, vcov = vcovHC(DistLM, "HC1"))
# center around mean of the Reactance variable
dfsub$Reactance <- dfsub$Reactance - mean(dfsub$Reactance)
DistLMM <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD + gender, dfsub)
DistRLMM <- coeftest(DistLMM, vcov = vcovHC(DistLMM, "HC1"))
# center around median of the Reactance variable
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv)
dfsub$Reactance <- dfsub$Reactance - median(dfsub$Reactance)
DistLMMM <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD + gender, dfsub)
DistRLMMM <- coeftest(DistLMMM, vcov = vcovHC(DistLMMM, "HC1"))
stargazer(DistRLM, DistRLMM, DistRLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DistLM, DistLMM, DistLMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance

# Predictor 5: beliefs
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance
DistLM <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD + gender + belief, dfsub)
DistRLM <- coeftest(DistLM, vcov = vcovHC(DistLM, "HC1"))
# center around mean of the Reactance variable
dfsub$Reactance <- dfsub$Reactance - mean(dfsub$Reactance)
DistLMM <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD + gender + belief, dfsub)
DistRLMM <- coeftest(DistLMM, vcov = vcovHC(DistLMM, "HC1"))
# center around median of the Reactance variable
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv)
dfsub$Reactance <- dfsub$Reactance - median(dfsub$Reactance)
DistLMMM <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD + gender + belief, dfsub)
DistRLMMM <- coeftest(DistLMMM, vcov = vcovHC(DistLMMM, "HC1"))
stargazer(DistRLM, DistRLMM, DistRLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DistLM, DistLMM, DistLMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance

## Graphical representation of interactions --------------
### Interventiontype x Sourcetype x Reactance interaction, with mean and median centering of Reactance ---------
# http://www.statsblogs.com/2013/08/27/creating-marginal-effect-plots-for-linear-regression-models-in-r/
#df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance
#df$Reactance <- df$Reactance - mean(df$Reactance)
# estimate the model with a product term (ONLY LINEAR MODELS)
#m <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance, df)

df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance
df$Reactance <- df$Reactance - mean(df$Reactance)
m <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance, df)
# pull out coefficient estimates
beta.hat <- coef(m) 
# pull out the covariance matrix
cov <- vcov(m)

# a set of values of Reactance to compute the (instantaneous) 
# effect of Intervention type (RecvsDefD) 
z0 <- seq(min(df$ReactanceM), max(df$ReactanceM))

# calculate the instantaneous effect of Interventiontype as Reactance varies
dy.dx <- beta.hat["RecvsDefDDef"] + beta.hat["RecvsDefDDef:ReactanceM"]*z0
dy.dx1 <- beta.hat["RecvsDefDDef"] + beta.hat["RecvsDefDDef:ReactanceM"]*z0 + beta.hat["RecvsDefDDef:NosvsSomeDSome Source:ReactanceM"]*z0

# calculate the standard error of each estimated effect
se.dy.dx <- sqrt(cov["RecvsDefDDef", "RecvsDefDDef"] + z0^2*cov["RecvsDefDDef:ReactanceM", "RecvsDefDDef:ReactanceM"] + 2*z0*cov["RecvsDefDDef", "RecvsDefDDef:ReactanceM"])

# calculate upper and lower bounds of a 90% CI
upr <- dy.dx + 1.64*se.dy.dx
lwr <- dy.dx - 1.64*se.dy.dx
upr95 <- dy.dx + 1.96*se.dy.dx
lwr95 <- dy.dx - 1.96*se.dy.dx

# combine to data.frame in order to use ggplot
rm(dat)
dat <- cbind(z0, dy.dx)
dat <- as.data.frame(dat)
dat <- cbind(dat, dy.dx1)
dat <- cbind(dat, lwr)
dat <- cbind(dat, upr)
dat <- cbind(dat, upr95)
dat <- cbind(dat, lwr95)

# Plot of marginal interaction effect (RecvsDefD x Reactaance) for NoSource with 90% and 95% 
# confidence intervals
 ggplot(data = dat, aes(x = z0, y = dy.dx)) +
  scale_x_continuous(breaks = -4:6) +
  scale_y_continuous(breaks = -3:5) +
  geom_line(size = .5) +
  geom_line(data = dat, aes(x = z0, y= dy.dx1, color = "red"))+
  xlab(expression(Reactance)) +
  ylab(expression(frac(partialdiff*Dist, partialdiff*IntType))) +
 # geom_path(data = dat, aes(z0, upr95), linetype = "dotdash", color = "blue") +
#  geom_path(data = dat, aes(z0, lwr95), linetype = "dotdash", color = "blue") +
#  geom_path(data = dat, aes(z0, upr), linetype = "dotdash", color = "red") +
#  geom_path(data = dat, aes(z0, lwr), linetype = "dotdash", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ggtitle(expression(atop("Interaction of Intervention type and Reactance scale", atop(italic("with 90% & 95% confidence intervals"),"for No Source and Some Source (red)"))))


# Plot of marginal interaction effect (RecvsDefD x Reactaance) for SomeSource with 90% and 95% 
# confidence intervals
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance
dfsub$Reactance <- dfsub$Reactance - mean(dfsub$Reactance)
m <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance, df)
# pull out coefficient estimates
beta.hat1 <- coef(m) 

# pull out the covariance matrix
cov1 <- vcov(m)

# a set of values of Reactance to compute the (instantaneous) 
# effect of Intervention type (RecvsDefD) 
z01 <- seq(min(df$Reactance), max(df$Reactance))

# calculate the instantaneous effect of Interventiontype as Reactance varies
dy.dx1 <- beta.hat["RecvsDefDDef"] + beta.hat["RecvsDefDDef:Reactance"]*z0 + beta.hat["RecvsDefDDef:NosvsSomeDSome Source:Reactance"]

# combine to data.frame in order to use ggplot
dat <- cbind(dat, dy.dx1)


# calculate the standard error of each estimated effect (HERE I AM UNSURE IF THIS HOLDS FOR
# 3 WAY INTERACTIONS AS WELL)
#se.dy.dx <- sqrt(cov["RecvsDefDDef", "RecvsDefDDef"] + z0^2*cov["RecvsDefDDef:Reactance", "RecvsDefDDef:Reactance"] + 2*z0*cov["RecvsDefDDef", "RecvsDefDDef:Reactance"])

# short and easy (but less dynamic) version
interplot(m, var1 = "RecvsDefD", var2 = "Reactance", plot = TRUE, hist = TRUE) + # shows how 
  # the change in Reactance affects the coefficient of changing to Default on Distance
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed")

interplot(m, var1 = "Reactance", var2 = "RecvsDefD") # shows how the change to Default affects
# the coefficient of Reactance on Distance
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed")

# Sourcetype x Reactance
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance
dfsub$Reactance <- dfsub$Reactance - mean(dfsub$Reactance)
m <- lm(Dist ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender, dfsub)
interplot(m, var1 = "Sourcetype", var2 = "Reactance", plot = TRUE, hist = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed")

interplot(m, var1 = "Reactance", var2 = "Sourcetype")
  

### Interventiontype x NosvsSomeD x Reactance interaction, with mean and meadian centering of Reactance -------------
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance
dfsub$Reactance <- dfsub$Reactance - mean(dfsub$Reactance)
m <- lm(Dist ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD , df)
# pull out coefficient estimates
beta.hat <- coef(m) 

# pull out the covariance matrix
cov <- vcov(m)

# a set of values of Reactance to compute the (instantaneous) 
# effect of Intervention type (RecvsDefD) 
z0 <- seq(min(dfsub$Reactance), max(dfsub$Reactance))

# calculate the instantaneous effect of Interventiontype as Reactance varies
dy.dx <- beta.hat["NosvsSomeDSome Source"] + beta.hat["NosvsSomeDSome Source:Reactance"]*z0 # does this also need to account for the tripple interaction?

# calculate the standard error of each estimated effect
se.dy.dx <- sqrt(cov["NosvsSomeDSome Source", "NosvsSomeDSome Source"] + z0^2*cov["NosvsSomeDSome Source:Reactance", "NosvsSomeDSome Source:Reactance"] + 2*z0*cov["NosvsSomeDSome Source", "NosvsSomeDSome Source:Reactance"])

# calculate upper and lower bounds of a 90% CI
upr <- dy.dx + 1.64*se.dy.dx
lwr <- dy.dx - 1.64*se.dy.dx
upr95 <- dy.dx + 1.96*se.dy.dx
lwr95 <- dy.dx - 1.96*se.dy.dx

# combine to data.frame in order to use ggplot
rm(dat)
dat <- cbind(z0, dy.dx)
dat <- as.data.frame(dat)
dat <- cbind(dat, lwr)
dat <- cbind(dat, upr)
dat <- cbind(dat, upr95)
dat <- cbind(dat, lwr95)

# Plot with 90% confidence intervals
ggplot(data = dat, aes(x = z0, y = dy.dx)) +
  scale_x_continuous(breaks = -4:6) +
  scale_y_continuous(breaks = -3:5) +
  geom_line(size = 1) +
  xlab(expression(Reactance)) +
  ylab(expression(frac(partialdiff*Dist, partialdiff*IntType))) +
  geom_path(data = dat, aes(z0, upr95), linetype = "dotdash", color = "blue") +
  geom_path(data = dat, aes(z0, lwr95), linetype = "dotdash", color = "blue") +
  geom_path(data = dat, aes(z0, upr), linetype = "dotdash", color = "red") +
  geom_path(data = dat, aes(z0, lwr), linetype = "dotdash", color = "red")  +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ggtitle(expression(atop("Interaction of Some Source and Reactance scale", atop(italic("with 90% & 95% confidence intervals"),""))))

# short and easy (but less dynamic) version
interplot(m, var1 = "NosvsSomeD", var2 = "Reactance", plot = TRUE, hist = TRUE) + # shows how 
  # the change in Reactance affects the coefficient of changing to Some Source on Distance
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed")
# this function does also not seem to include the tripple interaction

plot(effect("RecvsDefD:NosvsSomeD:Reactance",m), multiline=TRUE) # calculates and plots predicted values 
# for different levels of reactance
lsmeans(m, specs = c("RecvsDefD", "NosvsSomeD", "Reactance"))

lm<- (lm(Dist ~ NosvsSomeD*Reactance, df))


# EXTENSIVE MARGIN --------------------------------
# Interventiontype x Sourcetype x Reactance interaction, with mean and median centering of Reactance -----------------------
DonatedGLM <- glm(Donated ~ RecvsDefD * Sourcetype * Reactance, df, family = "binomial")
DonatedRGLM <- coeftest(DonatedGLM, vcov = vcovHC(DonatedGLM, "HC1"))
#DonatedRGLMma <- logitmfx(formula = Donated ~ RecvsDefD * Sourcetype * Reactance, data = df, robust = T)
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
DonatedGLMM <- glm(Donated ~ RecvsDefD * Sourcetype * Reactance, df, family = "binomial")
DonatedRGLMM <- coeftest(DonatedGLMM, vcov = vcovHC(DonatedGLMM, "HC1"))
#DonatedRGLMMma <- logitmfx(formula = Donated ~ RecvsDefD * Sourcetype * Reactance, data = df, robust = T)
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
DonatedGLMMM <- glm(Donated ~ RecvsDefD * Sourcetype * Reactance, df, family = "binomial")
DonatedRGLMMM <- coeftest(DonatedGLMMM, vcov = vcovHC(DonatedGLMMM, "HC1"))
#DonatedRGLMMMma <- logitmfx(formula = Donated ~ RecvsDefD * Sourcetype * Reactance, data = df, robust = T)

# Put into stargazer
stargazer(DonatedRGLM, DonatedRGLMM, DonatedRGLMMM, type="html", style = "aer", title = "Robust Logit", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DonatedGLM, DonatedGLMM, DonatedGLMMM, type="html", style = "aer", title = "Non-robust Logit", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))

logitmfx(Donated ~ RecvsDefD * Sourcetype * Reactance, df, robust = T)
# Interventiontype x NosvsSomeD x Reactance interaction, with mean and median centering of Reactance -----------------------
DonatedGLM <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance, df)
DonatedRGLM <- coeftest(DonatedGLM, vcov = vcovHC(DonatedGLM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
DonatedGLMM <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance, df)
DonatedRGLMM <- coeftest(DonatedGLMM, vcov = vcovHC(DonatedGLMM, "HC1"))
#DonatedRGLMMma <- logitmfx(formula = Donated ~ RecvsDefD * NosvsSomeD * Reactance, data = df, robust = T)

# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
DonatedGLMMM <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance, df)
DonatedRGLMMM <- coeftest(DonatedGLMMM, vcov = vcovHC(DonatedGLMMM, "HC1"))
# Put into stargazer
stargazer(DonatedRGLM, DonatedRGLMM, DonatedRGLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DonatedGLM, DonatedGLMM, DonatedGLMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Interventiontype x Sourcetype x Reactance interaction with more predictors -------------------------
# Predictor 1: EAI
DonatedGLM <- glm(Donated ~ RecvsDefD * Sourcetype * Reactance + EAI, df, family = "binomial")
DonatedRGLM <- coeftest(DonatedGLM, vcov = vcovHC(DonatedGLM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
DonatedGLMM <- glm(Donated ~ RecvsDefD * Sourcetype * Reactance + EAI, df, family = "binomial")
DonatedRGLMM <- coeftest(DonatedGLMM, vcov = vcovHC(DonatedGLMM, "HC1"))
# DonatedRGLMMma <- logitmfx(formula = Donated ~ RecvsDefD * Sourcetype * Reactance + EAI, data = df, robust = T)
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
DonatedGLMMM <- glm(Donated ~ RecvsDefD * Sourcetype * Reactance + EAI, df, family = "binomial")
DonatedRGLMMM <- coeftest(DonatedGLMMM, vcov = vcovHC(DonatedGLMMM, "HC1"))
stargazer(DonatedRGLM, DonatedRGLMM, DonatedRGLMMM, type="html", style = "aer", title = "Robust Logit", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DonatedGLM, DonatedGLMM, DonatedGLMMM, type="html", style = "aer", title = "Non-robust Logit", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Predictor 2: moralD
DonatedGLM <- glm(Donated ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD, df, family = "binomial")
DonatedRGLM <- coeftest(DonatedGLM, vcov = vcovHC(DonatedGLM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
DonatedGLMM <- glm(Donated ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD, df, family = "binomial")
DonatedRGLMM <- coeftest(DonatedGLMM, vcov = vcovHC(DonatedGLMM, "HC1"))
#DonatedRGLMMma <- logitmfx(formula = Donated ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD, data = df, robust = T)
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
DonatedGLMMM <- glm(Donated ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD, df, family = "binomial")
DonatedRGLMMM <- coeftest(DonatedGLMMM, vcov = vcovHC(DonatedGLMMM, "HC1"))
stargazer(DonatedRGLM, DonatedRGLMM, DonatedRGLMMM, type="html", style = "aer", title = "Robust Logit", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DonatedGLM, DonatedGLMM, DonatedGLMMM, type="html", style = "aer", title = "Non-robust Logit", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Predictor 3: retireEffD
DonatedGLM <- glm(Donated ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD, df, family = "binomial")
DonatedRGLM <- coeftest(DonatedGLM, vcov = vcovHC(DonatedGLM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
DonatedGLMM <- glm(Donated ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD, df, family = "binomial")
DonatedRGLMM <- coeftest(DonatedGLMM, vcov = vcovHC(DonatedGLMM, "HC1"))
#DonatedRGLMMma <- logitmfx(formula = Donated ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD, data = df, robust = T)
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
DonatedGLMMM <- glm(Donated ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD, df, family = "binomial")
DonatedRGLMMM <- coeftest(DonatedGLMMM, vcov = vcovHC(DonatedGLMMM, "HC1"))
stargazer(DonatedRGLM, DonatedRGLMM, DonatedRGLMMM, type="html", style = "aer", title = "Robust Logit", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DonatedGLM, DonatedGLMM, DonatedGLMMM, type="html", style = "aer", title = "Non-robust Logit", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Predictor 4: demographics (gender)
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance
DonatedGLM <- glm(Donated ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender, dfsub, family = "binomial")
DonatedRGLM <- coeftest(DonatedGLM, vcov = vcovHC(DonatedGLM, "HC1"))
# center around mean of the Reactance variable
dfsub$Reactance <- dfsub$Reactance - mean(dfsub$Reactance)
DonatedGLMM <- glm(Donated ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender, dfsub, family = "binomial")
DonatedRGLMM <- coeftest(DonatedGLMM, vcov = vcovHC(DonatedGLMM, "HC1"))
#DonatedRGLMMma <- logitmfx(formula = Donated ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender, data = dfsub, robust = T)
# center around median of the Reactance variable
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv)
dfsub$Reactance <- dfsub$Reactance - median(dfsub$Reactance)
DonatedGLMMM <- glm(Donated ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender, dfsub, family = "binomial")
DonatedRGLMMM <- coeftest(DonatedGLMMM, vcov = vcovHC(DonatedGLMMM, "HC1"))
stargazer(DonatedRGLM, DonatedRGLMM, DonatedRGLMMM, type="html", style = "aer", title = "Robust Logit", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DonatedGLM, DonatedGLMM, DonatedGLMMM, type="html", style = "aer", title = "Non-robust Logit", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance

# Predictor 5: beliefs
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance
DonatedGLM <- glm(Donated ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender + belief, dfsub, family = "binomial")
DonatedRGLM <- coeftest(DonatedGLM, vcov = vcovHC(DonatedGLM, "HC1"))
# center around mean of the Reactance variable
dfsub$Reactance <- dfsub$Reactance - mean(dfsub$Reactance)
DonatedGLMM <- glm(Donated ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender + belief, dfsub, family = "binomial")
DonatedRGLMM <- coeftest(DonatedGLMM, vcov = vcovHC(DonatedGLMM, "HC1"))
#DonatedRGLMMma <- logitmfx(formula = Donated ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender + belief, data = dfsub, robust = T)
# center around median of the Reactance variable
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv)
dfsub$Reactance <- dfsub$Reactance - median(dfsub$Reactance)
DonatedGLMMM <- glm(Donated ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender + belief, dfsub, family = "binomial")
DonatedRGLMMM <- coeftest(DonatedGLMMM, vcov = vcovHC(DonatedGLMMM, "HC1"))
stargazer(DonatedRGLM, DonatedRGLMM, DonatedRGLMMM, type="html", style = "aer", title = "Robust Logit", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DonatedGLM, DonatedGLMM, DonatedGLMMM, type="html", style = "aer", title = "Non-robust Logit", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance

# Interventiontype x NosvsSomeD x Reactance interaction with more predictors -------------------------
# Predictor 1: EAI
DonatedGLM <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance + EAI, df)
DonatedRGLM <- coeftest(DonatedGLM, vcov = vcovHC(DonatedGLM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
DonatedGLMM <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance + EAI, df)
DonatedRGLMM <- coeftest(DonatedGLMM, vcov = vcovHC(DonatedGLMM, "HC1"))
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
DonatedGLMMM <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance + EAI, df)
DonatedRGLMMM <- coeftest(DonatedGLMMM, vcov = vcovHC(DonatedGLMMM, "HC1"))
stargazer(DonatedRGLM, DonatedRGLMM, DonatedRGLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DonatedGLM, DonatedGLMM, DonatedGLMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Predictor 2: moralD
DonatedGLM <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD, df)
DonatedRGLM <- coeftest(DonatedGLM, vcov = vcovHC(DonatedGLM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
DonatedGLMM <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD, df)
DonatedRGLMM <- coeftest(DonatedGLMM, vcov = vcovHC(DonatedGLMM, "HC1"))
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
DonatedGLMMM <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD, df)
DonatedRGLMMM <- coeftest(DonatedGLMMM, vcov = vcovHC(DonatedGLMMM, "HC1"))
stargazer(DonatedRGLM, DonatedRGLMM, DonatedRGLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DonatedGLM, DonatedGLMM, DonatedGLMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Predictor 3: retireEffD
DonatedGLM <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD, df)
DonatedRGLM <- coeftest(DonatedGLM, vcov = vcovHC(DonatedGLM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
DonatedGLMM <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD, df)
DonatedRGLMM <- coeftest(DonatedGLMM, vcov = vcovHC(DonatedGLMM, "HC1"))
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
DonatedGLMMM <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD, df)
DonatedRGLMMM <- coeftest(DonatedGLMMM, vcov = vcovHC(DonatedGLMMM, "HC1"))
stargazer(DonatedRGLM, DonatedRGLMM, DonatedRGLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DonatedGLM, DonatedGLMM, DonatedGLMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Predictor 4: demographics
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance
DonatedGLM <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD + gender, dfsub)
DonatedRGLM <- coeftest(DonatedGLM, vcov = vcovHC(DonatedGLM, "HC1"))
# center around mean of the Reactance variable
dfsub$Reactance <- dfsub$Reactance - mean(dfsub$Reactance)
DonatedGLMM <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD + gender, dfsub)
DonatedRGLMM <- coeftest(DonatedGLMM, vcov = vcovHC(DonatedGLMM, "HC1"))
# center around median of the Reactance variable
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv)
dfsub$Reactance <- dfsub$Reactance - median(dfsub$Reactance)
DonatedGLMMM <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD + gender, dfsub)
DonatedRGLMMM <- coeftest(DonatedGLMMM, vcov = vcovHC(DonatedGLMMM, "HC1"))
stargazer(DonatedRGLM, DonatedRGLMM, DonatedRGLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DonatedGLM, DonatedGLMM, DonatedGLMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance

# Predictor 5: beliefs
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance
DonatedGLM <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD + gender + belief, dfsub)
DonatedRGLM <- coeftest(DonatedGLM, vcov = vcovHC(DonatedGLM, "HC1"))
# center around mean of the Reactance variable
dfsub$Reactance <- dfsub$Reactance - mean(dfsub$Reactance)
DonatedGLMM <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD + gender + belief, dfsub)
DonatedRGLMM <- coeftest(DonatedGLMM, vcov = vcovHC(DonatedGLMM, "HC1"))
# center around median of the Reactance variable
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv)
dfsub$Reactance <- dfsub$Reactance - median(dfsub$Reactance)
DonatedGLMMM <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD + gender + belief, dfsub)
DonatedRGLMMM <- coeftest(DonatedGLMMM, vcov = vcovHC(DonatedGLMMM, "HC1"))
stargazer(DonatedRGLM, DonatedRGLMM, DonatedRGLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(DonatedGLM, DonatedGLMM, DonatedGLMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance

# TWO-STEP (Intensive margin, because extensive does not change) ---------------------

## Interventiontype x Sourcetype x Reactance interaction, with mean and median centering of Reactance -----------------------
Distno5LM <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance, df)
Distno5RLM <- coeftest(Distno5LM, vcov = vcovHC(Distno5LM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
Distno5LMM <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance, df)
Distno5RLMM <- coeftest(Distno5LMM, vcov = vcovHC(Distno5LMM, "HC1"))
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
Distno5LMMM <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance, df)
Distno5RLMMM <- coeftest(Distno5LMMM, vcov = vcovHC(Distno5LMMM, "HC1"))
# Put into stargazer
stargazer(Distno5RLM, Distno5RLMM, Distno5RLMMM, type="html", style = "aer", title = "Robust conditional OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(Distno5LM, Distno5LMM, Distno5LMMM, type="html", style = "aer", title = "Non-robust conditional OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

## Interventiontype x NosvsSomeD x Reactance interaction, with mean and median centering of Reactance -----------------------
Distno5LM <- lm(Distno5 ~ RecvsDefD * NosvsSomeD * Reactance, df)
Distno5RLM <- coeftest(Distno5LM, vcov = vcovHC(Distno5LM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
Distno5LMM <- lm(Distno5 ~ RecvsDefD * NosvsSomeD * Reactance, df)
Distno5RLMM <- coeftest(Distno5LMM, vcov = vcovHC(Distno5LMM, "HC1"))
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
Distno5LMMM <- lm(Distno5 ~ RecvsDefD * NosvsSomeD * Reactance, df)
Distno5RLMMM <- coeftest(Distno5LMMM, vcov = vcovHC(Distno5LMMM, "HC1"))
# Put into stargazer
stargazer(Distno5RLM, Distno5RLMM, Distno5RLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(Distno5LM, Distno5LMM, Distno5LMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

## Interventiontype x Sourcetype x Reactance interaction with more predictors -------------------------
# Predictor 1: EAI
Distno5LM <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance + EAI, df)
Distno5RLM <- coeftest(Distno5LM, vcov = vcovHC(Distno5LM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
Distno5LMM <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance + EAI, df)
Distno5RLMM <- coeftest(Distno5LMM, vcov = vcovHC(Distno5LMM, "HC1"))
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
Distno5LMMM <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance + EAI, df)
Distno5RLMMM <- coeftest(Distno5LMMM, vcov = vcovHC(Distno5LMMM, "HC1"))
stargazer(Distno5RLM, Distno5RLMM, Distno5RLMMM, type="html", style = "aer", title = "Robust conditional OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(Distno5LM, Distno5LMM, Distno5LMMM, type="html", style = "aer", title = "Non-robust conditional OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Predictor 2: moralD
Distno5LM <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD, df)
Distno5RLM <- coeftest(Distno5LM, vcov = vcovHC(Distno5LM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
Distno5LMM <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD, df)
Distno5RLMM <- coeftest(Distno5LMM, vcov = vcovHC(Distno5LMM, "HC1"))
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
Distno5LMMM <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD, df)
Distno5RLMMM <- coeftest(Distno5LMMM, vcov = vcovHC(Distno5LMMM, "HC1"))
stargazer(Distno5RLM, Distno5RLMM, Distno5RLMMM, type="html", style = "aer", title = "Robust conditional OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(Distno5LM, Distno5LMM, Distno5LMMM, type="html", style = "aer", title = "Non-robust conditional OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Predictor 3: retireEffD
Distno5LM <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD, df)
Distno5RLM <- coeftest(Distno5LM, vcov = vcovHC(Distno5LM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
Distno5LMM <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD, df)
Distno5RLMM <- coeftest(Distno5LMM, vcov = vcovHC(Distno5LMM, "HC1"))
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
Distno5LMMM <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD, df)
Distno5RLMMM <- coeftest(Distno5LMMM, vcov = vcovHC(Distno5LMMM, "HC1"))
stargazer(Distno5RLM, Distno5RLMM, Distno5RLMMM, type="html", style = "aer", title = "Robust conditional OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(Distno5LM, Distno5LMM, Distno5LMMM, type="html", style = "aer", title = "Non-robust conditional OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Predictor 4: demographics
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance
Distno5LM <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender, dfsub)
Distno5RLM <- coeftest(Distno5LM, vcov = vcovHC(Distno5LM, "HC1"))
# center around mean of the Reactance variable
dfsub$Reactance <- dfsub$Reactance - mean(dfsub$Reactance)
Distno5LMM <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender, dfsub)
Distno5RLMM <- coeftest(Distno5LMM, vcov = vcovHC(Distno5LMM, "HC1"))
# center around median of the Reactance variable
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv)
dfsub$Reactance <- dfsub$Reactance - median(dfsub$Reactance)
Distno5LMMM <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender, dfsub)
Distno5RLMMM <- coeftest(Distno5LMMM, vcov = vcovHC(Distno5LMMM, "HC1"))
stargazer(Distno5RLM, Distno5RLMM, Distno5RLMMM, type="html", style = "aer", title = "Robust conditional OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(Distno5LM, Distno5LMM, Distno5LMMM, type="html", style = "aer", title = "Non-robust conditional OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance

# Predictor 5: beliefs
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance
Distno5LM <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender + belief, dfsub)
Distno5RLM <- coeftest(Distno5LM, vcov = vcovHC(Distno5LM, "HC1"))
# center around mean of the Reactance variable
dfsub$Reactance <- dfsub$Reactance - mean(dfsub$Reactance)
Distno5LMM <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender + belief, dfsub)
Distno5RLMM <- coeftest(Distno5LMM, vcov = vcovHC(Distno5LMM, "HC1"))
# center around median of the Reactance variable
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv)
dfsub$Reactance <- dfsub$Reactance - median(dfsub$Reactance)
Distno5LMMM <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender + belief, dfsub)
Distno5RLMMM <- coeftest(Distno5LMMM, vcov = vcovHC(Distno5LMMM, "HC1"))
stargazer(Distno5RLM, Distno5RLMM, Distno5RLMMM, type="html", style = "aer", title = "Robust conditional OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(Distno5LM, Distno5LMM, Distno5LMMM, type="html", style = "aer", title = "Non-robust conditional OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance

## Interventiontype x NosvsSomeD x Reactance interaction with more predictors -------------------------
# Predictor 1: EAI
Distno5LM <- lm(Distno5 ~ RecvsDefD * NosvsSomeD * Reactance + EAI, df)
Distno5RLM <- coeftest(Distno5LM, vcov = vcovHC(Distno5LM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
Distno5LMM <- lm(Distno5 ~ RecvsDefD * NosvsSomeD * Reactance + EAI, df)
Distno5RLMM <- coeftest(Distno5LMM, vcov = vcovHC(Distno5LMM, "HC1"))
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
Distno5LMMM <- lm(Distno5 ~ RecvsDefD * NosvsSomeD * Reactance + EAI, df)
Distno5RLMMM <- coeftest(Distno5LMMM, vcov = vcovHC(Distno5LMMM, "HC1"))
stargazer(Distno5RLM, Distno5RLMM, Distno5RLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(Distno5LM, Distno5LMM, Distno5LMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Predictor 2: moralD
Distno5LM <- lm(Distno5 ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD, df)
Distno5RLM <- coeftest(Distno5LM, vcov = vcovHC(Distno5LM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
Distno5LMM <- lm(Distno5 ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD, df)
Distno5RLMM <- coeftest(Distno5LMM, vcov = vcovHC(Distno5LMM, "HC1"))
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
Distno5LMMM <- lm(Distno5 ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD, df)
Distno5RLMMM <- coeftest(Distno5LMMM, vcov = vcovHC(Distno5LMMM, "HC1"))
stargazer(Distno5RLM, Distno5RLMM, Distno5RLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(Distno5LM, Distno5LMM, Distno5LMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Predictor 3: retireEffD
Distno5LM <- lm(Distno5 ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD, df)
Distno5RLM <- coeftest(Distno5LM, vcov = vcovHC(Distno5LM, "HC1"))
# center around mean of the Reactance variable
df$Reactance <- df$Reactance - mean(df$Reactance)
Distno5LMM <- lm(Distno5 ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD, df)
Distno5RLMM <- coeftest(Distno5LMM, vcov = vcovHC(Distno5LMM, "HC1"))
# center around median of the Reactance variable
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)
df$Reactance <- df$Reactance - median(df$Reactance)
Distno5LMMM <- lm(Distno5 ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD, df)
Distno5RLMMM <- coeftest(Distno5LMMM, vcov = vcovHC(Distno5LMMM, "HC1"))
stargazer(Distno5RLM, Distno5RLMM, Distno5RLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(Distno5LM, Distno5LMM, Distno5LMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance

# Predictor 4: demographics
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance
Distno5LM <- lm(Distno5 ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD + gender, dfsub)
Distno5RLM <- coeftest(Distno5LM, vcov = vcovHC(Distno5LM, "HC1"))
# center around mean of the Reactance variable
dfsub$Reactance <- dfsub$Reactance - mean(dfsub$Reactance)
Distno5LMM <- lm(Distno5 ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD + gender, dfsub)
Distno5RLMM <- coeftest(Distno5LMM, vcov = vcovHC(Distno5LMM, "HC1"))
# center around median of the Reactance variable
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv)
dfsub$Reactance <- dfsub$Reactance - median(dfsub$Reactance)
Distno5LMMM <- lm(Distno5 ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD + gender, dfsub)
Distno5RLMMM <- coeftest(Distno5LMMM, vcov = vcovHC(Distno5LMMM, "HC1"))
stargazer(Distno5RLM, Distno5RLMM, Distno5RLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(Distno5LM, Distno5LMM, Distno5LMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance

# Predictor 5: beliefs
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance
Distno5LM <- lm(Distno5 ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD + gender + belief, dfsub)
Distno5RLM <- coeftest(Distno5LM, vcov = vcovHC(Distno5LM, "HC1"))
# center around mean of the Reactance variable
dfsub$Reactance <- dfsub$Reactance - mean(dfsub$Reactance)
Distno5LMM <- lm(Distno5 ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD + gender + belief, dfsub)
Distno5RLMM <- coeftest(Distno5LMM, vcov = vcovHC(Distno5LMM, "HC1"))
# center around median of the Reactance variable
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv)
dfsub$Reactance <- dfsub$Reactance - median(dfsub$Reactance)
Distno5LMMM <- lm(Distno5 ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD + gender + belief, dfsub)
Distno5RLMMM <- coeftest(Distno5LMMM, vcov = vcovHC(Distno5LMMM, "HC1"))
stargazer(Distno5RLM, Distno5RLMM, Distno5RLMMM, type="html", style = "aer", title = "Robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
stargazer(Distno5LM, Distno5LMM, Distno5LMMM, type="html", style = "aer", title = "Non-robust OLS", column.labels = c("Metric Reactance", "Mean centered Reactance", "Median centered Reactance"))
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance

## Graphical representation of interactions --------------
### Interventiontype x Sourcetype x Reactance interaction, with mean and median centering of Reactance ---------
# http://www.statsblogs.com/2013/08/27/creating-marginal-effect-plots-for-linear-regression-models-in-r/
#df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv) # remember the correctly recode Readctance
#df$Reactance <- df$Reactance - mean(df$Reactance)
# estimate the model with a product term (ONLY LINEAR MODELS)
#m <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance, df)

dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance
dfsub$Reactance <- dfsub$Reactance - mean(dfsub$Reactance)
m <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender, dfsub)
# pull out coefficient estimates
beta.hat <- coef(m) 

# pull out the covariance matrix
cov <- vcov(m)

# a set of values of Reactance to compute the (instantaneous) 
# effect of Intervention type (RecvsDefD) 
z0 <- seq(min(df$Reactance), max(df$Reactance))

# calculate the instantaneous effect of Interventiontype as Reactance varies
dy.dx <- beta.hat["RecvsDefDDef"] + beta.hat["RecvsDefDDef:Reactance"]*z0

# calculate the standard error of each estimated effect
se.dy.dx <- sqrt(cov["RecvsDefDDef", "RecvsDefDDef"] + z0^2*cov["RecvsDefDDef:Reactance", "RecvsDefDDef:Reactance"] + 2*z0*cov["RecvsDefDDef", "RecvsDefDDef:Reactance"])

# calculate upper and lower bounds of a 90% CI
upr <- dy.dx + 1.64*se.dy.dx
lwr <- dy.dx - 1.64*se.dy.dx
upr95 <- dy.dx + 1.96*se.dy.dx
lwr95 <- dy.dx - 1.96*se.dy.dx

# combine to data.frame in order to use ggplot
rm(dat)
dat <- cbind(z0, dy.dx)
dat <- as.data.frame(dat)
dat <- cbind(dat, lwr)
dat <- cbind(dat, upr)
dat <- cbind(dat, upr95)
dat <- cbind(dat, lwr95)

# Plot with 90% & 95% confidence intervals
ggplot(data = dat, aes(x = z0, y = dy.dx)) +
  scale_x_continuous(breaks = -4:6) +
  scale_y_continuous(breaks = -3:5) +
  geom_line(size = 1) +
  xlab(expression(Reactance)) +
  ylab(expression(frac(partialdiff*Distno5, partialdiff*IntType))) +
  geom_path(data = dat, aes(z0, upr95), linetype = "dotdash", color = "blue") +
  geom_path(data = dat, aes(z0, lwr95), linetype = "dotdash", color = "blue") +
  geom_path(data = dat, aes(z0, upr), linetype = "dotdash", color = "red") +
  geom_path(data = dat, aes(z0, lwr), linetype = "dotdash", color = "red") +
  ggtitle(expression(atop("Interaction of Intervention type and Reactance scale", atop(italic("with 90% & 95% confidence intervals"),""))))

# short and easy (but less dynamic) version
interplot(m, var1 = "RecvsDefD", var2 = "Reactance", plot = TRUE, hist = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed")

# Sourcetype x Reactance
dfsub$Reactance <- (dfsub$EmotionResp + dfsub$ReactCompl + dfsub$ResistInfl + dfsub$ReactAdv) # remember the correctly recode Readctance
dfsub$Reactance <- dfsub$Reactance - mean(dfsub$Reactance)
m <- lm(Distno5 ~ RecvsDefD * Sourcetype * Reactance + EAI + moralD + retireEffD + gender, dfsub)
interplot(m, var1 = "Sourcetype", var2 = "Reactance", plot = TRUE, hist = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed")





# TWO-STEP with left-censored OLS ----------------

# TWO-STEP Model (Hurdle, Heckman) - allowes different predictors ---------------
# choosing different predictors for the respective model part (logit, OLS) should be guided by
# theory! 