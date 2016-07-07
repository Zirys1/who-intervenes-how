## Create regression table for power point
# Distance Regressions (Logistic, OLS Dist, OLS conditional Dist) ----
log_nvs_r <- glm(Donated ~ RecvsDefD * NosvsSomeD * ReactanceM, df, family = "binomial")
log_nvs_rr <- coeftest(log_nvs_r, vcov = vcovHC(log_nvs_r, "HC1"))
log_nvs_ur <- glm(Donated ~ RecvsDefD * NosvsSomeD * Reactance + EAI + moralD + retireEffD + genderB + green, df, family = "binomial")
log_nvs_urr <- coeftest(log_nvs_ur, vcov = vcovHC(log_nvs_ur, "HC1"))
log_st_r <- glm(Donated ~ RecvsDefD * Sourcetype * ReactanceM, df, family = "binomial")
log_st_rr <- coeftest(log_st_r, vcov = vcovHC(log_st_r, "HC1"))
log_st_ur <- glm(Donated ~ RecvsDefD * Sourcetype * ReactanceM + EAI + moralD + retireEffD + genderB + green, df, family = "binomial")
log_st_urr <- coeftest(log_st_ur, vcov = vcovHC(log_st_ur, "HC1"))

ols_nvs_r <- lm(Dist ~ RecvsDefD * NosvsSomeD * ReactanceM, df)
ols_nvs_rr <- coeftest(ols_nvs_r, vcov = vcovHC(ols_nvs_r, "HC1"))
ols_nvs_ur <- lm(Dist ~ RecvsDefD * NosvsSomeD * ReactanceM + EAI + moralD + retireEffD + genderB + green, df)
ols_nvs_urr <- coeftest(ols_nvs_ur, vcov = vcovHC(ols_nvs_ur, "HC1"))
ols_st_r <- lm(Dist ~ RecvsDefD * Sourcetype * ReactanceM, df)
ols_st_rr <- coeftest(ols_st_r, vcov = vcovHC(ols_st_r, "HC1"))
ols_st_ur <-lm(Dist ~ RecvsDefD * Sourcetype * ReactanceM + EAI + moralD + retireEffD + genderB + green, df)
ols_st_urr <- coeftest(ols_st_ur, vcov = vcovHC(ols_st_ur, "HC1"))

cols_nvs_r <- lm(Distno5 ~ RecvsDefD * NosvsSomeD * ReactanceM, df)
cols_nvs_rr <- coeftest(cols_nvs_r, vcov = vcovHC(cols_nvs_r, "HC1"))
cols_nvs_ur <-lm(Distno5 ~ RecvsDefD * NosvsSomeD * ReactanceM + EAI + moralD + retireEffD + genderB + green, df)
cols_nvs_urr <- coeftest(cols_nvs_ur, vcov = vcovHC(cols_nvs_ur, "HC1"))
cols_st_r <- lm(Distno5 ~ RecvsDefD * Sourcetype * ReactanceM, df)
cols_st_rr <- coeftest(cols_st_r, vcov = vcovHC(cols_st_r, "HC1"))
cols_st_ur <- lm(Distno5 ~ RecvsDefD * Sourcetype * ReactanceM + EAI + moralD + retireEffD + genderB + green, df)
cols_st_urr <- coeftest(cols_st_ur, vcov = vcovHC(cols_st_ur, "HC1"))

# Donation Regressions (OLS Donation, OLS conditional Donation) ----
# in order to check whether the coefficients more or less match those of Dist
dols_nvs_r <- lm(Donation ~ RecvsDefD * NosvsSomeD * ReactanceM, df)
dols_nvs_rr <- coeftest(dols_nvs_r, vcov = vcovHC(dols_nvs_r, "HC1"))
dols_nvs_ur <- lm(Donation ~ RecvsDefD * NosvsSomeD * ReactanceM + EAI + moralD + retireEffD + genderB + green, df)
dols_nvs_urr <- coeftest(dols_nvs_ur, vcov = vcovHC(dols_nvs_ur, "HC1"))
dols_st_r <- lm(Donation ~ RecvsDefD * Sourcetype * ReactanceM, df)
dols_st_rr <- coeftest(dols_st_r, vcov = vcovHC(dols_st_r, "HC1"))
dols_st_ur <-lm(Donation ~ RecvsDefD * Sourcetype * ReactanceM + EAI + moralD + retireEffD + genderB + green, df)
dols_st_urr <- coeftest(dols_st_ur, vcov = vcovHC(dols_st_ur, "HC1"))

dcols_nvs_r <- lm(Donationno0 ~ RecvsDefD * NosvsSomeD * ReactanceM, df)
dcols_nvs_rr <- coeftest(dcols_nvs_r, vcov = vcovHC(dcols_nvs_r, "HC1"))
dcols_nvs_ur <-lm(Donationno0 ~ RecvsDefD * NosvsSomeD * ReactanceM + EAI + moralD + retireEffD + genderB + green, df)
dcols_nvs_urr <- coeftest(dcols_nvs_ur, vcov = vcovHC(dcols_nvs_ur, "HC1"))
dcols_st_r <- lm(Donationno0 ~ RecvsDefD * Sourcetype * ReactanceM, df)
dcols_st_rr <- coeftest(dcols_st_r, vcov = vcovHC(dcols_st_r, "HC1"))
dcols_st_ur <- lm(Donationno0 ~ RecvsDefD * Sourcetype * ReactanceM + EAI + moralD + retireEffD + genderB + green, df)
dcols_st_urr <- coeftest(dcols_st_ur, vcov = vcovHC(dcols_st_ur, "HC1"))

# Stargazer Output ----
stargazer(log_nvs_r, log_nvs_ur, log_st_r, log_st_ur, ols_nvs_r, ols_nvs_ur, ols_st_r,
          ols_st_ur, cols_nvs_r, cols_nvs_ur, cols_st_r, cols_st_ur, 
          style = "aer", type = "html",
          column.labels = c("Some Source", "Sourcetype", "Some Source", "Sourcetype", "Some Source", "Sourcetype"),
          column.separate = c(2, 2, 2, 2, 2, 2),
          covariate.labels = c("Default", "Some Source", "Name and Picture", "Knowledgeable",
                               "Political", "Partisan", "Reactance (centered)", "EAI",
                               "Moral duty", "Retirement efficient", "Female", "Green party",
                               "Default x Some Source", "Default x Name and Picture",
                               "Default x Knowledgeable", "Default x x Political", "Default x Partisan",
                               "Default x Reactance", "Some Source x Reactance", "Default x Some Source x Reactance",
                               "Name and Picture x Reactance", "Knowledgeable x Reactance", "Political x Reactance",
                               "Partisan x Reactance", "Default x Name and Picture x Reactance", "Default x Knowledgeable x Reactance",
                               "Default x Political x Reactance", "Default x Partisan x Reactance"),
          dep.var.labels = c("Donated", "Distance to Recommendation/Default", "Distance to Recommendation/Default of those that donated"))

## only unrestricted
stargazer(log_nvs_ur, log_st_ur, ols_nvs_ur,
          ols_st_ur, cols_nvs_ur, cols_st_ur, 
          style = "aer", type = "html",
          column.labels = c("Some Source", "Sourcetype", "Some Source", "Sourcetype", "Some Source", "Sourcetype"),
          covariate.labels = c("Default", "Some Source", "Name and Picture", "Knowledgeable",
                               "Political", "Partisan", "Reactance (centered)", "EAI",
                               "Moral duty", "Retirement efficient", "Female", "Green party",
                               "Default x Some Source", "Default x Name and Picture",
                               "Default x Knowledgeable", "Default x x Political", "Default x Partisan",
                               "Default x Reactance", "Some Source x Reactance", "Default x Some Source x Reactance",
                               "Name and Picture x Reactance", "Knowledgeable x Reactance", "Political x Reactance",
                               "Partisan x Reactance", "Default x Name and Picture x Reactance", "Default x Knowledgeable x Reactance",
                               "Default x Political x Reactance", "Default x Partisan x Reactance"),
          dep.var.labels = c("Donated", "Distance to Recommendation/Default", "Distance to Recommendation/Default of those that donated"))

## only unrestricted and NosvsSomeD
stargazer(log_nvs_ur, ols_nvs_ur,
          cols_nvs_ur, 
          style = "aer", type = "html",
          covariate.labels = c("Default", "Some Source", "Reactance (centered)", "EAI",
                               "Moral duty", "Retirement efficient", "Female", "Green party",
                               "Default x Some Source", "Default x Reactance",
                               "Some Source x Reactance", "Default x Some Source x Reactance"),
          dep.var.labels = c("Donated", "Distance to Rec/Def", "Distance to Rec/Def | donated"))

## only unrestricted and NosvsSomeD ROBUST
stargazer(ols_nvs_urr, log_nvs_urr, 
          cols_nvs_urr,
          style = "aer", type = "html",
          covariate.labels = c("Default", "Some Source", "Reactance (centered)", "EAI",
                               "Moral duty", "Retirement efficient", "Female", "Green party",
                               "Default x Some Source", "Default x Reactance",
                               "Some Source x Reactance", "Default x Some Source x Reactance"),
          column.labels = c("Distance" , "Donated", "Distance | Donated"))

# only restricted and NosvsSomeD ROBUST
stargazer(ols_nvs_rr, log_nvs_rr, 
          cols_nvs_rr,
          style = "aer", type = "html",
          covariate.labels = c("Default", "Some Source", "Reactance (centered)",
                               "Default x Some Source", "Default x Reactance",
                               "Some Source x Reactance", "Default x Some Source x Reactance"),
          column.labels = c("Distance" , "Donated", "Distance | Donated"))

# with Donation variable 
stargazer(dols_nvs_urr, dcols_nvs_urr,
          style = "aer", type = "html",
          covariate.labels = c("Default", "Some Source", "Reactance (centered)", "EAI",
                               "Moral duty", "Retirement efficient", "Female", "Green party",
                               "Default x Some Source", "Default x Reactance",
                               "Some Source x Reactance", "Default x Some Source x Reactance"),
          column.labels = c("Distance" , "Donated", "Distance | Donated", "Donation", "Donation | Donated"))

#### Logistic regression with dummy variable default +-2 / +-1
df$Deftwo <- ifelse(df$Dist > 2, 1, 0) 
df$Defone <- ifelse(df$Dist > 3 & df$Dist < 7, 1, 0) 
defone_nvs_ur <- glm(Defone ~ RecvsDefD * NosvsSomeD * ReactanceM + EAI + moralD + retireEffD + genderB + green, df, family = "binomial")
defone_nvs_urr <- coeftest(defone_nvs_ur, vcov = vcovHC(defone_nvs_ur, "HC1"))
deftwo_nvs_ur <- glm(Deftwo ~ RecvsDefD * NosvsSomeD * ReactanceM + EAI + moralD + retireEffD + genderB + green, df, family = "binomial")
deftwo_nvs_urr <- coeftest(deftwo_nvs_ur, vcov = vcovHC(deftwo_nvs_ur, "HC1"))

stargazer(defone_nvs_urr, deftwo_nvs_urr, style = "aer", type = "html")

#### Ordered Logistic regression
ol_nvs_r <- polr(DistOrd ~ RecvsDefD * NosvsSomeD * ReactanceM, df, Hess = T, method = "logistic") 
ol_nvs_ur <- polr(DistOrd ~ RecvsDefD * NosvsSomeD * ReactanceM + EAI + moralD + retireEffD + genderB + green, df, Hess = T, method = "logistic") 

ol_st_r <- polr(DistOrd ~ RecvsDefD * Sourcetype * ReactanceM, df, Hess = T, method = "logistic") 
ol_st_ur <- polr(DistOrd ~ RecvsDefD * Sourcetype * ReactanceM + EAI + moralD + retireEffD + genderB + green, df, Hess = T, method = "logistic") 

stargazer(ol_nvs_ur, style = "aer", type = "html",
covariate.labels = c("Default", "Some Source", "Reactance (centered)", "EAI",
                     "Moral duty", "Retirement efficient", "Female", "Green party",
                     "Default x Some Source", "Default x Reactance",
                     "Some Source x Reactance", "Default x Some Source x Reactance"))

### Interaction plots
## For ols
plot(effect("RecvsDefD:NosvsSomeD:ReactanceM",ols_nvs_ur, xlevels = 12, confidence.level = 0.90), xlab = "Reactance (centered)", ylab = "Distance")
plot(effect("RecvsDefD:NosvsSomeD:ReactanceM",ols_nvs_ur, xlevels = 12, confidence.level = 0.90), xlab = "Reactance (centered)", ylab = "Distance", multiline = T)
# the abovve plots are equivalent to
## margins ib2.RecvsDefDf#NosvsSomeDf, at(ReactanceM=(-4.493976(1)6.506024))
## marginsplot

## For conditional ols
plot(effect("RecvsDefD:NosvsSomeD:ReactanceM",cols_nvs_ur, xlevels = 12, confidence.level = 0.90), xlab = "Reactance (centered)", ylab = "Distance conditional on giving")
plot(effect("RecvsDefD:NosvsSomeD:ReactanceM",cols_nvs_ur, xlevels = 12, confidence.level = 0.90), xlab = "Reactance (centered)", ylab = "Distance conditional on giving", multiline = T)

## For logit
plot(effect("RecvsDefD:NosvsSomeD:ReactanceM",log_nvs_ur, xlevels = 12, confidence.level = 0.90), xlab = "Reactance (centered)", ylab = "Distance")
plot(effect("RecvsDefD:NosvsSomeD:ReactanceM",log_nvs_ur, xlevels = 12, confidence.level = 0.90), xlab = "Reactance (centered)", ylab = "Distance", multiline = T)

## for ordered logit
plot(effect("RecvsDefD:NosvsSomeD:ReactanceM",ol_nvs_ur, xlevels = 12, confidence.level = 0.90), xlab = "Reactance (centered)", ylab = "Distance")
plot(effect("RecvsDefD:NosvsSomeD:ReactanceM",ol_nvs_ur, xlevels = 12, confidence.level = 0.90), xlab = "Reactance (centered)", ylab = "Distance", multiline = T)


### Marginal Interaction effect plots
limits <- c(-7, 7)
breaks <- seq(limits[1], limits[2], by=1)
interplot(ols_nvs_ur, var1 = "NosvsSomeD", var2 = "ReactanceM", plot = TRUE, hist = TRUE) +
  xlab("Reactance (centered)") +
  ylab("Marginal effect") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
 # scale_y_continuous(limits=c(-6,6), breaks=c(-4,4)) +
  ggtitle(expression(atop("Figure X: Marginal interaction effect of Reactance on Distance", atop(italic("for No Source"),""))))

interplot(cols_nvs_ur, var1 = "RecvsDefD", var2 = "ReactanceM", plot = TRUE, hist = TRUE) +
  xlab("Reactance (centered)") +
  ylab("Marginal effect") +
  #scale_y_continuous(limits=c(-5,5), breaks=c(-4,4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ggtitle(expression(atop("Figure X: Marginal interaction effect of Reactance on Distance", atop(italic("for No Source (conditional on giving)"),""))))

interplot(log_nvs_ur, var1 = "RecvsDefD", var2 = "ReactanceM", plot = T, hist = T) +
  xlab("Reactance (centered)") +
  ylab("Marginal effect") +
  #scale_y_continuous(limits=c(-5,5), breaks=c(-4,4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ggtitle(expression(atop("Figure X: Marginal interaction effect of Reactance on Distance", atop(italic("for Defaults (logit model)"),""))))


ggplot(df, aes(RecvsDef, Dist)) +
  facet_grid(~NosvsSome) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour="darkred", geom="point", shape=18, size=3) +
  xlab("Intervention type") +
  ylab("Distance") +
  ggtitle(expression(atop("Figure 2: Box plots of Distance variable by factor levels", atop(italic(""),""))))



## Analysis of robustness of findings
ggplot(df, aes(Reactance, Dist)) +
  facet_grid(RecvsDefD~NosvsSomeD) +
  geom_jitter(shape = 1) +
  #  stat_summary(fun.y = mean, colour="darkred", geom="point", shape=18, size=3) +
  geom_smooth(method = "lm")

### what if leaving out (1) all 0's, (2) all 0's and 5's
dfno5 <- df[df$Dist != 0,]
ggplot(dfno5, aes(Reactance, Dist)) +
  facet_grid(RecvsDefD~NosvsSomeD) +
  geom_jitter(shape = 1) +
  #  stat_summary(fun.y = mean, colour="darkred", geom="point", shape=18, size=3) +
  geom_smooth(method = "lm")
### are interactions here still significant (if at all)

ggplot(dfDon, aes(Reactance, Dist)) +
  facet_grid(RecvsDefD~NosvsSomeD) +
  geom_jitter(shape = 1) +
  #  stat_summary(fun.y = mean, colour="darkred", geom="point", shape=18, size=3) +
  geom_smooth(method = "lm")

dfno55 <- dfDon[dfDon$Dist != 0,]
ggplot(dfno55, aes(Reactance, Dist)) +
  facet_grid(RecvsDefD~NosvsSomeD) +
  geom_jitter(shape = 1) +
  #  stat_summary(fun.y = mean, colour="darkred", geom="point", shape=18, size=3) +
  geom_smooth(method = "lm")



xx <- as.data.frame(effect("RecvsDefD:NosvsSomeD:ReactanceM",ol_nvs_ur, xlevels = 12))
interplot(ol_nvs_ur, var1 = "RecvsDefD", var2 = "ReactanceM", plot = TRUE, hist = TRUE) +
  xlab("Reactance (centered)") +
  ylab("Marginal effect") +
  ggtitle("Figure X: Marginal interaction effect of Reactance on Distance for Defaults")


xx1 <- xx[xx$NosvsSomeD == "Some Source",]
xx2 <- xx[xx$NosvsSomeD == "No Source",]
p <- ggplot() +
 geom_path(data=xx1, aes(x=ReactanceM, y=prob.X0, group = RecvsDefD, color = RecvsDefD), linetype = "dotdash")  +
  geom_path(data=xx2, aes(x=ReactanceM, y=prob.X0, group = RecvsDefD, color = RecvsDefD)) +
  ylab("Fitted values")

## Try for interactive graph but this is not really needed atm, interesting to do
manipulate({
  p <- ggplot() 
  p +  geom_path(data=xx1, aes(x=ReactanceM, y=yscale, group = RecvsDefD, color = RecvsDefD), linetype = "dotdash")  +
 # geom_path(data=xx2, aes(x=ReactanceM, y=prob.X0, group = RecvsDefD, color = RecvsDefD)) +
  ylab("Fitted values")
},
yscale = picker("0" = xx1$prob.X0, "1" = xx1$prob.X1, "2" = xx1$prob.X2, "3" = xx1$prob.X3, "4" = xx1$prob.X4, "5" = xx1$prob.X5, label = "Ologit"))



ols_nvs_ur
# pull out coefficient estimates
beta.hat <- coef(ols_nvs_ur) ## IMPORTANT THIS IS NON ROBUST!!!

# pull out the covariance matrix
cov <- vcov(ols_nvs_ur) ## IMPORTANT THIS IS NON ROBUST!!! I SHOULD CHANGE THE NON ROBUST 
# SE's WITH THE ROBUST SE'S MANUALLY! SHOULD NOT BE TOO DIFFICULT!!!

# a set of values of Reactance to compute the (instantaneous) 
# effect of Intervention type (RecvsDefD) 
z0 <- seq(min(df$ReactanceM), max(df$ReactanceM))

# calculate the instantaneous effect of Interventiontype as Reactance varies
dy.dx <- beta.hat["RecvsDefDDef"] + beta.hat["RecvsDefDDef:ReactanceM"]*z0 # without Reactance main effect
dy.dx1 <- beta.hat["RecvsDefDDef"] + beta.hat["RecvsDefDDef:NosvsSomeDSome Source"] + beta.hat["RecvsDefDDef:ReactanceM"]*z0 + beta.hat["RecvsDefDDef:NosvsSomeDSome Source:ReactanceM"]*z0

# calculate the standard error of each estimated effect (SE NON RUBST SE'S ABOVE)
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
ggplot() +
   geom_line(data = dat, aes(x = z0, y = dy.dx, color = "No Source"), size = 1.5, show.legend = T) +
  geom_line(data = dat, aes(x = z0, y= dy.dx1, color = "Some Source"), size = 1.5, show.legend = T)+
  xlab(expression(Reactance (centered))) +
  ylab(expression(frac(partialdiff*Distance, partialdiff*InterventionType))) +
# geom_path(data = dat, aes(z0, upr95), linetype = "dotdash", color = "blue") +
 #   geom_path(data = dat, aes(z0, lwr95), linetype = "dotdash", color = "blue") +
#  geom_ribbon(data=data,aes(x = z0, ymin=lwr,ymax=upr),alpha=0.3, inherit.aes=FALSE)+
  geom_path(data = dat, aes(z0, upr), linetype = "dotdash", color = "red") +
  geom_path(data = dat, aes(z0, lwr), linetype = "dotdash", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_histogram(data = df, aes(x = ReactanceM, y = (..count../sum(..count..)*2)), binwidth = 0.5, alpha = 0.6) +
  scale_x_continuous(breaks = -4:6) +
  scale_y_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3, 4)) +
  ggtitle(expression(atop("Marginal effect of Instrument type on Distance for Reactance scale", atop(italic("with 90% confidence intervals"),"for No Source and Some Source"))))

### For NosvsSomeD
# pull out coefficient estimates
beta.hat <- coef(cols_nvs_ur) 

# pull out the covariance matrix
cov <- vcov(cols_nvs_ur)

# a set of values of Reactance to compute the (instantaneous) 
# effect of Intervention type (RecvsDefD) 
z0 <- seq(min(df$ReactanceM), max(df$ReactanceM))

# calculate the instantaneous effect of Interventiontype as Reactance varies
dy.dx <- beta.hat["NosvsSomeDSome Source"] + beta.hat["NosvsSomeDSome Source:ReactanceM"]*z0 # without Reactance main effect
dy.dx1 <- beta.hat["NosvsSomeDSome Source"] + beta.hat["RecvsDefDDef:NosvsSomeDSome Source"] + beta.hat["NosvsSomeDSome Source:ReactanceM"]*z0 + beta.hat["RecvsDefDDef:NosvsSomeDSome Source:ReactanceM"]*z0

# calculate the standard error of each estimated effect
se.dy.dx <- sqrt(cov["NosvsSomeDSome Source", "NosvsSomeDSome Source"] + z0^2*cov["NosvsSomeDSome Source:ReactanceM", "NosvsSomeDSome Source:ReactanceM"] + 2*z0*cov["NosvsSomeDSome Source", "NosvsSomeDSome Source:ReactanceM"])
se.dy.dx1 <- sqrt(cov["NosvsSomeDSome Source", "NosvsSomeDSome Source"] + z0^2*cov["NosvsSomeDSome Source:ReactanceM", "NosvsSomeDSome Source:ReactanceM"] + 2*z0*cov["NosvsSomeDSome Source", "NosvsSomeDSome Source:ReactanceM"])

# calculate upper and lower bounds of a 90% CI
upr <- dy.dx + 1.64*se.dy.dx
lwr <- dy.dx - 1.64*se.dy.dx
upr95 <- dy.dx + 1.96*se.dy.dx
lwr95 <- dy.dx - 1.96*se.dy.dx
upr1 <- dy.dx1 + 1.64*se.dy.dx1
lwr1 <- dy.dx1 - 1.64*se.dy.dx1

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
ggplot() +
  scale_x_continuous(breaks = -4:6) +
  scale_y_continuous(breaks = -3:5) +
  geom_line(data = dat, aes(x = z0, y = dy.dx, color = "Recommendation"), size = 1, show.legend = T) +
  geom_line(data = dat, aes(x = z0, y= dy.dx1, color = "Default"), size = 1, show.legend = T)+
  xlab(expression(Reactance (centered))) +
  ylab(expression(frac(partialdiff*Distance, partialdiff*SourceType))) +
  geom_path(data = dat, aes(z0, upr1), linetype = "dotdash", color = "red") +
  geom_path(data = dat, aes(z0, lwr1), linetype = "dotdash", color = "red") +
  #  geom_ribbon(data=data,aes(x = z0, ymin=lwr,ymax=upr),alpha=0.3, inherit.aes=FALSE)+
#  geom_path(data = dat, aes(z0, upr), linetype = "dotdash", color = "blue") +
#  geom_path(data = dat, aes(z0, lwr), linetype = "dotdash", color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_histogram(data = df, aes(x = ReactanceM, y = (..count../sum(..count..)*2)), binwidth = 0.5, alpha = 0.6) +
  ggtitle(expression(atop("Conditional marginal effect of Source type on Distance for Reactance scale", atop(italic("with 90% confidence intervals"),"for Recommendation and Default"))))



### 07062016
### Hypotheses as in the presentation: Rec != Def | Sourcetype
#### Ext margin
chisq.test(df$Donated[df$Sourcetype == "NoSource"], df$RecvsDefD[df$Sourcetype == "NoSource"])
chisq.test(df$Donated[df$Sourcetype == "NameAndPicture"], df$RecvsDefD[df$Sourcetype == "NameAndPicture"])
chisq.test(df$Donated[df$Sourcetype == "Knowledgeable"], df$RecvsDefD[df$Sourcetype == "Knowledgeable"])
chisq.test(df$Donated[df$Sourcetype == "Political"], df$RecvsDefD[df$Sourcetype == "Political"])
chisq.test(df$Donated[df$Sourcetype == "Partisan"], df$RecvsDefD[df$Sourcetype == "Partisan"])

#### Int margin
wilcox.test(df$Dist[df$Sourcetype == "NoSource"] ~ df$RecvsDefD[df$Sourcetype == "NoSource"], exact = F, conf.int = T, conf.level = 0.9)
wilcox.test(df$Dist[df$Sourcetype == "NameAndPicture"] ~ df$RecvsDefD[df$Sourcetype == "NameAndPicture"], exact = F, conf.int = T, conf.level = 0.9)
wilcox.test(df$Dist[df$Sourcetype == "Knowledgeable"] ~ df$RecvsDefD[df$Sourcetype == "Knowledgeable"], exact = F, conf.int = T, conf.level = 0.9)
wilcox.test(df$Dist[df$Sourcetype == "Political"] ~ df$RecvsDefD[df$Sourcetype == "Political"], exact = F, conf.int = T, conf.level = 0.9)
wilcox.test(df$Dist[df$Sourcetype == "Partisan"] ~ df$RecvsDefD[df$Sourcetype == "Partisan"], exact = F, conf.int = T, conf.level = 0.9)
#### Int margin (conditional)
wilcox.test(dfDon$Dist[dfDon$Sourcetype == "NoSource"] ~ dfDon$RecvsDefD[dfDon$Sourcetype == "NoSource"], exact = F, conf.int = T, conf.level = 0.9)
wilcox.test(dfDon$Dist[dfDon$Sourcetype == "NameAndPicture"] ~ dfDon$RecvsDefD[dfDon$Sourcetype == "NameAndPicture"], exact = F, conf.int = T, conf.level = 0.9)
wilcox.test(dfDon$Dist[dfDon$Sourcetype == "Knowledgeable"] ~ dfDon$RecvsDefD[dfDon$Sourcetype == "Knowledgeable"], exact = F, conf.int = T, conf.level = 0.9)
# < .05
wilcox.test(dfDon$Dist[dfDon$Sourcetype == "Political"] ~ dfDon$RecvsDefD[dfDon$Sourcetype == "Political"], exact = F, conf.int = T, conf.level = 0.9)
wilcox.test(dfDon$Dist[dfDon$Sourcetype == "Partisan"] ~ dfDon$RecvsDefD[dfDon$Sourcetype == "Partisan"], exact = F, conf.int = T, conf.level = 0.9)


### Each vs Control
chisq.test(df$Donated, df$DefNosvsC)
chisq.test(df$Donated,df$RecNosvsC)
chisq.test(df$Donated,df$DefNapvsC)
chisq.test(df$Donated,df$RecNapvsC)
chisq.test(df$Donated,df$DefKnovsC)
chisq.test(df$Donated,df$RecKnovsC)
chisq.test(df$Donated,df$DefPolvsC) # <.1
chisq.test(df$Donated,df$RecPolvsC)
chisq.test(df$Donated,df$DefParvsC)
chisq.test(df$Donated,df$RecParvsC)

wilcox.test(df$Dist ~ df$DefNosvsC, exact = F, conf.int = T, conf.level = 0.9)
wilcox.test(df$Dist ~ df$RecNosvsC, exact = F, conf.int = T, conf.level = 0.9)
wilcox.test(df$Dist ~ df$DefNapvsC, exact = F, conf.int = T, conf.level = 0.9)
wilcox.test(df$Dist ~ df$RecNapvsC, exact = F, conf.int = T, conf.level = 0.9)
wilcox.test(df$Dist ~ df$DefKnovsC, exact = F, conf.int = T, conf.level = 0.9)
wilcox.test(df$Dist ~ df$RecKnovsC, exact = F, conf.int = T, conf.level = 0.9)
wilcox.test(df$Dist ~ df$DefPolvsC, exact = F, conf.int = T, conf.level = 0.9)
wilcox.test(df$Dist ~ df$RecPolvsC, exact = F, conf.int = T, conf.level = 0.9)
wilcox.test(df$Dist ~ df$DefParvsC, exact = F, conf.int = T, conf.level = 0.9)
wilcox.test(df$Dist ~ df$RecParvsC, exact = F, conf.int = T, conf.level = 0.9)


wilcox.test(df$Dist[df$NosvsSomeD == "No Source"] ~ df$RecvsDefD[df$NosvsSomeD == "No Source"], exact = FALSE)
wilcox.test(df$Dist[df$NosvsSomeD == "Some Source"] ~ df$RecvsDefD[df$NosvsSomeD == "Some Source"], exact = FALSE)
wilcox.test(df$Dist[df$RecvsDefD == "Rec"] ~ df$NosvsSomeD[df$RecvsDefD == "Rec"], conf.int = T, conf.level = 0.9)
wilcox.test(df$Dist[df$RecvsDefD == "Def"] ~ df$NosvsSomeD[df$RecvsDefD == "Def"])

chisq.test(df$Donated[df$RecvsDefD == "Rec"], df$NosvsSomeD[df$RecvsDefD == "Rec"])
chisq.test(df$Donated[df$RecvsDefD == "Def"], df$NosvsSomeD[df$RecvsDefD == "Def"])


  kruskal.test(df$Dist[df$RecvsDefD == "Def"] ~ df$SourcetypeC[df$RecvsDefD == "Def"])


ks.test(df$Dist[df$treatment == "DefNos"], df$Dist[df$treatment == "RecNos"], exact = F)

ggplot(data = df, aes(x = Donation, color = RecvsDefD)) +
  stat_ecdf() +
  facet_grid(~Sourcetype)

ggplot(data = df, aes(x = Donation)) +
  geom_histogram(binwidth = .1) +
  scale_y_continuous(breaks = c(25, 50, 75, 100, 125, 150)) +
  scale_x_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7))+
  geom_vline(xintercept = 5, linetype = "dashed") +
  xlab("Donation values") +
  ylab("Number of observations")

ggplot(data = df, aes(x = RecvsDef, y = Donation)) +
  facet_grid(~SourcetypeC) +
  geom_boxplot() +
  xlab("Intervention Type by Source Type") +
  ylab("Donation amount")

ggplot(data = df, aes(x = treatment, y = Donation)) +
  geom_boxplot() +
  scale_y_continuous(breaks = c(0, 1,2,3,4,5,6,7)) +
  geom_hline(yintercept = 5, linetype = "dashed") +
  xlab("Treatment") +
  ylab("Donation amount")

ggplot(data = df, aes(x = RecvsDef, y = Dist)) +
  facet_grid(~SourcetypeC) +
  geom_boxplot()

ggplot(data = df, aes(x = Donation)) +
  geom_histogram() +
  facet_grid(RecvsDefD~Sourcetype) +
  xlab("Donation amount") +
  ylab("Overall fraction") 

ggplot(data = df, aes(x = Donation, y = ..count../sum(..count..))) +
  geom_histogram() +
  facet_grid(RecvsDefD~Sourcetype) +
  xlab("Donation amount") +
  ylab("Overall fraction") # do I need treatment specific fractions? AGGREGATE!?
  
ggplot(data = df, aes(x = Donation, y = ..count../sum(..count..))) +
  geom_histogram() +
  xlab("Donation amount") +
  ylab("Overall fraction") # do I need treatment specific fractions?


ggplot(data = df, aes(x = RecvsDef, y = Donation)) +
  geom_boxplot() +
  facet_wrap(~SourcetypeC) +
  xlab("Intervention type") +
  ylab("Donation")

