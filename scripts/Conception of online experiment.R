###############################################################
##### Reduced dataset for conception of online experiment #####
###############################################################

dfonline <- df[df$Sourcetype != "NameAndPicture" & df$Sourcetype != "Partisan" | df$treatment == "Control",]
dfonline$Sourcetype <- droplevels(dfonline$Sourcetype)

## Treatment differences
ggplot(data = dfonline, aes(x = treatment, y = Donation)) +
  stat_boxplot(geom ='errorbar', width = 0.5) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour="darkred", geom="point", shape=18, size=3) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8,9,10)) +
  labs(x = "Experimental group", y='Contribution [in €]') +
  theme(legend.position="none",
        plot.title = element_text(angle = 45, vjust=.5,size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

kruskal.test(dfonline$Donation ~ dfonline$treatment) # n.s.
pairwise.wilcox.test(df$Donation, df$treatment, p.adjust.method = "none", exact = FALSE)
pairwise.wilcox.test(dfonline$Donation, dfonline$treatment, p.adjust.method = "none", exact = FALSE)

summary(lm(Donation ~ treatment, dfonline))
summary(lm(Donation ~ RecvsDef*Sourcetype*ReactanceM, dfonline))

df$ParvsOther <- ifelse(df$Sourcetype != "Partisan", 0, 1)
df$ParvsOther <- factor(df$ParvsOther, levels = c(0,1), labels = c("No Party", "Party"))

summary(lm(Donation ~ treatment*green, df))


ggplot(data = df, aes(x = ReactD, y = Donation)) +
  stat_boxplot(geom ='errorbar', width = 0.5) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour="darkred", geom="point", shape=18, size=3) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8,9,10)) +
  labs(x = "Experimental group", y='Contribution [in €]') +
  theme(legend.position="none",
        plot.title = element_text(angle = 45, vjust=.5,size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(vjust=.5, size=10))

var(df$Donation[df$ReactD == "BelowEq Med React"]) # within cluster variance
var(df$Donation[df$ReactD == "Above Med React"]) # within cluster variance
# between cluster variance?
var(df$Donation[df$ReactD == "BelowEq Med React"], df$Donation[df$ReactD == "Above Med React"], na.rm = TRUE, use = "everything")
length(df$Donation[df$ReactD == "Above Med React"])
sd(df$Donation[df$treatment == "Control" | df$treatment == "DefNos"])
