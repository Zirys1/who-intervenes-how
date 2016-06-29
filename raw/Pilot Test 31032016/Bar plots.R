### bar plots are bad, see: http://sharpstatistics.co.uk/stats/wily-data-analysis/

#### Bar Plot with standard deviation/error/ 95% CI bars ####
df.summary <- data.frame( #creates a data frame with the below information in it
  treatment <- levels(df$treatment), # creates treatment variable
  mean <- tapply(df$Donation, df$treatment, mean), # creates mean donations per treatment
  n <- tapply(df$Donation, df$treatment, length), # creates n per treatment
  sd <- tapply(df$Donation, df$treatment, sd) # creates sd of donations per treatment
)
colnames(df.summary)[1] <- "treatment"
colnames(df.summary)[2] <- "mean"
colnames(df.summary)[3] <- "n"
colnames(df.summary)[4] <- "sd"

# calculates standard error of the mean by sd/sqrt(n)
df.summary$sem <- df.summary$sd/sqrt(df.summary$n) 
# calculate margin of error for confidence interval
df.summary$me <- qt(1-0.05/2, df=df.summary$n)*df.summary$sem # gets the t-statistic for resp. df
?qt