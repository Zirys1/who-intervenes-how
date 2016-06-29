lor <- c(.2362, .1131, .4621, .3351)
se <- c(.1234, .0989, .1812, .1612)
chisq <-(lor/se)^2
summary.stats <-cbind(
  "Log Odds Ratio"=lor,
  "Standard Error"=se,
  "Odds Ratio" =exp(lor),
  "$\\chi^2$" =chisq,
  "$P$--value" =1-pchisq(chisq,1) )
# $..$ : puts .. in math notation (^=superscript)
# -- : LaTeX medium length dash
summary.stats # ordinary print


library(Hmisc) # get access to library
w <- latex(summary.stats, cdec=c(3, 3, 2, 2, 4), col.just=rep("c",5),
          rowname=c("Death (all cause)","Cancer Death",
                     "Relapse","Hospitalization"),
          rgroup=c("Fatal Events","Non--fatal Events"),
          rowlabel="", caption="Statistical Results",
          ctable=TRUE)