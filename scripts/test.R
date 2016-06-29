library(mgcfv)

FindIt(model.treat = Donation ~ RecvsDefD, model.main = ~ ReactanceM,
       model.int = ~ ReactanceM,
       data = df, type = "continuous",
       treat.type = "single")

summary(lm(Dist ~ NosvsSomeD*RecvsDefD * ReactanceM, df))


################################################### 
## Example 2: Treatment-Treatment Interaction
################################################### 

## Not run: 
data(GerberGreen)

## The model includes four factorial treatments and 
## all two, three, four-way interactions between them.
## Four pre-treatment covariates are adjusted.

## Run to search for lambdas.
summary(lm(Dist ~ RecvsDefD*NosvsSomeD*ReactanceM+EAI+moralD, df))
F2<- FindIt(model.treat= Dist ~ RecvsDefD+NosvsSomeD+ReactanceM,
            nway=3,
            model.main= ~ EAI+moralD,
            data = df,
            type="binary",
            treat.type="multiple")

summary(F2)
pred2 <- predict(F2, unique = T)
head(pred2$data, n = 10)
plot(pred2)

## Fit, given selected lambdas.
F2<- FindIt(model.treat= voted98 ~ persngrp+phnscrpt+mailings+appeal,
            nway=4,
            model.main= ~ age+majorpty+vote96.1+vote96.0,
            data = GerberGreen,
            type="binary",
            treat.type="multiple",
            search.lambdas=FALSE,
            lambdas=c(-15.000,-6.237))

## Returns coefficient estimates.
summary(F2)

## Returns predicted values for unique treatment combinations.
pred2 <- predict(F2,unique=TRUE)
## Top 10
head(pred2$data, n=10)
## Bottom 10
tail(pred2$data, n=10)

## Visualize predicted values for each treatment combination.
plot(pred2)

## End(Not run)