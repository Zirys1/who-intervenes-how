rm(list=ls())
# this file provides two functions: zTreeTables and zTreeSbj
# author: Oliver Kirchkamp http://www.kirchkamp.de/
# 2015-04-16: handles non-z-Tree files more gracefully
# 2014-10-26: added remark regarding locale (thanks to Igor Asanov for the hint).
# 2013-05-14: added "zTree.silent" option
# 2013-05-10: made code robust against non-existent tables, caught (predictable) warnings
# 2012-12-19: corrected merging of sbj files
# 2012-11-23: made merging of tables faster (at the cost of needing the plyr library)
# 2012-03-19: fixed encoding to Latin1 (this becomes an issue once chats are saved in tables)
# 2011-03-21: fixed problem where the first table was ignored in zTreeSbj (thanks to David Hugh-Jones for the hint)
#----------------------------------------
# zTreeTables is an R function to read zTree .xls files
# The return value is a list of dataframes, one for each table
#
# Example:
#
# source("http://www.kirchkamp.de/lab/zTree.R")
# options(zTree.silent=TRUE) # <- less chatty
#
# read data from one session:
# zTT <- zTreeTables ( "090602_1746.xls" , "contracts" )
#
# read data from several sessions:
# zTT <- zTreeTables ( c("090602_1746.xls","090602_1912.xls"), c("globals","subjects", "contracts" ))
#
# read data from all sessions in directories below the current one:
# sessions<-list.files(".","[0-9]{6}_[0-9]{4}.xls",recursive=TRUE)
# zTT<-zTreeTables(sessions,c("globals","subjects","session","summary","contracts"))
#
# use the data:
# plot(Profit ~ Period,data=zTT$subjects)
#
# Remarks:
# As far as I understand z-Tree uses Latin1 encoding. You might need to set your locale
# to something related, like the following:
# > Sys.setlocale("LC_CTYPE","en_GB.UTF-8")
#

require(plyr)

options(zTree.silent=FALSE)

zTreeTables <- function(filelist,tables=c("globals","subjects"),sep = "\t") {
  splittable <- function(filename,tables=c("globals","subjects")) {
    getTable <- function(start, stop) {
      if (!is.na(stop) && !is.na(start)) {
        names<-aa2[[start]][-3]
        names[1]<-"Date"
        names[2]<-"Treatment"
        tab<-as.data.frame(matrix(nrow=stop-start-1,ncol=length(names)))
        colnames(tab)<-names
        for( i in 1:(stop-start-1)) {
          tab[i,] <- aa2[[start+i]][-3]
        }
        for (n in colnames(tab)) {
          if (is.character(tab[[n]])) {
            tab[[n]][tab[[n]]=="-"] <- NA
            mm<-tryCatch(mean(as.numeric(tab[[n]]),na.rm=TRUE),warning=function(x) NA)
            if (!is.na(mm)) {
              tab[[n]]<-as.numeric(tab[[n]])
            }
          }
        }
        tab
      }
    }
    
    getTables <- function(name) {
      tab<-NULL
      for (i in which ((splitname==name))) {
        new<-getTable(splitpoints[i],splitpoints[i+1])
        fail<-names(new)==""
        if (sum(fail)>0) warning(sprintf("*** %s contains empty cells. This is not a z-Tree file ***",name))
        new<-new[,!fail]
        if (length(new)>0) {
          if (is.null(tab)) {
            tab<-new
          } else {
            tab <- rbind.fill(tab,new)
          }
        }
      }
      tab
    }
    if(!getOption("zTree.silent")) cat("reading ",filename,"...\n")
    Tfile<-file(filename,"r",encoding="LATIN1")
    aa<-readLines(Tfile)
    close(Tfile)
    aa2<-strsplit(aa,sep)
    if(length(aa2[[1]])<3) 
      stop(sprintf("*** cells are not separated by '%s'. Proper z-Tree files use \\t as a separator. Use the \"sep\" option! ***",ifelse(sep=="\t","\\t",sep)))
    splitpoints<-array()
    splitname<-array()
    splittreat<-array()
    table(splitname)
    splitcols<-array()
    last<-0
    for (i in 1:length(aa2)) {
      if (last==0 || (aa2[[i]][3] != aa2[[i-1]][3])) {
        last<-last+1
        splitpoints[last]<-i
        splitname[last]<-aa2[[i]][3]
        splittreat[last]<-aa2[[i]][2]
        splitcols[last]<-length(aa2[[i]])
      }
      splitpoints[last+1]<-i+1
    }
    # cat(splitpoints)
    result<-list()
    do <- intersect(splitname,tables)
    miss <- setdiff(splitname,tables)
    #if (length(miss)>0)
    if(!getOption("zTree.silent")) cat ("Skipping:",miss,"\n")
    for (name in do) {
      if(!getOption("zTree.silent")) cat ("Doing:",name,"\n")
      aTable<-getTables(name)
      if (!is.null(aTable)) result[[name]]<-aTable
    }
    result
  }
  
  z<-splittable(filelist[1],tables)
  for (name in filelist[-1]) {
    if(!getOption("zTree.silent")) cat (sprintf("*** %s is file %d / %d ***\n",name,which(name==filelist),length(filelist)))
    a<-splittable(name,tables)
    for(t in tables) {
      if (!is.null(a[[t]])) # there is no such table
        z[[t]]<-rbind.fill(z[[t]],a[[t]])
    }
  }
  # try to convert characters to numbers if this does not introduce more NAs:
  for (t in tables)
    for(n in names(z[[t]]))
      if(typeof(z[[t]][[n]])=="character") 
        if(!is.null(q<-tryCatch(as.numeric(z[[t]][[n]]),warning=function(x) NULL))) z[[t]][[n]]<-q
  z
}
#
# zTreeSbj takes a vector of .sbj-files and returns a matrix
# Example:
# files <- list.files(pattern = "*.sbj$",recursive=TRUE)
# fname <- sub(".*/","",files)
# sbj <- zTreeSbj(aggregate(files,list(fname),function(x) x[1])$x)
#
zTreeSbj <- function(files,sep="\t") {
  sbj<-NULL
  for (filename in files) {
    if(!getOption("zTree.silent")) cat("reading ",filename,"...\n")
    Tfile<-file(filename,"r",encoding="LATIN1")
    aa<-readLines(Tfile)
    close(Tfile)
    aa2<-strsplit(aa,sep)
    N <- length(aa2[[2]])-1
    aa3<-as.data.frame(list(Date=rep(sub(".sbj$","",sub(".*/","",filename)),N)))
    lapply(aa2,function(x) if (length(x)==N+1) aa3[[x[1]]]<<- x[-1])
    if ( is.null(sbj) ) sbj<-aa3 else sbj<-merge(sbj,aa3,all=TRUE)
  }
  sbj
}
#
toLongDate <- function (shortDate) {
  sapply(as.character(shortDate),function(zz) {
    pre <- ifelse(substr(zz,1,2)<"80","20","19")
    if (nchar(zz)==8) {
      #      hour  <- which(LETTERS==substr(zz,7,7))-1
      minute<- 60*which(LETTERS==substr(zz,7,7)) + (which(c(as.character(0:9),LETTERS)==substr(zz,8,8)))*2 - 21
      sprintf("%s%s-%02d:%02d",pre,substr(zz,1,6),minute%/%60,minute%%60)
    }
    else if (nchar(zz)==11) sprintf("%s%s-%s:%s",pre,substr(zz,1,6),substr(zz,8,9),substr(zz,10,11))
    else zz
  })
}

###### Import data from zTree ####
library(plyr)
library(xlsx)
library(dplyr)
#### IMPORTANT: names of the excel files have to be coded as follows: DDMMYYYY_HHMM_treatment.xls

##### Read in xlsx file from the directory (Treatment) ####
## Attention! For Default treatments I need to repair column names (delete <>:; and Spaces) [see code book]

setwd("Z:/Projects/Who intervenes/Experiment/Data/Pilot Test 31032016") ## Pilot-Data
control <- zTreeTables("31032016_1400_control.xls", tables = "subjects")
control <- control$subjects ## converts list to data frame
defnos <- zTreeTables("31032016_1400_defnos.xls", tables = "subjects")
defnos <- defnos$subjects
defpol <- zTreeTables("31032016_1400_defpol.xls", tables = "subjects")
defpol <- defpol$subjects
recpar <- zTreeTables("31032016_1400_recpar.xls", tables = "subjects")
recpar <- recpar$subjects

setwd("Z:/Projects/Who intervenes/Experiment/Data/Session 1 12042016") # Session 1 Data
control1 <- zTreeTables("12042016_1100_control.xls", tables = "subjects")
control1 <- control1$subjects
recnos <- zTreeTables("12042016_1230_recnos.xls", tables = "subjects")
recnos <- recnos$subjects
defnos1 <- zTreeTables("12042016_1400_defnos.xls", tables = "subjects")
defnos1 <- defnos1$subjects
recnap <- zTreeTables("12042016_1530_recnap.xls", tables = "subjects")
recnap <- recnap$subjects

setwd("Z:/Projects/Who intervenes/Experiment/Data/Session 2 14042016") # Session 2 Data
defnap <- zTreeTables("14042016_1100_defnap.xls", tables = "subjects")
defnap <- defnap$subjects
recpol <- zTreeTables("14042016_1230_recpol.xls", tables = "subjects")
recpol <- recpol$subjects
defpol1 <- zTreeTables("14042016_1400_defpol.xls", tables = "subjects")
defpol1 <- defpol1$subjects
recpar1 <- zTreeTables("14042016_1530_recpar.xls", tables = "subjects")
recpar1 <- recpar1$subjects

setwd("Z:/Projects/Who intervenes/Experiment/Data/Session 3 19042016") # Session 3 Data
defpar <- zTreeTables("19042016_1100_defpar.xls", tables = "subjects")
defpar <- defpar$subjects
reckno <- zTreeTables("19042016_1230_reckno.xls", tables = "subjects")
reckno <- reckno$subjects
defkno <- zTreeTables("19042016_1400_defkno.xls", tables = "subjects")
defkno <- defkno$subjects

setwd("Z:/Projects/Who intervenes/Experiment/Data/Session 4 26042016") ## Session 4 Data (Backup)
recnap1 <- zTreeTables("26042016_1100_recnap.xls", tables = "subjects")
recnap1 <- recnap1$subjects
recnos1 <- zTreeTables("26042016_1100_recnos.xls", tables = "subjects")
recnos1 <- recnos1$subjects
recpol1 <- zTreeTables("26042016_1100_recpol.xls", tables = "subjects")
recpol1 <- recpol1$subjects
control2 <- zTreeTables("26042016_1230_control.xls", tables = "subjects")
control2 <- control2$subjects
defkno1 <- zTreeTables("26042016_1230_defkno.xls", tables = "subjects")
defkno1 <- defkno1$subjects
defnap1 <- zTreeTables("26042016_1230_defnap.xls", tables = "subjects")
defnap1 <- defnap1$subjects
defpar1 <- zTreeTables("26042016_1230_defpar.xls", tables = "subjects")
defpar1 <- defpar1$subjects
reckno1 <- zTreeTables("26042016_1100_reckno.xls", tables = "subjects")
reckno1 <- reckno1$subjects

setwd("Z:/Projects/Who intervenes/Experiment/Data/Session 5 20062016") # Session 5 Data
recnos2 <- zTreeTables("20062016_1400_recnos.xls", tables = "subjects")
recnos2 <- recnos2$subjects
defnos2 <- zTreeTables("20062016_1530_defnos.xls", tables = "subjects")
defnos2 <- defnos2$subjects

# Add treatment variables (0 - 10) to respective datasets
control$treatment <- c(rep(0, 5)) 
defnos$treatment <- c(rep(2, 5))
defpol$treatment <- c(rep(6, 5))
recpar$treatment <- c(rep(7, 5))

control1$treatment <- c(rep(0, 24))
recnos$treatment <- c(rep(1, 28))
defnos1$treatment <- c(rep(2, 30))
recnap$treatment <- c(rep(3, 27))

defnap$treatment <- c(rep(4, 29))
recpol$treatment <- c(rep(5, 26))
defpol1$treatment <- c(rep(6, 26))
recpar1$treatment <- c(rep(7, 27))

defpar$treatment <- c(rep(8, 26))
reckno$treatment <- c(rep(9, 25))
defkno$treatment <- c(rep(10, 27))

recnap1$treatment <- c(rep(3, 6))
recnos1$treatment <- c(rep(1, 3))
recpol1$treatment <- c(rep(5, 7))
control2$treatment <- c(rep(0, 3))
defkno1$treatment <- c(rep(10, 7))
defnap1$treatment <- c(rep(4, 5))
defpar1$treatment <- c(rep(8, 8))
reckno1$treatment <- c(rep(9, 9))

recnos2$treatment <- c(rep(1, 27))
defnos2$treatment <- c(rep(2, 27))

##### Add questionnaire data ####
## Read sbj file with function from Oliver Kirchkamp 
setwd("Z:/Projects/Who intervenes/Experiment/Data/Pilot Test 31032016") # Pilot-Data
controlQ <- zTreeSbj("31032016_1400_control.sbj") # signs (commas, etc) need to be removed from text input manually
defnosQ <- zTreeSbj("31032016_1400_defnos.sbj")
defpolQ <- zTreeSbj("31032016_1400_defpol.sbj")
recparQ <- zTreeSbj("31032016_1400_recpar.sbj")

setwd("Z:/Projects/Who intervenes/Experiment/Data/Session 1 12042016") # Session 1 Data
control1Q <- zTreeSbj("12042016_1100_control.sbj")
recnosQ <- zTreeSbj("12042016_1230_recnos.sbj")
defnos1Q <- zTreeSbj("12042016_1400_defnos.sbj") 
recnapQ <- zTreeSbj("12042016_1530_recnap.sbj") # signs (commas, etc) need to be removed from text input manually

setwd("Z:/Projects/Who intervenes/Experiment/Data/Session 2 14042016")
defnapQ <- zTreeSbj("14042016_1100_defnap.sbj")
recpolQ <- zTreeSbj("14042016_1230_recpol.sbj")
defpol1Q <- zTreeSbj("14042016_1400_defpol.sbj")
recpar1Q <- zTreeSbj("14042016_1530_recpar.sbj")

setwd("Z:/Projects/Who intervenes/Experiment/Data/Session 3 19042016")
defparQ <- zTreeSbj("19042016_1100_defpar.sbj")
recknoQ <- zTreeSbj("19042016_1230_reckno.sbj")
defknoQ <- zTreeSbj("19042016_1400_defkno.sbj")

setwd("Z:/Projects/Who intervenes/Experiment/Data/Session 4 26042016")
recnap1Q <- zTreeSbj("26042016_1100_recnap.sbj")
recnos1Q <- zTreeSbj("26042016_1100_recnos.sbj")
recpol1Q <- zTreeSbj("26042016_1100_recpol.sbj")
control2Q <- zTreeSbj("26042016_1230_control.sbj")
defkno1Q <- zTreeSbj("26042016_1230_defkno.sbj")
defnap1Q <- zTreeSbj("26042016_1230_defnap.sbj")
defpar1Q <- zTreeSbj("26042016_1230_defpar.sbj")
reckno1Q <- zTreeSbj("26042016_1100_reckno.sbj")

setwd("Z:/Projects/Who intervenes/Experiment/Data/Session 5 20062016") 
recnos2Q <- zTreeSbj("20062016_1400_recnos.sbj")
defnos2Q <- zTreeSbj("20062016_1530_defnos.sbj")


## Combine treatment (.xls) and questionnaire (.sbj) files ####
controlComplete <- merge(control, controlQ, by="Subject")
defnosComplete <- merge(defnos, defnosQ, by="Subject")
defpolComplete <- merge(defpol, defpolQ, by="Subject")
recparComplete <- merge(recpar, recparQ, by="Subject")

control1Complete <- merge(control1, control1Q, by="Subject")
recnosComplete <- merge(recnos, recnosQ, by="Subject")
defnos1Complete <- merge(defnos1, defnos1Q, by="Subject")
recnapComplete <- merge(recnap, recnapQ, by="Subject")

defnapComplete <- merge(defnap, defnapQ, by="Subject")
recpolComplete <- merge(recpol, recpolQ, by="Subject")
defpol1Complete <- merge(defpol1, defpol1Q, by="Subject")
recpar1Complete <- merge(recpar1, recpar1Q, by="Subject")

defparComplete <- merge(defpar, defparQ, by="Subject")
recknoComplete <- merge(reckno, recknoQ, by="Subject")
defknoComplete <- merge(defkno, defknoQ, by="Subject")

recnap1Complete <- merge(recnap1, recnap1Q, by="Subject")
recnos1Complete <- merge(recnos1, recnos1Q, by="Subject")
recpol1Complete <- merge(recpol1, recpol1Q, by="Subject")
control2Complete <- merge(control2, control2Q, by="Subject")
defkno1Complete <- merge(defkno1, defkno1Q, by="Subject")
defnap1Complete <- merge(defnap1, defnap1Q, by="Subject")
defpar1Complete <- merge(defpar1, defpar1Q, by="Subject")
reckno1Complete <- merge(reckno1, reckno1Q, by="Subject")

recnos2Complete <- merge(recnos2, recnos2Q, by="Subject")
defnos2Complete <- merge(defnos2, defnos2Q, by="Subject")

##### Combine all horizontally ####

df <- rbind.fill(controlComplete, defnosComplete, defpolComplete, recparComplete, 
                 control1Complete, recnosComplete, defnos1Complete, recnapComplete, 
                 defnapComplete, recpolComplete, defpol1Complete, recpar1Complete, 
                 defparComplete, recknoComplete, defknoComplete, recnap1Complete,
                 recnos1Complete, recpol1Complete, control2Complete, defkno1Complete,
                 defnap1Complete, defpar1Complete, reckno1Complete, recnos2Complete,
                 defnos2Complete)
rm(list=setdiff(ls(), "df"))

##### Order Columns  ####
## The following will change depending on which data I include (CHANGE WHEN ALL DATA AVAILABLE - also following)
df2 <- df[, c(1:2, 6, 10, 13, 60, 16:58, 68:70, 75:77, 3:5, 7:9, 11:12, 14:15, 59, 61:67, 71:74, 78:79)]

## Remove the two observations from Pilot that were missing in the experiment and that I just clicked through
df3 <- df2[!(df2$client == "EL-13" & df2$Date.x == "160331_1314"),] ## change for all data
df3 <- df3[!(df3$client == "EL-30" & df3$Date.x == "160331_1314"),] ## change for all data

## edit one variable were subject incidentally entered his individual payment instead of donation
# so donation is not 5 but 2, profit and totalprofit not 2 but 5
## from "12042016_1100_control client "EL-19"
df3$Profit[df3$Date.x == "160412_1019" & df3$client == "EL-19"] <- 5
df3$Payment[df3$Date.x == "160412_1019" & df3$client == "EL-19"] <- 2
df3$TotalProfit[df3$Date.x == "160412_1019" & df3$client == "EL-19"] <- 5

## Edit donation variable (and consequently Profit and TotalProfit) because one subject indicated
# different value on his/her receipt and this was paid out to him/her (19042016, defkno)
df3$Profit[df3$Date.x == "160419_1342" & df3$client == "EL-18"] <- 6
df3$Payment[df3$Date.x == "160419_1342" & df3$client == "EL-18"] <- 1
df3$TotalProfit[df3$Date.x == "160419_1342" & df3$client == "EL-18"] <- 6



##### Create Donation variable that is = Payment if altPayment == 0 and = altPayment if altPayment != 0 ####
df3$Donation <- (7-df3$Profit)
df3 <- df3[, c(1:3, 80, 4:79)]
df <- df3
rm(list=setdiff(ls(), "df"))


##### Store variables as right type (factor, numeric, character) ####
dfs <- df
df[] <- lapply(df, function(x) if(is.character(x)){
  factor(trimws(x))
} else x
)
i <- sapply(df, is.character) ## creates a list with TRUE for each variable that is character
df[i] <- lapply(df[i], as.factor) ## changes each variable for which character is true to factor
df$treatment <- as.factor(df$treatment)
df$ok <- as.factor(df$ok)
df$OtherPayment <- as.factor(df$OtherPayment)
df$leftempty <- as.factor(df$leftempty)
df$payforInf <- as.character(df$payforInf)
df$payforInf <- as.factor(df$payforInf)
df$income <- as.numeric(as.character(df$income))
df$belief <- as.numeric(as.character(df$belief))
df$age <- as.numeric(as.character(df$age))
df$payforInf <- as.numeric(as.character(df$payforInf))

##### Convert treatment variable so that it make sense and is still factor, not character ####
df$treatment <- factor(df$treatment, levels = c(0,1,2,3,4,5,6,7,8,9,10), labels=c("Control", "RecNos", "DefNos", "RecNap", "DefNap", "RecPol", "DefPol", "RecPar", "DefPar", "RecKno", "DefKno")) 
df$ok <- factor(df$ok, levels = c(0,1), labels = c("Nein", "Ja"))

### Delete some unimportant variables that were created by zTree ####
df <- df[, -c(57:61, 70, 5, 7)]

### Renaming column names (instead of column numbers use name of columns)
colnames(df)[70] <- "DefaultHitTime"
colnames(df)[64] <- "AltAmountHitTime"
colnames(df)[66] <- "AltAmountOKButtonHitTime"
colnames(df)[22] <- "reactance4"

### Create Dummy = 1 if subject chose the default ####
df$Default <- ifelse(df$Donation == 5 & df$DefaultHitTime > 0 & (df$treatment == "DefNos" | df$treatment == "DefNap" | df$treatment == "DefPol" | df$treatment == "DefPar" | df$treatment == "DefKno"), 1, 0)
df$Don5 <- ifelse(df$Donation == 5, 1, 0)
df$Don456 <- ifelse(df$Donation >= 4 & df$Donation <= 6, 1, 0)
df <- df[, c(1:5, 73, 6:72)]
df$Default <- as.factor(df$Default)

dfs <- df

##### Create Reactance score from Hong's Psychological reactance scale ####
### I will create a dummy variable that is 1 when the resp. item is 3 or 4
### I will then add them to create an ordinal variable
## Four factor model (Dillard & Shen 2005) ##
## Emotional response (reactance1, reactance3, reactance10)
df$reactance1d <- ifelse(df$reactance1 == 3 | df$reactance1 == 4,1,0)
df$reactance3d <- ifelse(df$reactance3 == 3 | df$reactance3 == 4,1,0)
df$reactance10d <- ifelse(df$reactance10 == 3 | df$reactance10 == 4,1,0)
df$EmotionResp <- (df$reactance1d + df$reactance3d + df$reactance10d)

## Reactance to compliance (reactance5, reactance7, reactance9)
df$reactance5d <- ifelse(df$reactance5 == 3 | df$reactance5 == 4,1,0)
df$reactance7d <- ifelse(df$reactance7 == 3 | df$reactance7 == 4,1,0)
df$reactance9d <- ifelse(df$reactance9 == 3 | df$reactance9 == 4,1,0)
df$ReactCompl <- (df$reactance5d + df$reactance7d + df$reactance9d)

## Resisting influence (reactance2, reactance6, reactance8)
df$reactance2d <- ifelse(df$reactance2 == 3 | df$reactance2 == 4,1,0)
df$reactance6d <- ifelse(df$reactance6 == 3 | df$reactance6 == 4,1,0)
df$reactance8d <- ifelse(df$reactance8 == 3 | df$reactance8 == 4,1,0)
df$ResistInfl <- (df$reactance2d + df$reactance6d + df$reactance8d)

## Reactance to advice (reactance4, reactance11)
df$reactance4d <- ifelse(df$reactance4 == 3 | df$reactance4 == 4,1,0)
df$reactance11d <- ifelse(df$reactance11 == 3 | df$reactance11 == 4,1,0)
df$ReactAdv <- (df$reactance4d + df$reactance11d)


## One factor model ## - This is not the only option to code this (would also be posible: df$Reactance <- (df$reactance1 + df$reactance2+...))
df$Reactance <- (df$EmotionResp + df$ReactCompl + df$ResistInfl + df$ReactAdv)


##### Create EAI score from EAI scale ####
df$EAI1d <- ifelse(df$EAI1 == 3 | df$EAI1 == 4,1,0)
df$EAI2d <- ifelse(df$EAI2 == 3 | df$EAI2 == 4,1,0)
df$EAI3d <- ifelse(df$EAI3 == 3 | df$EAI3 == 4,1,0)
df$EAI4d <- ifelse(df$EAI4 == 3 | df$EAI4 == 4,1,0)
df$EAI5d <- ifelse(df$EAI5 == 3 | df$EAI5 == 4,1,0)
df$EAI6d <- ifelse(df$EAI6 == 3 | df$EAI6 == 4,1,0)
df$EAI7d <- ifelse(df$EAI7 == 3 | df$EAI7 == 4,1,0)
df$EAI8d <- ifelse(df$EAI8 == 3 | df$EAI8 == 4,1,0)
df$EAI9d <- ifelse(df$EAI9 == 3 | df$EAI9 == 4,1,0)
df$EAI10d <- ifelse(df$EAI10 == 3 | df$EAI10 == 4,1,0)
df$EAI11d <- ifelse(df$EAI11 == 3 | df$EAI11 == 4,1,0)
df$EAI12d <- ifelse(df$EAI12 == 3 | df$EAI12 == 4,1,0)

df$EAI <- (df$EAI1d + df$EAI2d + df$EAI3d + df$EAI4d + df$EAI5d + df$EAI6d + df$EAI7d + df$EAI8d + df$EAI9d + df$EAI10d + df$EAI11d + df$EAI12d)
rm(i)

df$trustPolD <- ifelse(df$trustPol == 3 | df$trustPol == 4, 1, 0)
df$trustPolD <- as.factor(as.character(df$trustPolD))
df$conformityD <- ifelse(df$conformity == 3 | df$conformity == 4, 1, 0)
df$conformityD <- as.factor(as.character(df$conformityD))
dfs <- df



#### Exclude leftempty variable ####

df <- df[, -63]


#### This constructs dfReactMeans with one column having the means of the Reactance scores ####
dfp <- df[, 20:30]
# the following changes reactance stuff to numeric
i <- sapply(dfp, is.factor)
dfp[i] <- lapply(dfp[i], as.character)
i <- sapply(dfp, is.character)
dfp[i] <- lapply(dfp[i], as.numeric)

ReactMeans <- rowMeans(dfp)
dfReactMeans <- as.data.frame(ReactMeans)

df$ReactMeans <- dfReactMeans

#### Creating more nuanced treatment variable dummies ####
# Create one factor variable indicating the Intervention factor
# 3 levels: 0 = Control, 1 = Recommendation, 2 = Default
df$Intervention <- ifelse(df$treatment == "Control", 0, ifelse((df$treatment == "RecNos" | df$treatment == "RecPol" | df$treatment == "RecNap" | df$treatment == "RecPar" | df$treatment == "RecKno"), 1, ifelse((df$treatment == "DefNos" | df$treatment == "DefPol" | df$treatment == "DefNap" | df$treatment == "DefPar" | df$treatment == "DefKno"), 2, NA)))
df$Intervention <- as.factor(as.character(df$Intervention))
df$Intervention <- factor(df$Intervention, levels = c(0,1,2), labels = c("Control", "Recommendation", "Default"))

# Create one factor variable indicating the Sourcetype factor
# 4 levels: 0 = No Source, 1 = Name and Picture, 2 = Knowledgeable, 3 = Political, 4 = Partisan
df$Sourcetype <- ifelse((df$treatment == "DefNos" | df$treatment == "RecNos"), 0, 
                        ifelse((df$treatment == "DefNap" | df$treatment == "RecNap"), 1, 
                               ifelse((df$treatment == "DefKno" | df$treatment == "RecKno"), 2,
                                      ifelse((df$treatment == "DefPol" | df$treatment == "RecPol"), 3,
                                             ifelse((df$treatment == "DefPar" | df$treatment == "RecPar"), 4, NA)))))
df$Sourcetype <- as.factor(as.character(df$Sourcetype))
df$Sourcetype <- factor(df$Sourcetype, levels = c(0,1,2,3,4), labels = c("NoSource", "NameAndPicture", "Knowledgeable", "Political", "Partisan"))

#### Create dummy to compare each treatment against control ####
df$DefNosvsC <- ifelse(df$treatment == "Control" , 0, ifelse(df$treatment == "DefNos", 1, NA))
df$DefNapvsC <- ifelse(df$treatment == "Control" , 0, ifelse(df$treatment == "DefNap", 1, NA))
df$DefPolvsC <- ifelse(df$treatment == "Control" , 0, ifelse(df$treatment == "DefPol", 1, NA))
df$DefParvsC <- ifelse(df$treatment == "Control" , 0, ifelse(df$treatment == "DefPar", 1, NA))
df$DefKnovsC <- ifelse(df$treatment == "Control" , 0, ifelse(df$treatment == "DefKno", 1, NA))

df$RecNosvsC <- ifelse(df$treatment == "Control" , 0, ifelse(df$treatment == "RecNos", 1, NA))
df$RecNapvsC <- ifelse(df$treatment == "Control" , 0, ifelse(df$treatment == "RecNap", 1, NA))
df$RecPolvsC <- ifelse(df$treatment == "Control" , 0, ifelse(df$treatment == "RecPol", 1, NA))
df$RecParvsC <- ifelse(df$treatment == "Control" , 0, ifelse(df$treatment == "RecPar", 1, NA))
df$RecKnovsC <- ifelse(df$treatment == "Control" , 0, ifelse(df$treatment == "RecKno", 1, NA))

df$DefNosvsC <- as.factor(as.character(df$DefNosvsC))
df$DefNapvsC <- as.factor(as.character(df$DefNapvsC))
df$DefPolvsC <- as.factor(as.character(df$DefPolvsC))
df$DefParvsC <- as.factor(as.character(df$DefParvsC))
df$DefKnovsC <- as.factor(as.character(df$DefKnovsC))

df$RecNosvsC <- as.factor(as.character(df$RecNosvsC))
df$RecNapvsC <- as.factor(as.character(df$RecNapvsC))
df$RecPolvsC <- as.factor(as.character(df$RecPolvsC))
df$RecParvsC <- as.factor(as.character(df$RecParvsC))
df$RecKnovsC <- as.factor(as.character(df$RecKnovsC))

df$DefvsC <- ifelse(df$treatment == "Control" , 0, ifelse(df$Intervention == "Default", 1, NA))
df$RecvsC <- ifelse(df$treatment == "Control", 0, ifelse(df$Intervention == "Recommendation", 1, NA))
df$RecvsDef <- ifelse(df$Intervention == "Recommendation", 0, ifelse(df$Intervention == "Default", 1, 2))
df$DefNapvsDefNos <- ifelse((df$treatment == "DefNap"), 0, ifelse((df$treatment == "DefNos"), 1, NA))
df$RecNapvsRecNos <- ifelse((df$treatment == "RecNap"), 0, ifelse((df$treatment == "RecNos"), 1, NA))
df$RecNapvsRecKno <- ifelse((df$treatment == "RecNap"), 0, ifelse((df$treatment == "RecKno"), 1, NA))
df$DefNapvsDefKno <- ifelse((df$treatment == "DefNap"), 0, ifelse((df$treatment == "DefKno"), 1, NA))
df$DefNapvsDefPol <- ifelse((df$treatment == "DefNap"), 0, ifelse((df$treatment == "DefPol"), 1, NA))
df$RecNapvsRecPol <- ifelse((df$treatment == "RecNap"), 0, ifelse((df$treatment == "RecPol"), 1, NA))
df$DefNapvsDefPar <- ifelse((df$treatment == "DefNap"), 0, ifelse((df$treatment == "DefPar"), 1, NA))
df$RecNapvsRecPar <- ifelse((df$treatment == "RecNap"), 0, ifelse((df$treatment == "RecPar"), 1, NA))

df$DefPolvsDefPar <- ifelse((df$treatment == "DefPol"), 0, ifelse((df$treatment == "DefPar"), 1, NA))
df$RecPolvsRecPar <- ifelse((df$treatment == "RecPol"), 0, ifelse((df$treatment == "RecPar"), 1, NA))
df$RecPolvsDefPol <- ifelse((df$treatment == "RecPol"), 0, ifelse((df$treatment == "DefPol"), 1, NA))
df$RecNosvsRecKno <- ifelse((df$treatment == "RecNos"), 0, ifelse((df$treatment == "RecKno"), 1, NA))
df$DefNosvsDefKno <- ifelse((df$treatment == "DefNos"), 0, ifelse((df$treatment == "DefKno"), 1, NA))
df$DefNosvsDefPol <- ifelse((df$treatment == "DefNos"), 0, ifelse((df$treatment == "DefPol"), 1, NA))
df$RecNosvsRecPol <- ifelse((df$treatment == "RecNos"), 0, ifelse((df$treatment == "RecPol"), 1, NA))
df$DefNosvsDefPar <- ifelse((df$treatment == "DefNos"), 0, ifelse((df$treatment == "DefPar"), 1, NA))
df$RecNosvsRecPar <- ifelse((df$treatment == "RecNos"), 0, ifelse((df$treatment == "RecPar"), 1, NA))

df$RecParvsDefPar <- ifelse((df$treatment == "RecPar"), 0, ifelse((df$treatment == "DefPar"), 1, NA))


df$Donated <- ifelse(df$Donation > 0, 1, 0)

nrows <- nrow(df)
df$ID <- c(1:nrows)
df <- df[, c(139, 1:138)] ## EDIT


rm(list=setdiff(ls(), "df"))
dfs <- df

# delete variables i do not use
df <- df[, -c(62:73, 57:59)]

#### Correctly labeling some factor variables, which I previously did in the Analysis script, but
# now transferred to this script.

df$RecvsDef <- factor(df$RecvsDef, levels = c(0,1,2), labels = c("Rec", "Def", "Control"))
df$RecvsC <- factor(df$RecvsC, levels = c(0,1), labels = c("Control", "Rec"))
df$DefvsC <- factor(df$DefvsC, levels = c(0,1), labels = c("Control", "Def"))
df$RecNapvsRecNos <- factor(df$RecNapvsRecNos, levels = c(0,1), labels = c("RecNap", "RecNos"))
df$DefNapvsDefNos <- factor(df$DefNapvsDefNos, levels = c(0,1), labels = c("DefNap", "DefNos"))
df$RecNapvsRecKno <- factor(df$RecNapvsRecKno, levels = c(0,1), labels = c("RecNap", "RecKno"))
df$DefNapvsDefKno <- factor(df$DefNapvsDefKno, levels = c(0,1), labels = c("DefNap", "DefKno"))
df$RecNapvsRecPol <- factor(df$RecNapvsRecPol, levels = c(0,1), labels = c("RecNap", "RecPol"))
df$DefNapvsDefPol <- factor(df$DefNapvsDefPol, levels = c(0,1), labels = c("DefNap", "DefPol"))
df$RecPolvsDefPol <- factor(df$RecPolvsDefPol, levels = c(0,1), labels = c("RecPol", "DefPol"))
df$RecParvsDefPar <- factor(df$RecParvsDefPar, levels = c(0,1), labels=c("RecPar", "DefPar"))
df$Donated <- as.factor(as.character(df$Donated))
df$Donated <- factor(df$Donated, levels = c(0,1), labels = c("Not donated", "Donated"))
df$green <- ifelse(df$party == "Bündnis90/Grüne", 1, 0)
df$green <- factor(df$green, levels = c(0,1), labels = c("Not green", "Green"))


# Session variable for daytime
df$Daytime <- ifelse((df$Date.x == "160412_1019" | df$Date.x == "160414_1035" | df$Date.x == "160419_1040" | df$Date.x == "160426_1017" |df$Date.x == "160426_1016"), 0, ifelse((df$Date.x == "160412_1144" | df$Date.x == "160414_1213" | df$Date.x == "160419_1215" | df$Date.x == "160426_1157" | df$Date.x == "160426_1158"), 1, ifelse((df$Date.x == "160331_1314" | df$Date.x == "160412_1333" | df$Date.x == "160414_1333" | df$Date.x == "160419_1342" | df$Date.x == "160620_1318"),2, ifelse((df$Date.x == "160412_1510" | df$Date.x == "160414_1515" | df$Date.x == "160620_1440"), 3, NA))))
df$Daytime <- as.factor(as.character(df$Daytime))
df$Daytime <- factor(df$Daytime, levels = c(0,1,2,3), labels = c("11:00 Uhr", "12:30 Uhr", "14:00 Uhr", "15:30 Uhr"))
df <- df[, c(1, 126, 2:125)]

df$Donatedm <- as.numeric(df$Donated)
df$Donatedm <- ifelse(df$Donatedm == 2, 1, 0)

df$NosvsSome <- ifelse((df$Sourcetype == "NoSource" & (df$Intervention == "Default" | df$Intervention == "Recommendation")), 0, ifelse((df$Sourcetype != "NoSource" & (df$Intervention == "Default" | df$Intervention == "Recommendation")), 1, 2))
df$NosvsSome <- as.factor(as.character(df$NosvsSome))
df$NosvsSome <- factor(df$NosvsSome, levels = c(0,1,2), labels = c("No Source", "Some Source", "Control"))

df$NosvsSomeD <- ifelse((df$Sourcetype == "NoSource" & (df$Intervention == "Default" | df$Intervention == "Recommendation")), 0, ifelse((df$Sourcetype != "NoSource" & (df$Intervention == "Default" | df$Intervention == "Recommendation")), 1, NA))
df$NosvsSomeD <- factor(df$NosvsSomeD, levels = c(0,1), labels = c("No Source", "Some Source"))

## create dummy for retireEffic, moral
df$retireEffD <- ifelse(df$retireEffic == 3 | df$retireEffic == 4, 1, 0)
df$retireEffD <- factor(df$retireEffD, levels = c(0,1), labels = c("Unefficient", "Efficient"))

df$moralD <- ifelse(df$moral == 3 | df$moral == 4, 1, 0)
df$moralD <- factor(df$moralD, levels = c(0,1), labels = c("No moral duty", "Moral duty"))

## create RecvsDef-Dummy for t.tests
df$RecvsDefD <- ifelse(df$RecvsDef == "Rec", 0, ifelse(df$RecvsDef == "Def", 1, NA))
df$RecvsDefD <- factor(df$RecvsDefD, levels = c(0,1), labels = c("Rec", "Def"))

df$RecvsDefNonPolPar <- ifelse((df$Intervention == "Recommendation" & df$Sourcetype != "Political" & df$Sourcetype != "Partisan"), 0, ifelse((df$Intervention == "Default" & df$Sourcetype != "Political" & df$Sourcetype != "Partisan"), 1, NA))
df$RecvsDefNonPolPar <- as.factor(as.character(df$RecvsDefNonPolPar))
df$RecvsDefNonPolPar <- factor(df$RecvsDefNonPolPar, levels = c(0,1), labels = c("Non-political/partisan Rec", "Non-political/partisan Def"))

## generates factor to compare Some Source vs. Pol/Par Source (excluding no Source)
df$NonPolvsPol <- ifelse(df$Sourcetype == "NameAndPicture" | df$Sourcetype == "Knowledgeable", 0, ifelse(df$Sourcetype == "Partisan" | df$Sourcetype == "Political", 1, NA))
df$NonPolvsPol <- factor(df$NonPolvsPol, levels = c(0,1), labels = c("Non Political", "Political"))

## including other groups not as NAs
df$NonPolvsPolA <- ifelse(df$Sourcetype == "NameAndPicture" | df$Sourcetype == "Knowledgeable", 0, ifelse(df$Sourcetype == "Partisan" | df$Sourcetype == "Political", 1, ifelse(df$Sourcetype == "NoSource", 2, 3)))
df$NonPolvsPolA <- factor(df$NonPolvsPolA, levels = c(0,1,2), labels = c("Non Political", "Political", "NoSource"))

## including warmGlow dummy
df$warmGlowD <- ifelse(df$warmGlow == 3 | df$warmGlow == 4, 1, 0)
df$warmGlowD <- factor(df$warmGlowD, levels = c(0,1), labels = c("NoWG", "WG"))

df$KnovsPol <- ifelse(df$Sourcetype == "Knowledgeable",0, ifelse(df$Sourcetype == "Political", 1, NA))
df$KnovsPol <- as.factor(as.character(df$KnovsPol))
df$KnovsPol <- factor(df$KnovsPol, levels = c(0,1), labels = c("Kno", "Pol"))

df$demandEffectD <- ifelse(df$demandEffect == 3 |df$demandEffect == 4, 1,0)
df$demandEffectD <- factor(df$demandEffectD, levels = c(0,1), labels = c("NoDemEff", "DemEff"))

df$persIntD <- ifelse(df$persInt == 3 | df$persInt == 4, 1, 0)
df$persIntD <- factor(df$persIntD, levels = c(0,1), labels = c("NoPersInt", "PersInt"))
df$persKnowD <- ifelse(df$persKnow == 3 |df$persKnow == 4, 1, 0)
df$persKnowD <- factor(df$persKnowD, levels = c(0,1), labels = c("NoPersKno", "PersKno"))

df$sourceInterestD <- ifelse(df$sourceInterest == 3 | df$sourceInterest == 4, 1, 0)
df$sourceInterestD <- factor(df$sourceInterestD, levels = c(0,1), labels = c("NoSourInt", "SourInt"))
df$sourceKnowD <- ifelse(df$sourceKnow == 3 | df$sourceKnow == 4, 1, 0)
df$sourceKnowD <- factor(df$sourceKnowD, levels = c(0,1), labels = c("NoSourKno", "SourKno"))

df$intAlign <- ifelse(df$persIntD == "PersInt" & df$sourceInterestD == "SourInt", 1, 0)
df$intAlign <- factor(df$intAlign, levels = c(0,1), labels = c("IntNotAligned", "IntAligned"))
df$knowAlign <- ifelse(df$persKnowD == "PersKno" & df$sourceKnowD == "SourKno", 1, 0)
df$knowAlign <- factor(df$knowAlign, levels = c(0,1), labels = c("KnoNotAligned", "KnoAligned"))

df$SourcetypeD <- ifelse((df$treatment == "DefNap" | df$treatment == "RecNap"), 0, 
                               ifelse((df$treatment == "DefKno" | df$treatment == "RecKno"), 1,
                                      ifelse((df$treatment == "DefPol" | df$treatment == "RecPol"), 2,
                                             ifelse((df$treatment == "DefPar" | df$treatment == "RecPar"), 3, NA))))
df$SourcetypeD <- factor(df$SourcetypeD, levels = c(0,1,2,3), labels = c("NameAndPicture", "Knowledgeable", "Political", "Partisan"))

df$guiltD <- ifelse(df$guilt == 3 | df$guilt == 4, 1, 0)
df$guiltD <- factor(df$guiltD, levels = c(0,1), labels = c("No guilt", "Guilt"))

## include Reactance/ EAI dummy: 0 if below Median, 1 otherwise
df$ReactD <- ifelse(df$Reactance <= 4, 0, 1) # Median is 4, after excluding know Source
df$ReactD <- factor(df$ReactD, levels = c(0,1), labels = c("BelowEq Med React", "Above Med React"))

df$EAID <- ifelse(df$EAI <= 5, 0, 1) # Median is 5, after excluding know Source
df$EAID <- factor(df$EAID, levels = c(0,1), labels = c("BelowEq Med EAI", "Above Med EAI"))

## Include Dummy of Donation variable that is 0 if subject gave less than Median (1)
df$DonatedMed <- ifelse(df$Donation <= 1, 0, 1)
df$DonatedMed <- factor(df$DonatedMed, levels = c(0,1), labels = c("BelowEq Med Don", "Above Med Don"))

## Distance from recommended/defaulted variable (value, only positive, so below and above are treated equally)
df$Dist <- df$Donation - 5
df$Dist <- ifelse(df$Dist < 0, df$Dist * (-1), df$Dist)
## Distance from recommended/defaulted variable (all above 5 treated as 0s)
df$Distcen <- df$Donation - 5
df$Distcen <- ifelse(df$Distcen > 0, 0, df$Distcen*(-1))

## median coding of trustPol and conformity, because otherwise there would be not enough observations
# Problem: the median can only be calculated for numeric, not ordinal data (seems to be allowed though, see http://stats.stackexchange.com/questions/11219/median-value-on-ordinal-scales)
# median(as.numeric(as.character(df$trustPol))) #1
df$trustPolM <- ifelse(as.numeric(as.character(df$trustPol)) <= 1, 0, 1)
df$trustPolM <- factor(df$trustPolM, levels = c(0,1), labels = c("BelowEq Med PolTrust", "Above Med PolTrust"))

# median(as.numeric(as.character(df$conformity))) #1
df$conformityM <- ifelse(as.numeric(as.character(df$conformity)) <= 1, 0, 1)
df$conformityM <- factor(df$conformityM, levels = c(0,1), labels = c("BelowEq Med Conformity", "Above Med Conformity"))

# create dummy variables for each of the four reactance categories (above and below median)
df$EmotionRespM <- ifelse(as.numeric(as.character(df$EmotionResp)) <= 2, 0, 1) # median is 2
df$EmotionRespM <- factor(df$EmotionRespM, levels = c(0,1), labels = c("BelowEq Med EmoResp", "Above Med EmoResp"))
df$ReactComplM <- ifelse(as.numeric(as.character(df$EmotionResp)) <= 1, 0, 1) # median is 1
df$ReactComplM <- factor(df$ReactComplM, levels = c(0,1), labels = c("BelowEq Med ReactCompl", "Above Med ReactCompl"))
df$ResistInflM <- ifelse(as.numeric(as.character(df$EmotionResp)) <= 2, 0, 1) # median is 2
df$ResistInflM <- factor(df$ResistInflM, levels = c(0,1), labels = c("BelowEq Med ResistInfl", "Above Med ResistInfl"))
df$ReactAdvM <- ifelse(as.numeric(as.character(df$EmotionResp)) <= 0, 0, 1) # median is 0
df$ReactAdvM <- factor(df$ReactAdvM, levels = c(0,1), labels = c("BelowEq Med ReactAdv", "Above Med ReactAdv"))

# Donation variable (numeric) without 0s (for two-step regression)
df$Donationno0 <- ifelse(df$Donation == 0, NA, df$Donation)

# Distance variable (numeric) without 5's (for two-step regression), i.e. none that gave nothing
df$Distno5 <- ifelse(df$Dist == 5, NA, df$Dist)

# Variable to compare RecPolPar vs. DefPolPar
df$RecPPvsDefPP <- ifelse(df$treatment == "RecPar" | df$treatment == "RecPol", 0, ifelse(df$treatment == "DefPar" | df$treatment == "DefPol", 1, NA))
df$RecPPvsDefPP <- factor(df$RecPPvsDefPP, levels = c(0,1), labels = c("RecPolPar", "DefPolPar"))

df$genderB <- ifelse(df$gender == "Keine Angabe", NA, df$gender)
df$genderB <- factor(df$genderB, levels = c(2,3), labels = c("Male", "Female"))


df$DistOrd <- ifelse(df$Dist == 0, 0, ifelse(df$Dist > 0 & df$Dist <= 1, 1, ifelse(df$Dist > 1 & df$Dist <= 2, 2,
                                                                                   ifelse(df$Dist > 2 &df$Dist <= 3, 3,
                                                                                          ifelse(df$Dist > 3 & df$Dist <= 4, 4,
                                                                                                 ifelse(df$Dist > 4 & df$Dist <=5,5, NA))))))
df$DistOrd <- factor(df$DistOrd, levels = c(0,1,2,3,4,5), labels = c(0,1,2,3,4,5))

df$ReactanceM <- df$Reactance - mean(df$Reactance)

df$SourcetypeC <- ifelse((df$treatment == "DefNos" | df$treatment == "RecNos"), 0, 
                         ifelse((df$treatment == "DefNap" | df$treatment == "RecNap"), 1, 
                                ifelse((df$treatment == "DefKno" | df$treatment == "RecKno"), 2,
                                       ifelse((df$treatment == "DefPol" | df$treatment == "RecPol"), 3,
                                              ifelse((df$treatment == "DefPar" | df$treatment == "RecPar"), 4, 5)))))
df$SourcetypeC <- as.factor(as.character(df$SourcetypeC))
df$SourcetypeC <- factor(df$SourcetypeC, levels = c(0,1,2,3,4,5), labels = c("NoSource", "NameAndPicture", "Knowledgeable", "Political", "Partisan", "Control"))

# Create Session variable to compare data from new session (20.06.) with respective 
# treatments from older sessions
df$Session <- ifelse(df$Date.x == "160620_1318" | df$Date.x == "160620_1440", 1, 
                     ifelse(df$Date.x != "160620_1318" & df$Date.x != "160620_1440" & df$treatment == "RecNos" | df$treatment == "DefNos",0,NA))
df$Session <- factor(df$Session, levels = c(0,1), labels = c("old", "new"))

## compare Treatments without control 
df$treatmentnoC <- as.factor(ifelse(df$treatment == "Control", NA, df$treatment))
df$treatmentnoC <- factor(df$treatmentnoC, levels = c(1,2,3,4,5,6,7,8,9,10), labels=c("RecNos", "DefNos", "RecNap", "DefNap", "RecPol", "DefPol", "RecPar", "DefPar", "RecKno", "DefKno")) 

## Subdatasets

## create dataset without observations that did not believe we cooperated with Julia Verlinden
# includes NAs, in order to keep other treatments (keep DefNos in, because question made no sense)
dfbel <- df[(df$believe2 == "Ja" | is.na(df$believe2)) | df$treatment == "DefNos",]

dfsub <- df[df$gender != "Keine Angabe",] 
dfsub$gender <- factor(dfsub$gender) # to drop unused factor

# Subset dataset including only those that were confronted with Julia Verlinden
dfbelA <- dfsub[(!is.na(dfsub$believe2)) & (dfsub$treatment != "DefNos"),] #exclude DefNos, cause question made no sense
dfbelA$treatment <-  factor(dfbelA$treatment) # relevel after subsetting
dfbelA$believe2 <- ifelse(dfbelA$believe2 == "Ja", 1, 0)
dfbelA$believe2 <- factor(dfbelA$believe2, levels = c(0,1), labels = c("NonBeliever", "Believer"))

## Exclude subjects experiencing a demand effect
dfnoDE <- df[df$demandEffectD == "NoDemEff",]


dfKS <- df # to include all in mean donations, etc.

df <- df[df$knowSource == "Nein" | is.na(df$knowSource),] # this only makes sense when they
# are in the Source treatments

# only those before new session from 20.06.2016
dfold <- df[df$Session == "old" | is.na(df$Session),]

# Dataset only with donators
dfDon <- df[df$Donation > 0,]
dfDon$genderB <- ifelse(dfDon$gender == "Keine Angabe", NA, dfDon$gender)
## save of dataframe
dfs <- df
##### Create Excel File with data ####

library(qdap)
write.xlsx(x = condense(df), "Z:/Projects/Who intervenes/Experiment/Data/Complete_04072016.xlsx")

library(foreign)
dfstata <- df
dfstata$notes <- NULL
write.dta(condense(dfstata), "Z:/Projects/Who intervenes/Experiment/Data/Complete_04072016.dta")
write.foreign(dfstata,
              datafile="dfstata.csv",
              codefile="dfstata2.do",
              package="Stata")