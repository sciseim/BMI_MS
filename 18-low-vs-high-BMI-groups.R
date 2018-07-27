# survival acording to heatmap groups here...
# that is according to BMI

# run 17A-select-EC-subsets.R
# generated two dfs


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Pearson Chi square
# https://rcompanion.org/rcompanion/b_05.html
#
# EAC
EAC.normal.BMI.df <- EAC[which(EAC$BMItwogroup == "normal"),] # 20
EAC.high.BMI.df <- EAC[which(EAC$BMItwogroup != "normal"),] # 46
# ESCC
ESCC.normal.BMI.df <- ESCC[which(ESCC$BMItwogroup == "normal"),] # 16
ESCC.high.BMI.df <- ESCC[which(ESCC$BMItwogroup != "normal"),]  # 58

BMI.group = matrix( c(   length(EAC.normal.BMI.df$barcode),length(ESCC.normal.BMI.df$barcode),length(EAC.high.BMI.df$barcode),length(ESCC.high.BMI.df$barcode)),nrow=2,ncol=2) 
#
colnames(BMI.group) <- c("lean","high-BMI")
rownames(BMI.group) <- c("EAC", "ESCC")
#
chisq.test(BMI.group,
           correct=FALSE)
# p-value = 1.087e-08
# Descriptive statistics for patients among groups by BMI category were compared using a Pearson Chi-square test of significance.
# correct means...Yates' continuity correction... check if I should use
# chisq.test(B,
#           correct=TRUE)









# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# SURVIVAL
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# EAC.normal.BMI.df
# EAC.high.BMI.df
EAC.normal.BMI.df$subset.BMI.group <- "lean"
EAC.high.BMI.df$subset.BMI.group <- "H-BMI"
#
# ESCC.normal.BMI.df
# ESCC.high.BMI.df
ESCC.normal.BMI.df$subset.BMI.group <- "lean"
ESCC.high.BMI.df$subset.BMI.group <- "H-BMI"
# 


EC.subset.df <- rbind(ESCC.normal.BMI.df,ESCC.high.BMI.df) # ESCC
EC.subset.df <- rbind(EAC.normal.BMI.df,EAC.high.BMI.df) # EAC
table(EC.subset.df$Histological.Type...Oesophagus) # check which we have
table(EC.subset.df$subset.BMI.group) # check which we have



# ####
# let us load cBio
cBioPortal.clinical.DF <- read.delim("cBioPortal/ESCA.txt")
# only keep Primary Tumours
cBioPortal.clinical.DF <- cBioPortal.clinical.DF[which(cBioPortal.clinical.DF$Sample.Type == "Primary Tumor"),]
# cBioPortal.clinical.DF$Overall.Survival.Status
# cBioPortal.clinical.DF$Overall.Survival..Months.
# 185 samples
# cBioPortal.clinical.DF$Disease.Free..Months.
# cBioPortal.clinical.DF$Disease.Free.Status
# ##############################################
# ##############################################
row.names(cBioPortal.clinical.DF) <- cBioPortal.clinical.DF$Patient.ID
# Sample ID  # TCGA-Z6-A8JD-01
# Patient ID = TCGA-R6-A6KZ
row.names(EC.subset.df) <- EC.subset.df$Case.ID
cBioPortal.clinical.DF[c("TCGA-LN-A9FO"),]
# MERGE THEM
remove(the.merged.df)
# list of data.frames I want to merge
library(data.table)
# l = list(cBioPortal.clinical.DF,EC.subset.df) # put Nature first, so all the genes go a the end of the DF!
# the.merged.df <- transform(Reduce(merge, lapply(l, function(x) data.frame(x, rn = row.names(x)))), row.names=rn, rn=NULL)
# does not work now .... Not sure why
# try this

# the.merged.df <- merge(cBioPortal.clinical.DF,EC.subset.df, by=0, all=TRUE)
# switch!
the.merged.df <- merge(EC.subset.df,cBioPortal.clinical.DF, by=0, all=FALSE)
# yes .. only keep e.g. EAC now

# *slow*, but works
#
#
# or
# source("SUB.multimerge.R")
# the.merged.df <- multimerge( list (one=cBioPortal.clinical.DF, two=EC.subset.df ) )

head(the.merged.df[1:10,1:10])  # we now have the File.Names
# this works MUCH better than merge by row.names!
# the.merged.df$Overall.Survival..Months.
# the.merged.df$Overall.Survival.Status

# ##############################################
tempdata <- the.merged.df

length(the.merged.df$Row.names) # 185 no... do not want to keep all
the.merged.df[1:5,1:48]

table(tempdata$Histological.Type)
the.merged.df$Cancer.Type.Detailed 



# ##############################################
# ///////////////////////////////////////////////////////
# let us perform a 'rough' chi-square too..
# ///////////////////////////////////////////////////////

ESCC.subset.df <- rbind(ESCC.normal.BMI.df,ESCC.high.BMI.df) # ESCC


# need to pull out clinical info

# ESCC
EC.subset.df <- rbind(ESCC.normal.BMI.df,ESCC.high.BMI.df) # ESCC
# EAC
EC.subset.df <- rbind(EAC.normal.BMI.df,EAC.high.BMI.df) # EAC
#
#
EC.the.merged.df <- merge(EC.subset.df,cBioPortal.clinical.DF, by=0, all=FALSE)
table(EC.the.merged.df$BMItwogroup)
# normal: 20 overweightETobese 46
#
no.EC.normal.dead <- length( which(EC.the.merged.df$Vital.Status == "Dead" & EC.the.merged.df$BMItwogroup == "normal" ) )
no.EC.normal.alive <- length( which(EC.the.merged.df$Vital.Status == "Alive" & EC.the.merged.df$BMItwogroup == "normal" ) )
length(EC.the.merged.df$Row.names) # 66
no.EC.hBMI.dead <- length( which(EC.the.merged.df$Vital.Status == "Dead" & EC.the.merged.df$BMItwogroup == "overweightETobese" ) )
no.EC.hBMI.alive <- length( which(EC.the.merged.df$Vital.Status == "Alive" & EC.the.merged.df$BMItwogroup == "overweightETobese" ) )
#
death.matrix <-  matrix( c(no.EC.normal.alive,no.EC.hBMI.alive, no.EC.normal.dead ,no.EC.hBMI.dead),nrow=2,ncol=2) 
colnames(death.matrix) <- c("Alive","Dead")
rownames(death.matrix) <- c("lean", "high BMI")
death.matrix
#
chisq.test(death.matrix,
           correct=FALSE)
# p-value = 0.745

# ESCC

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# finally, just ESCC vs EAC survival
EC.subset.df <- rbind(EAC.normal.BMI.df,EAC.high.BMI.df,ESCC.normal.BMI.df,ESCC.high.BMI.df) # EAC and ESCC


EC.the.merged.df <- merge(EC.subset.df,cBioPortal.clinical.DF, by=0, all=FALSE)
table(EC.the.merged.df$BMItwogroup)
# normal: 20 overweightETobese 46
#



ESCC.high.BMI..EAC.low.BMI.dead <- length( which(
  EC.the.merged.df$Vital.Status == "Dead" & 
  EC.the.merged.df$Histological.Type...Oesophagus == "ESCC" &
  EC.the.merged.df$BMItwogroup == "overweightETobese" 
              |
    EC.the.merged.df$Vital.Status == "Dead" & 
    EC.the.merged.df$Histological.Type...Oesophagus == "EAC" &
    EC.the.merged.df$BMItwogroup == "normal")) # 13
  
ESCC.high.BMI..EAC.low.BMI.alive<- length( which(
  EC.the.merged.df$Vital.Status == "Alive" & 
    EC.the.merged.df$Histological.Type...Oesophagus == "ESCC" &
    EC.the.merged.df$BMItwogroup == "overweightETobese" 
  |
    EC.the.merged.df$Vital.Status == "Alive" & 
    EC.the.merged.df$Histological.Type...Oesophagus == "EAC" &
    EC.the.merged.df$BMItwogroup == "normal" ) ) # 14

ESCC.low.BMI..EAC.high.BMI.dead <- length( which(
  EC.the.merged.df$Vital.Status == "Dead" & 
    EC.the.merged.df$Histological.Type...Oesophagus == "ESCC" &
    EC.the.merged.df$BMItwogroup == "normal" 
  |
    EC.the.merged.df$Vital.Status == "Dead" & 
    EC.the.merged.df$Histological.Type...Oesophagus == "EAC" &
    EC.the.merged.df$BMItwogroup == "overweightETobese")) # 35

ESCC.low.BMI..EAC.high.BMI.alive <- length( which(
  EC.the.merged.df$Vital.Status == "Alive" & 
    EC.the.merged.df$Histological.Type...Oesophagus == "ESCC" &
    EC.the.merged.df$BMItwogroup == "normal" 
  |
    EC.the.merged.df$Vital.Status == "Alive" & 
    EC.the.merged.df$Histological.Type...Oesophagus == "EAC" &
    EC.the.merged.df$BMItwogroup == "overweightETobese")) # 61
14 + 13 + 35 + 61 # 123 
length(EC.the.merged.df$BMItwogroup) # 140
table(EC.the.merged.df$Vital.Status) # 140

death.matrix <-  matrix( c(
ESCC.high.BMI..EAC.low.BMI.alive,
ESCC.low.BMI..EAC.high.BMI.alive,
ESCC.high.BMI..EAC.low.BMI.dead,
ESCC.low.BMI..EAC.high.BMI.dead
),nrow=2,ncol=2) 
colnames(death.matrix) <- c("Alive","Dead")
rownames(death.matrix) <- c("ESCC.high.BMI..EAC.low.BMI", "ESCC.high.BMI..EAC.low.BMI")
death.matrix
#
chisq.test(death.matrix,
           correct=FALSE)
# Pearson's Chi-squared test
# data:  death.matrix
# X-squared = 1.2102, df = 1, p-value = 0.2713



# from Cell MS Supp Table (just dead vs alive)
# TCGA-CDR
death.matrix <-  matrix( c(64,44,32,45),nrow=2,ncol=2) 
colnames(death.matrix) <- c("Alive","Dead")
rownames(death.matrix) <- c("ESCC","EAC")
death.matrix
chisq.test(death.matrix,
           correct=FALSE)
# more deaths in EAC









# ##############################################################
# survival analysis
# ##############################################################
library(survival)
library(ggplot2)
library(ggthemes)
library(survminer) # required for ggsurvplot

tempdata$Overall.Survival.Status
tempdata$Overall.Survival..Months.

names(tempdata)[names(tempdata) == 'Overall.Survival.Status'] <- 'OS_EVENT'
names(tempdata)[names(tempdata) == 'Overall.Survival..Months.'] <- 'OS'
names(tempdata)[names(tempdata) == 'Disease.Free..Months.'] <- 'DFS'
names(tempdata)[names(tempdata) == 'Disease.Free.Status'] <- 'DFS_EVENT'

class(tempdata$OS_EVENT) # factor
tempdata$OS_EVENT <- as.character(tempdata$OS_EVENT)
class(tempdata$OS_EVENT) # factor
tempdata$DFS_EVENT <- as.character(tempdata$DFS_EVENT)
#
table(tempdata$OS_EVENT)
tempdata$OS_EVENT[tempdata$OS_EVENT=="DECEASED"] <-1
tempdata$OS_EVENT[tempdata$OS_EVENT=="LIVING"] <-0 # aka cencored 
#
tempdata$DFS_EVENT[tempdata$DFS_EVENT=="Recurred/Progressed"] <-1
tempdata$DFS_EVENT[tempdata$DFS_EVENT=="DiseaseFree"] <-0 # aka cencored 

tempdata$OS_EVENT <- as.numeric(tempdata$OS_EVENT)
tempdata$DFS_EVENT <- as.numeric(tempdata$DFS_EVENT)
#

survdiff(Surv(OS, OS_EVENT) ~ BMI.group ,data=tempdata , rho=0)  # 
survdiff(Surv(OS, OS_EVENT) ~ BMItwogroup ,data=tempdata , rho=0)  # 
survdiff(Surv(DFS, DFS_EVENT) ~ BMI.group ,data=tempdata , rho=0)  # 
survdiff(Surv(DFS, DFS_EVENT) ~ BMItwogroup ,data=tempdata , rho=0)  # 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
table(tempdata$Histological.Type)



table(tempdata$OS_EVENT)
# 0   1 
# 108  76
table(tempdata$DFS_EVENT)
# 0  1 
# 72 70



# try smaller groups
# LEAN VS OVERWEIGHT
testar <- testar[which(testar$BMI.group != "NA"),]
testar <- tempdata
testar$OS.years <- testar$OS/12 
testar$DFS.years <- testar$DFS/12 
testar <- testar[which(testar$BMI.group != "obese"),]
survdiff(Surv(OS, OS_EVENT) ~ BMI.group ,data=testar , rho=0)  # 
survdiff(Surv(DFS, DFS_EVENT) ~ BMI.group ,data=testar , rho=0)  # 

fit=survfit(Surv(OS.years, OS_EVENT) ~ BMI.group,data=testar)
pdf(paste(dataset,"-OS-lean-vs-overweight-cBioPortal.pdf",sep=""))
p <- ggsurvplot(fit, risk.table = FALSE,
                pval = TRUE, risk.table.y.text.col = TRUE, break.time.by=1,conf.int=FALSE, censor=TRUE, legend="top", palette=c("#53a362","#d9703c"))
# p$table <- p$table
print(p)
dev.off()
fit=survfit(Surv(DFS.years, DFS_EVENT) ~ BMI.group,data=testar)
pdf(paste(dataset,"-DFS-lean-vs-overweight-cBioPortal.pdf",sep=""))
p <- ggsurvplot(fit, risk.table = FALSE,
                pval = TRUE, risk.table.y.text.col = TRUE, break.time.by=1,conf.int=FALSE, censor=TRUE, legend="top", palette=c("#53a362","#d9703c"))
# p$table <- p$table
print(p)
dev.off()
# remove(testar)





# 
# 
# # LEAN VS OBESE
# testar <- tempdata
# testar <- testar[which(testar$BMI.group != "NA"),]
# testar$OS.years <- testar$OS/12 
# testar$DFS.years <- testar$DFS/12 
# testar <- testar[which(testar$BMI.group != "overweight"),]
# survdiff(Surv(OS, OS_EVENT) ~ BMI.group ,data=testar , rho=0)  # 
# survdiff(Surv(DFS, DFS_EVENT) ~ BMI.group ,data=testar , rho=0)  # 
# fit=survfit(Surv(OS.years, OS_EVENT) ~ BMI.group,data=testar)
# pdf(paste(dataset,"-OS-lean-vs-obese-cBioPortal.pdf",sep=""))
# p <- ggsurvplot(fit, risk.table = FALSE,
#                 pval = TRUE, risk.table.y.text.col = TRUE, break.time.by=1,conf.int=FALSE, censor=TRUE, legend="top", palette=c("#53a362","#d9703c"))
# # p$table <- p$table
# print(p)
# dev.off()
# fit=survfit(Surv(DFS.years, DFS_EVENT) ~ BMI.group,data=testar)
# pdf(paste(dataset,"-DFS-lean-vs-obese-cBioPortal.pdf",sep=""))
# p <- ggsurvplot(fit, risk.table = FALSE,
#                 pval = TRUE, risk.table.y.text.col = TRUE, break.time.by=1,conf.int=FALSE, censor=TRUE, legend="top", palette=c("#53a362","#d9703c"))
# # p$table <- p$table
# print(p)
# dev.off()
# remove(testar)
# # OVERWEIGHT VS OBESE
# testar <- testar[which(testar$BMI.group != "NA"),]
# testar <- tempdata
# testar$OS.years <- testar$OS/12 
# testar$DFS.years <- testar$DFS/12 
# testar <- testar[which(testar$BMI.group != "overweight"),]
# survdiff(Surv(OS, OS_EVENT) ~ BMI.group ,data=testar , rho=0)  # 
# survdiff(Surv(DFS, DFS_EVENT) ~ BMI.group ,data=testar , rho=0)  # 
# 
# fit=survfit(Surv(OS.years, OS_EVENT) ~ BMI.group,data=testar)
# pdf(paste(dataset,"-OS-overweight-vs-obese-cBioPortal.pdf",sep=""))
# p <- ggsurvplot(fit, risk.table = FALSE,
#                 pval = TRUE, risk.table.y.text.col = TRUE, break.time.by=1,conf.int=FALSE, censor=TRUE, legend="top", palette=c("#53a362","#d9703c"))
# # p$table <- p$table
# print(p)
# dev.off()
# fit=survfit(Surv(DFS.years, DFS_EVENT) ~ BMI.group,data=testar)
# pdf(paste(dataset,"-DFS-overweight-vs-obese-cBioPortal.pdf",sep=""))
# p <- ggsurvplot(fit, risk.table = FALSE,
#                 pval = TRUE, risk.table.y.text.col = TRUE, break.time.by=1,conf.int=FALSE, censor=TRUE, legend="top", palette=c("#53a362","#d9703c"))
# # p$table <- p$table
# print(p)
# dev.off()
# 
# 
#
#
