setwd("~/Downloads/BMI_vs_HFD/GDC/SCRIPTS/")
load("the.Big.df.Robj", .GlobalEnv)
length.of.df <- length(the.Big.df[1,]) # 60529 (47 are sampleinfo)
colnames(the.Big.df) # e.g. genes ... plus *46 SampleInfo columns*
gene.names <- names ( the.Big.df[,47:length.of.df] )
#
dataset <- "TCGA-ESCA"
the.Big.df.SUBSET <- the.Big.df 
keep.these <- which(the.Big.df.SUBSET$project_id == dataset) # given index
the.Big.df.SUBSET <- the.Big.df.SUBSET[keep.these,] 

# create output directory
dir.create(paste("./Spearman/",dataset,"/survival",sep=""))

tempdata <- the.Big.df.SUBSET
# ///////////////////////////////////////////////////////
# TCGA-ESCA only
# some in the Nature S1 (barcode) are in gdc_sample_sheet (Case ID) ...
# OK, so open
library("openxlsx")
Nature.MS.S1 <- read.xlsx("~/Downloads/BMI_vs_HFD/GDC/SCRIPTS/ESCA-data-from-NatureMS-TableS1.xlsx")
head(Nature.MS.S1)
# ##############################################
# ##############################################
row.names(Nature.MS.S1) <- Nature.MS.S1$barcode
row.names(the.Big.df.SUBSET) <- the.Big.df.SUBSET$Case.ID

# MERGE THEM
remove(the.merged.df)
# list of data.frames I want to merge
library(data.table)
l = list(Nature.MS.S1,the.Big.df.SUBSET) # put Nature first, so all the genes go a the end of the DF!
#
the.merged.df <- transform(Reduce(merge, lapply(l, function(x) data.frame(x, rn = row.names(x)))), row.names=rn, rn=NULL)
head(the.merged.df[1:10,1:10])  # we now have the File.Names
# this works MUCH better than merge by row.names!
# ##############################################
tempdata <- the.merged.df
# ##############################################
# ///////////////////////////////////////////////////////

# ##############################################################
# survival analysis
# ##############################################################
library(survival)
library(ggplot2)
library(ggthemes)
library(survminer) # required for ggsurvplot



tempdata$Days.to.last.known.alive
# tempdata$Days.from.surgery.to.last.followup # nothing


# Survival censoring was determined by the Vital Status label and if a patient was not alive the Days to Death data was used, otherwise Days to last known alive (if present) or Days to last follow-up were used.
# DEAD
# The time_to_event is in days, equals to days_to_death if patient deceased
tempdata.DEAD <- tempdata[  which(tempdata$Vital.Status == "Dead") , ]
# Days.from.surgery.to.death is BLANK!
tempdata.DEAD$OS <- tempdata.DEAD$days_to_death 
# tempdata.DEAD$OS <- tempdata.DEAD$Days.from.surgery.to.death # blank!
tempdata.DEAD$OS # 54 ESCA-TCGA patients
#
# or use the same as below!
# tempdata.DEAD$OS <- tempdata.DEAD$Days.to.last.known.alive



# ALIVE
# in the case of a patient is still living, the time variable is the maximum(days_to_last_known_alive, days_to_last_followup).
tempdata.ALIVE <- tempdata[  which(tempdata$Vital.Status == "Alive") , ]
 tempdata.ALIVE$OS <- tempdata.ALIVE$Days.to.last.known.alive
# or use the same as above!
# tempdata.ALIVE$OS <- tempdata.ALIVE$days_to_death



# merge them back
tempdata <- rbind(tempdata.ALIVE,tempdata.DEAD)

class(tempdata$OS)
tempdata$OS <- as.numeric(tempdata$OS)

# and OS status
# using paper from the Nature paper here, so use Vital.Status
# column instead of vital_status 
tempdata$vital_status <- as.character(tempdata$vital_status)
tempdata$Vital.Status <- as.character(tempdata$Vital.Status)
#
table(tempdata$Vital.Status)
table(tempdata$vital_status)
# each 88 54 Alive dEad
names(tempdata)[names(tempdata) == 'Vital.Status'] <- 'OS_EVENT'
tempdata$OS_EVENT[tempdata$OS_EVENT=="Dead"] <-1
tempdata$OS_EVENT[tempdata$OS_EVENT=="Alive"] <-0 # aka cencored 
class(tempdata$OS_EVENT)
tempdata$OS_EVENT <- as.numeric(tempdata$OS_EVENT)
# "1" denotes occurrence of your designated experimental outcome (death or other specified

#
survdiff(Surv(OS, OS_EVENT) ~ BMI.group ,data=tempdata , rho=0)  # 
survdiff(Surv(OS, OS_EVENT) ~ BMItwogroup ,data=tempdata , rho=0)  # 

survdiff(Surv(OS.years, OS_EVENT) ~ BMI.group ,data=tempdata , rho=0)  # 


# testing misc random...
tempdata$Number.pack.years.smoked
tempdata$Days.to.last.known.alive
#
survdiff(Surv(as.numeric(days_to_last_known_disease_status), OS_EVENT) ~ BMI.group ,data=tempdata , rho=0)  # 
min(tempdata$Days.to.last.known.alive) # 4 
tempdata$Histological.Type...Oesophagus
tempdata$Histological.Type
survdiff(Surv(OS, OS_EVENT) ~ Histological.Type ,data=tempdata , rho=0)  # 
#
#



# GDC GDC GDC GDC GDC GDC GDC GDC GDC GDC GDC GDC GDC GDC GDC GDC GDC GDC 
# GDC
# https://gdc.cancer.gov/about-data/data-harmonization-and-generation/clinical-data-harmonization


# days_to_last_known_disease_status
# Time interval from the date of last follow up to the date of initial pathologic diagnosis, represented as a calculated number of days.
tempdata$days_to_last_known_disease_status
# BLANK

# days_to_last_follow_up
# Time interval from the date of last follow up to the date of initial pathologic diagnosis, represented as a calculated number of days.
# tempdata$days_to_last_follow_up
tempdata$days_to_last_follow_up <- as.character(tempdata$days_to_last_follow_up)
tempdata$days_to_last_follow_up <- as.numeric(tempdata$days_to_last_follow_up)


# days_to_recurrence
# Number of days between the date used for index and the date the patient was diagnosed with a recurrent malignancy.
# tempdata$days_to_recurrence
#  BLANK
# testing them here
survdiff(Surv(as.numeric(days_to_last_follow_up), OS_EVENT) ~ BMI.group ,data=tempdata , rho=0)  # 
# GDC GDC GDC GDC GDC GDC GDC GDC GDC GDC GDC GDC GDC GDC GDC GDC GDC GDC 





# DRAW PLOT
tempdata$OS # currently in days 
tempdata$OS.years <- tempdata$OS/365 

# Days.to.last.known.alive
fit=survfit(Surv(OS.years, OS_EVENT) ~ BMI.group,data=tempdata)
pdf(paste(dataset,"survplot-OS-BMI.group.pdf",sep=""),  width = 7, family = "Helvetica") 
p <- ggsurvplot(fit, risk.table = FALSE,
                pval = TRUE, risk.table.y.text.col = TRUE, break.time.by=1,conf.int=FALSE, censor=TRUE, legend="top", palette=c("#53a362","#d82e2e","#d9703c"))
# p$table <- p$table
print(p)
dev.off()

# currently 
# normal overweight obese
# "#53a362", "#d9703c", "#d82e2e"
# normal obese overweight


# draw a plot
fit=survfit(Surv(OS.years, OS_EVENT) ~ BMItwogroup,data=tempdata)
pdf(paste(dataset,"survplot-OS-BMItwogroup.pdf",sep=""))
p <- ggsurvplot(fit, risk.table = FALSE,
                pval = TRUE, risk.table.y.text.col = TRUE, break.time.by=1,conf.int=FALSE, censor=TRUE, legend="top", palette=c("#53a362","#d82e2e"))
# p$table <- p$table
print(p)
dev.off()






# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LET US OUTPUT LEAN VS GROUPS etc
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# first colour 
# ("#53a362","#d82e2e","#d9703c")
# green red orange

# LEAN VS OVERWEIGHT
#
testar <- tempdata
testar$OS.years <- testar$OS/365 
testar <- testar[which(testar$BMI.group != "obese"),]
survdiff(Surv(OS, OS_EVENT) ~ BMI.group ,data=testar , rho=0)  # 
fit=survfit(Surv(OS.years, OS_EVENT) ~ BMI.group,data=testar)
testar$BMI.group
pdf(paste(dataset,"-TESTAR-lean-vs-overweight.pdf",sep=""))
p <- ggsurvplot(fit, risk.table = FALSE,
                pval = TRUE, risk.table.y.text.col = TRUE, break.time.by=1,conf.int=FALSE, censor=TRUE, legend="top", palette=c("#53a362","#d9703c"))
# p$table <- p$table
print(p)
dev.off()
#
# LEAN VS OBESE
#
testar <- tempdata
testar$OS.years <- testar$OS/365 
testar <- testar[which(testar$BMI.group != "overweight"),]
survdiff(Surv(OS, OS_EVENT) ~ BMI.group ,data=testar , rho=0)  # 
fit=survfit(Surv(OS.years, OS_EVENT) ~ BMI.group,data=testar)
testar$BMI.group
pdf(paste(dataset,"-TESTAR-lean-vs-obese.pdf",sep=""))
p <- ggsurvplot(fit, risk.table = FALSE,
                pval = TRUE, risk.table.y.text.col = TRUE, break.time.by=1,conf.int=FALSE, censor=TRUE, legend="top", palette=c("#53a362","#d82e2e"))
# p$table <- p$table
print(p)
dev.off()
# OVERWEIGHT VS OBESE
#
testar <- tempdata
testar$OS.years <- testar$OS/365 
testar <- testar[which(testar$BMI.group != "normal"),]
survdiff(Surv(OS, OS_EVENT) ~ BMI.group ,data=testar , rho=0)  # 
fit=survfit(Surv(OS.years, OS_EVENT) ~ BMI.group,data=testar)
testar$BMI.group
pdf(paste(dataset,"-TESTAR-obese-vs-overweight.pdf",sep=""))
p <- ggsurvplot(fit, risk.table = FALSE,
                pval = TRUE, risk.table.y.text.col = TRUE, break.time.by=1,conf.int=FALSE, censor=TRUE, legend="top", palette=c("#d82e2e","#d9703c"))
# p$table <- p$table
print(p)
dev.off()


