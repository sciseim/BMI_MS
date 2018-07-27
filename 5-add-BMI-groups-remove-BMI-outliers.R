# Add groups to SampleInfo
# ##############################
# these have BMI data
# BLCA	Bladder Cancer
# CESC	Cervical Cancer
# CHOL	Bile Duct Cancer
# DLBC	Large B-cell Lymphoma
# ESCA	Esophageal Cancer 
# KIRP	Kidney Papillary Cell Carcinoma
# LIHC	Liver Cancer
# UCEC	Endometrioid Cancer
# UCS	Uterine Carcinosarcoma
# UVM	Ocular Melanomas
# COAD	Colon Cancer
# READ	Rectal Cancer

# ////////////////////////////////
# remove samples with NA bmi
class(SampleInfo.df) # 3174 samples
SampleInfo.df$bmi # a factor ... fix below
SampleInfo.df$bmi <- as.character(SampleInfo.df$bmi)
SampleInfo.df = SampleInfo.df[-grep("--", SampleInfo.df$bmi),] # 2139
SampleInfo.df$bmi <- as.numeric(SampleInfo.df$bmi)
SampleInfo.df$bmi
#
# this it remove rows with NA ... here --
# completeFun <- function(data, desiredCols) {
#  completeVec <- complete.cases(data[, desiredCols])
#  return(data[completeVec, ]) }
# SampleInfo.df <- completeFun(SampleInfo.df, "bmi")
# ////////////////////////////////




# ////////////////////////////////
# log BMI values to get more 'normal' data
class(SampleInfo.df$bmi) # a factor!
# to generate numeric, must convert to character FIRST!
SampleInfo.df$bmi <- as.character(SampleInfo.df$bmi)
SampleInfo.df$bmi <- as.numeric(SampleInfo.df$bmi)
length( which(SampleInfo.df$bmi >= 30) ) # 780 patients are OBESE
length( which(SampleInfo.df$bmi < 30) ) # 1,539 patients are not OBESE
# should normalise BMI values!!
# normalised by log-transforming
SampleInfo.df$bmi.LOG <- log2(SampleInfo.df$bmi)
# ////////////////////////////////




# ////////////////////////////////
# add useful group: OBSESE and the rest
the.obese.rownames <- row.names(SampleInfo.df[which(SampleInfo.df$bmi >= 30),])
SampleInfo.df.obeseONLY <- SampleInfo.df[the.obese.rownames,]
#
the.overweight.rownames <- row.names(SampleInfo.df[which(SampleInfo.df$bmi < 30 & SampleInfo.df$bmi >=25),])
SampleInfo.df.overweightONLY <- SampleInfo.df[the.overweight.rownames,]
length(SampleInfo.df.obeseONLY$case_id) # 780
length(SampleInfo.df.overweightONLY$case_id) # 719
SampleInfo.df.obeseONLY$BMI.group <- "obese" 
SampleInfo.df.overweightONLY$BMI.group <- "overweight" 
#
SampleInfo.df.obeseONLY$BMItwogroup <- "overweightETobese" 
SampleInfo.df.overweightONLY$BMItwogroup <- "overweightETobese" 
class(the.obese.rownames) # character
#
# and the NORMAL BMI <25
remove.these <- c(the.obese.rownames,the.overweight.rownames)
length(remove.these) # 1499
class(remove.these) # character
length(SampleInfo.df$case_id) # 2,319
'%ni%' <- Negate('%in%') # let us get the opposite!
rows.to.keep<-which(rownames(SampleInfo.df) %ni% remove.these) 
SampleInfo.df.remaining <- SampleInfo.df[rows.to.keep,]
SampleInfo.df.remaining$BMI.group <- "normal" 
SampleInfo.df.remaining$BMItwogroup <- "normal" 
length(SampleInfo.df.remaining$case_id) # 820
#
# 820+780+719 = 2,319 correct

# combine them back!
temp <- rbind(SampleInfo.df.remaining,SampleInfo.df.overweightONLY,SampleInfo.df.obeseONLY)
head(temp)
# write temp to file to quickly assess in Excel
library("openxlsx")
#@ write.xlsx(temp, "./temp.xlsx", asTable = FALSE,row.names=TRUE, colNames=TRUE)
# appears to be some short people/dwarfs ... their BMI values should be removed since >131-400 !!! 
# higher BMI than https://en.wikipedia.org/wiki/List_of_the_heaviest_people
# see temp-identified-BMI-outliers.xlsx
#

# works up to this step
head(temp) 
temp$File.ID <- as.character(temp$File.ID) # otherwise grep *will* fail
# File.ID
# bac1f38e-728b-43d5-a7b4-f8c1c6d479fb
# 8e926857-2e92-4ac7-adfd-c6a6b6faf60d
# b3187c04-3566-4953-9ede-f6472e891685
# 142d1d72-6148-40fc-bce3-620314c6a9a8
# 1d939a56-2511-4f2b-83c2-1359aee50ac6
temp = temp[-grep("bac1f38e-728b-43d5-a7b4-f8c1c6d479fb", temp$File.ID),]
temp = temp[-grep("8e926857-2e92-4ac7-adfd-c6a6b6faf60d", temp$File.ID),]
temp = temp[-grep("b3187c04-3566-4953-9ede-f6472e891685", temp$File.ID),]
temp = temp[-grep("142d1d72-6148-40fc-bce3-620314c6a9a8", temp$File.ID),]
temp = temp[-grep("1d939a56-2511-4f2b-83c2-1359aee50ac6", temp$File.ID),]
# ////////////////////////////////
head(temp)

# AND RENAME AGAIN TO THE ORIGINAL DF
SampleInfo.df <- temp
table(SampleInfo.df$BMI.group)
# normal    obese overweight 
# 820        775        719 
# now have BMI groups, allowing us colours in heatmap, ANOVA, etc
length(SampleInfo.df$case_id) # 2314
# ////////////////////////////////

# remember, these are all tumours
table(SampleInfo.df$Sample.Type)
# removed the other samples in 
# 2-load-sample-data.R  -- later we can edit this script to keep only Normal samples etc


SampleInfo.df$age_at_diagnosis <- as.character(SampleInfo.df$age_at_diagnosis)
SampleInfo.df$age_at_diagnosis <- as.numeric(SampleInfo.df$age_at_diagnosis)
SampleInfo.df$gender <- as.character(SampleInfo.df$gender)
SampleInfo.df$race <- as.character(SampleInfo.df$race)

# e.g. TCGA-BLCA
SampleInfo.df$project_id <- as.character(SampleInfo.df$project_id)





# ////////////////////////////////
# ////////////////////////////////
# ////////////////////////////////
# ////////////////////////////////

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# IMPORTANT
# FILTER SCRIPT 2 WILL BE TO ONLY INCLUDE CERTAIN Sample Type
table(SampleInfo.df$Sample.Type)
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

library("openxlsx")
write.xlsx(SampleInfo.df, "./SampleInfo_BMIonly.xlsx", asTable = FALSE,row.names=TRUE, colNames=TRUE)






