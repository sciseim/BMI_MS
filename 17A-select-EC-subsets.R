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
dir.create(paste("./Spearman-EAC/",sep=""))
dir.create(paste("./Spearman-ESCC/",sep=""))
dir.create(paste("./Spearman-EAC/","/heatmap",sep=""))
dir.create(paste("./Spearman-ESCC/","/heatmap",sep=""))





# ************************************************************
# ************************************************************
# ************************************************************
# ************************************************************
# HERE... ADD DATA FROM NATURE MANUSCRIPT.. ENSURING IT MERGES WITH CORRECT ROWS ETC!
# OK... let us load in data from the Nature MS!
the.Big.df.SUBSET <- the.Big.df[which(the.Big.df$project_id == "TCGA-ESCA"),]

# let us look at some Case.IDs
class(the.Big.df.SUBSET$Case.ID)
the.Big.df.SUBSET$Case.ID <- as.character(the.Big.df.SUBSET$Case.ID)

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
# ##############################################
the.Big.df.SUBSET <-   the.merged.df # add it back!

# match with *barcode*
# pull out
# Histological Type - Oesophagus
# Histological Type
# Oesophageal tumor location
# SCNA High/Low
# Pathologic stage	
# Grade
# Gender
# Vital Status
# add these columns!

EAC <- the.Big.df.SUBSET[which( the.Big.df.SUBSET$Histological.Type == "AC"),]
# 66 patients

ESCC <- the.Big.df.SUBSET[which( the.Big.df.SUBSET$Histological.Type == "ESCC"),]
# 74 patients
# ////////////////////////////////////////////////////////////////
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



# obtain gene names
colnames(the.Big.df.SUBSET)
length.of.df <- length(the.Big.df.SUBSET[1,]) # 60529 (118 are sampleinfo)
gene.names <- names ( the.Big.df.SUBSET[,119:length.of.df] )
length(gene.names) # 60483 ... correct
# have all the gene names in a vector now
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

REGRESSION.RHO.CUTOFF <- 0.40
# REGRESSION.RHO.CUTOFF <- 0.50 # initial screen
# Bonferroni would be 0.05/length(gene.names)
# 8.266786e-07
# this is ... 
ADJ.P.CUTOFF <- 0.05/length(gene.names)
P.CUTOFF <- 0.0001



# only keep significant ...
# class(REGRESSION.RHO.CUTOFF) # must be numeric!
# class(ADJ.P.CUTOFF) # must be numeric!


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
save.Spearman.plot <- "yes"
dir.create("Spearman-EAC")
dir.create("Spearman-ESSC")
dir.create(paste("./Spearman-EAC/",paste("rho",REGRESSION.RHO.CUTOFF,"FDR",ADJ.P.CUTOFF),sep=""))
dir.create(paste("./Spearman-ESSC/",paste("rho",REGRESSION.RHO.CUTOFF,"FDR",ADJ.P.CUTOFF),sep=""))


# EAC
EC.subset <- "EAC"
dataset.subset.DF <- EAC # 66 patients
source("17B-EC-subset-Spearman-loop.R")

# ESCC
EC.subset <- "ESCC"
dataset.subset.DF <- ESCC # 74 patients
source("17B-EC-subset-Spearman-loop.R")

table(EAC$BMI.group)
table(ESCC$BMI.group)

head(dataset.subset.DF[1:5,1:4])
head(dataset.subset.DF[1:5,116:120])
colnames(dataset.subset.DF) # includes genes
