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
dir.create(paste("./Spearman/",dataset,"/heatmap",sep=""))















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







































# ************************************************************
# ************************************************************
# ************************************************************
# ************************************************************







# /////////////////////////////////////////////////
# /////////////////////////////////////////////////
# /////////////////////////////////////////////////
# denote dataset of interest
library("openxlsx")
# paste("./Spearman/",dataset,"-Spearman.xlsx",sep="") # save location # file name


# LOAD FROM SCRIPT 1
# spearman.results.SigOnly <- read.xlsx(paste("./Spearman/",dataset,"-Spearman-SigOnly.xlsx",sep=""))
# DATA FROM SCRIPT 10B ... NO F-TEST AND T-TEST FILTERED
#
#
#
# paste("./Spearman/",dataset,"-Spearman-SigOnly",sep="")
spearman.results.SigOnly <- read.xlsx(paste("./Spearman/",dataset,"-Spearman-SigOnly.xlsx",sep=""))
# DATA FROM SCRIPT 13
# /////////////////////////////////////////////////
# /////////////////////////////////////////////////
# /////////////////////////////////////////////////


# how many AC are overweight and obese?
length(  which(the.Big.df.SUBSET$BMItwogroup == "overweightETobese" & the.Big.df.SUBSET$Histological.Type == "AC") )
# 46
# how many ESCC are overweight and obese?
length ( which(the.Big.df.SUBSET$BMItwogroup == "overweightETobese" & the.Big.df.SUBSET$Histological.Type == "ESCC") )
# 16

table(the.Big.df.SUBSET$Histological.Type)
# AC 66  ESCC 74 
table(the.Big.df.SUBSET$Histological.Type...Oesophagus)
# EAC ESCC 
# 49 74 

# let us check expression of a gene that should be high in BMI (copied from heatmap)
# ENSG00000185499.15	12245	0.543958734	4.40593E-13	2.66484E-08	2.66484E-08	2.66484E-08	MUC1	chr1	protein_coding
#
AC <- the.Big.df.SUBSET[which( the.Big.df.SUBSET$Histological.Type == "AC"),]
ESCC <- the.Big.df.SUBSET[which( the.Big.df.SUBSET$Histological.Type == "ESCC"),]
mean(AC$ENSG00000185499.15) #   21783514 *much* higher 
mean(ESCC$ENSG00000185499.15) #  354832.6

# let us test another one that should be low(blue) in high BMI!
# 5174	ENSG00000121864.8	15710	-0.55058473	2.00335E-13	1.21168E-08	1.21168E-08	1.21168E-08	ZNF639	chr3	protein_coding
# so... should be high in ESCC!!!
mean(AC$ENSG00000121864.8) #      111555.4
mean(ESCC$ENSG00000121864.8) #    200883.1  # CORRECT

# TP63
# ENSG00000073282.11	3502	-0.513757314	1.29711E-11	7.84531E-07	7.84531E-07	7.84531E-07	TP63	chr3	protein_coding
mean(AC$ENSG00000073282.11) #     46035.98
mean(ESCC$ENSG00000073282.11) #  2493773
# Nature MS: Squamous Cell Carcinomas have TP63 amplifications 


