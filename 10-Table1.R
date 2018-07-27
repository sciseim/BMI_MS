# create Table 1
# BMI-related genes identified by Spearman’s correlation test
setwd("~/Downloads/BMI_vs_HFD/GDC/SCRIPTS/")
#@ load("the.Big.df.Robj", .GlobalEnv)
# load("../../the.Big.df.Robj", .GlobalEnv)
length.of.df <- length(the.Big.df[1,]) # 60529 (47 are sampleinfo)
colnames(the.Big.df) # e.g. genes ... plus *46 SampleInfo columns*
gene.names <- names ( the.Big.df[,47:length.of.df] )



# ///////////////////////////////////////////
# obtain gene names!
#@ source("X-obtain-gene-symbol-from-gencode_GTF.R")
# gencode.df.READY
# add column info in script 10B but load her to avoid 
# loading 1.3GB file several time
#
# parsed it already . Now only 1.7MB ... 60483 genes
# 
load(file="gencode.df.READY.Robj")
# ///////////////////////////////////////////







# ///////////////////////////////////////////
# OUTPUT
my.no.genes.examined <- length(gene.names) # 60483
# ///////////////////////////////////////////




# for each data set...
table(the.Big.df$project_id)
class(the.Big.df$project_id) # character
# testing
# dataset <- "TCGA-ESCA"
# source("10B-extract_results_per_dataset.R")
THE.DATA.SET.NAMES <- unique( the.Big.df$project_id )


# remove(modellist)
# modellist <- list() # call a list
#
for(i in THE.DATA.SET.NAMES){
dataset <-i
# or dummy
# dataset <- "TCGA-ESCA"

print(i)
source("10B-extract_results_per_dataset.R")
# cat("KAM")
}


# merge the DFs here
Table1 <- rbind(TCGA.BLCA.df,TCGA.CESC.df,TCGA.CHOL.df,TCGA.COAD.df,TCGA.DLBC.df,TCGA.ESCA.df,TCGA.KIRP.df,TCGA.LIHC.df,TCGA.READ.df,TCGA.UCEC.df,TCGA.UCS.df,TCGA.UVM.df)

# if only TCGA-ESCA
# Table1 <- rbind(TCGA.ESCA.df)
# 
# Table1 <- rbind(TCGA.ESCA.df,TCGA.ESCA.df,TCGA.ESCA.df,TCGA.ESCA.df)
head(Table1)

# write SampleInfo to file to quickly assess in Excel
library("openxlsx")
write.xlsx(Table1, paste("./Spearman/Table1.xlsx",sep=""), asTable = FALSE,row.names=FALSE, colNames=TRUE)
# END OF FILE

