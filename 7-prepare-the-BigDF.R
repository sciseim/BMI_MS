# 7
#
# load the data set
# setwd("~/Downloads/BMI_vs_HFD/GDC/SCRIPTS/")
load("GeneExpression.READY.Robj", .GlobalEnv)
load("SampleInfo.READY.Robj", .GlobalEnv)
#
#
#
geneofinterest <- "ENSG00000167578.15" # 
# subset GeneExpression.df
all.GeneExpression.row.names.AKA.filename <- row.names(GeneExpression.READY.df)
# obtain the expression of your gene of interest
GeneExpression.geneofinterest <- GeneExpression.READY.df[,c(geneofinterest)] 
# THIS WORKS NICELY, BUT WOULD BE NICE TO JUST MERGE WITH sample information DF and further subset


# later -- subset by cancer type
# 
# SampleInfo.READY.df$project_id <- as.character(SampleInfo.READY.df$project_id)
SampleInfo.READY.df$project_id
table(SampleInfo.READY.df$project_id)
# TCGA-BLCA TCGA-CESC TCGA-CHOL
# 358       259        35 
# TCGA-COAD TCGA-DLBC TCGA-ESCA
# 231        48       152 
# TCGA-KIRP TCGA-LIHC TCGA-READ
# 210       334       72
# TCGA-UCEC  TCGA-UCS  TCGA-UVM 
# 511        51        53 




# actually, can merge the DFs... just assign genes from e.g. in loop
# e.g. the.Big.df is 1-46 SampleInfo ... then GeneExpression
gene.names <- colnames(GeneExpression.READY.df)
row.names(GeneExpression.READY.df) # file names
#
row.names(SampleInfo.READY.df) # currently TCGA-UY-A78M
temp <- SampleInfo.READY.df
row.names(temp) <- temp$File.Name
#
#
remove(the.merged.df)
# list of data.frames I want to merge
library(data.table)
l = list(temp,GeneExpression.READY.df)
#
the.merged.df <- transform(Reduce(merge, lapply(l, function(x) data.frame(x, rn = row.names(x)))), row.names=rn, rn=NULL)
head(the.merged.df)  # we now have the File.Names
# this works MUCH better than merge by row.names!


the.merged.df # 60483 variables... first 46 are sample information
the.Big.df <- the.merged.df
#
the.Big.df[1,1:47] # this is correct!
save(the.Big.df, file = "the.Big.df.Robj")
