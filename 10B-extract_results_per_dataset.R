# X-10B
my.data.set <- dataset
the.Big.df.SUBSET <- the.Big.df 
keep.these <- which(the.Big.df.SUBSET$project_id == dataset) # given index
the.Big.df.SUBSET <- the.Big.df.SUBSET[keep.these,] # 2,314 all - TCGA-BLCA 


# Int. J. Obesity Table 1: Data were described as mean (lower quartile–upper quartile).
BMI.mean <- mean(the.Big.df.SUBSET$bmi)
# 
quantileinfo <- quantile(the.Big.df.SUBSET$bmi)
# so
BMI.firstquartile <- quantileinfo[2] # 25%
BMI.thirdquartile <- quantileinfo[4] # 75%
# change sig.figs
BMI.mean <- format(round(BMI.mean, 2), nsmall = 2)
BMI.firstquartile <- format(round(BMI.firstquartile, 2), nsmall = 2)
BMI.thirdquartile <- format(round(BMI.thirdquartile, 2), nsmall = 2)
#
paste(BMI.mean," (",BMI.firstquartile,"-",BMI.thirdquartile,")",sep="")
my.BMI.info <- paste(BMI.mean," (",BMI.firstquartile,"-",BMI.thirdquartile,")",sep="")

# also want to know how many are obese etc in Table 1
No.obese  <-  length( which(the.Big.df.SUBSET$BMI.group == "obese") )
No.overweight <- length( which(the.Big.df.SUBSET$BMI.group == "overweight") )
No.normal <-  length( which(the.Big.df.SUBSET$BMI.group == "normal") )
table(the.Big.df.SUBSET$BMI.group)


# Ncor
# load Excel sheet
# how many are ALL, UP, DOWN?
library("openxlsx")
# paste("./Spearman/",dataset,"-Spearman.xlsx",sep="") # save location # file name
spearman.results <- read.xlsx(paste("./Spearman/",dataset,"-Spearman.xlsx",sep=""))
# length(spearman.results$the.gene.name) # 47402

length(which(spearman.results$bonferroni.P <= 0.001 & abs(spearman.results$spearman.r) >= 0.50)) # 145
colnames(spearman.results)

# //////////////////////////////////////////////////////////////////
# let us look up the actual gene symbols!
# source("X-obtain-gene-symbol-from-gencode_GTF.R")
# call once in 10-Table1.R
# subset gencode.df.READY
# want to keep
# seqnames # e.g. chr1
# gene_id # e.g. ENSG00000223972.5
# gene_type # e.g. transcribed_unprocessed_pseudogene
# gene_name # DDX11L1
load(file="gencode.df.READY.Robj")
gencode.df.READY.subset <- subset(gencode.df.READY, select=c(gene_id,gene_name,seqnames,gene_type))
head(gencode.df.READY.subset)
# add to 10B  once finished ..
spearman.results <- merge(x=spearman.results, y=gencode.df.READY.subset, by.x="the.gene.name", by.y="gene_id",all.x = TRUE)
# //////////////////////////////////////////////////////////////////
# head(spearman.results[1:5,1:10]) 
# length(spearman.results$ENSEMBL.gene) # 58,153

# which(gencode.df.READY.subset$gene_name=="LEPR")
# 1890
# gencode.df.READY.subset[1890,]
#                  gene_id gene_name seqnames      gene_type
# 90064 ENSG00000116678.17      LEPR     chr1 protein_coding







REGRESSION.RHO.CUTOFF <- 0.30
# Bonferroni would be 0.05/length(gene.names)
# 8.266786e-07
# this is ... 
# ADJ.P.CUTOFF <- 0.05/length(gene.names)
ADJ.P.CUTOFF <- 0.01
keep.these <- which(abs(spearman.results$spearman.r) >= REGRESSION.RHO.CUTOFF & spearman.results$bonferroni.P <= ADJ.P.CUTOFF)
length(keep.these) # 99 genes in ESCA
spearman.results <- spearman.results[keep.these,]



# length(which((spearman.results$spearman.r) >= REGRESSION.RHO.CUTOFF)) # 54 
# length(which((spearman.results$spearman.r) <= -REGRESSION.RHO.CUTOFF)) # 45
#
my.N.Cor.All <- length(spearman.results$spearman.r)
my.N.Cor.UP <- length(which((spearman.results$spearman.r) >= REGRESSION.RHO.CUTOFF)) # 54 
my.N.Cor.DOWN <- length(which((spearman.results$spearman.r) <= -REGRESSION.RHO.CUTOFF)) # 45
#
my.data.set
my.no.genes.examined
my.BMI.info
my.N.Cor.All
my.N.Cor.UP
my.N.Cor.DOWN
# ~~~~~~~~~~~~~~~~~~~~~~~~~~








write.xlsx(spearman.results, paste("./Spearman/",dataset,"-Spearman-SigOnly.xlsx",sep=""), asTable = FALSE,row.names=TRUE, colNames=TRUE)
#
#
#
# a - in string/dfs/etc does not work in R, so
# TCGA-ESCA.df *must* become TCGA.ESCA.df
my.data.set <- gsub("-",".",dataset)
#
#
assign(paste(my.data.set,".df",sep=""), cbind(dataset,my.no.genes.examined,my.BMI.info,No.normal,No.overweight,No.obese,my.N.Cor.All,my.N.Cor.UP,my.N.Cor.DOWN)   )
# so later we will have e.g. TCGA-ESCA.df
# merge at the end
# TCGA.ESCA.df
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
