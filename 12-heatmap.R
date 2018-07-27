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

dir.create(paste("./Spearman/",dataset,"/heatmap",sep=""))
# denote dataset of interest
library("openxlsx")
# paste("./Spearman/",dataset,"-Spearman.xlsx",sep="") # save location # file name



# /////////////////////////////////////////////////
# /////////////////////////////////////////////////
# /////////////////////////////////////////////////
# LOAD FROM SCRIPT 1
# spearman.results.SigOnly <- read.xlsx(paste("./Spearman/",dataset,"-Spearman-SigOnly.xlsx",sep=""))
# DATA FROM SCRIPT 10B ... NO F-TEST AND T-TEST FILTERED
#
#
#
spearman.results.SigOnly <- read.xlsx(paste("./Spearman/",dataset,"-Spearman-SigOnly-stats-filtered-script13-.xlsx",sep=""))
# DATA FROM SCRIPT 13
# /////////////////////////////////////////////////
# /////////////////////////////////////////////////
# /////////////////////////////////////////////////



# for each of these
the.sig.Spearman.genes <- spearman.results.SigOnly$the.gene.name

class(spearman.results.SigOnly) # df
colnames(the.Big.df.SUBSET) # inc. genes


temp <- subset(the.Big.df.SUBSET, select=c(the.sig.Spearman.genes,"BMI.group"))
# sort by BMI.group
#@ temp <- temp[order(temp$BMI.group),] # WORKS, but sort alphabetically!
# 
temp$BMI.group <- factor(temp$BMI.group, levels=c("normal","overweight","obese")) 
temp <- temp[order(temp$BMI.group),] 



head(temp)
temp.genes.only <- temp[,the.sig.Spearman.genes]
temp.BMI.info.only <- temp[,"BMI.group"]


# LOG FPKM-UQ VALUES
# add a small constant to your FPKM values (say 0.25) to account for the zero
# https://support.bioconductor.org/p/66233/
temp.genes.only <- log2(  (0.01+temp.genes.only) )






# heat map
# keepgenes <- as.character(row.names(curated.outTableF))
# mergedDF <- counts[row.names(counts) %in% keepgenes, ]

mergedMATRIX <- as.matrix(temp.genes.only)
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# get rid of blanks, NAs
# remember, we already have a matrix
class(mergedMATRIX) 
mergedMATRIX[mergedMATRIX==""] <- 0  # kill blanks
mergedMATRIX[is.na(mergedMATRIX)] <- 0 # and kill NAs 
# ... the above will choke heatmap
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Load necessary packages
library("RColorBrewer")
library("gplots")
library("devtools")
#Load latest version of heatmap.3 function
source_url("https://raw.githubusercontent.com/obigriffith/biostar-tutorials/master/Heatmaps/heatmap.3.R")


# ///// add groups ///////////////////////
groupcolours <- temp$BMI.group
class(groupcolours) # character
groupcolours <- as.factor(groupcolours)
class(groupcolours) # character
levels(groupcolours)[levels(groupcolours)=="normal"] <- "#53a362"
levels(groupcolours)[levels(groupcolours)=="overweight"] <- "#d9703c"
levels(groupcolours)[levels(groupcolours)=="obese"] <- "#d82e2e"
groupcolours <- as.character(groupcolours)
rlab=cbind(groupcolours) # can add as many as we want. E.g. colour for stage
colnames(rlab)=c("group")
# combine
rlab=t(cbind(groupcolours))

# to ensure you are not stuffing up... check out the different cluster sizes
length(which(temp$BMI.group == "normal")) # 84 samples
length(which(temp$BMI.group == "overweight")) # 46 samples
length(which(temp$BMI.group == "obese")) # 22 samples
# ///// add groups ///////////////////////


#Define custom dist and hclust functions for use with heatmaps
mydist=function(c) {dist(c,method="euclidian")}
myclust=function(c) {hclust(c,method="average")}

# COLORSET 5
cols <- rev(colorRampPalette(brewer.pal(10,"RdBu"))(256))

# COLORSET 4
cols<- colorRampPalette(c("red", "white", "blue"))(256)

# cols<-colorRampPalette( rev(brewer.pal(9, "RdBu")) )(256) # looks good
# just work out whether up or down in normal ... rev or not
# ENSG00000168237.16 elevated in obesity, so


# F4.large

# COLORSET 3
cols<- colorRampPalette(c("#245bab", "#fefaf1", "#fdbf3a"))(256)
# cols<- colorRampPalette(c("#954f98", "#fffefc", "#eaef5f"))(256)


# COLORSET 2
cols<-colorRampPalette( rev(brewer.pal(9, "RdBu")) )(256) # looks good!

# DENOTE COLORSET 1
# looks the best
cols<- colorRampPalette(c("green", "black", "red"))(256)

# COLORSET 6
cols<- colorRampPalette(c("999999", "#E69F00", "#56B4E9"))(256)

# try to transpose instead
pdf("heatmap3-test.pdf", height=15, width=10)
h <- heatmap.3((mergedMATRIX), col=cols, scale="column", trace="none",density.info="none", dendrogram="none",Colv=TRUE,Rowv=FALSE, key=TRUE,cexRow=0.5,breaks=seq(-3,3,6/256) , hclustfun=myclust, distfun=mydist,RowSideColors=rlab)
print(h)
dev.off()


# not that nice
cols<- colorRampPalette(c("#0f0928", "#474387", "#e2d086"))(128)
h <- heatmap.3((mergedMATRIX), col=cols, scale="column", trace="none",density.info="none", dendrogram="none",Colv=FALSE,Rowv=FALSE, key=TRUE,cexRow=0.5,breaks=seq(-2,2,4/128) , hclustfun=myclust, distfun=mydist,RowSideColors=rlab)
print(h)


