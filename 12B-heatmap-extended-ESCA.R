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



# for each of these
the.sig.Spearman.genes <- spearman.results.SigOnly$the.gene.name

class(spearman.results.SigOnly) # df
colnames(the.Big.df.SUBSET) # inc. genes


#@ temp <- subset(the.Big.df.SUBSET, select=c(the.sig.Spearman.genes,"BMI.group"))
temp <- subset(the.Big.df.SUBSET, select=c(the.sig.Spearman.genes,"BMI.group")) # OLD
the.Big.df.SUBSET$Gastroesophageal.location
colnames(the.Big.df.SUBSET)
the.Big.df.SUBSET$"Histological.Type...Oesophagus" # need to fix
colnames(the.Big.df.SUBSET) <- as.character(colnames(the.Big.df.SUBSET))
the.Big.df.SUBSET$Histological.Type
the.Big.df.SUBSET$Histological.Type...Oesophagus
the.Big.df.SUBSET$Oesophageal.tumor.location
the.Big.df.SUBSET$Pathologic.stage
the.Big.df.SUBSET$Grade
the.Big.df.SUBSET$Vital.Status
the.Big.df.SUBSET$gender
temp <- subset(the.Big.df.SUBSET, select=c(the.sig.Spearman.genes,"BMI.group","Histological.Type","Histological.Type...Oesophagus","Oesophageal.tumor.location","Pathologic.stage","Grade","Vital.Status","gender"))

















# /////////////////////////////////////////////////////////
# sort by BMI.group
temp <- temp[order(temp$BMI.group),] # WORKS, but sort alphabetically!
# 
temp$BMI.group <- factor(temp$BMI.group, levels=c("normal","overweight","obese")) 
temp <- temp[order(temp$BMI.group),] 
# /////////////////////////////////////////////////////////




head(temp)
temp.genes.only <- temp[,the.sig.Spearman.genes]
temp.BMI.info.only <- temp[,"BMI.group"]
the.Histological.Type.info.only <- temp[,"Histological.Type"]
the.Histological.Type...Oesophagus.info.only <- temp[,"Histological.Type...Oesophagus"]
the.Oesophageal.tumor.location.info.only <- temp[,"Oesophageal.tumor.location"]
the.Pathologic.stage.info.only <- temp[,"Pathologic.stage"]
the.Grade.info.only <- temp[,"Grade"]
the.Vital.Status.info.only <- temp[,"Vital.Status"]
the.gender.only <- temp[,"gender"]
# do not really need to use e.g. 'the.gender.only' later
# NOT recommended since we will ORDER/SORT!










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

# ===================== BMI 
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
# =====================  
# ===================== Histological.Type 
table(temp$Histological.Type)
Histological.Type.colours <- temp$Histological.Type
class(Histological.Type.colours) # character
Histological.Type.colours <- as.factor(Histological.Type.colours)
class(Histological.Type.colours) # character
levels(Histological.Type.colours)[levels(Histological.Type.colours)=="AC"] <- "#f0ac56"
levels(Histological.Type.colours)[levels(Histological.Type.colours)=="ESCC"] <- "#a82a2a"
Histological.Type.colours <- as.character(Histological.Type.colours)
rlab=cbind(Histological.Type.colours) # can add as many as we want. E.g. colour for stage
colnames(rlab)=c("group")
# combine
rlab=t(cbind(Histological.Type.colours))
# =====================  
# ===================== Histological.Type...Oesophagus 
table(temp$Histological.Type...Oesophagus)
#
Histological.Type...Oesophagus.colours <- temp$Histological.Type...Oesophagus
class(Histological.Type...Oesophagus.colours) # character
Histological.Type...Oesophagus.colours <- as.factor(Histological.Type...Oesophagus.colours)
class(Histological.Type...Oesophagus.colours) # character
levels(Histological.Type...Oesophagus.colours)[levels(Histological.Type...Oesophagus.colours)=="EAC"] <- "#f0ac56"
levels(Histological.Type...Oesophagus.colours)[levels(Histological.Type...Oesophagus.colours)=="ESCC"] <- "#a82a2a"
Histological.Type...Oesophagus.colours <- as.character(Histological.Type...Oesophagus.colours)
rlab=cbind(Histological.Type...Oesophagus.colours) # can add as many as we want. E.g. colour for stage
colnames(rlab)=c("group")
# combine
rlab=t(cbind(Histological.Type...Oesophagus.colours))
# ===================== 
# ===================== Oesophageal.tumor.location 
table(temp$Oesophageal.tumor.location)
#
Oesophageal.tumor.location.colours <- temp$Oesophageal.tumor.location
class(Oesophageal.tumor.location.colours) # character
Oesophageal.tumor.location.colours <- as.factor(Oesophageal.tumor.location.colours)
class(Oesophageal.tumor.location.colours) # character
levels(Oesophageal.tumor.location.colours)[levels(Oesophageal.tumor.location.colours)=="proximal"] <- "#f3f3f4"
levels(Oesophageal.tumor.location.colours)[levels(Oesophageal.tumor.location.colours)=="mid"] <- "#cdcccc"
levels(Oesophageal.tumor.location.colours)[levels(Oesophageal.tumor.location.colours)=="mid-distal"] <- "#cdcccc"
levels(Oesophageal.tumor.location.colours)[levels(Oesophageal.tumor.location.colours)=="distal"] <- "#636363"
Oesophageal.tumor.location.colours <- as.character(Oesophageal.tumor.location.colours)
rlab=cbind(Oesophageal.tumor.location.colours) # can add as many as we want. E.g. colour for stage
colnames(rlab)=c("group")
# combine
rlab=t(cbind(Oesophageal.tumor.location.colours))
# =====================  
# ===================== gender 
table(temp$gender)
#
gender.colours <- temp$gender
class(gender.colours) # character
gender.colours <- as.factor(gender.colours)
class(gender.colours) # character
levels(gender.colours)[levels(gender.colours)=="male"] <- "#4b87c6"
levels(gender.colours)[levels(gender.colours)=="female"] <- "#f9bfcc"
gender.colours <- as.character(gender.colours)
rlab=cbind(gender.colours) # can add as many as we want. E.g. colour for stage
colnames(rlab)=c("group")
# combine
rlab=t(cbind(gender.colours))
# =====================  
# ===================== Vital.Status 
table(temp$Vital.Status)
#
Vital.Status.colours <- temp$Vital.Status
class(Vital.Status.colours) # character
Vital.Status.colours <- as.factor(Vital.Status.colours)
class(Vital.Status.colours) # character
levels(Vital.Status.colours)[levels(Vital.Status.colours)=="Alive"] <- "white"
levels(Vital.Status.colours)[levels(Vital.Status.colours)=="Dead"] <- "black"
Vital.Status.colours <- as.character(Vital.Status.colours)
rlab=cbind(Vital.Status.colours) # can add as many as we want. E.g. colour for stage
colnames(rlab)=c("group")
# combine
rlab=t(cbind(Vital.Status.colours))
# =====================  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ADD SIDE-BAR
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPACER BAR SPACER BAR SPACER BAR SPACER BAR SPACER BAR SPACER BAR SPACER BAR SPACER BAR
spacercolours=sample(c("white"), length(temp[,1]), replace = TRUE, prob = NULL)
# SPACER BAR SPACER BAR SPACER BAR SPACER BAR SPACER BAR SPACER BAR SPACER BAR SPACER BAR
# ///// add groups ///////////////////////
# temp n=142 now
# combine the groups we want!
rlab=t(cbind(groupcolours,spacercolours, Histological.Type.colours,spacercolours,Histological.Type...Oesophagus.colours,spacercolours, Oesophageal.tumor.location.colours,spacercolours, gender.colours,spacercolours,Vital.Status.colours))


# only need one Histogroups
rlab=t(cbind(groupcolours,spacercolours, Histological.Type.colours,spacercolours, Oesophageal.tumor.location.colours,spacercolours, gender.colours,spacercolours,Vital.Status.colours))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# add Spearman rho 
# the.Big.df.SUBSET
mergedMATRIX <- as.matrix(temp.genes.only)
library("openxlsx")
Sig.Only <- read.xlsx("Spearman/TCGA-ESCA-Spearman-SigOnly.xlsx")
head(Sig.Only)
# column: spearman.r 
# column: the.gene.name

colnames(mergedMATRIX ) #  genes
# 1. sort Sig.Only by rho
# easier to just split up
POS.rho <- Sig.Only[which(Sig.Only$spearman.r >= 0.1),]
NEG.rho <- Sig.Only[which(Sig.Only$spearman.r <= -0.01),]
NEG.rho$direction <- "blue" # set color here
POS.rho$direction <- "red" # set color here

Sig.Only <- rbind(NEG.rho,POS.rho)
Sig.Only <- Sig.Only[order(Sig.Only$spearman.r),] # order by rho
# 2. sort mergedMatrix columns in order of Sig.Only
rho.gene.order <- Sig.Only$the.gene.name
class(rho.gene.order) # character
class(mergedMATRIX) # matrix

mergedMATRIX.df <- as.data.frame(mergedMATRIX)
library(data.table)
setcolorder(mergedMATRIX.df, as.character(rho.gene.order))
head(colnames(mergedMATRIX.df)) # correct
tail(colnames(mergedMATRIX.df)) # correct
mergedMATRIX <- as.matrix(mergedMATRIX.df)
rho.colours <- c(NEG.rho$direction,POS.rho$direction)
length(rho.colours) # 145
clab <- (cbind(rho.colours))
clab <- as.matrix(clab)
class(gender.colours) # character
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~













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




# COLORSET 2
cols<-colorRampPalette( rev(brewer.pal(9, "RdBu")) )(256) # looks good!

# DENOTE COLORSET 1
# looks the best
cols<- colorRampPalette(c("green", "black", "red"))(256)

# COLORSET 6 # not nice!
# cols<- colorRampPalette(c("999999", "#E69F00", "#56B4E9"))(256)


# F4.large
# COLORSET 3
cols<- colorRampPalette(c("#245bab", "#fefaf1", "#fdbf3a"))(256)
# cols<- colorRampPalette(c("#954f98", "#fffefc", "#eaef5f"))(256)



# try to transpose instead
pdf("heatmap3-test.pdf", height=15, width=10)
h <- heatmap.3((mergedMATRIX), col=cols, scale="column", trace="none",density.info="none", dendrogram="none",Colv=TRUE,Rowv=FALSE, key=TRUE,cexRow=0.5,breaks=seq(-3,3,6/256) , hclustfun=myclust, distfun=mydist,RowSideColors=rlab,RowSideColorsSize=7,ColSideColors=clab,ColSideColorsSize=1)
print(h)
dev.off()



# testing

# force BMI on LHS
h <- heatmap.3((mergedMATRIX), col=cols, scale="column", trace="none",density.info="none", dendrogram="none",Colv=FALSE,Rowv=FALSE, key=TRUE,cexRow=0.5,breaks=seq(-3,3,6/256) , hclustfun=myclust, distfun=mydist,RowSideColors=rlab,RowSideColorsSize=7,ColSideColors=clab,ColSideColorsSize=1)
print(h)


# Rowv
# determines if and how the row dendrogram should be computed and reordered.
# default=NULL
# Colv
# Column
pdf("rhobar-heatmap3-test.pdf", height=15, width=10)
h <- heatmap.3((mergedMATRIX), col=cols, scale="column", trace="none",density.info="none", dendrogram="none", key=TRUE,cexRow=0.5,breaks=seq(-3,3,6/256) , hclustfun=myclust, distfun=mydist,RowSideColors=rlab,Colv=FALSE,RowSideColorsSize=7,ColSideColors=clab,ColSideColorsSize=1)
print(h)
dev.off()




# # not that nice
# cols<- colorRampPalette(c("#0f0928", "#474387", "#e2d086"))(128)
# h <- heatmap.3((mergedMATRIX), col=cols, scale="column", trace="none",density.info="none", dendrogram="none",Colv=FALSE,Rowv=FALSE, key=TRUE,cexRow=0.5,breaks=seq(-2,2,4/128) , hclustfun=myclust, distfun=mydist,RowSideColors=rlab)
# print(h)










