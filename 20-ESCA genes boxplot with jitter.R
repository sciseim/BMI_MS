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


# boxplot output
dir.create(paste("./Spearman/",dataset,"/2grps",sep=""))
dir.create(paste("./Spearman/",dataset,"/3grps",sep=""))

# ////////////////////////////////////////////////////////////





# #################################################################################
# ADD HISTOLOGY
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




# ////////////////////////////////////////////////////////////
# DRAW

# i <- # is ensembl... so for 
i <- "ENSG00000132170.18" # 
target.gene <- i
#  the.Big.df.SUBSET[,target.gene] # no, use subset
#
#  the.Big.df.SUBSET$BMI.group
remove(temp)
the.Big.df.SUBSET$Histological.Type # AC AND ESCC
# the.Big.df.SUBSET$Histological.Type...Oesophagus
# temp <- subset(the.Big.df.SUBSET, select=c(target.gene,"BMI.group"))
temp <- subset(the.Big.df.SUBSET, select=c(target.gene,"BMI.group","Histological.Type"))
head(temp)
names(temp)[1] <- "GENE"
temp$GENE <- log2(temp$GENE)
temp$BMI.group[temp$BMI.group == "normal"] <- "lean"
temp$BMI.group <- as.factor(temp$BMI.group)
table(temp$BMI.group)
temp$BMI.group.sorted <- factor(temp$BMI.group, levels = c("lean","overweight","obese"))
#
#
class(temp$BMI.group) # factor
class(temp$Histological.Type)
#
temp$Histological.Type.color <- temp$Histological.Type
temp$Histological.Type.color <- as.character(temp$Histological.Type.color) 
temp$Histological.Type.color[temp$Histological.Type.color == "NA"] <- "grey"
temp$Histological.Type.color[temp$Histological.Type.color == "AC"] <- "#f0ac56"
temp$Histological.Type.color[temp$Histological.Type.color == "ESCC"] <- "#a82a2a"
temp$Histological.Type.color <- as.factor(temp$Histological.Type.color)

temp$Histological.Type[temp$Histological.Type == "AC"] <- "EAC"
temp$Histological.Type <- as.factor(temp$Histological.Type)


p<-ggplot(temp, aes(x=BMI.group.sorted, y=GENE, fill=BMI.group.sorted)) +
  geom_boxplot() + geom_point(aes(colour = factor(Histological.Type))) 
p+scale_fill_manual(values=c("#53a362", "#d9703c", "#d82e2e")) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +xlab("weight group")+ylab(paste("log2(FPKM-UQ) ",target.gene,sep=""))
imagename <- target.gene
ggsave(filename=paste("./Spearman/",dataset,"/3grps/",imagename,".pdf",sep=""), dpi=600, scale=1.5)