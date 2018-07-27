setwd("~/Downloads/BMI_vs_HFD/GDC/SCRIPTS/Spearman/rho 0.5 FDR 0.001/")

dataset <- "TCGA-BLCA"




library("openxlsx")
# paste("./Spearman/",dataset,"-Spearman.xlsx",sep="") # save location # file name
# write.xlsx(SPEARMAN.results.df, paste("./Spearman/",dataset,"-Spearman.xlsx",sep=""), asTable = FALSE,row.names=TRUE, colNames=TRUE)
spearman.results <- read.xlsx(paste("./",dataset,"-Spearman.xlsx",sep=""))
load(file="../../gencode.df.READY.Robj") # add gene info
gencode.df.READY.subset <- subset(gencode.df.READY, select=c(gene_id,gene_name,seqnames,gene_type))
head(gencode.df.READY.subset)
# add to 10B  once finished ..
length  ( which(spearman.results$spearman.P <= 0.001)  ) # 1179
length  ( which(spearman.results$spearman.P <= 0.01)  ) # 3206
length  ( which(spearman.results$bonferroni.P <= 0.001)  ) # 17
length  ( which(spearman.results$bonferroni.P <= 0.01)  ) # 40
spearman.results$Q.BH.FDR <- p.adjust(spearman.results$spearman.P,method="BH")
length  ( which(spearman.results$Q.BH.FDR <= 0.01)  ) # 545
length  ( which(spearman.results$Q.BH.FDR <= 0.001)  ) # 113
results.df <- spearman.results[which(spearman.results$Q.BH.FDR <= 0.001),] # 113

# round up down. so 0.295439009 
spearman.results$spearman.r <- sign(spearman.results$spearman.r) * ceiling(abs(spearman.results$spearman.r) * 100) / 100


# with rho cutoff
length(which(spearman.results$Q.BH.FDR <= 0.001 & abs(spearman.results$spearman.r) >= 0.30)) # 3
length(which(spearman.results$Q.BH.FDR <= 0.001 & abs(spearman.results$spearman.r) >= 0.20)) # 113
# cut-off in my MS
length(which(spearman.results$bonferroni.P <= 0.01 & abs(spearman.results$spearman.r) >= 0.30)) # 3
#
#
#
spearman.results.filtered <- spearman.results[which(spearman.results$bonferroni.P <= 0.01 & abs(spearman.results$spearman.r) >= 0.30),]
spearman.results.filtered <- merge(x=spearman.results.filtered, y=gencode.df.READY.subset, by.x="the.gene.name", by.y="gene_id",all.x = TRUE)
#
# save
spearman.results.filtered
write.xlsx(spearman.results.filtered, paste("./FILTERED/",dataset,"-Spearman-SigOnly.xlsx",sep=""), asTable = FALSE,row.names=FALSE, colNames=TRUE)
#
length( which(spearman.results.filtered$spearman.r > 0)  ) # 6
length( which(spearman.results.filtered$spearman.r < 0)  ) #  0



# to generate useful Table
# run script 10