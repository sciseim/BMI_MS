# setwd("~/Downloads/BMI_vs_HFD/GDC/SCRIPTS/")
load("the.Big.df.Robj", .GlobalEnv)
length.of.df <- length(the.Big.df[1,])
colnames(the.Big.df)
gene.names <- names ( the.Big.df[,47:length.of.df] )


# subset e.g. only want TCGA-BLCA here
the.Big.df.SUBSET <- the.Big.df # if you want all!



# LET US SELECT RANDOM GENES AND DRAW
# geneofinterest <- "ENSG00000167578.15" # 
# from script 7 we have gene.names
geneofinterest <- sample(gene.names,1)

the.Big.df.SUBSET[,geneofinterest] # use this to obtain the expression data of a particular gene

# From my experience of the FPKM-UQ counts, a non-parametric t-test should be employed when comparing across samples. Fold-change calculations, however, do not appear to work that well on the FPKM-UQ scale.
# https://www.biostars.org/p/283372/


# and extract values
the.BMI.LOG <- the.Big.df.SUBSET$bmi.LOG
three.BMI.groups <- the.Big.df.SUBSET$BMI.group
two.BMI.groups <- the.Big.df.SUBSET$BMItwogroup
gene.expression.for.this.gene <- the.Big.df.SUBSET[,geneofinterest]


# add a small constant to your FPKM values (say 0.25) to account for the zero
# https://support.bioconductor.org/p/66233/
gene.expression.for.this.gene.LOG <- log2(  (0.01+the.Big.df.SUBSET[,geneofinterest]) )




  
# create a DF of your variables
tempDF <- data.frame(gene.expression.for.this.gene,the.BMI.LOG,three.BMI.groups,two.BMI.groups,gene.expression.for.this.gene.LOG)
head(tempDF) # geneofinterest the.BMI.LOG


# use this to draw 
# OK, use for regression, then

# need to find somewhere to put the correlation label 
# .... just make sure it is visible 
cor.label <- ( max(tempDF$gene.expression.for.this.gene.LOG) * 0.90)
#
#
library("ggpubr")
p <- ggscatter(tempDF, x = "the.BMI.LOG", y = "gene.expression.for.this.gene.LOG",
               add = "reg.line",                                 # Add regression line
               conf.int = TRUE,                                  # Add confidence interval
               add.params = list(color = "blue",
                                 fill = "lightgray")
)+
  stat_cor(method = "spearman", label.x = 5, label.y = cor.label)  # Add correlation coefficient
p + xlim((min(tempDF$the.BMI.LOG)),(max(tempDF$the.BMI.LOG))) + ylim ( min(tempDF$gene.expression.for.this.gene.LOG,na.rm=TRUE),max(tempDF$gene.expression.for.this.gene.LOG,na.rm=TRUE) )


spearman.test.temp <- cor.test(tempDF$the.BMI.LOG, tempDF$gene.expression.for.this.gene.LOG, method=c("spearman"),exact=FALSE) # 
spearman.P <- spearman.test.temp$p.value
spearman.r <- spearman.test.temp$estimate # aka rho
# works -- matches the figure above
# so, for each gene ... should output the diagram
