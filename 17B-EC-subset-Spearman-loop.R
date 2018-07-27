# and extract values
the.BMI.LOG <- dataset.subset.DF$bmi.LOG
three.BMI.groups <- dataset.subset.DF$BMI.group
two.BMI.groups <- dataset.subset.DF$BMItwogroup
#
table(two.BMI.groups)
# EAC
# normal overweightETobese 
#  20                46


# THE SAME FOR ALL!
numberofgenes <- length(names ( dataset.subset.DF[,119:length.of.df] ) ) # 60,483
modellist <- list() # call a list


# e.g. i 120 is gene number two
for (i in 119:numberofgenes)  # this is the one we want
# for (i in 119:1000)  # this is the one we want
{  # START OF BIG LOOP
  

  gene.expression.for.this.gene <- dataset.subset.DF[,i]
  # add a small constant to your FPKM values (say 0.25) to account for the zero
  # https://support.bioconductor.org/p/66233/
  gene.expression.for.this.gene.LOG <- log2(  (0.01+dataset.subset.DF[,i]) )
  # create a DF of your variables
  tempDF <- data.frame(gene.expression.for.this.gene,the.BMI.LOG,three.BMI.groups,two.BMI.groups,gene.expression.for.this.gene.LOG)

  temp <- tempDF$gene.expression.for.this.gene
  total.no.samples <- length(temp) # 152 
  no.samples.with.zero.fpkmUQ <- length(which(temp == 0))
  percent.with.zero <- (no.samples.with.zero.fpkmUQ/total.no.samples)*100
  if(percent.with.zero >= 80 ){cat("escape")}
  if(percent.with.zero < 80 ) {   
    spearman.test.temp <- cor.test(tempDF$the.BMI.LOG, tempDF$gene.expression.for.this.gene.LOG, method=c("spearman"),exact=FALSE) # 
    spearman.P <- spearman.test.temp$p.value
    spearman.r <- spearman.test.temp$estimate # aka rho
    # works -- matches the figure in script 8
    # so, for each gene ... should output the diagram
    spearman.P <- as.numeric(spearman.P)
    spearman.r <- as.numeric(spearman.r)
    class(spearman.P)
    class(spearman.r)
    
    
    # let us adjust the P-value
    # BH.P <- p.adjust(spearman.P, method = "BH", n = length(gene.names))
    # FDR.P <-  p.adjust(spearman.P, method = "fdr", n = length(gene.names))
    bonferroni.P <-  p.adjust(spearman.P, method = "bonferroni", n = length(gene.names))
    
    # remember, the 46 first columns are not GENES, but must honour them here!
    the.gene.name <- gene.names[i-118] # but
    # was 
    #   modeloutputDF <- try(    cbind(gene.names[i], spearman.r,spearman.P,BH.P,FDR.P,bonferroni.P) )
    
    modeloutputDF <- try(    cbind(the.gene.name, spearman.r,spearman.P,BH.P,FDR.P,bonferroni.P) )
    colnames(modeloutputDF) # ""           "spearman.P" "spearman.r"  etc
    modellist[[i]] <- modeloutputDF
    
    print(i) # to get an idea about how many GENES are left
    
    
    #    class(spearman.r) # numeric
    ADJ.P.CUTOFF <- as.numeric(ADJ.P.CUTOFF)
    REGRESSION.RHO.CUTOFF <- as.numeric(REGRESSION.RHO.CUTOFF)
    
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@    
    # if rho > 0.3 and P<0.05, draw a figure... actually demand an FDR P...
    # cat("should only be 4!!")
    # rho can be negative or positive, so abs!    
    if(bonferroni.P <= ADJ.P.CUTOFF & abs(spearman.r) >= REGRESSION.RHO.CUTOFF){    
      # if(save.Spearman.plot == "yes"){source("X-draw-Spearman.R")}
      try(source("X-draw-Spearman-EAC-subset.R"))
    }  
    
    # un-adjusted
      if(spearman.P <= P.CUTOFF & abs(spearman.r) >= REGRESSION.RHO.CUTOFF){            # if(save.Spearman.plot == "yes"){source("X-draw-Spearman.R")}
        try(source("X-draw-Spearman-EAC-subset.R"))
      }  
    
    
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@    
  }
  
  
  
  
} # END OF BIG LOOP
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@    












(cat("time to convert list to df")) 


SPEARMAN.results.df <- do.call(rbind.data.frame, modellist)  # convert the list to a data frame!

(cat("df converted")) 


colnames(SPEARMAN.results.df)[colnames(SPEARMAN.results.df)=="V1"] <- "ENSEMBL.gene"
#@ row.names(SPEARMAN.results.df) <- SPEARMAN.results.df$ENSEMBL.gene
# make them numeric
SPEARMAN.results.df$spearman.P <- as.character(SPEARMAN.results.df$spearman.P)
SPEARMAN.results.df$spearman.P <- as.numeric(SPEARMAN.results.df$spearman.P)
SPEARMAN.results.df$BH.P <- as.character(SPEARMAN.results.df$BH.P)
SPEARMAN.results.df$BH.P <- as.numeric(SPEARMAN.results.df$BH.P)
SPEARMAN.results.df$FDR.P <- as.character(SPEARMAN.results.df$FDR.P)
SPEARMAN.results.df$FDR.P <- as.numeric(SPEARMAN.results.df$FDR.P)
SPEARMAN.results.df$bonferroni.P <- as.character(SPEARMAN.results.df$bonferroni.P)
SPEARMAN.results.df$bonferroni.P <- as.numeric(SPEARMAN.results.df$bonferroni.P)
SPEARMAN.results.df$spearman.r <- as.character(SPEARMAN.results.df$spearman.r)
SPEARMAN.results.df$spearman.r <- as.numeric(SPEARMAN.results.df$spearman.r)

# save 
# dataset  # e.g. all for all genes
# write SampleInfo to file to quickly assess in Excel
library("openxlsx")
#
write.xlsx(SPEARMAN.results.df, paste("./Spearman-",EC.subset,"-Spearman.xlsx",sep=""), asTable = FALSE,row.names=TRUE, colNames=TRUE)


# draw some boxplots
# set run.type <- "subset"
# in 11-output-stats-MannWhitneyUtest.R" (skip script 10 for EC subsets)


# #### #### #### #### #### #### #### #### #### #### #### ####
# THIS IS THE END