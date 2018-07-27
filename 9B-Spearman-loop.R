# NOW 9B
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# create directory for this data set
dir.create(paste("./Spearman/",dataset,sep=""))




# LOOP!
# ##############################################
# SPEARMAN CORRELATIONS



# and extract values
the.BMI.LOG <- the.Big.df.SUBSET$bmi.LOG
three.BMI.groups <- the.Big.df.SUBSET$BMI.group
two.BMI.groups <- the.Big.df.SUBSET$BMItwogroup
# THE SAME FOR ALL!




# head(the.Big.df[,46:47])
numberofgenes <- length(names ( the.Big.df[,47:length.of.df] ) ) # 60,483
# remove(modellist)
modellist <- list() # call a list
# HERE THE GENES START AT 47 IN the.Big.df.SUBSET




# i <- 42269 # dummy
# for (i in 47:500)  # test 500-47 genes 
# for (i in 60000:60500)  # test 500-47 genes 

 for (i in 47:numberofgenes)  # this is the one we want
# for (i in 47:1000) 

   {  # START OF BIG LOOP

  
# ////////////////////////////////////////////////////////////////  
# TEMP TROUBLESHOOTING   
  (the.Big.df.SUBSET[,c("ENSG00000228923.1")]) # should pass
  (the.Big.df.SUBSET[,c("ENSG00000220311.1")]) # should fail since >80% 0.00
  grep("ENSG00000228923.1", colnames(the.Big.df.SUBSET)) # 42260 PASS
  grep("ENSG00000220311.1", colnames(the.Big.df.SUBSET)) # 42184 FAIL
 #  i <- 42260-47
 #
  grep("ENSG00000235262.1", colnames(the.Big.df.SUBSET)) # 42184 FAIL

  (the.Big.df.SUBSET[,c("ENSG00000257180.1")]) # 
  (the.Big.df.SUBSET[,c("ENSG00000224931.3")]) # 
  grep("ENSG00000257180.1", colnames(the.Big.df.SUBSET)) # 42306 ?
  grep("ENSG00000224931.3", colnames(the.Big.df.SUBSET)) # 42230 ?
  # ////////////////////////////////////////////////////////////////  

    
  
  
  
  gene.expression.for.this.gene <- the.Big.df.SUBSET[,i]
  # add a small constant to your FPKM values (say 0.25) to account for the zero
  # https://support.bioconductor.org/p/66233/
  gene.expression.for.this.gene.LOG <- log2(  (0.01+the.Big.df.SUBSET[,i]) )

  
  
  # TESTING HERE
#  gene.expression.for.this.gene <- the.Big.df.SUBSET[,c("ENSG00000223018.1")] 
#  gene.expression.for.this.gene.LOG <- log2(  (0.01 + the.Big.df.SUBSET[,c("ENSG00000223018.1")]) )
  
    
  # create a DF of your variables
  tempDF <- data.frame(gene.expression.for.this.gene,the.BMI.LOG,three.BMI.groups,two.BMI.groups,gene.expression.for.this.gene.LOG)
  # head(tempDF) # geneofinterest the.BMI.LOG
  
  
  # ////////////////////////////////////////////////////////////
    # ESCAPE genes which are all zero
    # if Colsums are 0, escape
  # delete columns with zero sums
# was 
#  ExpSum <- sum(tempDF$gene.expression.for.this.gene)
#  if(ExpSum == 0){cat("escape")}   
#  if(ExpSum != 0)
#  {   
  
  

  temp <- tempDF$gene.expression.for.this.gene
  # class(temp)
  # total.no.samples <- length(tempDF$gene.expression.for.this.gene) # 152 
    total.no.samples <- length(temp) # 152 
    no.samples.with.zero.fpkmUQ <- length(which(temp == 0))
    percent.with.zero <- (no.samples.with.zero.fpkmUQ/total.no.samples)*100
  # class(percent.with.zero) # numeric
    if(percent.with.zero >= 80 ){cat("escape")}
    if(percent.with.zero < 80 ) {   
#@      cat("pass")}
      # do some stats     
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
    BH.P <- p.adjust(spearman.P, method = "BH", n = length(gene.names))
    FDR.P <-  p.adjust(spearman.P, method = "fdr", n = length(gene.names))
    # add them, but tnot correct using this formula...
        
          bonferroni.P <-  p.adjust(spearman.P, method = "bonferroni", n = length(gene.names))
    
    # remember, the 46 first columns are not GENES, but must honour them here!
    the.gene.name <- gene.names[i-46] # but
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
     try(source("X-draw-Spearman.R"))
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


# Check that everything is OK
# head(SPEARMAN.results.df)
# which(SPEARMAN.results.df$spearman.P < 0.001)
# some...

# rho can be negative or positive, so abs!
# which(SPEARMAN.results.df$FDR.P < 0.05 & abs(SPEARMAN.results.df$spearman.r) > 0.30)
# NONE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# save 
# dataset  # e.g. all for all genes
# write SampleInfo to file to quickly assess in Excel
library("openxlsx")
# paste("./Spearman/",dataset,"-Spearman.xlsx",sep="") # save location # file name
write.xlsx(SPEARMAN.results.df, paste("./Spearman/",dataset,"-Spearman.xlsx",sep=""), asTable = FALSE,row.names=TRUE, colNames=TRUE)

# #### #### #### #### #### #### #### #### #### #### #### ####
# THIS IS THE END