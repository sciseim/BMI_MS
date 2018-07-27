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


# SETTINGS HERE
save.pics <- "no" # draw them all (including failed ones)
# if(save.pics =="yes"){cat("YOKI!")}
Bonferroni.P.cutoff <- 0.001 # 
remove(modellist)
# run.type <- "normal" # default ... e.g. TCGA-ESCA (entire data set)
run.type <- "normal" # default ... e.g. TCGA-ESCA subsets EAC and ESCC
# is ... EC.subset









# boxplot output
if(run.type == "normal")
{
dir.create(paste("./Spearman/",dataset,"/2grps",sep=""))
dir.create(paste("./Spearman/",dataset,"/3grps",sep=""))
}

if(run.type == "subset")
{
  dir.create(paste("./Spearman-",EC.subset,"/2grps",sep=""))
  dir.create(paste("./Spearman-",EC.subset,"/3grps",sep=""))
}



# denote dataset of interest
library("openxlsx")
# paste("./Spearman/",dataset,"-Spearman.xlsx",sep="") # save location # file name


if(run.type == "normal")
{
  spearman.results.SigOnly <- read.xlsx(paste("./Spearman/",dataset,"-Spearman-SigOnly.xlsx",sep=""))
  }

if(run.type == "subset")
{
  spearman.results.SigOnly <- read.xlsx(paste("./Spearman-",EC.subset,"-Spearman.xlsx",sep=""))
  

  # skip script 10 here, so do this to only boxplot genes passing our threshold!
  
  if(run.type == "normal")
  {
  keep.these <- which(abs(spearman.results.SigOnly$spearman.r) >= REGRESSION.RHO.CUTOFF & spearman.results.SigOnly$bonferroni.P <= ADJ.P.CUTOFF)
  length(keep.these) # 
  spearman.results.SigOnly <- spearman.results.SigOnly[keep.these,]
  length(spearman.results.SigOnly$the.gene.name) # 12 genes in EAC
  }
  if(run.type == "subset")
  {
    keep.these <- which(abs(spearman.results.SigOnly$spearman.r) >= REGRESSION.RHO.CUTOFF & spearman.results.SigOnly$spearman.P <= P.CUTOFF)
    length(keep.these) # 12 genes in EAC
    spearman.results.SigOnly <- spearman.results.SigOnly[keep.these,]
    length(spearman.results.SigOnly$the.gene.name) # 12 genes in EAC
  }
  
  
    }


# for each of these
the.sig.Spearman.genes <- spearman.results.SigOnly$the.gene.name


remove(modellist)
modellist <- list() # call a list
for(i in the.sig.Spearman.genes)
{cat(i)
  
  target.gene <- i
  #  the.Big.df.SUBSET[,target.gene] # no, use subset
  #
  #  the.Big.df.SUBSET$BMI.group
  remove(temp)
  
  if(run.type == "normal")
  {
  temp <- subset(the.Big.df.SUBSET, select=c(target.gene,"BMI.group"))
  }
  
  if(run.type == "subset")
  {
    temp <- subset(dataset.subset.DF, select=c(target.gene,"BMI.group"))
  }
  
  
  
  head(temp)
  names(temp)[1] <- "GENE"
  temp$GENE <- log2(temp$GENE)
  temp$BMI.group <- as.factor(temp$BMI.group)
  table(temp$BMI.group)
  temp$BMI.group.sorted <- factor(temp$BMI.group, levels = c("normal","overweight","obese"))
  
  
  # ////////////////////////////////////////////////////////////  

  if(save.pics == "yes"){  
    # DRAW PLOT 2grps
  p<-ggplot(temp, aes(x=BMI.group.sorted, y=GENE, fill=BMI.group.sorted)) +
    geom_boxplot()
  # Use custom color palettes
  # Use custom color palettes
  p+scale_fill_manual(values=c("#53a362", "#d9703c", "#d82e2e")) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +xlab("weight group")+ylab(paste("log2(FPKM-UQ) ",target.gene,sep=""))
  # save image
  imagename <- target.gene

  if(run.type == "normal")
  {
    ggsave(filename=paste("./Spearman/",dataset,"/3grps/",imagename,".png",sep=""), dpi=600, scale=1.5) 
  }
  
  if(run.type == "subset")
  {
    ggsave(filename=paste("./Spearman-",EC.subset,"/3grps/",imagename,".png",sep=""), dpi=600, scale=1.5) 
  }

}  
  
  # ////////////////////////////////////////////////////////////  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # NORMAL VS OVERWEIGHT AND OBESE TOGETHER
  #  the.Big.df.SUBSET$BMItwogroup
  #  remove(temp)
  temp <- subset(the.Big.df.SUBSET, select=c(target.gene,"BMItwogroup"))
  #  head(temp)
  names(temp)[1] <- "GENE"
  
  temp$GENE.log2 <- log2(temp$GENE)
    
  temp$BMItwogroup <- as.factor(temp$BMItwogroup)
  table(temp$BMItwogroup)
    # ///////////////////////////////////////////////////////////////////
  class(temp$BMItwogroup) # factor
  temp$BMItwogroup.sorted <- factor(temp$BMItwogroup, levels = c("normal","overweightETobese"))

  
  if(save.pics == "yes"){  
    p<-ggplot(temp, aes(x=BMItwogroup.sorted, y=GENE.log2, fill=BMItwogroup.sorted)) +
    geom_boxplot()
  # Use custom color palettes
  p+scale_fill_manual(values=c("#53a362", "#d9703c")) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +xlab("weight group")+ylab(paste("log2(FPKM-UQ) ",target.gene,sep=""))
  # save image
  imagename <- target.gene
  
  
  if(run.type == "normal")
  {
    ggsave(filename=paste("./Spearman/",dataset,"/2grps/",imagename,".png",sep=""), dpi=600, scale=1.5) 
  }
  
  if(run.type == "subset")
  {
    ggsave(filename=paste("./Spearman-",EC.subset,"/2grps/",imagename,".png",sep=""), dpi=600, scale=1.5) 
  }
  

  # ///////////////////////////////////////////////////////////////////
  }  
  # second last big loop here...
  
  
  
  
  
  
  
  
  
  
  
  # STATS HERE!!!
  # ////////////////////////////////////////////////////////////////  
  # non-parametric t-test 
  # The general rule is to test on the data you measure - in this case, this would be the un-logged reads per million!!!
  #  temp$GENE <- log2(temp$GENE)
  # cannot have NAs!
  # LOG FPKM-UQ VALUES
  # add a small constant to your FPKM values (say 0.25) to account for the zero
  # https://support.bioconductor.org/p/66233/
  #  temp$GENE <- log2(  (0.01+temp$GENE) )
  # ////////////////////////////////////////////////////////////////  
  
  # independent 2-group Mann-Whitney U Test
  # wilcox.test(y,x) # where y and x are numeric
   
  class(temp$BMItwogroup)
  temp$BMItwogroup <- as.numeric(temp$BMItwogroup)
  x <- temp[which(temp$BMItwogroup == 1),]
  y <- temp[which(temp$BMItwogroup == 2),]
  colnames(y)
  x <- x$GENE
  y <- y$GENE
  res.wilcox.test <-  wilcox.test(y,x)
  res.wilcox.test.P <- res.wilcox.test$p.value


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if(run.type == "normal")
  {  
  # Bonferroni-cutoff
  res.wilcox.test.result.Bonferroni <- "FAIL"
  # Bonferroni.P.cutoff <- 8.3e-07 # 8.3*10^-7 
  if(res.wilcox.test$p.value <= Bonferroni.P.cutoff){res.wilcox.test.result.Bonferroni="PASS"}
  
  res.wilcox.test.result <- "FAIL"
  if(res.wilcox.test$p.value <= 0.05){res.wilcox.test.result="PASS"}
  
  
  modeloutputDF <- try(    cbind(i, res.wilcox.test.P,res.wilcox.test.result,res.wilcox.test.result.Bonferroni ) )
  colnames(modeloutputDF) # ""           "spearman.P" "spearman.r"  etc
  modellist[[i]] <- modeloutputDF
# ////////////////////////////////////////////////////////////////
STATS.results.df <- do.call(rbind.data.frame, modellist)  # convert the list to a data frame!

# SAVE OUTPUT
head(STATS.results.df)
library("openxlsx")

  write.xlsx(STATS.results.df, paste("./Spearman/",dataset,"-Spearman-Sig-STATS.xlsx",sep=""), asTable = FALSE,row.names=TRUE, colNames=TRUE)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
}
  
if(run.type == "subset")
{  
  # Bonferroni-cutoff
  res.wilcox.test.result <- "FAIL"
  
  P.cutoff <- P.CUTOFF # was 0.0001
  P.cutoff <- 0.001 # 
  P.cutoff <- 0.05 # 
  
  # go for another cut.off here
#  if(res.wilcox.test$p.value <= P.cutoff){res.wilcox.test.result="PASS"}
  
  res.wilcox.test.result <- "FAIL"
  if(res.wilcox.test$p.value <= P.cutoff){res.wilcox.test.result="PASS"}
  
  
  modeloutputDF <- try(    cbind(i, res.wilcox.test.P,res.wilcox.test.result,res.wilcox.test.result ) )
  colnames(modeloutputDF) # ""           "spearman.P" "spearman.r"  etc
  modellist[[i]] <- modeloutputDF
# ////////////////////////////////////////////////////////////////
STATS.results.df <- do.call(rbind.data.frame, modellist)  # convert the list to a data frame!

# SAVE OUTPUT
head(STATS.results.df)
library("openxlsx")

write.xlsx(STATS.results.df, paste("./Spearman-",EC.subset,"/",EC.subset,"-Spearman-Sig-STATS.xlsx",sep=""), asTable = FALSE,row.names=TRUE, colNames=TRUE)
}

  
} # END OF BIG LOOP