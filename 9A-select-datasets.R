library("ggpubr")
library("magrittr")
library("ggplot2")

# load the data
setwd("~/Downloads/BMI_vs_HFD/GDC/SCRIPTS/")
load("the.Big.df.Robj", .GlobalEnv)
length.of.df <- length(the.Big.df[1,]) # 60529 (47 are sampleinfo)
colnames(the.Big.df) # e.g. genes
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# obtain gene names
gene.names <- names ( the.Big.df[,47:length.of.df] )
length(gene.names) # 60483
# have all the gene names in a vector now
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

REGRESSION.RHO.CUTOFF <- 0.30 # was 0.5
# Bonferroni would be 0.05/length(gene.names)
# 8.266786e-07
# this is ... 
ADJ.P.CUTOFF <- 0.01 # was 0.001
# ADJ.P.CUTOFF <- 0.05/length(gene.names) # very strict ...
# only keep significant ...
# class(REGRESSION.RHO.CUTOFF) # must be numeric!
# class(ADJ.P.CUTOFF) # must be numeric!










# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# do we want to save Spearman plots of significant genes?
save.Spearman.plot <- "yes"
# save to ./Spearman
dir.create("Spearman")



# paste("rho",REGRESSION.RHO.CUTOFF,"FDR",ADJ.P.CUTOFF)
dir.create("Spearman")
dir.create(paste("./Spearman/",paste("rho",REGRESSION.RHO.CUTOFF,"FDR",ADJ.P.CUTOFF),sep=""))
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



# ... all done
# # ////////////////////////////////////////////////////////////
 dataset <- "TCGA-ESCA"
 the.Big.df.SUBSET <- the.Big.df 
 keep.these <- which(the.Big.df.SUBSET$project_id == dataset) # given index
 the.Big.df.SUBSET <- the.Big.df.SUBSET[keep.these,] #
 remove(modellist)
 source("9B-Spearman-loop.R")
# # ////////////////////////////////////////////////////////////







# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# ////////////////////////////////////////////////////////////
# examine all tumours together
dataset <- "all"
the.Big.df.SUBSET <- the.Big.df 
# note: change loop to a set endpoint (e.g. 500, rather than ~60k when testing...)
remove(modellist)
source("9B-Spearman-loop.R")
# ////////////////////////////////////////////////////////////

# what data sets do we have?
the.Big.df$project_id <- as.character(the.Big.df$project_id)
table(the.Big.df$project_id)
# TCGA-BLCA TCGA-CESC TCGA-CHOL 
# 358       259        35 
# TCGA-COAD TCGA-DLBC TCGA-ESCA 
# 231        48       152  
# TCGA-KIRP TCGA-LIHC 
# 210       334 
# TCGA-READ TCGA-UCEC 
# 72       511
# TCGA-UCS  TCGA-UVM 
# 51        53 


# ////////////////////////////////////////////////////////////
dataset <- "TCGA-BLCA"
the.Big.df.SUBSET <- the.Big.df 
keep.these <- which(the.Big.df.SUBSET$project_id == dataset) # given index
the.Big.df.SUBSET <- the.Big.df.SUBSET[keep.these,] # 2,314 all - TCGA-BLCA should be ... 358 ... correct
# according to the table above!
remove(modellist)
source("9B-Spearman-loop.R")
# ////////////////////////////////////////////////////////////
dataset <- "TCGA-CESC"
the.Big.df.SUBSET <- the.Big.df 
keep.these <- which(the.Big.df.SUBSET$project_id == dataset) # given index
the.Big.df.SUBSET <- the.Big.df.SUBSET[keep.these,] #
remove(modellist)
source("9B-Spearman-loop.R")
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
dataset <- "TCGA-CHOL"
the.Big.df.SUBSET <- the.Big.df 
keep.these <- which(the.Big.df.SUBSET$project_id == dataset) # given index
the.Big.df.SUBSET <- the.Big.df.SUBSET[keep.these,] #
remove(modellist)
source("9B-Spearman-loop.R")
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
dataset <- "TCGA-COAD"
the.Big.df.SUBSET <- the.Big.df 
keep.these <- which(the.Big.df.SUBSET$project_id == dataset) # given index
the.Big.df.SUBSET <- the.Big.df.SUBSET[keep.these,] #
remove(modellist)
source("9B-Spearman-loop.R")
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
dataset <- "TCGA-DLBC"
the.Big.df.SUBSET <- the.Big.df 
keep.these <- which(the.Big.df.SUBSET$project_id == dataset) # given index
the.Big.df.SUBSET <- the.Big.df.SUBSET[keep.these,] #
remove(modellist)
source("9B-Spearman-loop.R")
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
dataset <- "TCGA-KIRP"
the.Big.df.SUBSET <- the.Big.df 
keep.these <- which(the.Big.df.SUBSET$project_id == dataset) # given index
the.Big.df.SUBSET <- the.Big.df.SUBSET[keep.these,] #
remove(modellist)
source("9B-Spearman-loop.R")
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
dataset <- "TCGA-LIHC"
the.Big.df.SUBSET <- the.Big.df 
keep.these <- which(the.Big.df.SUBSET$project_id == dataset) # given index
the.Big.df.SUBSET <- the.Big.df.SUBSET[keep.these,] #
remove(modellist)
source("9B-Spearman-loop.R")
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
dataset <- "TCGA-READ"
the.Big.df.SUBSET <- the.Big.df 
keep.these <- which(the.Big.df.SUBSET$project_id == dataset) # given index
the.Big.df.SUBSET <- the.Big.df.SUBSET[keep.these,] #
remove(modellist)
source("9B-Spearman-loop.R")
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
dataset <- "TCGA-UCEC"
the.Big.df.SUBSET <- the.Big.df 
keep.these <- which(the.Big.df.SUBSET$project_id == dataset) # given index
the.Big.df.SUBSET <- the.Big.df.SUBSET[keep.these,] #
remove(modellist)
source("9B-Spearman-loop.R")
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
dataset <- "TCGA-UCS"
the.Big.df.SUBSET <- the.Big.df 
keep.these <- which(the.Big.df.SUBSET$project_id == dataset) # given index
the.Big.df.SUBSET <- the.Big.df.SUBSET[keep.these,] #
remove(modellist)
source("9B-Spearman-loop.R")
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
dataset <- "TCGA-UVM"
the.Big.df.SUBSET <- the.Big.df 
keep.these <- which(the.Big.df.SUBSET$project_id == dataset) # given index
the.Big.df.SUBSET <- the.Big.df.SUBSET[keep.these,] #
remove(modellist)
source("9B-Spearman-loop.R")
# ////////////////////////////////////////////////////////////




# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////
# ////////////////////////////////////////////////////////////



