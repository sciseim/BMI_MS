# 2
# looks like GDC provides new IDs every time?
# downloaded from the cart this time (the day after)
# ###########################################################
clinical.df <- read.delim("./data/clinical.cart.2018-05-03/clinical.tsv")
exposure.df <- read.delim("./data/clinical.cart.2018-05-03/exposure.tsv")
# ... BMI etc here
sample.df <- read.delim("./data/gdc_sample_sheet.2018-05-03.tsv")
# indicate which are normal etc
# ~~~~~~~

#  clinical
# case_id	                                 submitter_id
# 9583c10b-b21a-4863-98fa-61e735e64ea5	TCGA-2E-A9G8
# exposure
# case_id	                              submitter_id
# d9121e5a-88f2-47a6-a0b1-25735c209e54	TCGA-AH-6644
# does NOT matter that these are not the same as the case_ids in the 

# TRY CASE_ID INSTEAD!
# OTHERWISE WE WILL LOSE E.G.
# TCGA-AJ-A2QL	TCGA-AJ-A2QL-01A	Primary Tumor
# TCGA-AJ-A2QL	TCGA-AJ-A2QL-11A	Solid Tissue Normal
#    SEE...
row.names(clinical.df) <- clinical.df$submitter_id # works
row.names(exposure.df) <- exposure.df$submitter_id # works
# list of data.frames I want to merge
library(data.table)
l = list(sample.df,clinical.df,exposure.df)
the.merged.df <- Reduce(merge, lapply(l, function(x) data.frame(x, rn = row.names(x))))
head(the.merged.df)  # we now have the File.Names
# this works MUCH better than merge by row.names!


# OLD 
row.names(clinical.df) <- clinical.df$submitter_id # works
row.names(exposure.df) <- exposure.df$submitter_id # works
# some are duplicates! Also true in TOIL!
#!!! row.names(sample.df) <- sample.df$Case.ID # FIX IT
IDs <- as.character (  sample.df$Case.ID  )
IDs <- IDs[duplicated(IDs)] # 240 are duplicated... has to go! Could be repeats..
# e.g. TCGA-AA-3511
# one is normal, one is tumour
# TCGA-AA-3511	TCGA-AA-3511-11A	Solid Tissue Normal
# TCGA-AA-3511	TCGA-AA-3511-01A	Primary Tumor
# OK. Fix here is to remove the normal. Interested in the tumour projectory here
table(sample.df$Sample.Type)
# Additional - New Primary               Metastatic            Primary Tumor 
# 1                        4                      2953 
# Recurrent Tumor      Solid Tissue Normal 
# 6                      210 
# if we want the normals, pull them out later with another 'which' here!
keep.these <- which(sample.df$Sample.Type != "Solid Tissue Normal") 
length(sample.df$Sample.Type) # 3174 - 210
sample.df.temp <- sample.df[keep.these,]
class(sample.df.temp) # df
row.names(sample.df.temp) <- sample.df.temp$Case.ID # FIX IT
# OK, let us just keep Primary Tumours!
keep.these <- which(sample.df$Sample.Type == "Primary Tumor") 
sample.df.temp <- sample.df[keep.these,]
length(sample.df$Sample.Type) # 2953
row.names(sample.df.temp) <- sample.df.temp$Case.ID # FIX IT
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OK, https://stackoverflow.com/questions/6835753/how-to-remove-duplicated-rows-by-a-column-in-a-matrix-in-r 
sample.df.temp.matrix <- as.matrix(sample.df.temp)
sample.df.temp.matrix[,c("Case.ID")]
testar <- sample.df.temp.matrix[!duplicated(sample.df.temp.matrix[,c("Case.ID")]),]
testar <- as.data.frame(testar) # 2917
row.names(testar) <- testar$Case.ID # FIX IT
sample.df <- testar
head(sample.df[1:4,1:4]) # so, row name is e.g. TCGA-F5-6864
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

remove(the.merged.df)
# list of data.frames I want to merge
library(data.table)
l = list(sample.df,clinical.df,exposure.df)
#
the.merged.df <- transform(Reduce(merge, lapply(l, function(x) data.frame(x, rn = row.names(x)))), row.names=rn, rn=NULL)
head(the.merged.df)  # we now have the File.Names
# this works MUCH better than merge by row.names!





