# #################################################
# Prepare the allGenes
# #################################################
# gene expression data is ready 
colnames(allGenes) # samples
row.names(allGenes) # ENSEMBL gene IDs
#
allGenes.transposed <- as.data.frame ( t(allGenes) ) # transpose
head(allGenes.transposed[1:4,1:4])
#                                          ENSG00000242268.2 ENSG00000270112.3
# X00106523.5b1d.44ad.a9f1.7d84db08722c             0.000          193.8039
# Kill the X
row.names.to.fix <- row.names(allGenes.transposed)
head(row.names(allGenes.transposed))
row.names.to.fix <-gsub("\\.", "-", row.names.to.fix)
# remove pesky X from importing
row.names.to.fix <- substring(row.names.to.fix, 2) 
row.names(allGenes.transposed) <- row.names.to.fix
# e.g. 53a9dbe8-a1b1-416b-8e7e-43f4862f7de0
# THIS IS THE 'File Name' ENTRY OF 
# ./data/gdc_sample_sheet.2018-05-03.tsv
# so, we can pull out the data we want now
# i.e.  the.merged.df$File.Name


# #################################################
# clinical data is all ready -- it just needs to have a corresponding gene expression file 
head(the.merged.df)  # we now have the File.Names
# just need to get rid of file name suffix e4544bfc-668c-4782-bc4a-760fb51fa62b.FPKM-UQ.txt.gz 
the.merged.df$File.Name <- gsub(".FPKM-UQ.txt.gz","",the.merged.df$File.Name)
# #################################################


# Ready to go
GeneExpression.df <- allGenes.transposed
SampleInfo.df <- the.merged.df 
#
save(GeneExpression.df, file = "GeneExpression.df.RData")
save(SampleInfo.df, file = "SampleInfo.df.RData")





# trick = decide which samples to keep in SampleInfo.df and pull out their gene expression from GeneExpression.df 
# no need to filter down GeneExpression.df to only match SampleInfo.df 
#


# CREATE A SEPARATE SCRIPT FOR FILTERING ... SEE OLD SCRIPT
# This is important since some samples are e.g. 'Solid Tissue Normal'
# most are 'Primary Tumor'
