# Ready to go
# GeneExpression.df 
# SampleInfo.df

setwd("~/Downloads/BMI_vs_HFD/GDC/SCRIPTS/")
load("SampleInfo.df.RData", .GlobalEnv)
load("GeneExpression.df.RData", .GlobalEnv)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
head(GeneExpression.df[c("4225530c-5f18-4e9f-b3b5-33c3db05be4d"),1:5]) # works
head(GeneExpression.df[,c("ENSG00000253983.2")])
row.names(GeneExpression.df) # File.Name in SampleInfo.df
#
SampleInfo.df$File.Name
head(GeneExpression.df[c("25c04d68-f867-4c8a-a597-2e30e13b9258"),1:5]) # works
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# write SampleInfo to file to quickly assess in Excel
library("openxlsx")
# write.xlsx(SampleInfo.df, "./SampleInfo..xlsx", asTable = FALSE,row.names=TRUE, colNames=TRUE)

