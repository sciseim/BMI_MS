# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# IMPORTANT
# FILTER SCRIPT 2 WILL BE TO ONLY INCLUDE CERTAIN Sample Type
table(SampleInfo.df$Sample.Type)
# for now, let us deal with Primary Tumours only
# only <10 total metastatic tumours in TCGA
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# OK, still have




# IMPORTANT!!!!!!!!
# note some file names in sampleinfo has some additional letters
# since these codes are fairly unique, let us remove everything up to the first dash in both!!! 
test.S <- "c696175b-9427-484b-8673-49a1c321f2fa" # in sampleinfo
test.G<- "696175b-9427-484b-8673-49a1c321f2fa" # in geneexp
unlist(  regmatches(test.S,gregexpr("(?<=-).*",test.S,perl=TRUE)) )
unlist(  regmatches(test.G,gregexpr("(?<=-).*",test.G,perl=TRUE)) )
# OUTPUTS: 9427-484b-8673-49a1c321f2fa ... THIS IS STILL UNIQUE TO THIS SAMPLE!

# OK, SO
SampleInfo.df$File.Name <- unlist(  regmatches(SampleInfo.df$File.Name,gregexpr("(?<=-).*",SampleInfo.df$File.Name,perl=TRUE)) )
#
GeneExpression.FileNames <- row.names(GeneExpression.df)
GeneExpression.FileNames <-  unlist(  regmatches(GeneExpression.FileNames,gregexpr("(?<=-).*",GeneExpression.FileNames,perl=TRUE)) )
row.names(GeneExpression.df) <- GeneExpression.FileNames 




# ONLY KEEP OVERLAPS!!!
# ################################################################### 
row.names(GeneExpression.df) # filenames ... 3,174 samples -- including ~500 Normal, a couple of metastatic etc (see script 2)
length(GeneExpression.df[,1]) # 3174
#
#
keep.these <- SampleInfo.df$File.Name # 
length(keep.these) # 2314

# keep GeneExpression data for the following samples
temp <- GeneExpression.df
temp$File.Name <- row.names(temp)
class(temp$File.Name) # character
#

# TWO WAYS TO ONLY KEEP $Filenames matching samples
# 1.
length(keep.these) # 2917
temp.MATCHING <- temp[  temp$File.Name %in% keep.these, ] # 1739 samples kept...
# FASTER!
length(unique(temp$File.Name)) # 3174
length(unique(SampleInfo.df$File.Name)) # 2314
length(temp.MATCHING$File.Name) # 2314
#
# 2. 
# temp.NOTMATCHING <- temp[!temp$File.Name %in% keep.these, ] #  !temp$Filename if 
# temp.MATCHING <- temp[temp$File.Name %in% keep.these, ] #  !temp$Filename if temp.NOTMATCHING

# ensure that they have the same order re:File.Name
# SampleInfo.df and temp.MATCHING
temp.MATCHING <- temp.MATCHING[with(temp.MATCHING, order(File.Name)), ]
SampleInfo.df <- SampleInfo.df[with(SampleInfo.df, order(File.Name)), ]
# since these have the same file names, sorting them will ensure that any downstream analysis is correct
head(temp.MATCHING$File.Name) # [1] "0012-4cfb-8749-d94f22b4ca75" "0064-4218-889d-4280e60092cb"
head(SampleInfo.df$File.Name) # matches
tail(temp.MATCHING$File.Name) # [5] "ffe2-466a-93ce-5bcefd609040" "ffef-4ed0-a786-dfaf520a81eb"
tail(SampleInfo.df$File.Name) # matches






# Drop Filenames column
temp.MATCHING <- subset(temp.MATCHING, select=-c(File.Name))


# SAVE FINAL OUTPUT
#  <
#  <
#  <
GeneExpression.READY.df <- temp.MATCHING
SampleInfo.READY.df<- SampleInfo.df


# OK, we now have a complete dataset of 60,484 genes and 2,314 primary tumours with BMI data!
save(GeneExpression.READY.df, file = "GeneExpression.READY.Robj")
save(SampleInfo.READY.df, file = "SampleInfo.READY.Robj")


