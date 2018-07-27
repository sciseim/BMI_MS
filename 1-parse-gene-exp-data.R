# The NCI's Genomic Data Commons (GDC) data
# cite
# https://gdc.cancer.gov/about-data/publications



# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# https://gdc.cancer.gov/access-data/gdc-data-transfer-tool
# https://docs.gdc.cancer.gov/Data_Transfer_Tool/Users_Guide/Data_Download_and_Upload/
# downloaded the data like so
# time ~/Downloads/gdc-client download -m gdc_manifest.2018-05-03.txt
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# tar -cvf FPKM-UQ.tar ./FPKM-UQ
# tar -cvf HTSeq-Counts.tar ./HTSeq-Counts
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# entered each directory and copied the FPKM files to ../FPKM
# run this on the command-line
# 0-copy_gene_exp-into_dirs_sh
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# these files have no headings!
# ENSG00000000003.13	3292

# want
# gene                sample1   sample2  sample3 
# ENSG00000000003.13	3292

# 3174 files
setwd("/Volumes/RaxUSB/FPKM-UQ/FPKM")
# setwd("/Volumes/RaxUSB/test/FPKM")
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# First, get a handle on the files that you want to import
# here, selecting all files with ".txt" suffix from the current working directory.
my.files <- list.files(pattern = ".FPKM-UQ.txt")
my.files


remove(FILElist)
FILElist <- list()
# we can then import all into a single data frame with sample name as the header -- give them TCGA names using the sample file later!
for (i in 1:(length(my.files)))
{
  if (i == 1) {
    # during the first run, I'm creating a dummy data frame with out any values. Later, I append the content of each file to this data frame
    my.data <- data.frame(NULL)
  }
  
  class(my.files) # character
  tempfilename <- my.files[i]
  tempfilename <- gsub(".FPKM-UQ.txt","",tempfilename)
  
  cur.file <- read.delim(file = my.files[i], sep = "\t",header=FALSE)

# I'm using the the column header of the second column as a new variable. This indicator is unique for each file
# head(cur.file)
row.names(cur.file) <- cur.file$V1
colnames(cur.file)
colnames(cur.file)[2] <- tempfilename
cur.file <- subset(cur.file, select=-c(V1))

  FILElist[[i]] <- cur.file
  print(i)
    }
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
class(FILElist)
# head(FILElist[1])
# head(FILElist[2])
FILElistDF <- as.data.frame(FILElist)
head(FILElistDF[1:2,1:2])
# denote allGenes 
allGenes <- FILElistDF
# next need to merge with the Biospecimen and Clinical data!
# head(allGenes)

# save as R object
setwd("~/Downloads/BMI_vs_HFD/GDC/")
save(allGenes, file = "allGenes-FPKM-UQ.RData")

