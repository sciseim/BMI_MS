# setwd("~/Downloads/BMI_vs_HFD/GDC/SCRIPTS/")
setwd("~/Dropbox/Manuscripts/BMI_vs_TCGA-RNA-seq/GDC/SCRIPTS/")

load("the.Big.df.Robj", .GlobalEnv)
length.of.df <- length(the.Big.df[1,]) # 60529 (47 are sampleinfo)
colnames(the.Big.df) # e.g. genes ... plus *46 SampleInfo columns*
gene.names <- names ( the.Big.df[,47:length.of.df] )
#
dataset <- "TCGA-ESCA"
the.Big.df.SUBSET <- the.Big.df 

# convert to character to 'which' will work
class(the.Big.df.SUBSET$project_id) # factor!
the.Big.df.SUBSET$project_id <- as.character(the.Big.df.SUBSET$project_id)
keep.these <- which(the.Big.df.SUBSET$project_id == dataset) # given index
the.Big.df.SUBSET <- the.Big.df.SUBSET[keep.these,] 

# create output directory
dir.create(paste("./Spearman/",dataset,"/survival",sep=""))

tempdata <- the.Big.df.SUBSET
# ///////////////////////////////////////////////////////

# #############################################################
# draw distribution plot (BMI per group)
# Density plots with semi-transparent fill
density.df <- tempdata
density.df$bmi

# check that it matches TCGA-ESCA Table 1 (see script 10B)
quantileinfo <- quantile(density.df$bmi)
BMI.firstquartile <- quantileinfo[2] # 25% # 21.29529
BMI.thirdquartile <- quantileinfo[4] # 75% # 27.46914

table(density.df$BMI.group)
# normal      obese overweight 
# 84         22         46 
class(density.df$BMI.group)

library(ggplot2)
ggplot(density.df, aes(x=bmi, fill=BMI.group)) + geom_density(alpha=.3)
# change 'normal' to lean
density.df$BMI.group[density.df$BMI.group=="normal"] <-"lean"
#
lean.df <- density.df[which(density.df$BMI.group == "lean"),]
lean.df$mean <- mean(lean.df$bmi)
overweight.df <- density.df[which(density.df$BMI.group == "overweight"),]
overweight.df$mean <- mean(overweight.df$bmi)
obese.df <- density.df[which(density.df$BMI.group == "obese"),]
obese.df$mean <- mean(obese.df$bmi)
density.df <- rbind(lean.df,overweight.df,obese.df) # add back
# Density plots with means
p <- ggplot(density.df, aes(x=bmi, fill=BMI.group)) + geom_density(alpha=.3) + geom_vline(aes(xintercept=lean.df$mean[1]),color="#53a362", linetype="dashed", size=1) + geom_vline(aes(xintercept=overweight.df$mean[1]),color="#d9703c", linetype="dashed", size=1) + geom_vline(aes(xintercept=obese.df$mean[1]),color="#d82e2e", linetype="dashed", size=1)  
p + scale_fill_manual(values=c("#53a362","#d82e2e","#d9703c"))



# geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
# use counts
# Density plots with means
pdf(paste(dataset,"-BMI.group.histogram.pdf",sep=""),height=3)
p <- ggplot(density.df, aes(x=bmi, fill=BMI.group)) + geom_histogram(binwidth=0.5) + geom_vline(aes(xintercept=lean.df$mean[1]),color="#53a362", linetype="dashed", size=0.5) + geom_vline(aes(xintercept=overweight.df$mean[1]),color="#d9703c", linetype="dashed", size=0.5) + geom_vline(aes(xintercept=obese.df$mean[1]),color="#d82e2e", linetype="dashed", size=0.5)  
#
p <- p + scale_fill_manual(values=c("#53a362","#d82e2e","#d9703c")) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +xlab("no. patients with BMI")+ylab(paste("BMI",sep=""))
print(p)
dev.off()


# lean
lean.df$mean[1] # mean # 21.47611
min(lean.df$bmi) # 13.39895
max(lean.df$bmi) # 24.84098
# overweight
overweight.df$mean[1] # mean 27.1116
min(overweight.df$bmi) # 25.08286
max(overweight.df$bmi) # 29.77778
# obese
obese.df$mean[1] # mean 34.512
min(obese.df$bmi) # 30.32334
max(obese.df$bmi) # 44.95673

# no samples
length(lean.df$bmi)
length(overweight.df$bmi)
length(obese.df$bmi)
length(lean.df$bmi)+length(overweight.df$bmi)+length(obese.df$bmi)

# p <- ggplot(density.df, aes(x=bmi, fill=BMI.group)) + geom_histogram(binwidth=0.3) + geom_vline(aes(xintercept=lean.df$mean[1]),color="#53a362", linetype="dashed", size=1) + geom_vline(aes(xintercept=overweight.df$mean[1]),color="#d9703c", linetype="dashed", size=1) + geom_vline(aes(xintercept=obese.df$mean[1]),color="#d82e2e", linetype="dashed", size=1)  
# #
# p + scale_fill_manual(values=c("#53a362","#d82e2e","#d9703c")) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +xlab("no. patients with BMI")+ylab(paste("BMI",sep="")) + geom_histogram(aes(y=..density..), colour="black", fill="white")+
#   geom_density(alpha=0.3, fill="#FF6666") 


