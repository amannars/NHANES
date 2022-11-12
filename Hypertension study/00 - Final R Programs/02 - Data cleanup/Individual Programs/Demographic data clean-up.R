dir1 <- "~/Desktop/Hypertension study/NHANES files"
dir2 <- "~/Desktop/Hypertension study/Cleaned Data Files"
setwd(dir1)
file_input <- "DEMO_I.csv"

#read data file that has already downloaded from NHANES
#SEQN - Respondent sequence number
#RIAGENDR - Gender
#RIDAGEYR - Age in years at screening
#RIDRETH1 - Race/Hispanic origin
#RIDRETH3 - Race/Hispanic origin w/ NH Asian
#DMDEDUC2 - Education level - Adults 20+
#DMDMARTL - Marital status
#INDFMPIR - Ratio of family income to poverty
demo_data <- read.csv(file=file_input, header=TRUE)

#convert age to categorical variable
age_cluster <- rep(3, times=length(demo_data$RIDAGEYR))
age_cluster[demo_data$RIDAGEYR < 65] <- 2
age_cluster[(demo_data$RIDAGEYR < 40) & (demo_data$RIDAGEYR >= 20)] <- 1

#convert FIPR to categorical variable
FIPR_cluster <- rep(3, times=length(demo_data$INDFMPIR))
FIPR_cluster[demo_data$INDFMPIR < 1.50] <- 2
FIPR_cluster[demo_data$INDFMPIR < 1.00] <- 1

demo_data$age_cluster <- age_cluster
demo_data$FIPR_cluster <- FIPR_cluster

demo_data2 <- demo_data[,c(1,2,3,4,5,8,17,18,40,41,42,43,44,47,52,53)]

#remove all rows with any NA values
for (i in ncol(demo_data2)){    #ncol(demo_data)
  keep <- !is.na(demo_data2[,i])
  demo_data2 <- demo_data2[keep,]
}

#clean RIAGENDR - Gender data
keep <- (demo_data2$RIAGENDR >= 1 & demo_data2$RIAGENDR <= 2)
demo_data2 <- demo_data2[keep,]

#clean RIDAGEYR - Age in years at screening
keep <- (demo_data2$RIDAGEYR >= 20 & demo_data2$RIDAGEYR <= 80)
demo_data2 <- demo_data2[keep,]

#clean RIDRETH3 - Race/Hispanic origin w/ NH Asian
keep <- (demo_data2$RIDRETH3 >= 1 & demo_data2$RIDRETH3 <= 7)
demo_data2 <- demo_data2[keep,]

#clan DMDEDUC2 - Education level - Adults 20+
keep <- (demo_data2$DMDEDUC2 >= 1 & demo_data2$DMDEDUC2 <= 5)
demo_data2 <- demo_data2[keep,]

#clean DMDMARTL - Marital status
keep <- (demo_data2$DMDMARTL >= 1 & demo_data2$DMDMARTL <= 6)
demo_data2 <- demo_data2[keep,]

#clean INDFMPIR - Ratio of family income to poverty
keep <- (demo_data2$INDFMPIR >= 0 & demo_data2$INDFMPIR <= 5)
demo_data2 <- demo_data2[keep,]

setwd(dir2)
write.csv(file="Cleaned demographic data.csv", x=demo_data2, row.names = FALSE)