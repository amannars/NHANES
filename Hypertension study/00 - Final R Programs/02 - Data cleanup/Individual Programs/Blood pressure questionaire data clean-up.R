dir1 <- "~/Desktop/Hypertension study/NHANES files"
dir2 <- "~/Desktop/Hypertension study/Cleaned Data Files"
setwd(dir1)
file_input <- "BPQ_I.csv"

#read data files that have already downloaded from NHANES
#SEQN - Respondent sequence number
#BPQ040A - Taking prescription for hypertension
#BPQ050A - Now taking prescribed medicine for HBP
#BPQ100D - Now taking prescribed medicine
BP_questions_data_all <- read.csv(file=file_input, header=TRUE)

#clean up BPQ040A data
keep <- !is.na(BP_questions_data_all$BPQ040A) & (BP_questions_data_all$BPQ040A <= 2)
BP_questions_data_all <- BP_questions_data_all[keep,]

#clean up BPQ050A data
keep <- !is.na(BP_questions_data_all$BPQ050A) & (BP_questions_data_all$BPQ050A <= 2)
BP_questions_data_all <- BP_questions_data_all[keep,]

#clean up BPQ100D data
keep <- !is.na(BP_questions_data_all$BPQ100D) & (BP_questions_data_all$BPQ100D <= 2)
BP_questions_data_all <- BP_questions_data_all[keep,]

setwd(dir2)
write.csv(file="Cleaned BP Questionaire data.csv", 
          x=BP_questions_data_all, row.names = FALSE)