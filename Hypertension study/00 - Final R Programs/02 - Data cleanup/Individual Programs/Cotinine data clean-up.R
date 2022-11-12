dir1 <- "~/Desktop/Hypertension study/NHANES files"
dir2 <- "~/Desktop/Hypertension study/Cleaned Data Files"
setwd(dir1)
file_input <- "COT_I.csv"

#read data files that have already downloaded from NHANES
#SEQN - Respondent Sequence Number
#LBXCOT - Cotinine, Serum (ng/mL)
#LBDCOTLC - Cotinine, Serum Comment Code
#LBXHCT - Hydroxycotinine, Serum (ng/mL)
#LBDHCTLC - Hydroxycotinine, Serum Comment Code
cotinine_data_all <- read.csv(file=file_input, header=TRUE)

#clean up cotinine data
keep <- (cotinine_data_all$LBDCOTLC == 0) | (cotinine_data_all$LBDCOTLC == 1)
cotinine_data_all <- cotinine_data_all[keep,]

#change flag to 0=below LLod, 1=above LLoD
cotinine_data_all$LBDCOTLC <- 1-cotinine_data_all$LBDCOTLC

setwd(dir2)
write.csv(file="Cleaned continine data.csv", x=cotinine_data_all, row.names = FALSE)