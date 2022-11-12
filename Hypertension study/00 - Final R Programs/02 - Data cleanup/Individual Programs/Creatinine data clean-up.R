dir1 <- "~/Desktop/Hypertension study/NHANES files"
dir2 <- "~/Desktop/Hypertension study/Cleaned Data Files"
setwd(dir1)
file_input <- "ALB_CR_I.csv"

#read data files that have already downloaded from NHANES
#SEQN - Respondent sequence number
#URXUMA - Albumin, urine (ug/mL)
#URDUMALC - Albumin, urine comment code
#URXUMS - Albumin, urine (mg/L)
#URXUCR - Creatinine, urine (mg/dL)
#URDUCRLC - Creatinine, urine comment code
#URXCRS - Creatinine, urine (umol/L)
#URDACT - Albumin creatinine ratio (mg/g)
creatinine_data_all <- read.csv(file=file_input, header=TRUE)

#clean up URXUCR data
keep <- !is.na(creatinine_data_all$URXUCR)
creatinine_data <- creatinine_data_all[keep,]

setwd(dir2)
write.csv(file="Cleaned creatinine data.csv", x=creatinine_data, row.names = FALSE)