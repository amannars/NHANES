dir1 <- "~/Desktop/Hypertension study/NHANES files"
dir2 <- "~/Desktop/Hypertension study/Cleaned Data Files"
setwd(dir1)
file_input <- "ALQ_I.csv"

#read data files that have already downloaded from NHANES
#SEQN - Respondent sequence number
#ALQ101 - Had at least 12 alcohol drinks/1 yr?
#ALQ110 - Had at least 12 alcohol drinks/lifetime?
#ALQ120Q - How often drink alcohol over past 12 mos
#ALQ120U - # days drink alcohol per wk, mo, yr
#ALQ130 - Avg # alcoholic drinks/day - past 12 mos
#ALQ141Q - # days have 4/5 drinks - past 12 mos
#ALQ141U - # days per week, month, year?
#ALQ151 - Ever have 4/5 or more drinks every day?
#ALQ154 - CHECK ITEM
#ALQ160 - # days have 4/5 or more drinks in 2 hrs
alq_data_all <- read.csv(file=file_input, header=TRUE)

#clean up ALQ101 - Had at least 12 alcohol drinks/1 yr?
keep <- (alq_data_all$ALQ101 == 1) | (alq_data_all$ALQ101 == 2)
alq_data <- alq_data_all[keep,]

setwd(dir2)
write.csv(file="Cleaned alcohol data.csv", x=alq_data, row.names = FALSE)