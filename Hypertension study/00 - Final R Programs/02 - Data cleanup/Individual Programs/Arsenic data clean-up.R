dir1 <- "~/Desktop/Hypertension study/NHANES files"
dir2 <- "~/Desktop/Hypertension study/Cleaned Data Files"
setwd(dir1)
file_input <- "UAS_I.csv"

#read data file that has already downloaded from NHANES
#SEQN - sequence number
#WTSA2YR - Subsample A weights
#URXUAS3 - Urinary Arsenous acid (ug/L)
#URDUA3LC - Urinary Arsenous acid comment code
#URXUAS5 - Urinary Arsenic acid (ug/L)
#URDUA5LC - Urinary Arsenic acid comment code
#URXUAB - Urinary Arsenobetaine (ug/L)
#URDUABLC - Urinary Arsenobetaine comment code
#URXUAC - Urinary Arsenocholine (ug/L)
#URDUACLC - Urinary Arsenocholine comment code
#URXUDMA - Urinary Dimethylarsinic acid (ug/L)
#URDUDALC - Urinary Dimethylarsonic acid comment
#URXUMMA - Urinary Monomethylacrsonic acid (ug/L)
#URDUMMAL - Urinary Monomethylacrsonic acid comment
arsenic_data_all <- read.csv(file="UAS_I.csv", header=TRUE)

#clean up based on comments, since they are flagged as 0=above LDL, 1=below LDL
arsenic_data2 <- arsenic_data_all

change <- arsenic_data2$URDUA3LC == 1
arsenic_data2$URXUAS3[change] <- NA

change <- arsenic_data2$URDUA5LC == 1
arsenic_data2$URXUAS5[change] <- NA

change <- arsenic_data2$URDUABLC == 1
arsenic_data2$URXUAB[change] <- NA

change <- arsenic_data2$URDUACLC == 1
arsenic_data2$URXUAC[change] <- NA

change <- arsenic_data2$URDUDALC == 1
arsenic_data2$URXUDMA[change] <- NA

change <- arsenic_data2$URDUMMAL == 1
arsenic_data2$URXUMMA[change] <- NA

#get creatinine data to use as normalization factor
#SEQN - Respondent sequence number
#URXUMA - Albumin, urine (ug/mL)
#URDUMALC - Albumin, urine comment code
#URXUMS - Albumin, urine (mg/L)
#URXUCR - Creatinine, urine (mg/dL)
#URDUCRLC - Creatinine, urine comment code
#URXCRS - Creatinine, urine (umol/L)
#URDACT - Albumin creatinine ratio (mg/g)
creatinine_data <- read.csv(file="ALB_CR_I.csv", header=TRUE)

#merge arsenic and creatine data files
merged_data <- merge(arsenic_data2, creatinine_data, 
                     by.x="SEQN", by.y="SEQN", all=FALSE)

#create normalized arsenic concentrations
merged_data$AS3norm <- (merged_data$URXUAS3/merged_data$URXUCR)
merged_data$AS5norm <- (merged_data$URXUAS5/merged_data$URXUCR)
merged_data$ABnorm <- (merged_data$URXUAB/merged_data$URXUCR)
merged_data$ACnorm <- (merged_data$URXUAC/merged_data$URXUCR)
merged_data$DMAnorm <- (merged_data$URXUDMA/merged_data$URXUCR)
merged_data$MMAnorm <- (merged_data$URXUMMA/merged_data$URXUCR)

hist(merged_data$AS3norm, 
     xlab=expression("Normalized Arsenous Acid Concentration"),
     main="")
hist(merged_data$AS5norm, 
     xlab=expression("Normalized Arsenic Acid Concentration"),
     main="")
hist(merged_data$ABnorm, 
     xlab=expression("Normalized Arsenobetaine Concentration"),
     main="")
hist(merged_data$ACnorm, 
     xlab=expression("Normalized Arsenocholine Concentration"),
     main="")
hist(merged_data$DMAnorm, 
     xlab=expression("Normalized DMA Concentration"),
     main="")
hist(merged_data$MMAnorm, 
     xlab=expression("Normalized MMA Concentration"),
     main="")

setwd(dir2)
write.csv(file="Cleaned arsenic data.csv", x=merged_data, row.names=FALSE)

#read data file that has already downloaded from NHANES
setwd(dir1)
file_input <- "UTAS_I.csv" 
TA_data <- read.csv(file=file_input, header=TRUE)[,c(1:3)]
keep <- !is.na(TA_data$URXUAS)
TA_data <- TA_data[keep,]
TA_data <- merge(TA_data, creatinine_data, by.x="SEQN", by.y="SEQN", all=FALSE)
TA_data$TAnorm <- TA_data$URXUAS/TA_data$URXUCR

setwd(dir2)
write.csv(file="Cleaned total arsenic data.csv", x=TA_data, row.names=FALSE)
