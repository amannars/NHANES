dir1 <- "~/Desktop/Hypertension study/NHANES files"
dir2 <- "~/Desktop/Hypertension study/Cleaned Data Files"

#read data files that have already downloaded from NHANES
setwd(dir1)
#SEQN - Respondent sequence number
#BPXSY1 - Systolic: Blood pres (1st rdg) mm Hg
#BPXDI1 - Diastolic: Blood pres (1st rdg) mm Hg
#BPAEN1 - Enhancement used first reading
#BPXSY2 - Systolic: Blood pres (2nd rdg) mm Hg
#BPXDI2 - Diastolic: Blood pres (2nd rdg) mm Hg
#BPAEN2 - Enhancement used second reading
#BPXSY3 - Systolic: Blood pres (3rd rdg) mm Hg
#BPXDI3 - Diastolic: Blood pres (3rd rdg) mm Hg
#BPAEN3 - Enhancement used third reading
#BPXSY4 - Systolic: Blood pres (4th rdg) mm Hg
#BPXDI4 - Diastolic: Blood pres (4th rdg) mm Hg
#BPAEN4 - Enhancement used fourth reading
bp_data <- read.csv(file="BPX_I.csv", header=TRUE)

#SEQN - Respondent sequence number
#BPQ020 - Ever told you had high blood pressure
#BPQ030 - Told had high blood pressure - 2+ times
#BPD035 - Age told had hypertension
#BPQ040A - Taking prescription for hypertension
#BPQ050A - Now taking prescribed medicine for HBP
#BPQ080 - Doctor told you - high cholesterol level
#BPQ060 - Ever had blood cholesterol checked
#BPQ070 - When blood cholesterol last checked
#BPQ090D - Told to take prescriptn for cholesterol
#BPQ100D - Now taking prescribed medicine
bp_question_data <- read.csv(file="BPQ_I.csv",header=TRUE)

#merge both blood pressure files
bp_data_all <- merge(bp_data, bp_question_data, by.x="SEQN", by.y="SEQN", all=FALSE)

#clean systolic data
keep <- !is.na(bp_data_all$BPXSY2) & !is.na(bp_data_all$BPXSY3)
bp_data <- bp_data_all[keep,]

#clean diastolic data
keep <- !is.na(bp_data$BPXDI2) & !is.na(bp_data$BPXDI3)
bp_data <- bp_data[keep,]

#get average systolic and diastolic BP
systolic_avg <- mapply(function(i) mean(bp_data$BPXSY2[i],bp_data$BPXSY3[i],
                     bp_data$BPXSY4[i], na.rm=TRUE), i=1:length(bp_data$BPXSY2))

diastolic_avg <- mapply(function(i) mean(bp_data$BPXDI2[i],bp_data$BPXDI3[i],
                     bp_data$BPXDI4[i], na.rm=TRUE), i=1:length(bp_data$BPXDI2))

#create hypertension category
hypertension <- rep(0, times=length(systolic_avg))
change <- (systolic_avg >= 130) | (diastolic_avg >= 80) | 
  (bp_data$BPQ040A == 1) | (bp_data$BPQ050A == 1)
hypertension[change] <- 1

bp_data$hypertension <- hypertension
hypertension_data <- bp_data[,c(1,40)]

setwd(dir2)
write.csv(file="Cleaned hypertension data.csv", x=hypertension_data, 
          row.names = FALSE)
