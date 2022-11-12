dir1 <- "~/Desktop/Hypertension study/NHANES files"
dir2 <- "~/Desktop/Hypertension study/Cleaned Data Files"
setwd(dir1)
file_input <- "BMX_I.csv"

#read data files that have already downloaded from NHANES
#SEQN - Respondent sequence number
#BMXBMI - Body Mass Index (kg/m**2)
bmi_data_all <- read.csv(file=file_input, header=TRUE)

#clean up BMXBMI - Body Mass Index (kg/m**2) data
keep <- !is.na(bmi_data_all$BMXBMI)
bmi_data <- bmi_data_all[keep,]

#underweight BMI upper limit <18.5
#normal weight BMI upper limit <24.9
#pre-obese BMI upper limi  <29.9
#obese I BMI upper limit <34.9
#obese II BMI upper limit <39.9
#obese III BMI lower limit >=40

BMI_category <- rep(3, times=length(bmi_data$BMXBMI))
BMI_category[bmi_data$BMXBMI < 29.9] <- 2
BMI_category[bmi_data$BMXBMI < 24.9] <- 0  #use 0 as the default normal
BMI_category[bmi_data$BMXBMI < 18.5] <- 1

bmi_data$BMI_category <- BMI_category

setwd(dir2)
write.csv(file="Cleaned BMI data.csv", x=bmi_data, row.names = FALSE)