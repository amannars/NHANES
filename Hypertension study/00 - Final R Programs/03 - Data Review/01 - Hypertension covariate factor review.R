library(dplyr)
library(survey)
library(viridis)

# Define a function to call svymean and unweighted count
getSummary <- function(varformula, byformula, design){
  # Get mean, stderr, and unweighted sample size
  c <- svyby(varformula, byformula, design, unwtd.count ) 
  p <- svyby(varformula, byformula, design, svymean ) 
  outSum <- left_join(select(c,-se), p) 
  outSum
}

make_factors <- function(that_data){
  this_data <- that_data
  #define categorical factors
  this_data$RIAGENDR <- as.factor(this_data$RIAGENDR)
  this_data$RIDRETH3 <- as.factor(this_data$RIDRETH3)
  this_data$DMDEDUC2 <- as.factor(this_data$DMDEDUC2)
  this_data$DMDMARTL <- as.factor(this_data$DMDMARTL)
  this_data$age_cluster <- as.factor(this_data$age_cluster)
  this_data$FIPR_cluster <- as.factor(this_data$FIPR_cluster)
  
  #BMI, continine, and creatinine data
  this_data$BMI_category <- as.factor(this_data$BMI_category)
  this_data$LBDCOTLC <- as.factor(this_data$LBDCOTLC)
  this_data$ALQ101 <- as.factor(this_data$ALQ101)
  return(this_data)
}

source("~/Desktop/Hypertension study/00 - Final R Programs/03 - Data Review/covariate_analysis.R")
dir1 <- "~/Desktop/Hypertension study/Cleaned Data Files"
dir2 <- "~/Desktop/Hypertension study/Covariate Analysis Results"
sink_file <- paste(dir2,"/Covariate Analysis.txt",sep="")

#input cleaned NHANES data files
setwd(dir1)
hypertension_data <- read.csv(file="Cleaned hypertension data.csv", header=TRUE)
BMI_data <- read.csv(file="Cleaned BMI data.csv", header=TRUE)[,c(1,31)]
continine_data <- read.csv(file="Cleaned continine data.csv", header=TRUE)[,c(1:5)]
#creatinine_data <- read.csv(file="Cleaned creatinine data.csv", header=TRUE)
demographic <- read.csv(file="Cleaned demographic data.csv", header=TRUE)[,-c(2,3,5,9,14)]
alcohol_data <- read.csv(file="Cleaned alcohol data.csv", header=TRUE)[,c(1:10)]

#base model for hypertension has covariates:
#  demographic data
#  BMI data
#  continine data
#  blood pressure questionaire data
base_model0 <- merge(hypertension_data, demographic, by.x="SEQN", by.y="SEQN", all=FALSE)
base_model1 <- merge(base_model0, BMI_data,  by.x="SEQN", by.y="SEQN", all=FALSE)
print(dim(base_model1))
base_model2 <- merge(base_model1, continine_data,  by.x="SEQN", by.y="SEQN", all=FALSE)
print(dim(base_model2))
base_model3 <- merge(base_model2, alcohol_data,  by.x="SEQN", by.y="SEQN", all=FALSE)
print(dim(base_model3))
model_data <- base_model3

sink(file=sink_file)
# Create a survey design object - demographic data only
factor_data <- make_factors(model_data)
NHANES_base <- svydesign(data=factor_data, id=~SDMVPSU, 
                         strata=~SDMVSTRA, weights=~WTINT2YR, nest=TRUE)
setwd(dir2)
covariate_analysis(NHANES_base,"Demographics Data Set")
setwd(dir1)

# Create a survey design object - demographic and arsenic data
arsenic <- read.csv(file="Cleaned arsenic data.csv", header=TRUE)[,c(1:14,19:25,30:35)]
total_arsenic <- read.csv(file="Cleaned total arsenic data.csv", header=TRUE)[,c(1,3:10,15)]
AS1_base <- merge(model_data, arsenic, by.x="SEQN", by.y="SEQN", all=FALSE)
AS2_base <- merge(AS1_base, total_arsenic, by.x="SEQN", by.y="SEQN", all=FALSE)
factor_data <- make_factors(AS2_base)
NHANES_base <- svydesign(data=factor_data, id=~SDMVPSU, 
                         strata=~SDMVSTRA, weights=~WTSA2YR, nest=TRUE)
setwd(dir2)
covariate_analysis(NHANES_base,"Demographics and Arsenic Data Sets")
setwd(dir1)

# Create a survey design object - demographic data and metals data
HM_data <- read.csv(file="Cleaned heavy metals data.csv", header=TRUE)
mercury <- read.csv(file="Cleaned Hg data.csv", header=TRUE)
HM1_base <- merge(model_data, HM_data, by.x="SEQN", by.y="SEQN", all=FALSE)
HM2_base <- merge(HM1_base, mercury, by.x="SEQN", by.y="SEQN", all=FALSE)
factor_data <- make_factors(HM2_base)
NHANES_base <- svydesign(data=factor_data, id=~SDMVPSU, 
                         strata=~SDMVSTRA, weights=~WTFSM, nest=TRUE)
setwd(dir2)
covariate_analysis(NHANES_base, "Demographics and Heavy Metals Data Sets")
setwd(dir1)

sink()