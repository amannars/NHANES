library(dplyr)
library(survey)
library(viridis)
library(InformationValue)
library(plot3D)

dir2 <- "~/Desktop/Formaldehyde study/Cleaned Data Files"
setwd(dir2)
#input cleaned NHANES data files for covariates
CKD <- read.csv(file="Cleaned kidney disease data.csv", header=TRUE)
demographic <- read.csv(file="Cleaned demographic data.csv", header=TRUE)[,-16]

#Kidney disease
base_model10 <- merge(CKD, demographic, by.x="SEQN", by.y="SEQN", all=FALSE)
#sink(file="CKD Covariate Logit Models.txt")
colnames(model_data)[2] <- "disease"

#declare variables as factors as needed
model_data$RIAGENDR <- as.factor(model_data$RIAGENDR)  #gender
model_data$RIDRETH3 <- as.factor(model_data$RIDRETH3)  #ethnicity
model_data$DMDEDUC2 <- as.factor(model_data$DMDEDUC2)  #education
model_data$DMDMARTL <- as.factor(model_data$DMDMARTL)  #marital status
model_data$age_cluster <- as.factor(model_data$age_cluster)  #age
model_data$FIPR_cluster <- as.factor(model_data$FIPR_cluster)  #FIPR
model_data$BMI_category <- as.factor(model_data$BMI_category)  #body mass index data
model_data$LBDCOTLC <- as.factor(model_data$LBDCOTLC)  #cotinine
model_data$ALQ101 <- as.factor(model_data$ALQ101)  #alcohol consumption

#Create a survey design object
base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                         weights=~WTINT2YR, nest=TRUE)

#Main-effects model
logit_model1 <- lm(disease ~ RIDRETH3, 
                       #family = quasibinomial(),
                       data   = model_data)
                       #design = base_survey)
print(summary(logit_model1))

library(multcompView)
ANOVA1 <- aov(logit_model1)
print(ANOVA1)
TUKEY <- TukeyHSD(ANOVA1, 'model_data$RIDRETH3', conf.level=0.95)
print(TUKEY)