library(dplyr)
library(survey)
library(viridis)
library(aod)
library(InformationValue)
library(plot3D)

print_summary <- function(base_model, model_data, n_model){
  print(data.frame(coef(base_model), confint(base_model)))
  base_confint <- confint(base_model)
  predicted <- plogis(predict(base_model, model_data))  # predicted scores
  optCutOff <- optimalCutoff(model_data$hypertension, predicted)[1]
  df_metric <- model_validation(model_data, predicted, optCutOff, model="Base Model",n_model)
  return(predicted)
}

model_validation <- function(Data_set, predicted, optCutOff=0.47, model, i_value){
  misClass <- misClassError(Data_set$hypertension, predicted, threshold = optCutOff)
  pdf(file=paste("ROC Curve ",model," ",i_value,".pdf",sep=""))
  abc <- plotROC(Data_set$hypertension, predicted, returnSensitivityMat = TRUE)
  dev.off()
  x <- abc[,1]
  y <- abc[,2]
  abcd <- mapply(function(i) (x[i+1]-x[i])*(y[i+1]+y[i])/2, i=1:(length(x)-1))
  AuC <- sum(abcd)
  #concord <- Concordance(Data_set$hypertension, predicted)
  #cat("\n\nConcordance\n")
  #print(concord)
  
  #following from:
  #https://www.jigsawacademy.com/sensitivity-vs-specificity-in-logistic-regression/
  
  #Sensitivity and specificity are statistical measures of the performance 
  #of a binary classification test, also known in statistics as classification 
  #function:
    
  #Sensitivity (also called the true positive rate, or the recall in some fields) 
  #measures the proportion of actual positives which are correctly identified as 
  #such (e.g., the percentage of sick people who are correctly identified as having 
  #the condition), and is complementary to the false negative rate. 
  #Sensitivity = true positives/(true positive + false negative)
  
  #Specificity (also called the true negative rate) measures the proportion of 
  #negatives which are correctly identified as such (e.g., the percentage of 
  #healthy people who are correctly identified as not having the condition), 
  #and is complementary to the false positive rate. 
  #Specificity = true negatives/(true negative + false positives)
  
  sens <- sensitivity(Data_set$hypertension, predicted, threshold = optCutOff)
  spec <- specificity(Data_set$hypertension, predicted, threshold = optCutOff)
  cat("\n")
  df_print <- data.frame(optCutOff,misClass,sens,spec,AuC)
  colnames(df_print) <- c("Cutoff", "Missclassification", "Sensitivity", "Specificity","AuC")
  print(df_print)
  con_mat <- confusionMatrix(Data_set$hypertension, predicted, threshold = optCutOff)
  cat("\nConfusion matrix\nThe columns are actuals, while rows are predicteds\n")
  print(con_mat)
  return(c(optCutOff,misClass,sens,spec,AuC))
}

sink_file <- "Covariate Logit Models.txt"
dir1 <- "~/Desktop/Hypertension study/Covariate Logit Model Results"
dir2 <- "~/Desktop/Hypertension study/Cleaned Data Files"
setwd(dir2)
#input cleaned NHANES data files for covariates
hypertension <- read.csv(file="Cleaned hypertension data.csv", header=TRUE)
BMI_data <- read.csv(file="Cleaned BMI data.csv", header=TRUE)[,c(1,11,31)]
BP_question_data <- read.csv(file="Cleaned BP Questionaire data.csv", header=TRUE)
continine_data <- read.csv(file="Cleaned continine data.csv", header=TRUE)
creatinine_data <- read.csv(file="Cleaned creatinine data.csv", header=TRUE)
demographic <- read.csv(file="Cleaned demographic data.csv", header=TRUE)
alcohol_data <- read.csv(file="Cleaned alcohol data.csv", header=TRUE)

setwd(dir1)
sink(file=sink_file)
base_model10 <- merge(hypertension, demographic, by.x="SEQN", by.y="SEQN", all=FALSE)
base_model11 <- merge(base_model10, BMI_data, by.x="SEQN", by.y="SEQN", all=FALSE)
base_model12 <- merge(base_model11, continine_data, by.x="SEQN", by.y="SEQN", all=FALSE)
model_data <- merge(base_model12, alcohol_data, by.x="SEQN", by.y="SEQN", all=FALSE)

#declare variables as factors as needed
#demographic factors
model_data$RIAGENDR <- as.factor(model_data$RIAGENDR)
model_data$RIDRETH3 <- as.factor(model_data$RIDRETH3)
model_data$DMDEDUC2 <- as.factor(model_data$DMDEDUC2)
model_data$DMDMARTL <- as.factor(model_data$DMDMARTL)
model_data$age_cluster <- as.factor(model_data$age_cluster)
model_data$FIPR_cluster <- as.factor(model_data$FIPR_cluster)

#body mass index data
model_data$BMI_category <- as.factor(model_data$BMI_category)

#cotinine
model_data$LBDCOTLC <- as.factor(model_data$LBDCOTLC)

#alcohol consumption
model_data$ALQ101 <- as.factor(model_data$ALQ101)

# Create a survey design object
base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                         weights=~WTINT2YR, nest=TRUE)

#create main-effects model
logit_model1 <- svyglm(hypertension ~ 
                 RIAGENDR + RIDRETH3 + DMDEDUC2 + DMDMARTL + 
                 age_cluster + FIPR_cluster + 
                 BMI_category + LBDCOTLC + ALQ101,
               family = quasibinomial(),
               data   = model_data,
               design = base_survey)
predicted1 <- print_summary(logit_model1, model_data, 1)

logit_model2 <- svyglm(hypertension~ 
                        RIAGENDR + RIDRETH3 + DMDEDUC2 + DMDMARTL + 
                        age_cluster + FIPR_cluster + 
                        BMI_category + LBDCOTLC + ALQ101 +
                        RIAGENDR*RIDRETH3 +        #Gender × Race/Ethnicity
                        RIAGENDR*DMDEDUC2 +        #Gender × Education
                        RIAGENDR*DMDMARTL +        #Gender × Marital status
                        RIAGENDR*age_cluster +     #Gender × Age
                        RIAGENDR*FIPR_cluster +    #Gender x income
                        RIAGENDR*BMI_category +    #Gender × BMI
                        RIAGENDR*LBDCOTLC +        #Gender x cotinine concentration
                        RIAGENDR*ALQ101 +          #Gender × Alcohol consumption
                        
                        RIDRETH3*DMDEDUC2 +        #Race/Ethnicity × Education
                        RIDRETH3*DMDMARTL +        #Race/Ethnicity × Marital status
                        RIDRETH3*age_cluster +     #Race/Ethnicity × Age
                        RIDRETH3*FIPR_cluster +    #Race/Ethnicity x income
                        RIDRETH3*BMI_category +    #Race/Ethnicity × BMI
                        RIDRETH3*LBDCOTLC +        #Race/Ethnicity x cotinine concentration
                        RIDRETH3*ALQ101 +          #Race/Ethnicity × Alcohol consumption
                        
                        DMDEDUC2*DMDMARTL +        #Education × Marital status
                        DMDEDUC2*age_cluster +     #Education × Age
                        DMDEDUC2*FIPR_cluster +    #Education x income
                        DMDEDUC2*BMI_category +    #Education × BMI
                        DMDEDUC2*LBDCOTLC +        #Education x cotinine concentration
                        DMDEDUC2*ALQ101 +          #Education × Alcohol consumption
                        
                        DMDMARTL*age_cluster +     #Marital status × Age
                        DMDMARTL*FIPR_cluster +    #Marital status x income
                        DMDMARTL*BMI_category +    #Marital status × BMI
                        DMDMARTL*LBDCOTLC +        #Marital status x cotinine concentration
                        DMDMARTL*ALQ101 +          #Marital status × Alcohol consumption
                        
                        age_cluster*FIPR_cluster +    #Age x income
                        age_cluster*BMI_category +    #Age × BMI
                        age_cluster*LBDCOTLC +        #Age x cotinine concentration
                        age_cluster*ALQ101 +          #Age × Alcohol consumption
                        
                        FIPR_cluster*BMI_category +    #Income × BMI
                        FIPR_cluster*LBDCOTLC +        #Income x cotinine concentration
                        FIPR_cluster*ALQ101 +          #Income × Alcohol consumption
                        
                        BMI_category*LBDCOTLC +        #BMI x cotinine concentration
                        BMI_category*ALQ101 +          #BMI × Alcohol consumption
                        
                        LBDCOTLC*ALQ101,          #cotinine × Alcohol consumption
                        
                      family = quasibinomial(),
                      data   = model_data,
                      design = base_survey)
predicted2 <- print_summary(logit_model2, model_data, 2)

#plot ROC curves
abc1 <- plotROC(model_data$hypertension, predicted1, returnSensitivityMat = TRUE)
abc2 <- plotROC(model_data$hypertension, predicted2, returnSensitivityMat = TRUE)
line_col <- viridis(3)
lines2D(x=abc1[,1],y=abc1[,2], ylab="Sensitivity (True Positive Rate)",
        xlab="1-Specificity (False Positive Rate)",
        col=line_col[1], lwd=2, add=FALSE)
lines2D(x=abc2[,1],y=abc2[,2], col=line_col[2],lwd=2, add=TRUE)
legend(x="bottomright",
       legend=c("Main-effects Only Model",
                "Two-factor Interaction Model"),
       cex=0.8, col=line_col, lty=1, lwd=2)

sink()