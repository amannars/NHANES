library(dplyr)
library(survey)
library(viridis)
library(aod)
library(InformationValue)

print_summary <- function(base_model, model_data, n_model){
  print(data.frame(coef(base_model), confint(base_model)))
  base_confint <- confint(base_model)
  predicted <- plogis(predict(base_model, model_data))  # predicted scores
  optCutOff <- optimalCutoff(model_data$hypertension, predicted)[1]
  df_metric <- model_validation(model_data, predicted, optCutOff, model="Base Model",n_model)
  return(predicted)
}

build_model_data <- function(base_model, response){
  model_data <- merge(covariate_data, response, 
                      by.x="SEQN", by.y="SEQN", all=FALSE)
  keep <- !is.na(model_data[,ncol(model_data)])
  model_data <- model_data[keep,]
  #declare variables as factors as needed
  #demographic factors
  model_data$RIAGENDR <- as.factor(model_data$RIAGENDR)
  model_data$RIDRETH3 <- as.factor(model_data$RIDRETH3)
  model_data$DMDEDUC2 <- as.factor(model_data$DMDEDUC2)
  model_data$DMDMARTL <- as.factor(model_data$DMDMARTL)
  model_data$age_cluster <- as.factor(model_data$age_cluster)
  model_data$FIPR_cluster <- as.factor(model_data$FIPR_cluster)
  
  #body mass index
  model_data$BMI_category <- as.factor(model_data$BMI_category)
  
  #cotinine
  model_data$LBDCOTLC <- as.factor(model_data$LBDCOTLC)
  
  #alcohol consumption
  model_data$ALQ101 <- as.factor(model_data$ALQ101)
  return(model_data)
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

sink_file <- "Arsenic and Metals Logit A.txt"
dir1 <- "~/Desktop/Hypertension study/Variate Logit Model Results" 
dir2 <- "~/Desktop/Hypertension study/Cleaned Data Files"

#input cleaned NHANES data files
setwd(dir2)
hypertension <- read.csv(file="Cleaned hypertension data.csv", header=TRUE)
BMI_data <- read.csv(file="Cleaned BMI data.csv", header=TRUE)[,c(1,11,31)]
BP_question_data <- read.csv(file="Cleaned BP Questionaire data.csv", header=TRUE)
continine_data <- read.csv(file="Cleaned continine data.csv", header=TRUE)
creatinine_data <- read.csv(file="Cleaned creatinine data.csv", header=TRUE)
demographic <- read.csv(file="Cleaned demographic data.csv", header=TRUE)
alcohol_data <- read.csv(file="Cleaned alcohol data.csv", header=TRUE)

base_model10 <- merge(hypertension, demographic, by.x="SEQN", by.y="SEQN", all=FALSE)
base_model11 <- merge(base_model10, BMI_data, by.x="SEQN", by.y="SEQN", all=FALSE)
base_model12 <- merge(base_model11, continine_data, by.x="SEQN", by.y="SEQN", all=FALSE)
covariate_data <- merge(base_model12, alcohol_data, by.x="SEQN", by.y="SEQN", all=FALSE)

#read arsenic data
arsenic <- read.csv(file="Cleaned arsenic data.csv", header=TRUE)[,c(1,2,30:35)]

#read total arsenic
TA_data <- read.csv(file="Cleaned total arsenic data.csv", header=TRUE)[,c(1,2,15)]

#read heavy metals
metal_data <- read.csv(file="Cleaned heavy metals data.csv", header=TRUE)
md_col <- ncol(metal_data)
metal_data <- metal_data[,c(1,2,(md_col-12):md_col)]

#read mercury
Hg_data <- read.csv(file="Cleaned Hg data.csv", header=TRUE)
Hg_data <- Hg_data[,c(1,2,7)]

setwd(dir1)
sink(file=sink_file)

for (i in c(0,3,5:21)){  #c(1,3,5:21)
  print(paste("i=",i))
  if (i == 0){
    model_data <- covariate_data
    #demographic factors
    model_data$RIAGENDR <- as.factor(model_data$RIAGENDR)
    model_data$RIDRETH3 <- as.factor(model_data$RIDRETH3)
    model_data$DMDEDUC2 <- as.factor(model_data$DMDEDUC2)
    model_data$DMDMARTL <- as.factor(model_data$DMDMARTL)
    model_data$age_cluster <- as.factor(model_data$age_cluster)
    model_data$FIPR_cluster <- as.factor(model_data$FIPR_cluster)
    
    #body mass index
    model_data$BMI_category <- as.factor(model_data$BMI_category)
    
    #cotinine
    model_data$LBDCOTLC <- as.factor(model_data$LBDCOTLC)
    
    #alcohol consumption
    model_data$ALQ101 <- as.factor(model_data$ALQ101)

    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTINT2YR, nest=TRUE)
    logit_model <- svyglm(hypertension ~
                            RIAGENDR + RIDRETH3 + DMDEDUC2 + DMDMARTL + 
                            age_cluster + FIPR_cluster + 
                            
                            #RIAGENDR*RIDRETH3 + #gender x race
                            #RIAGENDR*DMDEDUC2 + #gender x education
                            #RIAGENDR*DMDMARTL + #gender x marital status
                            #RIAGENDR*BMI_category + #gender x BMI
                            
                            #RIAGENDR*RIDRETH3 + #gender x race
                            #RIAGENDR*age_cluster + #gender X age
                            #RIDRETH3*RIDRETH3 + #race X FIPR
                            #DMDEDUC2*age_cluster + #education X age
                            
                            BMI_category + LBDCOTLC + ALQ101,
                          family = quasibinomial(),
                          data   = model_data,
                          design = base_survey)
  }
  
  else {if (i == 1){
    response <- arsenic[,c(1,2,3)]
    col_names <- colnames(response)
    print(paste("Response factor = ",col_names[3]))
    col_names[3] <- "Response"
    colnames(response) <- col_names
    model_data <- build_model_data(base_model=covariate_data, response=response)
    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTSA2YR, nest=TRUE)
  } else if (i == 2){
    response <- arsenic[,c(1,2,4)]
    col_names <- colnames(response)
    print(paste("Response factor = ",col_names[3]))
    col_names[3] <- "Response"
    colnames(response) <- col_names
    model_data <- build_model_data(base_model=covariate_data,response=response)
    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTSA2YR, nest=TRUE)
  } else if (i == 3){
    response <- arsenic[,c(1,2,5)]
    col_names <- colnames(response)
    print(paste("Response factor = ",col_names[3]))
    col_names[3] <- "Response"
    colnames(response) <- col_names
    model_data <- build_model_data(base_model=covariate_data,response=response)
    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTSA2YR, nest=TRUE)
  } else if (i == 4){
    response <- arsenic[,c(1,2,6)]
    col_names <- colnames(response)
    print(paste("Response factor = ",col_names[3]))
    col_names[3] <- "Response"
    colnames(response) <- col_names
    model_data <- build_model_data(base_model=covariate_data,response=response)
    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTSA2YR, nest=TRUE)
  } else if (i == 5){
    response <- arsenic[,c(1,2,7)]
    col_names <- colnames(response)
    print(paste("Response factor = ",col_names[3]))
    col_names[3] <- "Response"
    colnames(response) <- col_names
    model_data <- build_model_data(base_model=covariate_data,response=response)
    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTSA2YR, nest=TRUE)
  } else if (i == 6){
    response <- arsenic[,c(1,2,8)]
    col_names <- colnames(response)
    print(paste("Response factor = ",col_names[3]))
    col_names[3] <- "Response"
    colnames(response) <- col_names
    model_data <- build_model_data(base_model=covariate_data,response=response)
    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTSA2YR, nest=TRUE)
  } else if (i == 7){
    response <- TA_data[,c(1,2,3)]
    col_names <- colnames(response)
    print(paste("Response factor = ",col_names[3]))
    col_names[3] <- "Response"
    colnames(response) <- col_names
    model_data <- build_model_data(base_model=covariate_data,response=response)
    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTSA2YR, nest=TRUE)
  } else if (i == 8){
    response <- Hg_data[,c(1,2,3)]
    col_names <- colnames(response)
    print(paste("Response factor = ",col_names[3]))
    col_names[3] <- "Response"
    colnames(response) <- col_names
    model_data <- build_model_data(base_model=covariate_data,response=response)
    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTSA2YR, nest=TRUE)
  } else if (i == 9){
    response <- metal_data[,c(1,2,3)]
    col_names <- colnames(response)
    print(paste("Response factor = ",col_names[3]))
    col_names[3] <- "Response"
    colnames(response) <- col_names
    model_data <- build_model_data(base_model=covariate_data,response=response)
    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTFSM, nest=TRUE)
  } else if (i == 10){
    response <- metal_data[,c(1,2,4)]
    col_names <- colnames(response)
    print(paste("Response factor = ",col_names[3]))
    col_names[3] <- "Response"
    colnames(response) <- col_names
    model_data <- build_model_data(base_model=covariate_data,response=response)
    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTFSM, nest=TRUE)
  } else if (i == 11){
    response <- metal_data[,c(1,2,5)]
    col_names <- colnames(response)
    print(paste("Response factor = ",col_names[3]))
    col_names[3] <- "Response"
    colnames(response) <- col_names
    model_data <- build_model_data(base_model=covariate_data,response=response)
    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTFSM, nest=TRUE)
  } else if (i == 12){
    response <- metal_data[,c(1,2,6)]
    col_names <- colnames(response)
    print(paste("Response factor = ",col_names[3]))
    col_names[3] <- "Response"
    colnames(response) <- col_names
    model_data <- build_model_data(base_model=covariate_data,response=response)
    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTFSM, nest=TRUE)
  } else if (i == 13){
    response <- metal_data[,c(1,2,7)]
    col_names <- colnames(response)
    print(paste("Response factor = ",col_names[3]))
    col_names[3] <- "Response"
    colnames(response) <- col_names
    model_data <- build_model_data(base_model=covariate_data,response=response)
    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTFSM, nest=TRUE)
  } else if (i == 14){
    response <- metal_data[,c(1,2,8)]
    col_names <- colnames(response)
    print(paste("Response factor = ",col_names[3]))
    col_names[3] <- "Response"
    colnames(response) <- col_names
    model_data <- build_model_data(base_model=covariate_data,response=response)
    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTFSM, nest=TRUE)
  } else if (i == 15){
    response <- metal_data[,c(1,2,9)]
    col_names <- colnames(response)
    print(paste("Response factor = ",col_names[3]))
    col_names[3] <- "Response"
    colnames(response) <- col_names
    model_data <- build_model_data(base_model=covariate_data,response=response)
    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTFSM, nest=TRUE)
  } else if (i == 16){
    response <- metal_data[,c(1,2,10)]
    col_names <- colnames(response)
    print(paste("Response factor = ",col_names[3]))
    col_names[3] <- "Response"
    colnames(response) <- col_names
    model_data <- build_model_data(base_model=covariate_data,response=response)
    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTFSM, nest=TRUE)
  } else if (i == 17){
    response <- metal_data[,c(1,2,11)]
    col_names <- colnames(response)
    print(paste("Response factor = ",col_names[3]))
    col_names[3] <- "Response"
    colnames(response) <- col_names
    model_data <- build_model_data(base_model=covariate_data,response=response)
    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTFSM, nest=TRUE)
  } else if (i == 18){
    response <- metal_data[,c(1,2,12)]
    col_names <- colnames(response)
    print(paste("Response factor = ",col_names[3]))
    col_names[3] <- "Response"
    colnames(response) <- col_names
    model_data <- build_model_data(base_model=covariate_data,response=response)
    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTFSM, nest=TRUE)
  } else if (i == 19){
    response <- metal_data[,c(1,2,13)]
    col_names <- colnames(response)
    print(paste("Response factor = ",col_names[3]))
    col_names[3] <- "Response"
    colnames(response) <- col_names
    model_data <- build_model_data(base_model=covariate_data,response=response)
    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTFSM, nest=TRUE)
  } else if (i == 20){
    response <- metal_data[,c(1,2,14)]
    col_names <- colnames(response)
    print(paste("Response factor = ",col_names[3]))
    col_names[3] <- "Response"
    colnames(response) <- col_names
    model_data <- build_model_data(base_model=covariate_data,response=response)
    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTFSM, nest=TRUE)
  } else if (i == 21){
    response <- metal_data[,c(1,2,15)]
    col_names <- colnames(response)
    print(paste("Response factor = ",col_names[3]))
    col_names[3] <- "Response"
    colnames(response) <- col_names
    model_data <- build_model_data(base_model=covariate_data,response=response)
    base_survey <- svydesign(data=model_data, id=~SDMVPSU, strata=~SDMVSTRA, 
                             weights=~WTFSM, nest=TRUE)
  }
  logit_model <- svyglm(hypertension ~ 
                          log10(Response) + #Uranium with weighting WTFSM
                          RIAGENDR + RIDRETH3 + DMDEDUC2 + DMDMARTL + 
                          age_cluster + FIPR_cluster + 
                          
                          #RIAGENDR*RIDRETH3 + #gender x race
                          #RIAGENDR*DMDEDUC2 + #gender x education
                          #RIAGENDR*DMDMARTL + #gender x marital status
                          #RIAGENDR*BMI_category + #gender x BMI
                          
                          #RIAGENDR*RIDRETH3 + #gender x race
                          #RIAGENDR*age_cluster + #gender X age
                          #RIDRETH3*RIDRETH3 + #race X FIPR
                          #DMDEDUC2*age_cluster + #education X age
                          
                          BMI_category + LBDCOTLC + ALQ101,
                        family = quasibinomial(),
                        data   = model_data,
                        design = base_survey)
  predicted <- print_summary(logit_model, model_data, n_model=i)}
}

sink()