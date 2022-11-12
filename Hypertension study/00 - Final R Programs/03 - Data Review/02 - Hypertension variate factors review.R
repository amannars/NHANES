library(dplyr)
library(survey)
library(viridis)

variate_plot <- function(covariate_data, SEQN, test_data, main_title){
  test_data <- cbind(SEQN, test_data)
  colnames(test_data) <- c("SEQN", "Data")
  keep <- !is.na(test_data[,2])
  test_data <- test_data[keep,]
  test_data[,2] <- log10(test_data[,2])
  model_data <- merge(covariate_data, test_data, 
                      by.x="SEQN", by.y="SEQN", all=FALSE)
  hist_plot <- model_data$Data
  hist_data <- hist(hist_plot, plot=FALSE)
  a_hyper <- matrix(NA, nrow=1, ncol=(length(hist_data$counts)))
  for (i in 1:length(hist_data$counts)){
    keep <- (model_data$Data >= hist_data$breaks[i]) & 
      (model_data$Data < hist_data$breaks[i+1])
    a_hyper[i] <- sum(model_data$hypertension[keep] == 1)
  }
  a_plot <- rbind(a_hyper,hist_data$counts-a_hyper)
  barplot(hist_data$counts,
          xlab=expression("log"[10]*"(Concentration)"),
          ylab="Total Count", main=main_title, col=viridis(3)[2])
  yaxis <- par()$yaxp
  ydown <- -0.07*(yaxis[2]-yaxis[1])
  text(x=0.4+1.19*(0:(length(hist_data$breaks)-1)), y=ydown, 
       labels=format(hist_data$breaks,digits=2, nsmall=1), #las=1,
       adj=c(0.5,0), cex=1.0, xpd = NA, srt=90)
  barplot(a_hyper,
          xlab=expression("log"[10]*"(Concentration)"),
          ylab="Count with HBP", main=main_title, col=viridis(3)[1])
  yaxis <- par()$yaxp
  ydown <- -0.07*(yaxis[2]-yaxis[1])
  text(x=0.4+1.19*(0:(length(hist_data$breaks)-1)), y=ydown, 
       labels=format(hist_data$breaks,digits=2, nsmall=1), #las=1,
       adj=c(0.5,0), cex=1.0, xpd = NA, srt=90)
  divisor <- hist_data$counts
  change <- (divisor == 0)
  divisor[change] <- 1
  percent_hyper <- 100*a_hyper/divisor
  barplot(percent_hyper,
          xlab=expression("log"[10]*"(Concentration)"),
          ylab="Percent with HBP", main=main_title)
  yaxis <- par()$yaxp
  ydown <- -0.07*(yaxis[2]-yaxis[1])
  text(x=0.4+1.19*(0:(length(hist_data$breaks)-1)), y=ydown, 
       labels=format(hist_data$breaks,digits=2, nsmall=1), #las=1,
       adj=c(0.5,0), cex=1.0, xpd = NA, srt=90)
  barplot(a_plot, col=viridis(3)[c(1,3)],
          xlab=expression("log"[10]*"(Concentration)"),
          ylab="Count", main=main_title)
  yaxis <- par()$yaxp
  ydown <- -0.07*(yaxis[2]-yaxis[1])
  text(x=0.4+1.19*(0:(length(hist_data$breaks)-1)), y=ydown, 
       labels=format(hist_data$breaks,digits=2, nsmall=1), #las=1,
       adj=c(0.5,0), cex=1.0, xpd = NA, srt=90)
  return(a_plot)
}

dir1 <- "~/Desktop/Hypertension study/Cleaned Data Files"

#input cleaned NHANES data files
setwd(dir1)
file1 <- "Cleaned arsenic data.csv"
file2 <- "Cleaned hypertension data.csv"
file3 <- "Cleaned BMI data.csv"
file4 <- "Cleaned BP Questionaire data.csv"
file5 <- "Cleaned continine data.csv"
file6 <- "Cleaned creatinine data.csv"
file7 <- "Cleaned demographic data.csv"
file8 <- "Cleaned total arsenic data.csv"
file9 <- "Cleaned heavy metals data.csv"
file10 <- "Cleaned Hg data.csv"

arsenic_data <- read.csv(file=file1, header=TRUE)
hypertension <- read.csv(file=file2, header=TRUE)
BMI_data <- read.csv(file=file3, header=TRUE)
BP_question_data <- read.csv(file=file4, header=TRUE)
continine_data <- read.csv(file=file5, header=TRUE)
creatinine_data <- read.csv(file=file6, header=TRUE)
demographic <- read.csv(file=file7, header=TRUE)
TA_data <- read.csv(file=file8, header=TRUE)
metal_data <- read.csv(file=file9, header=TRUE)
Hg_data <- read.csv(file=file10, header=TRUE)

#base model for hypertension has covariates:
#  demographic data
#  BMI data
#  continine data
base_model0 <- merge(hypertension, demographic, by.x="SEQN", by.y="SEQN", all=FALSE)
base_model1 <- merge(base_model0, BMI_data, by.x="SEQN", by.y="SEQN", all=FALSE)
model_data <- merge(base_model1, continine_data, by.x="SEQN", by.y="SEQN", all=FALSE)

#declare variables as factors as needed
model_data$hypertension <- model_data$hypertension
model_data$RIAGENDR <- as.factor(model_data$RIAGENDR)
model_data$RIDRETH3 <- as.factor(model_data$RIDRETH3)
model_data$DMDEDUC2 <- as.factor(model_data$DMDEDUC2)
model_data$DMDMARTL <- as.factor(model_data$DMDMARTL)
model_data$age_cluster <- as.factor(model_data$age_cluster)
model_data$FIPR_cluster <- as.factor(model_data$FIPR_cluster)
model_data$BMI_category <- as.factor(model_data$BMI_category)
model_data$LBDCOTLC <- as.factor(model_data$LBDCOTLC)

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

#Arsenic
variate_plot(model_data, arsenic_data$SEQN, (arsenic_data$AS3norm),"Urinary Arsenous Acid")
variate_plot(model_data, arsenic_data$SEQN, (arsenic_data$AS5norm),"Urinary Arsenic Acid")
variate_plot(model_data, arsenic_data$SEQN, (arsenic_data$ABnorm),"Urinary Arsenobetaine")
variate_plot(model_data, arsenic_data$SEQN, (arsenic_data$ACnorm),"Urinary Arsenocholine")
variate_plot(model_data, arsenic_data$SEQN, (arsenic_data$DMAnorm),"Urinary Dimethylarsinic Acid")
variate_plot(model_data, arsenic_data$SEQN, (arsenic_data$MMAnorm),"Urinary Monomethylarsonic Acid")

#Total Arsenic
variate_plot(model_data, TA_data$SEQN, (TA_data$URXUAS),"Total Arsenic")

#Heavy metals
variate_plot(model_data, metal_data$SEQN, (metal_data$BAnorm),"Barium")     #OK
variate_plot(model_data, metal_data$SEQN, (metal_data$CDnorm),"Cadimum")    #OK
variate_plot(model_data, metal_data$SEQN, (metal_data$COnorm),"Cobalt")     #OK
variate_plot(model_data, metal_data$SEQN, (metal_data$CSnorm),"Cesium")     #OK
variate_plot(model_data, metal_data$SEQN, (metal_data$MOnorm),"Molybdenum") #OK
variate_plot(model_data, metal_data$SEQN, (metal_data$MNnorm),"Manganese")  #OK
variate_plot(model_data, metal_data$SEQN, (metal_data$PBnorm),"Lead")       #OK
variate_plot(model_data, metal_data$SEQN, (metal_data$SBnorm),"Antimony")   #OK
variate_plot(model_data, metal_data$SEQN, (metal_data$SNnorm),"Tin")        #OK
variate_plot(model_data, metal_data$SEQN, (metal_data$SRnorm),"Strontium")  #OK
variate_plot(model_data, metal_data$SEQN, (metal_data$TLnorm),"Thallium")   #OK
variate_plot(model_data, metal_data$SEQN, (metal_data$TUnorm),"Tungsten")   #OK
variate_plot(model_data, metal_data$SEQN, (metal_data$URnorm),"Uranium")    #OK

#Total mercury
variate_plot(model_data, Hg_data$SEQN, (Hg_data$URXUHG),"Mercury")