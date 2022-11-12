covariate_analysis <- function(HNAHES_base, main_title){
  #Calculate prevalence of hypertension - by demographic covariates
  cat("\n\nBy Sex\n")
  abc <- getSummary(~hypertension, ~RIAGENDR, NHANES_base)    #by sex
  print(abc)
  df <- rbind(abc$hypertension, 1-abc$hypertension)
  per_cent <- abc$counts/sum(abc$counts)*100
  pdf(file=paste(main_title,"01A.pdf"), width=5, height=4.5)
  barplot(per_cent,col="black",
          ylab="Unweighted Percentage", ylim=c(0,60),
          xlab="Gender", 
          names.arg=c("Male","Female"), cex.names=0.65, las=1,
          main=main_title)
  dev.off()
  pdf(file=paste(main_title,"01B.pdf"), width=5, height=4.5)
  barplot(df,col=viridis(2),
          ylab="Fraction with High BP (purple)",
          xlab="Gender", names.arg=c("Male","Female"), cex.names=0.65, las=1,
          main=main_title)
  text(x=c(0.7,1.9), y=rep(1.01,times=length(abc$counts)), 
       labels=paste("N=",abc$count,sep=""),
       adj=c(0.5,0), cex=0.8, xpd = NA)
  dev.off()
  print(svyttest(hypertension~RIAGENDR, NHANES_base)$p.value %>% as.numeric)
  
  cat("\n\nBy Race/Ethnicity\n")
  abc <- getSummary(~hypertension, ~RIDRETH3, NHANES_base)     #by race/ethnicity
  print(abc)
  df <- rbind(abc$hypertension, 1-abc$hypertension)
  per_cent <- abc$counts/sum(abc$counts)*100
  pdf(file=paste(main_title,"02A.pdf"), width=5, height=4.5)
  barplot(per_cent,col="black",
          ylab="Unweighted Percentage", ylim=c(0,40),
          xlab="Race/Ethnicity", 
          names.arg=c("Mexican\nAmerican",
                      "Other\nHispanic",
                      "Non-Hispanic\nWhite",
                      "Non-Hispanic\nBlack",
                      "Non-Hispanic\nAsian",
                      "Other Race\nIncluding\nMulti-Racial"), cex.names=0.65, las=1,
          main=main_title)
  dev.off()
  
  pdf(file=paste(main_title,"02B.pdf"), width=5, height=4.5)
  barplot(df,col=viridis(2),
          ylab="Fraction with High BP (purple)",
          xlab="Race/Ethnicity", 
          names.arg=c("Mexican\nAmerican",
                      "Other\nHispanic",
                      "Non-Hispanic\nWhite",
                      "Non-Hispanic\nBlack",
                      "Non-Hispanic\nAsian",
                      "Other Race\nIncluding\nMulti-Racial"), cex.names=0.65, las=1,
          main=main_title)
  text(x=c(0.7,1.9,3.1,4.3,5.5,6.7), y=rep(1.01,times=length(abc$counts)), 
       labels=paste("N=",abc$count,sep=""),
       adj=c(0.5,0), cex=0.8, xpd = NA)
  dev.off()
  print(svyttest(hypertension~RIDRETH3, NHANES_base)$p.value %>% as.numeric)
  cat("\n\nPairwise comparisons")
  def <- svyttest(hypertension~RIDRETH3, subset(NHANES_base, RIDRETH3 == 1 | RIDRETH3 == 2))
  cat("\n",def$p.value %>% as.numeric,"\tMexican American & Other Hispanic")
  def <- svyttest(hypertension~RIDRETH3, subset(NHANES_base, RIDRETH3 == 1 | RIDRETH3 == 3))
  cat("\n",def$p.value %>% as.numeric,"\tMexican American & Non-Hispanic White")
  def <- svyttest(hypertension~RIDRETH3, subset(NHANES_base, RIDRETH3 == 1 | RIDRETH3 == 4))
  cat("\n",def$p.value %>% as.numeric,"\tMexican American & Non-Hispanic Black")
  def <- svyttest(hypertension~RIDRETH3, subset(NHANES_base, RIDRETH3 == 1 | RIDRETH3 == 6))
  cat("\n",def$p.value %>% as.numeric,"\tMexican American & Non-Hispanic Asian")
  def <- svyttest(hypertension~RIDRETH3, subset(NHANES_base, RIDRETH3 == 1 | RIDRETH3 == 7))
  cat("\n",def$p.value %>% as.numeric,"\tMexican American & Other")
  
  def <- svyttest(hypertension~RIDRETH3, subset(NHANES_base, RIDRETH3 == 2 | RIDRETH3 == 3))
  cat("\n",def$p.value %>% as.numeric,"  Other Hispanic & Non-Hispanic White")
  def <- svyttest(hypertension~RIDRETH3, subset(NHANES_base, RIDRETH3 == 2 | RIDRETH3 == 4))
  cat("\n",def$p.value %>% as.numeric,"  Other Hispanic & Non-Hispanic Black")
  def <- svyttest(hypertension~RIDRETH3, subset(NHANES_base, RIDRETH3 == 2 | RIDRETH3 == 6))
  cat("\n",def$p.value %>% as.numeric,"  Other Hispanic & Non-Hispanic Asian")
  def <- svyttest(hypertension~RIDRETH3, subset(NHANES_base, RIDRETH3 == 2 | RIDRETH3 == 7))
  cat("\n",def$p.value %>% as.numeric,"  Other Hispanic & Other")
  
  def <- svyttest(hypertension~RIDRETH3, subset(NHANES_base, RIDRETH3 == 3 | RIDRETH3 == 4))
  cat("\n",def$p.value %>% as.numeric,"  Non-Hispanic White & Non-Hispanic Black")
  def <- svyttest(hypertension~RIDRETH3, subset(NHANES_base, RIDRETH3 == 3 | RIDRETH3 == 6))
  cat("\n",def$p.value %>% as.numeric,"  Non-Hispanic White & Non-Hispanic Asian")
  def <- svyttest(hypertension~RIDRETH3, subset(NHANES_base, RIDRETH3 == 3 | RIDRETH3 == 7))
  cat("\n",def$p.value %>% as.numeric,"  Non-Hispanic White & Other")
  
  def <- svyttest(hypertension~RIDRETH3, subset(NHANES_base, RIDRETH3 == 4 | RIDRETH3 == 6))
  cat("\n",def$p.value %>% as.numeric,"  Non-Hispanic Black & Non-Hispanic Asian")
  def <- svyttest(hypertension~RIDRETH3, subset(NHANES_base, RIDRETH3 == 4 | RIDRETH3 == 7))
  cat("\n",def$p.value %>% as.numeric,"  Non-Hispanic Black & Other")
  
  def <- svyttest(hypertension~RIDRETH3, subset(NHANES_base, RIDRETH3 == 6 | RIDRETH3 == 7))
  cat("\n",def$p.value %>% as.numeric,"  Non-Hispanic Asian & Other")
  
  cat("\n\nBy Education\n")
  abc <- getSummary(~hypertension, ~DMDEDUC2, NHANES_base)     #by education
  print(abc)
  df <- rbind(abc$hypertension, 1-abc$hypertension)
  per_cent <- abc$counts/sum(abc$counts)*100
  pdf(file=paste(main_title,"03A.pdf"), width=5, height=4.5)
  barplot(per_cent,col="black",
          ylab="Unweighted Percentage", ylim=c(0,35),
          xlab="Highest Education Level", 
          names.arg=c("Less than\n9th grade",
                      "Some 9-12th\ngrade",
                      "High school\ngraduate/GED",
                      "Some college\nor AA degree",
                      "College grad\nor above"), cex.names=0.65, las=1,
          main=main_title)
  dev.off()
  pdf(file=paste(main_title,"03B.pdf"), width=5, height=4.5)
  barplot(df,col=viridis(2),
          ylab="Fraction with High BP (purple)",
          xlab="Highest Education Level", 
          names.arg=c("Less than\n9th grade",
                      "Some 9-12th\ngrade",
                      "High school\ngraduate/GED",
                      "Some college\nor AA degree",
                      "College grad\nor above"),cex.names=0.65, las=1,
          main=main_title)
  text(x=c(0.7,1.9,3.1,4.3,5.5), y=rep(1.01,times=length(abc$counts)), 
       labels=paste("N=",abc$count,sep=""),
       adj=c(0.5,0), cex=0.8, xpd = NA)
  dev.off()
  print(svyttest(hypertension~DMDEDUC2, NHANES_base)$p.value %>% as.numeric)
  def <- svyttest(hypertension~DMDEDUC2, subset(NHANES_base, DMDEDUC2 == 1 | DMDEDUC2 == 2))
  cat("\n",def$p.value %>% as.numeric,"  Less than 9th grade & Some 9-12th grade")
  def <- svyttest(hypertension~DMDEDUC2, subset(NHANES_base, DMDEDUC2 == 1 | DMDEDUC2 == 3))
  cat("\n",def$p.value %>% as.numeric,"  Less than 9th grad & High school graduate/GED")
  def <- svyttest(hypertension~DMDEDUC2, subset(NHANES_base, DMDEDUC2 == 1 | DMDEDUC2 == 4))
  cat("\n",def$p.value %>% as.numeric,"  Less than 9th grad & Some college or AA degree")
  def <- svyttest(hypertension~DMDEDUC2, subset(NHANES_base, DMDEDUC2 == 1 | DMDEDUC2 == 5))
  cat("\n",def$p.value %>% as.numeric,"  Less than 9th grad & College graduate or above")
  
  def <- svyttest(hypertension~DMDEDUC2, subset(NHANES_base, DMDEDUC2 == 2 | DMDEDUC2 == 3))
  cat("\n",def$p.value %>% as.numeric,"  Some 9-12th grade & High school graduate/GED")
  def <- svyttest(hypertension~DMDEDUC2, subset(NHANES_base, DMDEDUC2 == 2 | DMDEDUC2 == 4))
  cat("\n",def$p.value %>% as.numeric,"  Some 9-12th grade & Some college or AA degree")
  def <- svyttest(hypertension~DMDEDUC2, subset(NHANES_base, DMDEDUC2 == 2 | DMDEDUC2 == 5))
  cat("\n",def$p.value %>% as.numeric,"  Some 9-12th grade & College graduate or above")
  
  def <- svyttest(hypertension~DMDEDUC2, subset(NHANES_base, DMDEDUC2 == 3 | DMDEDUC2 == 4))
  cat("\n",def$p.value %>% as.numeric,"  High school graduate/GED & Some college or AA degree")
  def <- svyttest(hypertension~DMDEDUC2, subset(NHANES_base, DMDEDUC2 == 3 | DMDEDUC2 == 5))
  cat("\n",def$p.value %>% as.numeric,"  High school graduate/GED & College graduate or above")
  
  def <- svyttest(hypertension~DMDEDUC2, subset(NHANES_base, DMDEDUC2 == 4 | DMDEDUC2 == 5))
  cat("\n",def$p.value %>% as.numeric,"  Some college or AA degree & College graduate or above")
  
  cat("\n\nBy Marital Status\n")
  abc <- getSummary(~hypertension, ~DMDMARTL, NHANES_base)     #by marital status
  print(abc)
  df <- rbind(abc$hypertension, 1-abc$hypertension)
  per_cent <- abc$counts/sum(abc$counts)*100
  pdf(file=paste(main_title,"05A.pdf"), width=5, height=4.5)
  barplot(per_cent,col="black",
          ylab="Unweighted Percentage", ylim=c(0,60),
          xlab="Marital Status", 
          names.arg=c("Married",
                      "Widowed",
                      "Divorced",
                      "Separated",
                      "Never\nmarried",
                      "Living with\npartner"), cex.names=0.65, las=1,
          main=main_title)
  dev.off()
  pdf(file=paste(main_title,"05B.pdf"), width=5, height=4.5)
  barplot(df,col=viridis(2),
          ylab="Fraction with High BP (purple)",
          xlab="Marital Status", 
          names.arg=c("Married",
                      "Widowed",
                      "Divorced",
                      "Separated",
                      "Never\nmarried",
                      "Living with\npartner"), cex.names=0.65, las=1,
          main=main_title)
  text(x=c(0.7,1.9,3.1,4.3,5.5,6.7), y=rep(1.01,times=length(abc$counts)), 
       labels=paste("N=",abc$count,sep=""),
       adj=c(0.5,0), cex=0.8, xpd = NA)
  dev.off()
  print(svyttest(hypertension~DMDMARTL, NHANES_base)$p.value %>% as.numeric)
  def <- svyttest(hypertension~DMDMARTL, subset(NHANES_base, DMDMARTL == 1 | DMDMARTL == 2))
  cat("\n",def$p.value %>% as.numeric,"  Married & Widowed")
  def <- svyttest(hypertension~DMDMARTL, subset(NHANES_base, DMDMARTL == 1 | DMDMARTL == 3))
  cat("\n",def$p.value %>% as.numeric,"  Married & Divorced")
  def <- svyttest(hypertension~DMDMARTL, subset(NHANES_base, DMDMARTL == 1 | DMDMARTL == 4))
  cat("\n",def$p.value %>% as.numeric,"  Married & Separated")
  def <- svyttest(hypertension~DMDMARTL, subset(NHANES_base, DMDMARTL == 1 | DMDMARTL == 5))
  cat("\n",def$p.value %>% as.numeric,"  Married & Never married")
  def <- svyttest(hypertension~DMDMARTL, subset(NHANES_base, DMDMARTL == 1 | DMDMARTL == 6))
  cat("\n",def$p.value %>% as.numeric,"  Married & Living with partner")
  
  def <- svyttest(hypertension~DMDMARTL, subset(NHANES_base, DMDMARTL == 2 | DMDMARTL == 3))
  cat("\n",def$p.value %>% as.numeric,"  Widowed & Divorced")
  def <- svyttest(hypertension~DMDMARTL, subset(NHANES_base, DMDMARTL == 2 | DMDMARTL == 4))
  cat("\n",def$p.value %>% as.numeric,"  Widowed & Separated")
  def <- svyttest(hypertension~DMDMARTL, subset(NHANES_base, DMDMARTL == 2 | DMDMARTL == 5))
  cat("\n",def$p.value %>% as.numeric,"  Widowed & Never married")
  def <- svyttest(hypertension~DMDMARTL, subset(NHANES_base, DMDMARTL == 2 | DMDMARTL == 6))
  cat("\n",def$p.value %>% as.numeric,"  Widowed & Living with partner")
  
  def <- svyttest(hypertension~DMDMARTL, subset(NHANES_base, DMDMARTL == 3 | DMDMARTL == 4))
  cat("\n",def$p.value %>% as.numeric,"  Divorced & Separated")
  def <- svyttest(hypertension~DMDMARTL, subset(NHANES_base, DMDMARTL == 3 | DMDMARTL == 5))
  cat("\n",def$p.value %>% as.numeric,"  Divorced & Never married")
  def <- svyttest(hypertension~DMDMARTL, subset(NHANES_base, DMDMARTL == 3 | DMDMARTL == 6))
  cat("\n",def$p.value %>% as.numeric,"  Divorced & Living with partner")
  
  def <- svyttest(hypertension~DMDMARTL, subset(NHANES_base, DMDMARTL == 4 | DMDMARTL == 5))
  cat("\n",def$p.value %>% as.numeric,"  Separated & Never married")
  def <- svyttest(hypertension~DMDMARTL, subset(NHANES_base, DMDMARTL == 4 | DMDMARTL == 6))
  cat("\n",def$p.value %>% as.numeric,"  Separated & Living with partner")
  
  def <- svyttest(hypertension~DMDMARTL, subset(NHANES_base, DMDMARTL == 5 | DMDMARTL == 6))
  cat("\n",def$p.value %>% as.numeric,"  Never married & Living with partner")
  
  cat("\n\nBy Age\n")
  abc <- getSummary(~hypertension, ~age_cluster, NHANES_base)  #by age
  print(abc)
  df <- rbind(abc$hypertension, 1-abc$hypertension)
  per_cent <- abc$counts/sum(abc$counts)*100
  pdf(file=paste(main_title,"06A.pdf"), width=5, height=4.5)
  barplot(per_cent,col="black",
          ylab="Unweighted Percentage", ylim=c(0,50),
          xlab="Age Category", 
          names.arg=c("20 - 39",
                      "40 - 64",
                      "65 and\nabove"), cex.names=0.65, las=1,
          main=main_title)
  dev.off()
  pdf(file=paste(main_title,"06B.pdf"), width=5, height=4.5)
  barplot(df,col=viridis(2),
          ylab="Fraction with High BP (purple)",
          xlab="Age Category", 
          names.arg=c("20 - 39",
                      "40 - 64",
                      "65 and\nabove"), cex.names=0.65, las=1,
          main=main_title)
  text(x=c(0.7,1.9,3.1), y=rep(1.01,times=length(abc$counts)), 
       labels=paste("N=",abc$count,sep=""),
       adj=c(0.5,0), cex=0.8, xpd = NA)
  dev.off()
  print(svyttest(hypertension~age_cluster, NHANES_base)$p.value %>% as.numeric)
  def <- svyttest(hypertension~age_cluster, subset(NHANES_base, age_cluster == 1 | age_cluster == 2))
  cat("\n",def$p.value %>% as.numeric,"  20 - 39 & 40 - 64")
  def <- svyttest(hypertension~age_cluster, subset(NHANES_base, age_cluster == 1 | age_cluster == 3))
  cat("\n",def$p.value %>% as.numeric,"  20 - 39 & 65 and above")
  
  def <- svyttest(hypertension~age_cluster, subset(NHANES_base, age_cluster == 2 | age_cluster == 3))
  cat("\n",def$p.value %>% as.numeric,"  40 - 64 & 65 and above")
  
  cat("\n\nBy Family Income to Poverty Ratio\n")
  abc <- getSummary(~hypertension, ~FIPR_cluster, NHANES_base) #by income
  print(abc)
  df <- rbind(abc$hypertension, 1-abc$hypertension)
  per_cent <- abc$counts/sum(abc$counts)*100
  pdf(file=paste(main_title,"07A.pdf"), width=5, height=4.5)
  barplot(per_cent,col="black",
          ylab="Unweighted Percentage", ylim=c(0,70),
          xlab="Family Income to Poverty Ratio", 
          names.arg=c("0.00% to 0.99%",
                      "1.00% to 1.49%",
                      "1.50% and\nabove"), cex.names=0.65, las=1,
          main=main_title)
  dev.off()
  pdf(file=paste(main_title,"07B.pdf"), width=5, height=4.5)
  barplot(df,col=viridis(2),
          ylab="Fraction with High BP (purple)",
          xlab="Family Income to Poverty Ratio", 
          names.arg=c("0.00% to 0.99%",
                      "1.00% to 1.49%",
                      "1.50% and\nabove"), cex.names=0.65, las=1,
          main=main_title)
  text(x=c(0.7,1.9,3.1), y=rep(1.01,times=length(abc$counts)), 
       labels=paste("N=",abc$count,sep=""),
       adj=c(0.5,0), cex=0.8, xpd = NA)
  dev.off()
  print(svyttest(hypertension~FIPR_cluster, NHANES_base)$p.value %>% as.numeric)
  def <- svyttest(hypertension~FIPR_cluster, subset(NHANES_base, FIPR_cluster == 1 | FIPR_cluster == 2))
  cat("\n",def$p.value %>% as.numeric,"  0.00% to 0.99% & 1.00% to 1.49%")
  def <- svyttest(hypertension~FIPR_cluster, subset(NHANES_base, FIPR_cluster == 1 | FIPR_cluster == 3))
  cat("\n",def$p.value %>% as.numeric,"  0.00% to 0.99% & 1.50% and above")
  
  def <- svyttest(hypertension~FIPR_cluster, subset(NHANES_base, FIPR_cluster == 2 | FIPR_cluster == 3))
  cat("\n",def$p.value %>% as.numeric,"  1.00% to 1.49% & 1.50% and above")
  
  #by other covariates - BMI
  cat("\n\nBy BMI\n")
  abc <- getSummary(~hypertension, ~BMI_category, NHANES_base)
  print(abc)
  df <- rbind(abc$hypertension, 1-abc$hypertension)
  per_cent <- abc$counts/sum(abc$counts)*100
  pdf(file=paste(main_title,"08A.pdf"), width=5, height=4.5)
  barplot(per_cent,col="black",
          ylab="Unweighted Percentage", ylim=c(0,50),
          xlab="BMI", 
          names.arg=c("Normal\nweight",
                      "Underweight",
                      "Overweight",
                      "Obese"), cex.names=0.65, las=1,
          main=main_title)
  dev.off()
  pdf(file=paste(main_title,"08B.pdf"), width=5, height=4.5)
  barplot(df,col=viridis(2),
          ylab="Fraction with High BP (purple)",
          xlab="BMI", 
          names.arg=c("Normal\nweight",
                      "Underweight",
                      "Overweight",
                      "Obese"), cex.names=0.65, las=1,
          main=main_title)
  text(x=c(0.7,1.9,3.1,4.3), y=rep(1.01,times=length(abc$counts)), 
       labels=paste("N=",abc$count,sep=""),
       adj=c(0.5,0), cex=0.8, xpd = NA)
  dev.off()
  print(svyttest(hypertension~BMI_category, NHANES_base)$p.value %>% as.numeric)
  def <- svyttest(hypertension~BMI_category, subset(NHANES_base, BMI_category == 0 | BMI_category == 1))
  cat("\n",def$p.value %>% as.numeric,"  Normal weight & Underweight")
  def <- svyttest(hypertension~BMI_category, subset(NHANES_base, BMI_category == 0 | BMI_category == 2))
  cat("\n",def$p.value %>% as.numeric,"  Normal weight & Overweight")
  def <- svyttest(hypertension~BMI_category, subset(NHANES_base, BMI_category == 0 | BMI_category == 3))
  cat("\n",def$p.value %>% as.numeric,"  Normal weight & Obese")
  
  def <- svyttest(hypertension~BMI_category, subset(NHANES_base, BMI_category == 1 | BMI_category == 2))
  cat("\n",def$p.value %>% as.numeric,"  Underweight & Overweight")
  def <- svyttest(hypertension~BMI_category, subset(NHANES_base, BMI_category == 1 | BMI_category == 3))
  cat("\n",def$p.value %>% as.numeric,"  Underweight & Obese")
  
  def <- svyttest(hypertension~BMI_category, subset(NHANES_base, BMI_category == 2 | BMI_category == 3))
  cat("\n",def$p.value %>% as.numeric,"  Overweight & Obese")
  
  #by other covariates - smoking(?) cotinine
  cat("\n\nBy Cotinine\n")
  abc <- getSummary(~hypertension, ~LBDCOTLC, NHANES_base)
  print(abc)
  df <- rbind(abc$hypertension, 1-abc$hypertension)
  per_cent <- abc$counts/sum(abc$counts)*100
  pdf(file=paste(main_title,"09A.pdf"), width=5, height=4.5)
  barplot(per_cent,col="black",
          ylab="Unweighted Percentage", ylim=c(0,70),
          xlab="Serum Continine", 
          names.arg=c("Compound\nBelow LLoD",
                      "Compound\nAbove LLoD"), cex.names=0.65, las=1,
          main=main_title)
  dev.off()
  pdf(file=paste(main_title,"09B.pdf"), width=5, height=4.5)
  barplot(df,col=viridis(2),
          ylab="Fraction with High BP (purple)",
          xlab="Serum Cotinine", 
          names.arg=c("Compound\nBelow LLoD",
                      "Compound\nAbove LLoD"), cex.names=0.65, las=1,
          main=main_title)
  text(x=c(0.7,1.9), y=rep(1.01,times=length(abc$counts)), 
       labels=paste("N=",abc$count,sep=""),
       adj=c(0.5,0), cex=0.8, xpd = NA)
  dev.off()
  print(svyttest(hypertension~LBDCOTLC, NHANES_base)$p.value %>% as.numeric)
  
  #by other covariates - creatinine
  #cat("\n\nBy Creatinine\n")
  #abc <- getSummary(~hypertension, ~Creat_cat, NHANES_base)
  #print(abc)
  #df <- rbind(abc$hypertension, 1-abc$hypertension)
  #per_cent <- abc$counts/sum(abc$counts)*100
  #arg.names <- c("0-100","100-200","200-300","300-400",">400")
  #barplot(per_cent,col="black",
  #        ylab="Unweighted Percentage", ylim=c(0,50),
  #        xlab="Urine Creatinine Concentration (mg/dL)", 
  #        names.arg=arg.names, cex.names=0.65, las=1,
  #        main=main_title)
  #barplot(df,col=viridis(2),
  #        ylab="Fraction with High BP (purple)",
  #        xlab="Urine Creatinine Concentration (mg/dL)", 
  #        names.arg=arg.names, cex.names=0.65, las=1,
  #        main=main_title)
  #text(x=c(0.7,1.9,3.1,4.3,5.5), y=rep(1.01,times=length(abc$counts)), #,6.7,7.9,9.1
  #     labels=paste("N=",abc$count,sep=""),
  #     adj=c(0.5,0), cex=0.8, xpd = NA)
  #print(svyttest(hypertension~Creat_cat, NHANES_base)$p.value %>% as.numeric)
  
  #def <- svyttest(hypertension~Creat_cat, subset(NHANES_base, Creat_cat == 0 | Creat_cat == 1))
  #cat("\n",def$p.value %>% as.numeric,"  0-100 & 100-200")
  #def <- svyttest(hypertension~Creat_cat, subset(NHANES_base, Creat_cat == 0 | Creat_cat == 2))
  #cat("\n",def$p.value %>% as.numeric,"  0-100 & 200-300")
  #def <- svyttest(hypertension~Creat_cat, subset(NHANES_base, Creat_cat == 0 | Creat_cat == 3))
  #cat("\n",def$p.value %>% as.numeric,"  0-100 & 300-400")
  #def <- svyttest(hypertension~Creat_cat, subset(NHANES_base, Creat_cat == 0 | Creat_cat == 4))
  #cat("\n",def$p.value %>% as.numeric,"  0-100 & >400")
  
  #def <- svyttest(hypertension~Creat_cat, subset(NHANES_base, Creat_cat == 1 | Creat_cat == 2))
  #cat("\n",def$p.value %>% as.numeric,"  100-200 & 200-300")
  #def <- svyttest(hypertension~Creat_cat, subset(NHANES_base, Creat_cat == 1 | Creat_cat == 3))
  #cat("\n",def$p.value %>% as.numeric,"  100-200 & 300-400")
  #def <- svyttest(hypertension~Creat_cat, subset(NHANES_base, Creat_cat == 1 | Creat_cat == 4))
  #cat("\n",def$p.value %>% as.numeric,"  100-200 & >400")
  
  #def <- svyttest(hypertension~Creat_cat, subset(NHANES_base, Creat_cat == 2 | Creat_cat == 3))
  #cat("\n",def$p.value %>% as.numeric,"  200-300 & 300-400")
  #def <- svyttest(hypertension~Creat_cat, subset(NHANES_base, Creat_cat == 2 | Creat_cat == 4))
  #cat("\n",def$p.value %>% as.numeric,"  200-300 & >400")
  
  #def <- svyttest(hypertension~Creat_cat, subset(NHANES_base, Creat_cat == 3 | Creat_cat == 4))
  #cat("\n",def$p.value %>% as.numeric,"  300-400 & >400")
  #########
  
  #by other covariates - alcohol
  cat("\n\nBy Alcohol\n")
  abc <- getSummary(~hypertension, ~ALQ101, NHANES_base)
  print(abc)
  df <- rbind(abc$hypertension, 1-abc$hypertension)
  per_cent <- abc$counts/sum(abc$counts)*100
  pdf(file=paste(main_title,"10A.pdf"), width=5, height=4.5)
  barplot(per_cent,col="black",
          ylab="Unweighted Percentage", ylim=c(0,70),
          xlab="12 or More Alcoholic Drinks in past Year", 
          names.arg=c("Yes",
                      "No"), cex.names=0.65, las=1,
          main=main_title)
  dev.off()
  pdf(file=paste(main_title,"10B.pdf"), width=5, height=4.5)
  barplot(df,col=viridis(2),
          ylab="Fraction with High BP (purple)",
          xlab="12 or More Alcoholic Drinks in past Year", 
          names.arg=c("Yes",
                      "No"), cex.names=0.65, las=1,
          main=main_title)
  text(x=c(0.7,1.9), y=rep(1.01,times=length(abc$counts)), 
       labels=paste("N=",abc$count,sep=""),
       adj=c(0.5,0), cex=0.8, xpd = NA)
  dev.off()
  print(svyttest(hypertension~LBDCOTLC, NHANES_base)$p.value %>% as.numeric)
}