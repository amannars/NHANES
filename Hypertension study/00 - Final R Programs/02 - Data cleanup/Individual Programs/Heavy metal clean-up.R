dir1 <- "~/Desktop/Hypertension study/NHANES files"
dir2 <- "~/Desktop/Hypertension study/Cleaned Data Files"
setwd(dir1)

#get creatinine data
#SEQN - Respondent sequence number
#URXUMA - Albumin, urine (ug/mL)
#URDUMALC - Albumin, urine comment code
#URXUMS - Albumin, urine (mg/L)
#URXUCR - Creatinine, urine (mg/dL)
#URDUCRLC - Creatinine, urine comment code
#URXCRS - Creatinine, urine (umol/L)
#URDACT - Albumin creatinine ratio (mg/g)
creatinine_data <- read.csv(file="ALB_CR_I.csv", header=TRUE)[,c(1,5,6)]

#get data for heavy metals
#SEQN - Respondent sequence number
#WTSA2YR - Subsample A weights
#URXUBA - Barium, urine (ug/L)
#URDUBALC - Urinary Barium comment code
#URXUCD - Cadmium, urine (ug/L)
#URDUCDLC - Urinary Cadmium comment code
#URXUCO - Cobalt, urine (ug/L)
#URDUCOLC - Urinary Cobalt (ug/L) comment code
#URXUCS - Cesium, urine (ug/L)
#URDUCSLC - Urinary Cesium comment code
#URXUMO - Molybdenum, urine (ug/L)
#URDUMOLC - Urinary Molybdenum comment code
#URXUMN - Manganese, urine (ug/L)
#URDUMNLC - Urinary Mn comment code
#URXUPB - Lead, urine (ug/L)
#URDUPBLC - Urinary Lead comment code
#URXUSB - Antimony, urine (ug/L)
#URDUSBLC - Urinary Antimony comment code
#URXUSN - Tin, urine (ug/L)
#URDUSNLC - Tin comment code
#URXUSR - Strontium, urine (ug/L)
#URDUSRLC - Strontium comment code
#URXUTL - Thallium, urine (ug/L)
#URDUTLLC - Urinary Thallium comment code
#URXUTU - Tungsten, urine (ug/L)
#URDUTULC - Urinary Tungsten comment code
#URXUUR - Uranium, urinary (ug/L)
#URDUURLC - Urinary Uranium comment code
file_input <- "UMS_I.csv" 
metal_data <- read.csv(file=file_input, header=TRUE)
for (i in seq(from=3, to=27, by=2)){
  keep <- !is.na(metal_data[,i])      #delete when data isn't present
  metal_data <- metal_data[keep,]
  change <- (metal_data[,i+1] == 1)   #change those below LLoD to NA
  metal_data[change,i] <- NA
}

#merge metal data and creatinine data so the metal data can be normalized
metal_data <- merge(metal_data, creatinine_data, 
                    by.x="SEQN", by.y="SEQN", all=FALSE)

metal_data$BAnorm <- metal_data$URXUBA/metal_data$URXUCR  #Barium
metal_data$CDnorm <- metal_data$URXUCD/metal_data$URXUCR  #Cadimum
metal_data$COnorm <- metal_data$URXUCO/metal_data$URXUCR  #Cobalt
metal_data$CSnorm <- metal_data$URXUCS/metal_data$URXUCR  #Cesium
metal_data$MOnorm <- metal_data$URXUMO/metal_data$URXUCR  #Molybdenum
metal_data$MNnorm <- metal_data$URXUMN/metal_data$URXUCR  #Manganese
metal_data$PBnorm <- metal_data$URXUPB/metal_data$URXUCR  #Lead
metal_data$SBnorm <- metal_data$URXUSB/metal_data$URXUCR  #Antimony
metal_data$SNnorm <- metal_data$URXUSN/metal_data$URXUCR  #Tin
metal_data$SRnorm <- metal_data$URXUSR/metal_data$URXUCR  #Strontium
metal_data$TLnorm <- metal_data$URXUTL/metal_data$URXUCR  #Thallium
metal_data$TUnorm <- metal_data$URXUTU/metal_data$URXUCR  #Tungsten
metal_data$URnorm <- metal_data$URXUUR/metal_data$URXUCR  #Uranium

setwd(dir2)
file_out <- "Cleaned heavy metals data.csv"
write.csv(file=file_out, metal_data, row.names = FALSE)

#get the mercury data
#SEQN - Respondent sequence number
#WTSA2YR - Subsample A weights
#URXUHG - Mercury, urine (ug/L)
#URDUHGLC - Mercury, urine comment code
setwd(dir1)
file_input <- "UHG_I.csv" 
Hg_data <- read.csv(file=file_input, header=TRUE)[,c(1:4)]
keep <- !is.na(Hg_data$URXUHG)
Hg_data <- Hg_data[keep,]
Hg_data <- merge(Hg_data, creatinine_data, by.x="SEQN", by.y="SEQN", all=FALSE)
Hg_data$HGnorm <- Hg_data$URXUHG/Hg_data$URXUCR

setwd(dir2)
file_out <- "Cleaned Hg data.csv"
write.csv(file=file_out, Hg_data, row.names = FALSE)