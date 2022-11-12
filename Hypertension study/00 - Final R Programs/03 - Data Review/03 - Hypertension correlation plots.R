library(Hmisc)
library(corrplot)

dir1 <- "~/Desktop/Hypertension study/Cleaned Data Files"
setwd(dir1)

#correlation plot for metals
metal_data <- read.csv(file="Cleaned heavy metals data.csv", header=TRUE)
for (i in seq(from=3, to=27, by=2)){
  keep <- !is.na(metal_data[,i])
  metal_data <- metal_data[keep,]
}

Hg_data <- read.csv(file="Cleaned Hg data.csv", header=TRUE)
keep <- !is.na(Hg_data$URXUHG)
Hg_data <- Hg_data[keep,]

metal_data <- merge(metal_data, Hg_data, 
                    by.x="SEQN", by.y="SEQN", all=FALSE)
metal_data2 <- metal_data[,c(35:47,53)]

corr_mat <- rcorr(x=as.matrix(log10(metal_data2)))
cor_matr <- corr_mat$r
metal_names <- c("Barium", "Cadmium", "Cobalt", "Cesium", "Molybdenum", "Manganese",
                 "Lead", "Antimoun", "Tin", "Strontium", "Thallium", "Tungsten",
                 "Uranium", "Mercury")
colnames(cor_matr) <- metal_names
rownames(cor_matr) <- metal_names
corrplot(as.matrix(cor_matr))

#correlation plot for arsenic compounds
arsenic_data <- read.csv(file="Cleaned arsenic data.csv", header=TRUE)
for (i in seq(from=4, to=14, by=2)){
  keep <- !is.na(arsenic_data[,(i-1)])
  arsenic_data <- arsenic_data[keep,]
}
TA_data <- read.csv(file="Cleaned total arsenic data.csv", header=TRUE)

arsenic <- merge(arsenic_data, TA_data, by.x="SEQN", by.y="SEQN", all=FALSE)
arsenic2 <- arsenic[,c(30:35,49)]
corr_mat <- rcorr(x=as.matrix(log10(arsenic2)))
cor_matr <- corr_mat$r
arsenic_names <- c("Arsenous Acid","Arsenic Acid","Arsenobetaine",
                   "Arsenocholine","DMA","MMA","Total Arsenic")
colnames(cor_matr) <- arsenic_names
rownames(cor_matr) <- arsenic_names
corrplot(as.matrix(cor_matr))