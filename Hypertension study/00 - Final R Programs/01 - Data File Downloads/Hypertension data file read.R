setwd("~/desktop")
library(RNHANES)

file_path <- "~/Desktop/Hypertension study/NHANES files"

#Download demographic data
nhanes_load_data("DEMO_I", "2015-2016", demographics = FALSE, destination=file_path)

#Download albumin data
nhanes_load_data("ALB_CR_I", "2015-2016", demographics = FALSE, destination=file_path)

#Download smoking (cotinine) data
nhanes_load_data("COT_I", "2015-2016", demographics = FALSE, destination=file_path)

#Download Blood Pressure & Cholesterol (BPQ_I) data
nhanes_load_data("BPQ_I", "2015-2016", demographics = FALSE, destination=file_path)

#Download Blood Pressure measurement (BPX_I) data
nhanes_load_data("BPX_I", "2015-2016", demographics = FALSE, destination=file_path)

#Download Body Measures (BMX_I) data
nhanes_load_data("BMX_I", "2015-2016", demographics = FALSE, destination=file_path)

#dowload alcohol data
nhanes_load_data("ALQ", "2015-2016", demographics = FALSE, destination=file_path)

#dowload arsenic data
nhanes_load_data("UAS", "2015-2016", demographics = FALSE, destination=file_path)

#Download arsenic total data
nhanes_load_data("UTAS_I", "2015-2016", demographics = FALSE, destination=file_path)

#dowload mercury data
nhanes_load_data("UHG", "2015-2016", demographics = FALSE, destination=file_path)

#Download urine heavy metal data
nhanes_load_data("UMS_I", "2015-2016", demographics = FALSE, destination=file_path)