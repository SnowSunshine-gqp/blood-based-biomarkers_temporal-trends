rm(list = ls())
Work_path = '/Users/panguoqing/Desktop/help/project'
Save_path = paste(Work_path, 'Data/Mental_assessments', sep = '/')
library(ggplot2)
library(reshape2)
library(dplyr)
library(cluster)
library(NbClust)
N = 65

covariance = read.csv(paste(Work_path, 'Data_collect/Cov.csv', sep = '/'))
covariance = covariance[, -1]
covariance$Month_of_Baseline = format(as.Date(covariance$BL_Date), "%m")
covariance$Day_of_Baseline = format(as.Date(covariance$BL_Date), "%d")

MD_score = read.csv(paste(Save_path, 'Mental_score.csv', sep = '/'))
MD_score = MD_score[, -1]
# MD = "SCH"#c("AX", "BIP", "DEP", "SCH")

for ( MD in c("AX", "BIP", "DEP", "SCH") ) {
  print(MD)
  data = read.csv(paste(Save_path, MD, 'case_control_match_eid_yrs.csv', sep = '/'))
  colnames(data)[2] = "duration"
  #zdata是下面的代码准备的，为了不再重新跑一遍直接进行存储
  data_cov = merge(data, covariance, by = 'eid')
  
  data_cov$SMK_Status = as.character(data_cov$SMK_Status)
  data_cov$ALC_Status = as.character(data_cov$ALC_Status)
  data_cov$Gender = as.character(data_cov$Gender)
  data_cov$Education = as.character(data_cov$Education)
  data_cov$Ethnicity = as.character(data_cov$Ethnicity)
  data_cov$age_gap = as.character(data_cov$age_gap)
  data_cov$Sites = as.character(data_cov$Sites)
  data_cov$Month_of_Baseline = as.character(data_cov$Month_of_Baseline)
  data_cov$Day_of_Baseline = as.character(data_cov$Day_of_Baseline)
  
  data_cov_pheno = merge(data_cov, MD_score, by = 'eid')
  
  #eliminate the influence of covariance
  data_cov_pheno_elimate = data_cov_pheno
  for (i in (N : length(colnames(data_cov_pheno)))){
    glm1 = glm(data_cov_pheno[,i] ~ Age + Gender*Ethnicity + Education + Gender*age_gap + BMI + Sites + TD_Index + SMK_Status + ALC_Status + Month_of_Baseline + Day_of_Baseline, 
               data = data_cov_pheno)
    res = residuals(glm1)
    new_data = res + glm1$coefficients[1]
    data_cov_pheno_elimate[complete.cases(data_cov_pheno_elimate[,i]),][,i] = new_data
  } #take so long time
  
  #calculate zscores
  zdata = data_cov_pheno_elimate
  for (i in (N : length(colnames(data_cov_pheno)))) {
    all_indx = complete.cases(zdata[,i])
    control_indx = zdata$group == 0 & all_indx
    temp = zdata[all_indx,i]
    temp_control = zdata[control_indx,i]
    zdata[all_indx,i] = ( temp - mean(temp_control) ) / sd( temp_control )
  }
  
  write.csv(zdata, paste(Save_path, MD, 'zdata_mental.csv',sep = '/'), row.names = FALSE)
  
}


