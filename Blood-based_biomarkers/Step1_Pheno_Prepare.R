rm(list = ls())
library(reshape2)
library(dplyr)

Work_path = '/Users/panguoqing/Desktop/help/project'
covariance = read.csv(paste(Work_path, 'Data_collect','Cov.csv',sep = '/'))
covariance = covariance[, -1]

UKB_BloodID = read.csv(paste(Work_path,'Data/UKB_BloodID.csv', sep = '/'))
UKB_BloodID = UKB_BloodID[,-1]

UKB_pheno = read.csv(paste(Work_path, 'Data/PhenoData2Analysis','UKB_pheno.csv',sep = '/'))
colnames(UKB_pheno) = sub('\\.', '-', sub('X', '', colnames(UKB_pheno)))

#UKB_pheno227
UKB_pheno227 = UKB_pheno[, c('eid', UKB_BloodID$FieldID_full)]

for (MD in c("AX", "BIP", "DEP", "SCH")){
  data = read.csv(paste(Work_path, 'Data/Mental_assessments', MD, 'case_control_match_eid_yrs.csv', sep = '/'))
  #zdata
  data_cov = merge(data, covariance, by = 'eid')
  
  data_cov_pheno = merge(data_cov, UKB_pheno227, by = 'eid')
  colnames(data_cov_pheno)[2] = "duration"
  
  data_cov_pheno = data_cov_pheno[complete.cases(data_cov_pheno$time.of.assessment),]
  colnames(data_cov_pheno) = sub('\\.', '\\_', colnames(data_cov_pheno))
  colnames(data_cov_pheno) = sub('\\.', '\\_', colnames(data_cov_pheno))
  
  outfile = paste(Work_path, 'Data/CaseControl', MD, 'data_cov_pheno_forPvalue.csv', sep = '/')
  write.csv(data_cov_pheno, outfile, row.names = FALSE)
  
}


