rm(list = ls())
library(tidyr)
Work_path = '/Users/panguoqing/Desktop/help/project'
Save_path = paste(Work_path, 'Data/CaseControl', sep = '/')
Threshold = 0.01
N = 63
covariance = read.csv(paste(Work_path, 'Data_collect/Cov.csv', sep = '/'))
covariance = covariance[,-1]
UKB_pheno = read.csv(paste(Work_path, 'Data/PhenoData2Analysis/UKB_pheno.csv', sep = '/'))
tmp = colnames(UKB_pheno)[2:383]
for (i in (1:382)) {
  tmp[i] = sub('X', '', tmp[i])
  tmp[i] = sub('\\.', '\\-', tmp[i])
}
colnames(UKB_pheno)[2:383] = tmp

data_FieldID = read.csv(paste(Save_path, 'data_FieldID.csv', sep = "/"))

for (MD in c("AX", "BIP", "DEP", "SCH")){
  data = read.csv(paste(Save_path, MD, 'Pvalue_beta.csv',sep = "/"))
  data$FieldID_full = sub('_', '.', data$FieldID_full)
  data = data[,c("FieldID_full","beta_group","p_group_bfi","beta_group_time","p_group_time_bfi","beta_group_time2","p_group_time2_bfi")]
  data_FieldID_beta_p = merge(data_FieldID, data, by = 'FieldID_full')
  write.csv(data_FieldID_beta_p,paste(Save_path,MD,'Pvalue_beta_field.csv',sep = "/"),row.names = FALSE)

  case_control_match_eid_yrs = read.csv(paste(Work_path, 'Data/Mental_assessments', MD, 'case_control_match_eid_yrs.csv', sep = '/'))
  colnames(data)[2] = "duration"
  data_cov = merge(case_control_match_eid_yrs, covariance, by = 'eid')
  
  pvalue_FiledID = data_FieldID_beta_p$FieldID_full[data_FieldID_beta_p$p_group_bfi< Threshold | data_FieldID_beta_p$p_group_time_bfi< Threshold | data_FieldID_beta_p$p_group_time2_bfi< Threshold]
  
  # 协变量处理
  Total_cov = c('group', 'Gender', 'Sites', 'TD_Index', 'SMK_Status', 'ALC_Status', 'Ethnicity', 'BMI', 'Age', 'Education', 'age_gap', 'fasting.time', 'month.of.assessment', 'time.of.sampling', 'day.of.assay', 'icosatiles')
  data_cov = drop_na(data_cov, Total_cov)
  
  #dummy 分类变量
  data_cov$SMK_Status = as.character(data_cov$SMK_Status)
  data_cov$ALC_Status = as.character(data_cov$ALC_Status)
  data_cov$Gender = as.character(data_cov$Gender)
  data_cov$Education = as.character(data_cov$Education)
  data_cov$Ethnicity = as.character(data_cov$Ethnicity)
  data_cov$Sites = as.character(data_cov$Sites)
  data_cov$fasting.time = as.character(data_cov$fasting.time)
  data_cov$age_gap = as.character(data_cov$age_gap)
  data_cov$month.of.assessment = as.character(data_cov$month.of.assessment)
  data_cov$day.of.assay = as.character(data_cov$day.of.assay)
  data_cov$time.of.sampling = as.character(data_cov$time.of.sampling)
  
  #连续变量规范化处理
  continues = c("TD_Index", "Age", "BMI")
  # 以group = 0 为对照组
  for (i in continues) {
    all_indx = complete.cases(data_cov[,i])
    control_indx = data_cov$group == 0 & all_indx
    temp = data_cov[all_indx,i]
    temp_control = data_cov[control_indx,i]
    data_cov[all_indx,i] = ( temp - mean(temp_control) ) / sd( temp_control )
  }
  
  UKB_pheno_sig = UKB_pheno[, c('eid', pvalue_FiledID)]
  
  data_cov_pheno = merge(data_cov, UKB_pheno_sig, by = 'eid')
  
  #eliminate the influence of covariance
  data_cov_pheno_elimate = data_cov_pheno
  for (i in (N : length(colnames(data_cov_pheno_elimate)))){
    print(i)
    glm1 = glm(data_cov_pheno[,i] ~ Age + Gender + Education + Ethnicity + TD_Index + ALC_Status  + SMK_Status + BMI + Sites +
                 fasting.time + month.of.assessment + day.of.assay + time.of.sampling + age_gap:Gender + age_gap:Ethnicity,
               data = data_cov_pheno_elimate)
    
    res = residuals(glm1)
    
    new_data = res + glm1$coefficients[1]
    
    data_cov_pheno_elimate[complete.cases(data_cov_pheno_elimate[,i]),][,i] = new_data
  } #take so long time
  
  #calculate zscores
  zdata = data_cov_pheno_elimate
  zdata_control = zdata$group == 0
  for (i in (N : length(colnames(zdata)))) {
    temp = complete.cases(zdata[,i])
    control_indx = temp & zdata_control
    zdata[temp,i] = ( zdata[temp,i] - mean(zdata[control_indx,i]) ) / sd(zdata[control_indx,i])
  }
  
  write.csv(zdata,paste(Save_path, MD, 'S4_zdata_MD.csv',sep = '/'),row.names = FALSE)
}

