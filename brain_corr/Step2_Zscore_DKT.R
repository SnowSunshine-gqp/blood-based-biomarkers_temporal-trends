rm(list = ls())
Project_path = '/Users/panguoqing/Desktop/help/project'
Work_path = paste(Project_path, 'Data/Brain_Corr/Step1', sep = '/')
Save_path = paste(Project_path, 'Data/Brain_Corr/Step2', sep = '/')

Volume = read.csv(paste(Work_path, 'Volume_of_DKT_mean.csv', sep = '/'))
Volume = Volume[,-1]
Regions = colnames(Volume)[c(3:length(colnames(Volume)))]

Cov = read.csv(paste(Project_path, 'Data_collect/Cov.csv', sep = '/'))
Cov = Cov[,-1]
Cov_site = read.csv('/Users/panguoqing/Desktop/Research/UKB Data/Common_Covariates/Covariates.csv')
Cov_site = subset(Cov_site,select = c('eid', 'Assessment_Date_2', 'Assessment_Center_2'))
Cov = merge(x = Cov, y = Cov_site, by = 'eid')

Data = merge(x = Volume, y = Cov,by = 'eid')
Data_rm = subset(Data, select = c('eid', Regions))
# 主要要去除 tiv。和 站点
for (region in Regions) {
  print(region)
  glm1 = glm(Data[,region] ~ Assessment_Center_2 + TIV, 
             data = Data)
  res = residuals(glm1)
  new_data = res + glm1$coefficients[1]
  Data_rm[complete.cases(Data_rm[,region]),region] = new_data
}

Cov_list = c("eid","duration","group","Gender","BL_Date","Sites","TD_Index","SMK_Status",         
             "ALC_Status","Ethnicity","BMI","Age","Education","age_gap","fasting_time",       
             "time_of_assessment","month_of_assessment","month","time_of_sampling",   
             "day_of_assay","icosatiles" )
 

for (MD in c("AX", "BIP", "DEP", "SCH")){
  MD_Data = read.csv(paste(Project_path, 'Data/CaseControl', MD, 'data_cov_pheno_forPvalue.csv', sep = '/'))
  MD_Data = subset(MD_Data, select = Cov_list)
  temp_Data = merge(x = MD_Data, y =Data_rm, by = 'eid')
  
  temp_Save_path = paste(Save_path, MD, sep = '/')
  if (!file.exists(temp_Save_path)) {
    dir.create(temp_Save_path, recursive = T)
  }
  write.csv(temp_Data, paste(temp_Save_path, 'Step2_DKT.csv', sep = '/'), row.names = FALSE)
}



