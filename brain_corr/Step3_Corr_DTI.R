rm(list = ls())
Project_path = '/Users/panguoqing/Desktop/help/project'
Work_path = paste(Project_path, 'Data/CaseControl', sep = '/')
Save_path = paste(Project_path, 'Data/Brain_Corr/Step3', sep = '/')

BL_Data = read.csv(paste(Work_path, 'S5_loess_predict_all.csv', sep = '/'))[-1]
Sig_Field_Replaced <- colnames(BL_Data)
temp_name <- sapply(X = Sig_Field_Replaced, FUN = function(x) strsplit(x, split = '_')[[1]][1])
temp_MD <- sapply(X = Sig_Field_Replaced, FUN = function(x) strsplit(x, split = '_')[[1]][2])
# 创建数据框
MD_Result <- data.frame(name = temp_name, MD = temp_MD)

UKB_pheno = read.csv(paste(Project_path, 'Data/PhenoData2Analysis', 'UKB_pheno.csv',sep = '/'))

for (MD in c("AX", "BIP", "DEP", "SCH")){
  temp_Sig_BL = MD_Result$name[MD_Result$MD == MD]
  temp_UKB = subset(UKB_pheno, select = c('eid', temp_Sig_BL))
  Image_Data = read.csv(paste(Project_path, 'Data/Brain_Corr/Step2', MD, 'Step2_DTI.csv', sep = '/'))
  Regions = colnames(Image_Data)[22:length(colnames(Image_Data))]
  
  temp_Data = merge(x = temp_UKB, y = Image_Data, by = 'eid')
  temp_Data$Sites = factor(temp_Data$Sites)
  temp_Data$ALC_Status = factor(temp_Data$ALC_Status)
  temp_Data$SMK_Status = factor(temp_Data$SMK_Status)
  temp_Data$Ethnicity = factor(temp_Data$Ethnicity)
  # temp_Data$age_gap = factor(temp_Data$age_gap)
  temp_Data$fasting_time = factor(temp_Data$fasting_time)
  temp_Data$month_of_assessment = factor(temp_Data$month_of_assessment)
  temp_Data$time_of_sampling = factor(temp_Data$time_of_sampling)
  temp_Data$day_of_assay = factor(temp_Data$day_of_assay)
  # 去除血液指标的协变量
  for (BL in temp_Sig_BL){
    glm1 = glm(temp_Data[,BL] ~ Sites + fasting_time + month_of_assessment + time_of_sampling + day_of_assay + icosatiles, 
               data = temp_Data)
    res = residuals(glm1)
    new_data = res + glm1$coefficients[1]
    complete_indices <- complete.cases(temp_Data[,BL]) & complete.cases(temp_Data[, c("Sites", "fasting_time", "month_of_assessment", 
                                                                                      "time_of_sampling", "day_of_assay", "icosatiles")])
    temp_Data[complete_indices,BL] = new_data
    temp_Data[!complete_indices,BL] = NA
  }
  # 考虑影像与血液指标的相关性并去除共有的协变量
  Corr_Result_X_group = data.frame()
  for (N_region in seq(length(Regions))){
    for (N_BL in seq(length(temp_Sig_BL))){
      analysis_Data = subset(temp_Data, select = c(Regions[N_region], temp_Sig_BL[N_BL], 'group', 'Gender', 'TD_Index', 'SMK_Status', 'ALC_Status', 'Ethnicity', 'BMI', 'Age', 'Education'))
      colnames(analysis_Data)[1:2] = c('Y', 'X')
      # 标准化回归系数
      analysis_Data$Y <- scale(analysis_Data$Y, center = TRUE, scale = TRUE)
      analysis_Data$X <- scale(analysis_Data$X, center = TRUE, scale = TRUE)
      analysis_Data$Age <- scale(analysis_Data$Age, center = TRUE, scale = TRUE)
      analysis_Data$BMI <- scale(analysis_Data$BMI, center = TRUE, scale = TRUE)
      analysis_Data$TD_Index <- scale(analysis_Data$TD_Index, center = TRUE, scale = TRUE)
      analysis_Data$Education <- scale(analysis_Data$Education, center = TRUE, scale = TRUE)
      
      temp_glm = glm(Y ~ X*group + Age*Gender + TD_Index + SMK_Status + ALC_Status + Gender:Ethnicity + Ethnicity + BMI + Education, 
                     data = analysis_Data)
      model_summary <- summary(temp_glm)
      
      Corr_Result_X_group = rbind(Corr_Result_X_group,
                            data.frame(Region = Regions[N_region],
                                       BL = temp_Sig_BL[N_BL],
                                       Tvalue = coef(summary(temp_glm))['X:group', "Estimate"],
                                       Pvalue =  coef(summary(temp_glm))['X:group', "Pr(>|t|)"])
      )
      print(paste(N_region, N_BL, sep = '-'))
    }
  }
  # 保存相关性结果
  temp_Save_path = paste(Save_path, MD, sep = '/')
  if (!file.exists(temp_Save_path)) {
    dir.create(temp_Save_path, recursive = T)
  }
  write.csv(Corr_Result_X_group, paste(temp_Save_path, 'Step3_DTI_Corr_Result_X_group.csv', sep = '/'), row.names = T)
  
}
