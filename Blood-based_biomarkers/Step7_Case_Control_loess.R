rm(list = ls())
library(stringr)
library(ggplot2)
Work_path = '/Users/panguoqing/Desktop/help/project'
Save_path = paste(Work_path, 'Data/CaseControl', sep = '/')
UKB_pheno = read.csv(paste(Work_path, 'Data/PhenoData2Analysis/UKB_pheno.csv', sep = '/'))
Data_FieldID = read.csv(paste(Save_path, 'data_FieldID.csv',sep = '/'))

Loess_all = read.csv(paste(Work_path, 'Data/CaseControl/S5_loess_predict_all.csv', sep = '/'))
Loess_all = Loess_all[,-1]

time = seq(-10, 10, by = 0.1)
N_indx = length(time)

MD_indx = sapply(sapply(colnames(Loess_all), strsplit, split = '_'), function(x) x[[2]])
Sig_FieldID = sapply(sapply(colnames(Loess_all), strsplit, split = '_'), function(x) x[[1]])
Sig_FieldID = sub('X', '', sub('\\.', '-', Sig_FieldID))
UKB_FieldID = sub('X', '', sub('\\.', '-', colnames(UKB_pheno)))
colnames(UKB_pheno) = UKB_FieldID

Unique_Sig_Abbr = unique(Data_FieldID$Abbreviation[match(Sig_FieldID, Data_FieldID$FieldID_full)])
Unique_Sig_FieldID = unique(Sig_FieldID)


UKB_Data = subset(UKB_pheno, select = c('eid', Unique_Sig_FieldID))
UKB_Data_name = Data_FieldID$Abbreviation[match(colnames(UKB_Data), Data_FieldID$FieldID_full)]
UKB_Data_name[1] = 'eid'
colnames(UKB_Data) = UKB_Data_name

for (MD in c("AX", "DEP", "BIP", "SCH")){
  temp_dir = paste(Save_path, MD, 'Case_Control_loess', sep = '/')
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)  # 如果路径中包含多级目录，需要设置recursive = TRUE
  }
  setwd(temp_dir)
  temp_data = read.csv(paste(Save_path, MD, 'S4_zdata_MD.csv', sep = '/'))
  colnames(temp_data)[2] = 'duration'
  temp_data = temp_data[, c('eid', 'duration', 'group')]
  temp_data_Case = temp_data[temp_data$group == 1,]
  temp_data_Control = temp_data[temp_data$group == 0,]
  
  Case = merge(temp_data_Case, UKB_Data, by = 'eid')
  Control = merge(temp_data_Control, UKB_Data, by = 'eid')
  
  temp_Sig = Data_FieldID$Abbreviation[match(Sig_FieldID[MD_indx == MD], Data_FieldID$FieldID_full)]
  temp_N = length(temp_Sig)
  
  loess_temp_Case_predict = data.frame(matrix(nrow = N_indx, ncol = temp_N))
  loess_temp_Control_predict = data.frame(matrix(nrow = N_indx, ncol = temp_N))
  for(i in seq(temp_N)){
    print(paste(MD, i, sep = '-'))
    temp_Case_complete = Case[complete.cases(Case[, temp_Sig[i]]), c('eid', 'duration', temp_Sig[i])]
    temp_Control_complete = Control[complete.cases(Control[, temp_Sig[i]]), c('eid', 'duration', temp_Sig[i])]
    
    loess_temp_Case <- loess(temp_Case_complete[,temp_Sig[i]] ~ duration, data=temp_Case_complete, span=2)
    loess_temp_Case_predict[i] = predict(loess_temp_Case, newdata = time)
    
    loess_temp_Control <- loess(temp_Control_complete[,temp_Sig[i]] ~ duration, data=temp_Control_complete, span=2)
    loess_temp_Control_predict[i] = predict(loess_temp_Control, newdata = time)
  }
  colnames(loess_temp_Case_predict) = temp_Sig
  colnames(loess_temp_Control_predict) = temp_Sig
  loess_temp_Case_predict$duration = time
  loess_temp_Control_predict$duration = time
  
  loess_Case_predict_melt = reshape2::melt(loess_temp_Case_predict, id.vars = 'duration')
  loess_Case_predict_melt$Group = 'Case'
  loess_Control_predict_melt = reshape2::melt(loess_temp_Control_predict, id.vars = 'duration')
  loess_Control_predict_melt$Group = 'Control'
  
  loess_predict_melt = rbind(loess_Case_predict_melt, loess_Control_predict_melt)
  loess_predict_melt$Group = factor(loess_predict_melt$Group)
  
  write.csv(loess_predict_melt, paste(temp_dir, 'loess_predict_melt.csv', sep = '/'))
  
}

