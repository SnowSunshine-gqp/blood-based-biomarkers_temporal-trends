rm(list = ls())
library("MatchIt")

Work_path = '/Users/panguoqing/Desktop/help/project'
Save_path = paste(Work_path, 'Data/Mental_assessments', sep = '/')
N = 3
MD_list = c("AX", "BIP", "DEP", "SCH")# = "BIP" #c("AX", "BIP", "DEP", "SCH")
S0_TargetData = read.csv(paste(Work_path, 'Data/Raw_Data/S0_TargetData_date.csv', sep = '/'))

#control组中剔除其他疾病的人群
Blood_immune = read.csv(paste(Work_path,'Data/Raw_Data/disease_need_exclusion_data','Blood and immune disorders.csv',sep = "/"))
Blood_immune_eid_lst = Blood_immune$eid[Blood_immune$target_y == 1 & Blood_immune$BL2Target_yrs <0]
Mental_behavioural = read.csv(paste(Work_path,'Data/Raw_Data/disease_need_exclusion_data','Mental and behavioural disorders.csv',sep = "/"))
Mental_behavioural_eid_lst = Mental_behavioural$eid[Mental_behavioural$target_y == 1 & Mental_behavioural$BL2Target_yrs <0]
Infections = read.csv(paste(Work_path,'Data/Raw_Data/disease_need_exclusion_data','Diseases of Infections.csv',sep = "/"))
Infections_eid_lst = Infections$eid[Infections$target_y == 1 & Infections$BL2Target_yrs <0]
Nervous = read.csv(paste(Work_path,'Data/Raw_Data/disease_need_exclusion_data','Nervous system disorders.csv',sep = "/"))
Nervous_eid_lst = Nervous$eid[Nervous$target_y == 1 & Nervous$BL2Target_yrs <0]
other_disease = unique(c(Blood_immune_eid_lst,Mental_behavioural_eid_lst,Infections_eid_lst,Nervous_eid_lst ))

data_cov = read.csv(paste(Work_path,"Data_collect/Cov.csv",sep = "/"))
data_name = colnames(data_cov)[2:61]
data_cov = merge(data_cov[data_name], S0_TargetData[c("eid", paste(MD_list,"_y",sep=""))], by = "eid")
#提取control
control = data_cov[data_cov$AX_y == 0 & data_cov$BIP_y == 0 & data_cov$DEP_y == 0 & data_cov$SCH_y == 0,] 

control_noohter = control[!control$eid %in% other_disease,]
control_noohter$'BL2MD_yrs' = 0

for (MD in c("AX", "BIP", "DEP", "SCH")){#c("AX", "BIP", "DEP", "SCH")
  print(MD)
  temp_data = subset(data_cov, select = c(data_name, paste(MD,"_y",sep="")))
  n = length(colnames(temp_data))
  colnames(temp_data)[n] = "treat"
  #提取 case
  temp_case = temp_data[temp_data$treat == 1, ]
  #提取BL2MD_yrs
  case_BL2MD_yrs = S0_TargetData[c("eid", paste("BL2",MD,"_yrs",sep = ""))]
  colnames(case_BL2MD_yrs) = c('eid', 'BL2MD_yrs')
  temp_case = merge(temp_case, case_BL2MD_yrs, by="eid")
  
  #提取发病前后10年以内的患者
  temp_case = temp_case[complete.cases(temp_case[62]), ]
  temp_case10 = temp_case[temp_case$BL2MD_yrs <=10 & temp_case$BL2MD_yrs >= -10, ]
  temp_name = colnames(temp_case10)[seq(length(colnames(temp_case10))-2)]
  
  temp_control = control_noohter[, temp_name]
  temp_control$treat = 0
  temp_control$BL2MD_yrs = control_noohter$BL2MD_yrs
  
  temp_all_individuals = rbind(temp_case10, temp_control)
  temp_all_individuals = temp_all_individuals[order(temp_all_individuals$eid), ]

  temp_all_individuals$Gender = factor(temp_all_individuals$Gender)
  temp_all_individuals$Sites = factor(temp_all_individuals$Sites)
  temp_all_individuals$SMK_Status = factor(temp_all_individuals$SMK_Status)
  temp_all_individuals$ALC_Status = factor(temp_all_individuals$ALC_Status)
  temp_all_individuals$Ethnicity = factor(temp_all_individuals$Ethnicity)
  temp_all_individuals$Education = factor(temp_all_individuals$Education)
  
  m.out1 <- matchit(treat ~ Education + TD_Index + BMI,
                    method = "nearest", 
                    distance = "glm", 
                    ratio = N,  
                    link = "logit" ,
                    exact = ~ Age + Gender + Ethnicity + SMK_Status + ALC_Status,     #+Smoking+Alcohol+ centre
                    data = temp_all_individuals)
  
  summary(m.out1)
  control_row = as.data.frame(m.out1$match.matrix)
  temp_control_eid = control_row
  
  for (i in seq(N)) {
    for (j in c(1:length(temp_control_eid[,i]))) {
      temp_control_eid[j,i] = temp_all_individuals[temp_control_eid[j,i],1]
    }
  }
  
  temp_case_control_eid = cbind(temp_case10["eid"], temp_control_eid)
  
  colnames(temp_case_control_eid) = c("eid", "control_id1", "control_id2",	"control_id3")
  for (i in seq(N+1)) {
    temp_case_control_eid[, i] = as.character(temp_case_control_eid[, i])
  }
  #把匹配不到的个体删去
  temp_case_control_eid = temp_case_control_eid[complete.cases(temp_case_control_eid$control_id1), ]
  write.csv(temp_case_control_eid, paste(Save_path, MD,'case_control_match_eid.csv', sep="/"), row.names = FALSE)
  
  # 记录匹配的id
  temp_case = S0_TargetData[S0_TargetData$eid %in% temp_case_control_eid$eid,]
  temp_case_BL2MD_yrs = temp_case[c("eid", paste("BL2", MD, "_yrs",sep = ""))]
  
  temp_case_control_BL2MD_yrs = merge(temp_case_control_eid, temp_case_BL2MD_yrs, by="eid")
  
  temp_case_control_match_yrs = temp_case_control_BL2MD_yrs[,c(1,N+2)]
  temp_case_control_match_yrs$group = 1
  for (i in (seq(N) + 1)) {
    temp_control = temp_case_control_BL2MD_yrs[,c(i, N+2)]
    colnames(temp_control)[1] = "eid"
    temp_control$group = 0
    temp_case_control_match_yrs = rbind(temp_case_control_match_yrs, temp_control)
  }
  temp_case_control_match_yrs = temp_case_control_match_yrs[complete.cases(temp_case_control_match_yrs$eid),]
  temp_case_control_match_yrs = temp_case_control_match_yrs[complete.cases(temp_case_control_match_yrs[2]),]
  write.csv(temp_case_control_match_yrs, paste(Save_path, MD, 'case_control_match_eid_yrs.csv', sep="/"), row.names = FALSE)

}



