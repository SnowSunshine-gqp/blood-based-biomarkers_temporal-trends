rm(list = ls())
library(dplyr)
library(ggplot2)
Project_path = '/Users/panguoqing/Desktop/help/project'
Data_cov = read.csv(paste(Project_path,"Data_collect/Cov.csv",sep = "/"))

TT <- data.frame(
  indx = seq(-5,4),
  Timeframe = c("-10~-8", "-8~-6", "-8~-4", "-4~-2", "-2~0", "0~2", "2~4", "4~6", "6~8", "8~10")
)
MD_list = c("AX", "BIP", "DEP", "SCH")
MD_name = c("Anxiety", "Bipolar", "Depression", "Schizophrenia")

Total = data.frame()
for (n in seq(length(MD_list))){
  MD = MD_list[n]
  temp_data = read.csv(paste(Project_path, 'Data/Mental_assessments', MD, 'case_control_match_eid_yrs.csv', sep = '/'))
  colnames(temp_data)[2] = 'duration'
  temp_data$MD = MD
  Total = rbind(Total, temp_data)
}
Total = merge(Total, Data_cov, by = 'eid')
length(unique(Total$eid))

Total_Case = Total[Total$group == 1,]
Total_Case_unique <- Total_Case[!duplicated(Total_Case$eid), ]

# mean(Total_Case$Age)
median(Total_Case_unique$Age)
quantile(Total_Case_unique$Age, 0.25)
quantile(Total_Case_unique$Age, 0.75)
sum(Total_Case_unique$Gender == 1)
sum(Total_Case_unique$Gender == 1) / length(Total_Case_unique$Gender) 

sum(Total_Case_unique$Ethnicity == 0) / length(Total_Case_unique$Ethnicity) 


sum(Total_Case$MD == 'AX')
sum(Total_Case$MD == 'DEP')
sum(Total_Case$MD == 'BIP')
sum(Total_Case$MD == 'SCH')




Total_Case_unique = Total_Case[match(Total_Case$eid,unique(Total_Case$eid)),]




  # 统计分组后的信息
# for (n in seq(length(MD_list))){
#   MD = MD_list[n]
#   temp_data = read.csv(paste(Project_path, 'Data/Mental_assessments', MD, 'case_control_match_eid_yrs.csv', sep = '/'))
#   colnames(temp_data)[2] = 'duration'
#   temp_Case = temp_data[temp_data$group == 1,]
#   temp_Control = temp_data[temp_data$group == 0,]
#   
#   temp_Case_Data = merge(temp_Case, Data_cov[, c('eid', 'Age', "Gender", "TD_Index", "BMI", "Ethnicity", "Education")], by = 'eid')
#   temp_Control_Data = merge(temp_Control, Data_cov[, c('eid', 'Age', "Gender", "TD_Index", "BMI", "Ethnicity", "Education")], by = 'eid')
#   
#   # number of case
#   print(paste(MD, 'Case number:', length(temp_Case_Data$eid)))
#   print(paste(MD, 'Control number:', length(temp_Control_Data$eid)))
#   # age at baseline
#   print('age at baseline')
#   temp1 = temp_Case_Data$Age
#   temp2 = temp_Control_Data$Age
#   print(paste(MD, 'Case :',mean(temp1), '[', quantile(temp1, 0.25), ', ', quantile(temp1, 0.75), ']'))
#   print(paste(MD, 'Control :', mean(temp2), '[', quantile(temp2, 0.25), ', ', quantile(temp2, 0.75), ']'))
#   # Gender
#   print('Gender')
#   temp1 = temp_Case_Data$Gender
#   temp2 = temp_Control_Data$Gender
#   print(paste(MD, 'Case :', sum(temp1 == 1), ' ', sum(temp1 == 1) / length(temp1), '%') )
#   print(paste(MD, 'Control :', sum(temp2 == 1), ' ', sum(temp2 == 1) / length(temp2), '%') )
#   # TD_Index
#   print('TD_Index')
#   temp1 = temp_Case_Data$TD_Index
#   temp2 = temp_Control_Data$TD_Index
#   print(paste(MD, 'Case :',mean(temp1), '[', quantile(temp1, 0.25), ', ', quantile(temp1, 0.75), ']'))
#   print(paste(MD, 'Control :', mean(temp2), '[', quantile(temp2, 0.25), ', ', quantile(temp2, 0.75), ']'))
#   # BMI
#   print('BMI')
#   temp1 = temp_Case_Data$BMI
#   temp2 = temp_Control_Data$BMI
#   print(paste(MD, 'Case :',mean(temp1), '[', quantile(temp1, 0.25), ', ', quantile(temp1, 0.75), ']'))
#   print(paste(MD, 'Control :', mean(temp2), '[', quantile(temp2, 0.25), ', ', quantile(temp2, 0.75), ']'))
#   # Ethnicity
#   print('Ethnicity : White')
#   temp1 = temp_Case_Data$Ethnicity == 0
#   temp2 = temp_Control_Data$Ethnicity == 0
#   print(paste(MD, 'Case :', sum(temp1), ' ', sum(temp1) / length(temp1), '%') )
#   print(paste(MD, 'Control :', sum(temp2), ' ', sum(temp2) / length(temp2), '%') )
#   print('Ethnicity : Black')
#   temp1 = temp_Case_Data$Ethnicity == 1
#   temp2 = temp_Control_Data$Ethnicity == 1
#   print(paste(MD, 'Case :', sum(temp1), ' ', sum(temp1) / length(temp1), '%') )
#   print(paste(MD, 'Control :', sum(temp2), ' ', sum(temp2) / length(temp2), '%') )
#   print('Ethnicity : Asian')
#   temp1 = temp_Case_Data$Ethnicity == 2
#   temp2 = temp_Control_Data$Ethnicity == 2
#   print(paste(MD, 'Case :', sum(temp1), ' ', sum(temp1) / length(temp1), '%') )
#   print(paste(MD, 'Control :', sum(temp2), ' ', sum(temp2) / length(temp2), '%') )
#   print('Ethnicity : Other')
#   temp1 = temp_Case_Data$Ethnicity == 3
#   temp2 = temp_Control_Data$Ethnicity == 3
#   print(paste(MD, 'Case :', sum(temp1), ' ', sum(temp1) / length(temp1), '%') )
#   print(paste(MD, 'Control :', sum(temp2), ' ', sum(temp2) / length(temp2), '%') )
#   # Education 
#   print('Education : <10')
#   temp1 = temp_Case_Data$Education <= 2
#   temp2 = temp_Control_Data$Education <= 2
#   print(paste(MD, 'Case :', sum(temp1), ' ', sum(temp1) / length(temp1), '%') )
#   print(paste(MD, 'Control :', sum(temp2), ' ', sum(temp2) / length(temp2), '%') )
#   
#   print('Education : 10~15')
#   temp1 = temp_Case_Data$Education > 2 & temp_Case_Data$Education < 6
#   temp2 = temp_Control_Data$Education > 2 & temp_Control_Data$Education < 6
#   print(paste(MD, 'Case :', sum(temp1), ' ', sum(temp1) / length(temp1), '%') )
#   print(paste(MD, 'Control :', sum(temp2), ' ', sum(temp2) / length(temp2), '%') )
#   
#   print('Education : >15')
#   temp1 = temp_Case_Data$Education > 6
#   temp2 = temp_Control_Data$Education > 6
#   print(paste(MD, 'Case :', sum(temp1), ' ', sum(temp1) / length(temp1), '%') )
#   print(paste(MD, 'Control :', sum(temp2), ' ', sum(temp2) / length(temp2), '%') )
#   
#   # duration
#   print('duration : -10~-5')
#   temp1 = temp_Case_Data$duration < -5
#   temp2 = temp_Control_Data$duration < -5
#   print(paste(MD, 'Case :', sum(temp1), ' ', sum(temp1) / length(temp1), '%') )
#   print(paste(MD, 'Control :', sum(temp2), ' ', sum(temp2) / length(temp2), '%') )
#   
#   print('duration : -5~0')
#   temp1 = temp_Case_Data$duration < 0 & temp_Case_Data$duration >= -5
#   temp2 = temp_Control_Data$duration < 0 & temp_Control_Data$duration >= -5
#   print(paste(MD, 'Case :', sum(temp1), ' ', sum(temp1) / length(temp1), '%') )
#   print(paste(MD, 'Control :', sum(temp2), ' ', sum(temp2) / length(temp2), '%') )
#   
#   print('duration : 0~5')
#   temp1 = temp_Case_Data$duration < 5 & temp_Case_Data$duration >= 0
#   temp2 = temp_Control_Data$duration < 5 & temp_Control_Data$duration >= 0
#   print(paste(MD, 'Case :', sum(temp1), ' ', sum(temp1) / length(temp1), '%') )
#   print(paste(MD, 'Control :', sum(temp2), ' ', sum(temp2) / length(temp2), '%') )
#   
#   print('duration : 5~10')
#   temp1 = temp_Case_Data$duration <= 10 & temp_Case_Data$duration >= 5
#   temp2 = temp_Control_Data$duration <= 10 & temp_Control_Data$duration >= 5
#   print(paste(MD, 'Case :', sum(temp1), ' ', sum(temp1) / length(temp1), '%') )
#   print(paste(MD, 'Control :', sum(temp2), ' ', sum(temp2) / length(temp2), '%') )
#   
# }












