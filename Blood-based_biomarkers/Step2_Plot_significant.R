rm(list = ls())
# 加载必要的包
library(openxlsx)
library(tidyr)
Work_path = '/Users/panguoqing/Desktop/help/project'
Save_path = paste(Work_path, 'Data/CaseControl', sep = '/')

data_FieldID = read.csv(paste(Save_path, 'data_FieldID.csv', sep = "/"))
Threshold = 0.01

# 初始化Excel文件路径
output_file <- paste(Save_path, 'combined_results.xlsx', sep = "/")

# 创建一个新的Excel工作簿
wb <- createWorkbook()

# 循环处理每个MD
for (MD in c("AX", "BIP", "DEP", "SCH")) {
  # 读取数据
  data <- read.csv(paste(Save_path, MD, 'Pvalue_beta.csv', sep = "/"))
  data$FieldID_full <- sub('_', '.', data$FieldID_full)
  data <- data[, c("FieldID_full", "beta_group", "p_group_bfi", "beta_group_time", "p_group_time_bfi", "beta_group_time2", "p_group_time2_bfi")]
  
  # 合并数据
  data_FieldID_beta_p <- merge(data_FieldID, data, by = 'FieldID_full')
  
  # 筛选显著性结果
  temp_indx <- data_FieldID_beta_p$p_group_bfi < Threshold | 
    data_FieldID_beta_p$p_group_time_bfi < Threshold | 
    data_FieldID_beta_p$p_group_time2_bfi < Threshold
  
  temp_result <- data_FieldID_beta_p[temp_indx, ]
  
  # 将结果写入工作簿，工作表名称为MD
  addWorksheet(wb, MD)
  writeData(wb, sheet = MD, temp_result)
}

# 保存工作簿到文件
saveWorkbook(wb, output_file, overwrite = TRUE)

cat("Results have been saved to", output_file, "\n")

