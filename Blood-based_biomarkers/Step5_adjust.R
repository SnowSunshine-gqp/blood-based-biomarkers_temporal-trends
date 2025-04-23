rm(list = ls())
Project_path = '/Users/panguoqing/Desktop/help/project'
Save_path = paste(Project_path, 'Data/CaseControl', sep = '/')

MD = "SCH" # c('AX', 'DEP', 'BIP', 'SCH')
data_FieldID = read.csv( paste(Save_path, 'data_FieldID.csv', sep = "/"))
clustered_data = read.csv(paste(Save_path, "S6_cluster_clustra", paste('S62_cluster_data_', MD, '.csv', sep = ''), sep = "/"))
Cluster_N = max(clustered_data$cluster)

# 调整聚类顺序，将类似的曲线构造放一起
if (MD == 'AX'){
  temp = clustered_data$cluster
  Adjust_cluster = NA
  Adjust_subgroup = NA 
  # 类别一
  Adjust_subgroup[temp == 4 | temp == 3] = 1
  Adjust_cluster[temp == 4 ] = 1
  Adjust_cluster[temp == 3 ] = 2
  # 类别二
  Adjust_subgroup[temp == 5 ] = 2
  Adjust_cluster[temp == 5 ] = 3
  # 类别三
  Adjust_subgroup[temp == 1 | temp == 2] = 3
  Adjust_cluster[temp == 1 ] = 4
  Adjust_cluster[temp == 2 ] = 5
  
  clustered_data$Adjust_cluster = Adjust_cluster
  clustered_data$Adjust_subgroup = Adjust_subgroup
}

if (MD == 'DEP'){  
  temp = clustered_data$cluster
  Adjust_cluster = NA
  Adjust_subgroup = NA
  # 类型一
  Adjust_subgroup[temp == 1 | temp == 5 ] = 1
  Adjust_cluster[temp == 1 ] = 1
  Adjust_cluster[temp == 5 ] = 2
  # 类型二
  Adjust_subgroup[temp == 6 | temp == 4 ] = 2
  Adjust_cluster[temp == 6 ] = 3
  Adjust_cluster[temp == 4 ] = 4
  # 类型三
  Adjust_subgroup[temp == 7 | temp == 8] = 3
  Adjust_cluster[temp == 7 ] = 5
  Adjust_cluster[temp == 8 ] = 6
  # 类型四
  Adjust_subgroup[temp == 3 ] = 4
  Adjust_cluster[temp == 3 ] = 7
  # 类型五
  Adjust_subgroup[temp == 2 | temp == 9 ] = 5
  Adjust_cluster[temp == 2 ] = 8
  Adjust_cluster[temp == 9 ] = 9
  
  clustered_data$Adjust_cluster = Adjust_cluster
  clustered_data$Adjust_subgroup = Adjust_subgroup
}

if (MD == 'BIP'){
  temp = clustered_data$cluster
  Adjust_cluster = NA
  Adjust_subgroup = NA
  # 类型一
  Adjust_subgroup[temp == 3 ] = 1
  Adjust_cluster[temp == 3 ] = 1
  # 类型二
  Adjust_subgroup[temp == 1 ] = 2
  Adjust_cluster[temp == 1 ] = 2
  # 类型三
  Adjust_subgroup[temp == 4] = 3
  Adjust_cluster[temp == 4 ] = 3
  # 类型四
  Adjust_subgroup[temp == 2] = 4
  Adjust_cluster[temp == 2 ] = 4
  
  clustered_data$Adjust_cluster = Adjust_cluster
  clustered_data$Adjust_subgroup = Adjust_subgroup
}

if (MD == 'SCH'){
  temp = clustered_data$cluster
  Adjust_cluster = NA
  Adjust_subgroup = NA
  # 类型一
  Adjust_subgroup[temp == 3 | temp == 4] = 1
  Adjust_cluster[temp == 3 ] = 1
  Adjust_cluster[temp == 4 ] = 2
  # 类型二
  Adjust_subgroup[temp == 1 | temp == 2] = 2
  Adjust_cluster[temp == 1 ] = 3
  Adjust_cluster[temp == 2 ] = 4
  
  clustered_data$Adjust_cluster = Adjust_cluster
  clustered_data$Adjust_subgroup = Adjust_subgroup
}
write.csv(clustered_data, paste(Project_path, paste('Data/CaseControl/S6_cluster_clustra/Adjust_cluster_data_', MD, '.csv', sep = ''), sep = '/'), row.names = F)

