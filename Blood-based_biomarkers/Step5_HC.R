rm(list = ls())
library(ggplot2)
library(cluster)
library(ggdendro)
library(dendextend)
library(factoextra)
Project_path = '/Users/panguoqing/Desktop/help/project'
Save_path = paste(Project_path, 'Data/CaseControl', sep = '/')


data_FieldID = read.csv(paste(Project_path, 'Data/CaseControl/data_FieldID.csv', sep = '/'))

loess_predict_all = read.csv(paste(Project_path, "Data/CaseControl/S5_loess_predict_all.csv", sep = '/'))
colnames(loess_predict_all)[1] = 'time'

MD_list = list('AX' = c(2:40), 'DEP' = c(41:95), 'BIP' = c(96:101), 'SCH' = c(102:113))
loess_predict_all$time = seq(-10, 10, by = 0.1)

for (MD in c('AX', 'DEP', 'BIP', 'SCH') ){
  loess_predict = loess_predict_all[,MD_list[[MD]]]
  Sig_n = length(colnames(loess_predict))
  tmp = colnames(loess_predict)
  for (i in (1:Sig_n)) {
    tmp[i] = sub('X', '', tmp[i])
    tmp[i] = sub('\\.', '\\-', tmp[i])
  }
  colnames(loess_predict) = tmp
  
  tmp = colnames(loess_predict)
  dmp = tmp
  cmp = tmp
  hmp = tmp
  for (i in (1:Sig_n)) {
    tmp[i] = substr(tmp[i], 1, 9)
    dmp[i] = substring(cmp[i], 10)
    tmp[i] = data_FieldID$Abbreviation[data_FieldID$FieldID == tmp[i]]
    hmp[i] = paste(tmp[i],dmp[i], sep = "")
  }
  colnames(loess_predict) = hmp
  
  
  
  data = matrix(unlist(loess_predict[-1,]), nrow = length(colnames(loess_predict)) , byrow = T)
  # 1. 计算距离矩阵
  euclidean_dist <- dist(data, method = "euclidean")
  # 2. 执行层次聚类（完全连接法）
  hc_result <- hclust(euclidean_dist, method = 'complete' )
  hc_result$labels = tmp
  
  temp_path = paste(Project_path, 'Data/CaseControl/S6_cluster_clustra/figures', sep = '/')
  setwd(temp_path)
  if (MD == 'AX'){
    p = fviz_dend(hc_result, 
                  h = 0.7, 
                  cex=0.7, 
                  horiz=F,                         # 垂直摆放图形
                  show_labels = T,
                  # xlab = tmp,
                  ylab = "Height",            # y 轴标签
                  labels_track_height = 0.7,
                  color_labels_by_k=TRUE,              # 自动设置数据标签颜色
                  lwd=0.3,                             # 设置分支和矩形的线宽
                  type="rectangle",                    # 设置绘图类型为矩形
                  rect=T,                           # 使用不同的颜色矩形标记类别
                  main= MD)
    
    ggsave(paste(MD, "_dendrogram.pdf", sep = ''), p, width = 8, height = 5)
    
    clusters <- cutree(hc_result, h = 0.68)
    cluster_data = data.frame(
      name = sub(paste('_', MD, sep = ''), '', hmp),
      cluster = clusters
    )
    
  }
  
  if (MD == 'DEP'){
    p = fviz_dend(hc_result, 
                  h = 0.65, 
                  cex=0.7, 
                  horiz=F,                         # 垂直摆放图形
                  show_labels = T,
                  # xlab = tmp,
                  ylab = "Height",            # y 轴标签
                  labels_track_height = 0.8,
                  color_labels_by_k=TRUE,              # 自动设置数据标签颜色
                  lwd=0.3,                             # 设置分支和矩形的线宽
                  type="rectangle",                    # 设置绘图类型为矩形
                  rect=T,                           # 使用不同的颜色矩形标记类别
                  main= MD)
    
    ggsave(paste(MD, "_dendrogram.pdf", sep = ''), p, width = 10, height = 5)
    
    clusters <- cutree(hc_result, h = 0.6)
    cluster_data = data.frame(
      name = sub(paste('_', MD, sep = ''), '', hmp),
      cluster = clusters
    )
  }
  
  if (MD == 'BIP'){
    temp_cut = 1.4
    p = fviz_dend(hc_result, 
                  h = temp_cut, 
                  cex=0.7, 
                  horiz=F,                         # 垂直摆放图形
                  show_labels = T,
                  # xlab = tmp,
                  ylab = "Height",            # y 轴标签
                  labels_track_height = 1.5,
                  color_labels_by_k=TRUE,              # 自动设置数据标签颜色
                  lwd=0.3,                             # 设置分支和矩形的线宽
                  type="rectangle",                    # 设置绘图类型为矩形
                  rect=T,                           # 使用不同的颜色矩形标记类别
                  main= MD)
    
    ggsave(paste(MD, "_dendrogram.pdf", sep = ''), p, width = 6, height = 5)
    
    clusters <- cutree(hc_result, h = 1.35)
    cluster_data = data.frame(
      name = sub(paste('_', MD, sep = ''), '', hmp),
      cluster = clusters
    )
  }
  
  if (MD == 'SCH'){
    p = fviz_dend(hc_result, 
                  h = 2, 
                  cex=0.7, 
                  horiz=F,                         # 垂直摆放图形
                  show_labels = T,
                  # xlab = tmp,
                  labels_track_height = 1.5,
                  ylab = "Height",            # y 轴标签
                  color_labels_by_k=TRUE,              # 自动设置数据标签颜色
                  lwd=0.3,                             # 设置分支和矩形的线宽
                  type="rectangle",                    # 设置绘图类型为矩形
                  rect=T,                           # 使用不同的颜色矩形标记类别
                  main= MD)
    
    ggsave(paste(MD, "_dendrogram.pdf", sep = ''), p, width = 5, height = 5)
    
    clusters <- cutree(hc_result, h = 1.5)
    cluster_data = data.frame(
      name = sub(paste('_', MD, sep = ''), '', hmp),
      cluster = clusters
    )
  }
  
  write.csv(cluster_data,paste(Project_path, paste('Data/CaseControl/S6_cluster_clustra/S62_cluster_data_', MD, '.csv', sep = ''), sep = '/'), row.names = F)
}










