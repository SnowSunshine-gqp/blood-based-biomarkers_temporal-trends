rm(list = ls())
Project_path = '/Users/panguoqing/Desktop/help/project'
Work_path = paste(Project_path, 'Data/Neuroimaging', sep = '/')
Save_path = paste(Project_path, 'Data/Brain_Corr', sep = '/')
#提取脑区数据
library(stringr)
library(reshape2)
library(dplyr)
library(ggplot2)
library(Hmisc)

Regions = read.csv(paste(Work_path,"mddadult_case-controls_CortThick.csv",sep = "/"))
Regions_lst = Regions$Structure
for (i in c(1: length(Regions_lst))) {
  Regions_lst[i] = substr(Regions_lst[i],3,nchar(Regions_lst[i]))
}

# 服务器提取的DKT volume 的数据
Volume = read.csv(paste(Work_path,"Freesurfer_DKT_bl_data.csv",sep = "/"))
Volume_Field_lst = colnames(Volume)[2:length(colnames(Volume))]
for (i in c(1:length(Volume_Field_lst))) {
  Volume_Field_lst[i] = substr(Volume_Field_lst[i],2,6)
}
Volume_Field_lst = c('eid', Volume_Field_lst)

# 分析需要使用的 DKT volume 的field id数据
Field = read.csv(paste(Work_path,"Freesurfer DKT.csv",sep = "/"))
Field = Field[c(125:186,233:236,247:248,258:259,262:263,265:268,272:273),] # 限制在DKT volume 中
Field_lst = Field$Description
Field_lst_hemisphere = Field_lst
for (i in c(1:length(Field_lst))) {
  temp = str_split(Field_lst[i],' ',simplify = T)
  Field_lst[i] = temp[1,3]
  Field_lst_hemisphere[i] = if_else(substr(temp[1, 4],2,2) == 'r','Right', 'Left')
}
Field$Description = Field_lst
Field$hemisphere = Field_lst_hemisphere

# 储存需要分析的 DKT volume 的数据
Volume_of_DKT <- data.frame(
  eid = Volume$eid,
  TIV = Volume$x26521_2_0
)
temp_indx = Volume_Field_lst %in% Field$Field.ID
Volume_of_DKT = cbind(Volume_of_DKT, Volume[temp_indx])
colnames(Volume_of_DKT)[3:length(colnames(Volume_of_DKT))] = Volume_Field_lst[temp_indx]
# 匹配Volume_of_DKT 的数据信息
Volume_of_DKT_name <- data.frame(Field = colnames(Volume_of_DKT))
Volume_of_DKT_name$Description <- Field$Description[match(Volume_of_DKT_name$Field, Field$Field.ID)]
Volume_of_DKT_name$hemisphere <- Field$hemisphere[match(Volume_of_DKT_name$Field, Field$Field.ID)]
# 纳入考虑的脑区名称
Regions_name = unique(Volume_of_DKT_name$Description[3:length(Volume_of_DKT_name$Description)])
# 左右半脑同一个脑区取平均值
Volume_of_DKT_mean = as.data.frame(matrix(nrow = nrow(Volume_of_DKT), ncol = 2 + length(Regions_name)))
Volume_of_DKT_mean[, c(1,2)] = Volume_of_DKT[,c('eid', 'TIV')]
for (i in seq(length(Regions_name)) ){
  region = Regions_name[i]
  temp_indx = sapply(Volume_of_DKT_name$Description == region, isTRUE)
  Volume_of_DKT_mean[,2 + i] = rowMeans(Volume_of_DKT[,temp_indx])
}
colnames(Volume_of_DKT_mean) = c('eid', 'TIV', Regions_name)
write.csv(x = Volume_of_DKT_mean, 'Step1', file = paste(Save_path, 'Volume_of_DKT_mean.csv', sep = '/'))


