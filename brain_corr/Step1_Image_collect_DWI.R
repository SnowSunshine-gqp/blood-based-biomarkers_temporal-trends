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

DTI_field = read.csv(paste(Work_path, "Diffusion_MRI_weighted_means.csv",sep = "/"))
DTI = read.csv(paste(Work_path, "Diffusion_MRI_bl_data.csv",sep = "/"))
tmp = colnames(DTI)
for (i in c(2:length(tmp))) {
  tmp[i] = sub('x', '', tmp[i])
  tmp[i] = substr(tmp[i], 1, 5)
}
colnames(DTI) = tmp

DTI_field_FA = DTI_field[c(1:27),]
DTI_field_ICVF = DTI_field[c(28:54),]
DTI_field_MD = DTI_field[c(163:189),]

#添加缩写
for (i in c(1:27)) {
  a = substr(DTI_field_FA$Description[i],27,nchar(DTI_field_FA$Description[i]))
  b = str_split(a,' ',simplify = T)
  b<-unlist(b)
  c = list(substr(b, start = 1,stop = 1))
  d = unlist(c)
  e = ""
  for (j in c(1:length(d)-1)) {
    e = paste(e,d[j],sep = "")
  }
  e = toupper(e)
  e = paste("FA",e)
  DTI_field_FA[i,3] = e
}
colnames(DTI_field_FA)[3] = c("abbreviation")
DTI_field_FA[9,3] = "FA FMA"
DTI_field_FA[10,3] = "FA FMI"

for (i in c(1:27)) {
  a = substr(DTI_field_MD$Description[i],27,nchar(DTI_field_MD$Description[i]))
  b = str_split(a,' ',simplify = T)
  b<-unlist(b)
  c = list(substr(b, start = 1,stop = 1))
  d = unlist(c)
  e = ""
  for (j in c(1:length(d)-1)) {
    e = paste(e,d[j],sep = "")
  }
  e = toupper(e)
  e = paste("MD",e)
  DTI_field_MD[i,3] = e
}
colnames(DTI_field_MD)[3] = c("abbreviation")
DTI_field_MD[9,3] = "MD FMA"
DTI_field_MD[10,3] = "MD FMI"


for (i in c(1:27)) {
  a = substr(DTI_field_ICVF$Description[i],29,nchar(DTI_field_ICVF$Description[i]))
  b = str_split(a,' ',simplify = T)
  b<-unlist(b)
  c = list(substr(b, start = 1,stop = 1))
  d = unlist(c)
  e = ""
  for (j in c(1:length(d)-1)) {
    e = paste(e,d[j],sep = "")
  }
  e = toupper(e)
  e = paste("ICVF",e)
  DTI_field_ICVF[i,3] = e
}
colnames(DTI_field_ICVF)[3] = c("abbreviation")
DTI_field_ICVF[9,3] = "ICVF FMA"
DTI_field_ICVF[10,3] = "ICVF FMI"
DTI_field_all = rbind(DTI_field_FA, DTI_field_MD, DTI_field_ICVF)

#计算左右脑平均
unique3 = c("25498","25499","25504","25525","25526","25531","25660","25661","25666")
DTI_field_all1 = DTI_field_all[!DTI_field_all$Field.ID %in% unique3,]
DTI_all = DTI[,colnames(DTI) %in% DTI_field_all$Field.ID & !(colnames(DTI) %in% unique3)]
DTI_3 = DTI[,colnames(DTI) %in% unique3]
colnames(DTI_3) = c("FA FMA","FA FMI","FA MC","MD FMA","MD FMI", "MD MC", "ICVF FMA","ICVF FMI","ICVF MC")
DTI_mean1 = data.frame(matrix(nrow = 40747,ncol = 39))

for (i in seq(1,72,2)) {
  DTI_mean1[(i+1)/2]=(DTI_all[i]+DTI_all[i+1])/2
}
colnames(DTI_mean1) = unique(DTI_field_all1$abbreviation)
DTI_mean1 = cbind(DTI_mean1,DTI_3)
DTI_mean1 = DTI_mean1[, order(colnames(DTI_mean1))]
DTI_mean = cbind(DTI[1],DTI_mean1)
DTI_mean = DTI_mean[,1:46]
write.csv(x = DTI_mean, file = paste(Save_path, 'Step1', 'Volume_of_DTI_mean.csv', sep = '/'))

