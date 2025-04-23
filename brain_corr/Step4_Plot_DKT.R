rm(list = ls())
library(dplyr)
library(ggplot2)
Project_path = '/Users/panguoqing/Desktop/help/project'
Work_path = paste(Project_path, 'Data/Brain_Corr/Step3', sep = '/')
Save_path = paste(Project_path, 'Data/Brain_Corr/Step4', sep = '/')
Filed_ID = read.csv(paste(Project_path, 'Data/CaseControl/data_FieldID.csv', sep = '/'))
Region_name = read.csv(paste(Project_path, 'Data/Brain_Corr/Region_name.csv', sep = '/'))
Threshold = 0.05

Corr_Result_Total = data.frame()
for (MD in c("AX", "BIP", "DEP", "SCH")){
  temp_Save_path = paste(Work_path, MD, sep = '/')
  Corr_Result = read.csv(paste(temp_Save_path, 'Step3_DKT_Corr_Result_X_group.csv', sep = '/'))
  Corr_Result = Corr_Result[,-1]
  Corr_Result$BL = sub(x = Corr_Result$BL, pattern = 'X', replacement = '')
  Corr_Result$BL = sub(x = Corr_Result$BL, pattern = '\\.', replacement = '-')
  Corr_Result$BL = Filed_ID$Abbreviation[match(Corr_Result$BL, table = Filed_ID$FieldID_full)]
  
  Corr_Result$BFI_P = NA
  Regions = unique(Corr_Result$Region)
  SigBL = unique(Corr_Result$BL)
  # 进行bonferroni校正
  for (region in Regions){
    temp_indx = Corr_Result$Region == region
    Corr_Result$BFI_P[temp_indx] = p.adjust(Corr_Result$Pvalue[temp_indx], method = "bonferroni")
  }
  
  # 删去不显著的指标
  del_index <- rep(FALSE, length(Corr_Result$BL))
  for (bl in SigBL){
    temp_indx = Corr_Result$BL == bl
    if (any(Corr_Result$BFI_P[temp_indx] < Threshold) ){
      del_index[temp_indx] = T
    }
  }
  Corr_Result = Corr_Result[del_index,]
  if (nrow(Corr_Result)){
    Corr_Result$BL = paste(MD, Corr_Result$BL, sep = ':')
    Corr_Result_Total = rbind(Corr_Result_Total, Corr_Result)
  }
}

Region_name$New = factor(Region_name$New, levels = rev(Region_name$New))

Corr_Result_Total$Region = Region_name$New[match(Corr_Result_Total$Region, Region_name$Ori)]

Corr_Result_Total <- Corr_Result_Total %>% mutate(text = 
                                                    case_when(0.01 < BFI_P & BFI_P < 0.05 ~ paste("*"), 
                                                              0.005 < BFI_P & BFI_P <= 0.01 ~ paste("**"),
                                                              BFI_P <= 0.005 ~ paste("***")
                                                    ))

ggplot(Corr_Result_Total, aes(BL, Region)) + 
  geom_tile(aes(fill = Tvalue), colour = "grey", size = 0.5)+
  scale_fill_gradient2(low = "#0f86a9",mid = "white",high = "#FC8452") +
  geom_text(aes(label=text),col ="black",size = 5,vjust = 0.8) +
  theme_minimal() + 
  theme(axis.title.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(), 
        axis.text.x = element_text(hjust = 1, size = 15, face = "bold",angle=45), 
        axis.text.y = element_text(size = 16, face = "bold", color = 'black'),
        legend.text = element_text( size = 16, face = "bold", color = 'black'),
        legend.title = element_text( size = 16, face = "bold", color = 'black')) + 
  labs( fill = paste0(" * p_bfi < 0.05", "\n\n", 
                      "** p_bfi < 0.01", "\n\n",
                      "*** p_bfi < 0.005", "\n\n",
                     "beta")) 

temp_Save_path = paste(Save_path, sep = '/')
setwd(temp_Save_path)
ggsave(paste('DKT_Result_X_group of ', ".pdf", sep = ""), width = 10, height = 10)

