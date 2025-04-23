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

for (n in seq(length(MD_list))){
  MD = MD_list[n]
  temp_data = read.csv(paste(Project_path, 'Data/Mental_assessments', MD, 'case_control_match_eid_yrs.csv', sep = '/'))
  colnames(temp_data)[2] = 'duration'
  temp_Case = temp_data[temp_data$group == 1,]
  
  temp_Case_Age = merge(temp_Case, Data_cov[, c('eid', 'Age')], by = 'eid')
  temp_Case_Age$gap = floor(temp_Case_Age$duration ) %/% 2
  temp_Case_Age$gap[temp_Case_Age$duration == 10] = 4
  temp_Case_Age = temp_Case_Age[complete.cases(temp_Case_Age$gap),]
  
  temp_Case_Age$Timeframe = TT$Timeframe[match(temp_Case_Age$gap, TT$indx)]
  
  # 创建图表
  p <- ggplot(temp_Case_Age, aes(x = Timeframe, y = Age, fill = Timeframe)) +
    geom_boxplot() +  # 添加箱线图
    theme_minimal() +  # 使用简洁的主题
    labs(x = "Timeframe", y = "Age", title = MD_name[n]) +  # 添加标签
    theme(
      plot.title = element_text(hjust = 0.5),  # 居中标题
      axis.title.x = element_text(face = "bold"),  # 加粗 x 轴标题
      axis.title.y = element_text(face = "bold")  # 加粗 y 轴标题
    ) +
    # scale_fill_brewer(palette = "palette")+  # 更改填充颜色
    theme_classic() +
    theme(plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"),
          panel.spacing =unit(c(0,0,0,0), "cm"),
          axis.text.x = element_text(color="black", size=15, face = "bold"),
          axis.text.y = element_text(color="black", size=15, face = "bold"),
          axis.title.x = element_text(color="black", size=20, face = "bold"),
          axis.title.y = element_text(color="black", size=20, face = "bold"),
          axis.ticks.x = element_line(colour = "black", size = 1),
          axis.ticks.y = element_line(colour = "black", size = 1),
          axis.ticks.length.x = unit(0.4,'cm'),
          axis.ticks.length.y = unit(0.4,'cm'),
          axis.line.x=element_line(size=2),
          axis.line.y=element_line(size=2),
          legend.position="none",
          plot.title = element_text(color="black", hjust = 0.5, size=20, face = "bold")
    )
  # 保存图表
  ggsave(paste(Project_path, 'Data/Mental_assessments/Figures', paste('age_distribution', MD, '.pdf',sep = '_'), sep = '/'),
         plot = p, width = 8, height = 6)
}



