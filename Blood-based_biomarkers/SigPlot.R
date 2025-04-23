rm(list = ls())
library(ggplot2)
library(ggrepel)
library(ggbreak)
require(dplyr)
require(tidyr)
require(RColorBrewer)

Project_path = '/Users/panguoqing/Desktop/help/project'
Save_path = paste(Project_path, 'Data/CaseControl', sep = '/')
Threshold = 0.05
Threshold_1 = 0.01
Threshold_2 = 0.005
colors <- c('Blood count' = "#28A745",
            'Blood biochemistry' = 'chartreuse3', 
            "Amino acids" = "#17A2B8",
            "Apolipoproteins" = 'darkred', 
            "Cholesterol" = 'chocolate3', 
            "Cholesteryl esters" = 'deepskyblue3',
            "Fatty acids" = 'purple3',
            "Fluid balance" = 'cyan4', 
            "Free cholesterol" = 'navajowhite4',
            "Glycolysis related metabolites" = 'seagreen3',
            "Inflammation" = 'coral3',
            "Ketone bodies" = 'violetred2',
            "Lipoprotein particle concentrations" = 'cyan3',
            "Lipoprotein particle sizes" = 'magenta3',
            "Lipoprotein subclasses" = 'maroon',
            "Other lipids" = 'mediumorchid4',
            "Phospholipids" = 'cadetblue2',
            "Total lipids" = 'yellow2',
            "Triglycerides" = 'blue2',
            "Not Significant" = "grey30")

data_FieldID = read.csv(paste(Save_path, 'data_FieldID.csv', sep = '/'))
# unique(data_FieldID$new_Category)
Size_txt = c("AX" = 3,
             "DEP" = 3,
             "BIP" = 6,
             "SCH" = 6
)

setwd(Project_path)
plot_Data = data.frame()
for ( MD in c('AX', 'DEP', 'BIP', 'SCH')) {
  cluster_data = read.csv(paste(Save_path, 'S6_cluster_clustra', paste('Adjust_cluster_data_', MD, '.csv', sep = ''),  sep = '/'))
  
  temp_data = read.csv(paste(Save_path, MD, 'Pvalue_beta.csv', sep = '/'))
  temp_data = temp_data[, c('FieldID_full', 'beta_group', 'p_group_bfi', 'beta_group_time', 'p_group_time_bfi')]
  temp_data$FieldID_full = sub('_', '\\.', temp_data$FieldID_full)
  # temp_data = temp_data[temp_data$p_group_bfi < Threshold | temp_data$p_group_time_bfi < Threshold,]
  
  temp_data$Significant = !(temp_data$p_group_bfi > 0.05 & temp_data$p_group_time_bfi > 0.05)
  
  temp_data$Abbreviation = data_FieldID$Abbreviation[match(temp_data$FieldID_full, data_FieldID$FieldID_full)]
  temp_data$Name = paste(temp_data$Abbreviation , '|', MD, sep = '')
  temp_data$Domain = data_FieldID$new_Category[match(temp_data$FieldID_full, data_FieldID$FieldID_full)]
  # temp_data$Domain = ifelse(temp_data$Significant , temp_data$Domain, "Not Significant") #这里考虑legend修改位置
  temp_data$Text = ifelse(temp_data$Significant , as.character(temp_data$Abbreviation), "")
  
  size1 = -log10(temp_data$p_group_bfi)
  size2 = -log10(temp_data$p_group_time_bfi)
  TT = if_else(size1 > size2, size1, size2)
  temp_data$size =  if_else(!temp_data$Significant, 1, 3+5*( (TT - min(TT) ) / (max(TT) - min(TT) ) ) ) 
  
  temp_data_sorted <- temp_data[order(temp_data$Domain), ]
  temp_data_sorted$plotx = seq(1,nrow(temp_data_sorted)+10)[1:nrow(temp_data_sorted)]
  temp_Domains = unique(temp_data_sorted$Domain)
  # r_upper = min(temp_data$beta_group[temp_data$beta_group > 0])
  # r_lower = max(temp_data$beta_group[temp_data$beta_group < 0])
  # 
  p<-ggplot(temp_data_sorted, aes(x = plotx, y= beta_group)) + 
    # geom_hline(yintercept=r_upper, color='red', linewidth=0.4,linetype="longdash")+
    # geom_hline(yintercept=r_lower, color='red', linewidth=0.4,linetype="longdash")+
    geom_hline(yintercept=0, color="red", linewidth=1)+
    # geom_hline(yintercept=-log10(thr), color=ss[6], size=0.4,linetype="longdash")+
    geom_point(aes(colour = Domain, y = beta_group), size = temp_data_sorted$size)+
    #shape=Correlation_directions
    labs(color="Categories", 
         x="", 
         y=expression('Beta'),
         title = MD) +
    ggrepel::geom_text_repel( 
                             aes(label = Text),
                             colour="black", 
                             segment.colour="black",
                             size=Size_txt[MD], 
                             box.padding = unit(0.1, "inches"),
                             point.padding = unit(0.1, "inches"),
                             arrow = NULL,  # 创建箭头对象
                             max.time = 0.5,
                             max.overlaps = getOption("ggrepel.max.overlaps", default = 10),
                             min.segment.length = unit(0.1, "inches"))+
    scale_colour_manual(values = colors,
                        limits = temp_Domains )+
    guides(colour = guide_legend(override.aes = list(size = 6))) + # 设置颜色标注圈的大小
    theme_classic() + 
    theme(
      axis.title = element_text(face="bold", size=16),
      axis.line = element_line(color = "black", size=0.4), # 应用于x和y轴的线条
      axis.line.x = element_blank(), # x轴的线条不显示
      axis.line.y.left = element_line(color = "black", size=0.4), # y轴的线条显示
      axis.line.y.right = element_line(color = "white", size=0),
      axis.text.x = element_blank(), # x轴的文本标签不显示
      axis.ticks.x = element_blank(), # x轴的刻度不显示
      axis.text.y = element_text(color = "black", size=14), # y轴左侧的文本标签显示
      axis.ticks.y = element_line(color = "black"), # y轴左侧的刻度显示
      axis.title.y = element_text(angle = 90, vjust = 0.5), # y轴标题的旋转和垂直对齐
      legend.position = "none",
      legend.text = element_text(size=20),
      legend.title = element_text(size=20),
      legend.key.size = unit(0.3, "inches"),
      panel.grid.minor = element_blank(),
      # 隐藏右侧的y轴文本标签和刻度
      axis.text.y.right = element_blank(),
      axis.ticks.y.right = element_blank()
    ) #+
  # scale_y_continuous(
  #   breaks = seq(y_min, y_max, by = step_value),  # 设置刻度间隔
  #   limits = c(y_min, y_max), # 设置纵坐标的范围
  # )
  
  ggsave(paste(Save_path, 'Figures', paste(MD,'_Manhattan_map.pdf', sep = ''), sep = '/'), p, dpi = 500, width = 12, height = 8, units = "in")
  # ggsave(paste(Save_path, 'Figures', paste('legend.pdf', sep = ''), sep = '/'), p, dpi = 500, width = 12, height = 8, units = "in")
  
}
