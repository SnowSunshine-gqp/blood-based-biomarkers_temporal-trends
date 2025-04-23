rm(list = ls())
Project_path = '/Users/panguoqing/Desktop/help/project'
Save_path = paste(Project_path, 'Data/CaseControl', sep = '/')
library(ggplot2)
library(reshape2)
library(dplyr)
library(cluster)
library(NbClust)
library(ggrepel)
loess_predict_all = read.csv(paste(Project_path, "Data/CaseControl/S5_loess_predict_all.csv", sep = '/'))
loess_predict_all = loess_predict_all[,-1]
MD_index <- sapply(strsplit(colnames(loess_predict_all),'_'), `[`, 2)

color_ = c('darkred', 'chocolate3', 'chartreuse3', 
           'deepskyblue3', 'purple3','cyan4', 'navajowhite4',
           'seagreen3','coral3','violetred2',
           'cyan3','magenta3','maroon','mediumorchid4','cadetblue2')
MD = "AX" # c('AX', 'DEP', 'BIP', 'SCH')

data_FieldID = read.csv( paste(Save_path, 'data_FieldID.csv', sep = "/"))
clustered_data = read.csv(paste(Save_path, "S6_cluster_clustra", paste('Adjust_cluster_data_', MD, '.csv', sep = ''), sep = "/"))
Cluster_N = max(clustered_data$cluster)

temp_dir = paste(Save_path, MD, "trend_cluster_clustra", 'Adjust', sep = "/")
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir, recursive = TRUE)  # 如果路径中包含多级目录，需要设置recursive = TRUE
}

temp_data = loess_predict_all[,sapply(MD_index == MD, isTRUE)]
temp_name = colnames(temp_data)
for (n in (1:length(temp_name))) {
  temp_name[n] = sub('X', '', temp_name[n])
  temp_name[n] = sub('\\.', '\\-', temp_name[n])
}
temp_name = sapply(strsplit(temp_name,'_'), `[`, 1)
match_indices <- match(temp_name, data_FieldID$FieldID)
colnames(temp_data) <- ifelse(is.na(match_indices), temp_name, data_FieldID$Abbreviation[match_indices])

temp_data$duration = seq(-10,10,by=0.1)

setwd(temp_dir)
for (n1 in c(1:Cluster_N)) {

  data_plot_cluster = temp_data[colnames(temp_data) %in% c("duration", clustered_data$name[clustered_data$Adjust_cluster == n1])]
  temp_color = color_[unique(clustered_data$Adjust_subgroup[clustered_data$Adjust_cluster == n1])]
  
  data_plot_melt = reshape2::melt(data_plot_cluster, id = 'duration')
  data_plot_melt = data_plot_melt[complete.cases(data_plot_melt[,3]),]
  data_plot_melt <- data_plot_melt %>%
    group_by(variable) %>%
    mutate(label_x = max(duration), label_y = value[which.max(duration)]) %>%
    ungroup()
  
  data_plot_melt[data_plot_melt$duration == 0,]

  p_cluster <- ggplot() +
    geom_line(stat="smooth", data=data_plot_melt, aes(x=duration, y=value, group = variable), 
              method = 'loess', span = 2, se = FALSE, color = temp_color, linewidth = 1, alpha = 0.4) +
    labs(x="Years to Diagnosis", y="Z-Score", title = paste('Custer', n1, sep = " ")) +
    scale_x_continuous(limits=c(-10,11), breaks = seq(-10, 10, by = 2)) +
    guides(color=guide_legend(title='Field')) +
    geom_smooth(data=data_plot_melt, aes(x=duration, y=value), method = 'loess', span = 2, se = FALSE, 
                color = temp_color, linewidth = 6, show.legend = F) +
    theme_classic() +
    theme(plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"),
          panel.spacing =unit(c(0,0,0,0), "cm"),
          axis.text.x = element_text(color="black", size=20, face = "bold"),
          axis.text.y = element_text(color="black", size=20, face = "bold"),
          axis.title.x = element_text(color="black", size=30, face = "bold"),
          axis.title.y = element_text(color="black", size=30, face = "bold"),
          axis.ticks.x = element_line(colour = "black", size = 1),
          axis.ticks.y = element_line(colour = "black", size = 1),
          axis.ticks.length.x = unit(0.4,'cm'),
          axis.ticks.length.y = unit(0.4,'cm'),
          axis.line.x=element_line(size=2),
          axis.line.y=element_line(size=2),
          legend.position="none",
          plot.title = element_text(color="black", hjust = 0.5, size=30, face = "bold")
          ) #+
    # geom_text(data = data_plot_melt, aes(x = label_x, y = label_y, label = variable), hjust = -0.1, size = 7, color = "black") 
  
  ggsave(paste(MD, '_cluster', n1, ".pdf", sep = ""), width = 8, height = 7)
}



# 
# setwd(temp_dir)
# for (n1 in c(1:Cluster_N)) {
#   temp_name = colnames(temp_data)
#   for (n2 in (1:length(temp_name))) {
#     temp_name[n2] = sub('X', '', temp_name[n2])
#     temp_name[n2] = sub('\\.', '\\-', temp_name[n2])
#   }
#   temp_name = sapply(strsplit(temp_name,'_'), `[`, 1)
#   match_indices <- match(temp_name, data_FieldID$FieldID)
#   colnames(temp_data) <- ifelse(is.na(match_indices), temp_name, data_FieldID$Abbreviation[match_indices])
# 
#   temp_data$duration = seq(-10,10,by=0.1)
#   data_plot_cluster = temp_data[colnames(temp_data) %in% c("duration",clustered_data$X[clustered_data$cluster == n1])]
#   # temp_color = color_[unique(clustered_data$Adjust_subgroup[clustered_data$cluster == n1])]
# 
#   data_plot_melt = melt(data_plot_cluster, id = 'duration')
#   data_plot_melt = data_plot_melt[complete.cases(data_plot_melt[,3]),]
# 
#   p_cluster <- ggplot() +
#     geom_line(stat="smooth", data=data_plot_melt, aes(x=duration, y=value, color = variable), method = 'loess', span = 2, se = FALSE, linewidth = 1, alpha = 0.4) +
#     labs(x="Years to Diagnosis", y="Z-Score", title = paste(MD, 'cluster', n1, sep = " ")) +
#     scale_x_continuous(limits=c(-10,10), breaks = seq(-10, 10, by = 2)) +
#     guides(color=guide_legend(title='Field')) +
#     geom_smooth(data=data_plot_melt, aes(x=duration, y=value), method = 'loess', span = 2, se = FALSE, linewidth = 6, show.legend = F) +
#     theme_classic() +
#     theme(plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"),
#           panel.spacing =unit(c(0,0,0,0), "cm"),
#           axis.text.x = element_text(color="black", size=20, face = "bold"),
#           axis.text.y = element_text(color="black", size=20, face = "bold"),
#           axis.title.x = element_text(color="black", size=20, face = "bold"),
#           axis.title.y = element_text(color="black", size=20, face = "bold"),
#           axis.ticks.x = element_line(colour = "black", size = 1.2),
#           axis.ticks.y = element_line(colour = "black", size = 1.2),
#           axis.ticks.length.x = unit(0.4,'cm'),
#           axis.ticks.length.y = unit(0.4,'cm'),
#           axis.line.x=element_line(size=2),
#           axis.line.y=element_line(size=2),
#           legend.position="none",
#           plot.title = element_text(color="black", hjust = 0.5, size=20, face = "bold"))
# 
#   ggsave(paste(MD, '_cluster', n1, ".pdf", sep = ""), width = 10, height = 8)
# }
# 
# 
