rm(list = ls())
Work_path = '/Users/panguoqing/Desktop/help/project'
Save_path = paste(Work_path, 'Data/Mental_assessments', sep = '/')
library(ggplot2)
library(reshape2)
library(dplyr)
library(cluster)
library(NbClust)
library(RColorBrewer)

color = c('darkred', 'chocolate3', 'chartreuse3', 
          'deepskyblue3', 'purple3','cyan4',
          'navajowhite2', 'navajowhite4',
          'seagreen3','burlywood3','coral3','violetred2',
          'cyan3','magenta3','maroon','mediumorchid4','cadetblue2')
MD_score_lst = c("PHQ4", "neuroticism")#"anxiety",	"mania",	"wellbeing",	"psychotic_experience",	"selfharm",	"mental_distress",	"trauma",	"depressnew", 

zdata_all = data.frame()
for (MD in c("AX", "BIP", "DEP", "SCH")) {
  zdata = read.csv(paste(Save_path, MD, 'zdata_mental.csv', sep = '/'))
  zdata1 = zdata[, c("duration", MD_score_lst)]
  temp_save_path = paste(Save_path, MD, 'Figures', sep = '/')
  if (!dir.exists(temp_save_path)) {
    dir.create(temp_save_path, recursive = TRUE)
  }
  
  setwd(temp_save_path)
  for (i in MD_score_lst) {
    zdata_plot_melt = zdata1[,c("duration", i)]
    zdata_plot_melt = zdata_plot_melt[complete.cases(zdata_plot_melt[,2]),]
    colnames(zdata_plot_melt)[2] = "value"
    p <- ggplot(data = zdata_plot_melt, aes(x = duration, y = value)) +
      labs(x = "Years to Diagnosis", y = " ") +
      scale_x_continuous(limits = c(-10, 10), breaks = seq(-10, 10, by = 2)) +
      geom_smooth(aes(color = MD), method = 'loess', span = 2, se = FALSE, linewidth = 8, show.legend = FALSE) +
      theme_classic() +
      theme(
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        panel.spacing = unit(c(0, 0, 0, 0), "cm"),
        axis.text.x = element_text(color = "black", size = 20, face = "bold"),
        axis.text.y = element_text(color = "black", size = 20, face = "bold"),
        axis.title.x = element_text(color = "black", size = 35, face = "bold"),
        axis.title.y = element_text(color = "black", size = 35, face = "bold"),
        axis.ticks.x = element_line(colour = "black", linewidth = 1.2),
        axis.ticks.y = element_line(colour = "black", linewidth = 1.2),
        axis.ticks.length.x = unit(0.4, 'cm'),
        axis.ticks.length.y = unit(0.4, 'cm'),
        axis.line.x = element_line(linewidth = 2),
        axis.line.y = element_line(linewidth = 2)
      ) +
      scale_color_brewer(palette = "Set1")
    #p_cluster
    ggsave(paste(i , ".pdf",sep = ""), width = 10, height = 8)
  }
}







