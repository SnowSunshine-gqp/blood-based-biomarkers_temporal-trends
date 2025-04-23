rm(list = ls())
library(ggplot2)
Work_path = '/Users/panguoqing/Desktop/help/project'
Save_path = paste(Work_path, 'Data/CaseControl', sep = '/')

for (MD in c("AX", "DEP", "BIP", "SCH")){ #c("AX", "DEP", "BIP", "SCH")
  temp_dir = paste(Save_path, MD, 'Case_Control_loess', sep = '/')
  setwd(temp_dir)
  loess_predict_melt = read.csv(paste(temp_dir, 'loess_predict_melt.csv', sep = '/'))
  loess_predict_melt = loess_predict_melt[,-1]
  temp_name = unique(loess_predict_melt$variable)
  for (j in seq(temp_name)){
    temp_loess_predict_melt = loess_predict_melt[loess_predict_melt$variable == temp_name[j], ]
    
    p_cluster <- ggplot() +
      geom_line(stat="smooth", data=temp_loess_predict_melt, aes(x=duration, y=value, group = Group, colour = Group), 
                method = 'loess', span = 2, se = FALSE, linewidth = 1, alpha = 0.4) +
      labs(x="Years to Diagnosis", y=temp_name[j]) +
      guides(color=guide_legend(title='Field')) +
      geom_smooth(data=temp_loess_predict_melt, aes(x=duration, y=value, group = Group, colour = Group), method = 'loess', span = 2, se = FALSE,
                  linewidth = 6, show.legend = T) +
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
      )
    
    ggsave(paste(temp_name[j], ".pdf", sep = ""), width = 8, height = 7)
    
  }
  
}

