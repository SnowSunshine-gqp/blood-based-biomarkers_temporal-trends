rm(list = ls())
Work_path = '/Users/panguoqing/Desktop/help/project'
Save_path = paste(Work_path, 'Data/CaseControl', sep = '/')

N = 63
time = seq(-10, 10, by = 0.1)
N_indx = length(time)
loess_predict_all = data.frame(matrix(nrow = N_indx, ncol = 0))
for (MD in c("AX","DEP","BIP","SCH")) {
  zdata = read.csv(paste(Work_path, 'Data/CaseControl', MD, 'S4_zdata_MD.csv',sep = '/'))
  zdata = zdata[zdata$group == 1,]
  tmp = colnames(zdata)[N : length(colnames(zdata))]
  for (i in (1 :length(tmp))) {
    tmp[i] = sub('X', '', tmp[i])
    tmp[i] = sub('\\.', '\\-', tmp[i])
  }
  colnames(zdata)[N : length(colnames(zdata))] = tmp
  colnames(zdata)[2] = 'duration'
  
  loess_predict = list()
  zdata_forloess = zdata[, c(2, N : length(colnames(zdata)))]
  temp_N = length(zdata_forloess)
  loess_predict_MD = data.frame(matrix(nrow = N_indx, ncol = temp_N -1))
  for(i in (2:temp_N)){
    print(paste(MD, i, sep = '-'))
    zdata_complete = zdata_forloess[complete.cases(zdata_forloess[,i]),]
    loess_variable <- loess(zdata_complete[,i] ~ duration, data=zdata_complete, span=2)
    smooth_variable <- predict(loess_variable, newdata = time)
    loess_predict_MD[i-1] = smooth_variable
    colnames(loess_predict_MD)[i-1] = paste(colnames(zdata_forloess)[i], MD, sep = '_')
    # smooth_variable <- data.frame(smooth_variable)
    # colnames(smooth_variable) = 
    # loess_predict_MD = cbind(loess_predict_MD, smooth_variable)
    
  }
  loess_predict_all = cbind(loess_predict_all, loess_predict_MD)
}
# loess_predict_all = select(loess_predict_all, -c('X31','X55','X7', 'X14'))
write.csv(loess_predict_all,
          paste(Save_path, 'S5_loess_predict_all.csv', sep = '/'))

