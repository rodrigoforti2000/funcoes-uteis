#ROC curva
#Precisa do ggplot e AUC
ROC_ggplot = function(score,resp){
  roc_table = NULL
  auc_roc = round(auc(roc(score,resp)),3)
  for (i in seq(0,1,0.005)){
    conf_matrix = table(resp,score > i)
    tpr = conf_matrix[4]/(conf_matrix[4] + conf_matrix[2])
    fpr = conf_matrix[3]/(conf_matrix[1] + conf_matrix[3])
    roc_table = rbind(roc_table,c(tpr,fpr))
  }
  roc_table = as.data.frame(roc_table)
  roc_table = rbind(roc_table, c(roc_table[length(roc_table),1], 1))
  colnames(roc_table) = c("tpr","fpr")
  ggplot(roc_table, aes(fpr,tpr)) +
    geom_line()+
    geom_abline(intercept = 0, slope = 1, linetype=2) + 
    xlab("Taxa de falso positivo") +
    ylab("Taxa de verdadeiro positivo") +
    ylim(c(0,1.0))+
    xlim(c(0,1.0))+
    annotate(geom="text", x=0.8, y=0.3, col="#1FB477", 
           label=paste("Área sob a curva = ",auc_roc)) +
    theme_bw()
}
