## Accuracy Calculation Function
## make sure actuals and classifications are 0 (no) or 1 (yes) only
##  Built by Matthew J. Schneider

c_accuracy=function(actuals,classifications){
  df=data.frame(actuals,classifications);


  TP=nrow(df[df$classifications==1 & df$actuals==1,]);  # true positive
  FP=nrow(df[df$classifications==1 & df$actuals==0,]);  # false positive
  FN=nrow(df[df$classifications==0 & df$actuals==1,]);  # false negative
  TN=nrow(df[df$classifications==0 & df$actuals==0,]);  # true negative


  recall=round(TP/(TP+FN),3)
  precision=round(TP/(TP+FP),3)
  accuracy=round((TP+TN)/(TP+FN+FP+TN),3)
  tpr=recall
  fpr=round(FP/(FP+TN),3)
  fmeasure=round(2*precision*recall/(precision+recall),3)
  scores=c(recall,precision,accuracy,tpr,fpr,fmeasure,TP,FP,FP,FN)
  names(scores)=c("recall","precision","accuracy","tpr","fpr","fmeasure","TP","TN","FP","FN")

  scoresB=c(TP,FP,FP,FN)
  names(scoresB)=c("TP","TN","FP","FN")

  # print(scoresB)
  return(list(scores));
}
