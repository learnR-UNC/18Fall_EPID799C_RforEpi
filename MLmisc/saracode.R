causal_model <- glm(data = df, preterm_f ~ pnc5_f + mage + mage*mage + raceeth_f + pnc5_f*raceeth_f + smoker_f, family=binomial("logit"))
df$model_prediction <- predict(causal_model, type = "response")
ggplot(data=df,aes(x=model_prediction, group=preterm_f, fill=preterm_f))+
  geom_histogram(aes(y=..density..),alpha = 0.75,binwidth=0.01,position = position_dodge(width=0.005)) +
  theme_classic() +
  xlab("Predicted probability of preterm") +
  xlim(0,0.25) +
  labs(fill = "Preterm birth")

library(ROCR)
library(plotROC)

pred.eval <- function(preds,outcome){
  pred.sum=prediction(preds,outcome)
  auc=as.numeric(performance(pred.sum,"auc")@y.values)
  mspe=mean((preds-outcome)^2)
  list(auc=auc,mspe=mspe)
}

pred.eval(df$model_prediction,df$preterm)
df$predictor <- jitter(df$wksgest, amount = 5)
pred_model <- glm(data = df, preterm_f ~ predictor, family=binomial("logit"))
df$better_prediction <- predict(pred_model, type = "response")

ggplot(data=df,aes(x=better_prediction, group=preterm_f, fill=preterm_f)) +
  geom_histogram(aes(y=..density..),alpha = 0.75,binwidth=0.01,position = position_dodge(width=0.005)) +
  theme_classic() +
  xlab("Predicted probability of preterm") +
  labs(fill = "Preterm birth")

pred.eval(df$better_prediction,df$preterm)




roc <- function(preds.list,outcome,labels){

  for(i in 1:length(preds.list)){
    pred.sum=prediction(preds.list[[i]],outcome[[i]])
    perf=performance(pred.sum,"tpr","fpr")
    temp.ds=data.frame(x=perf@x.values[[1]],y=perf@y.values[[1]],
                       labels=labels[i])

    if(i==1){
      temp=temp.ds
    }else{
      temp=rbind(temp,temp.ds)

    }
  }

  ggplot(data=temp,aes(x=x,y=y,color=labels))+geom_step() +
    ylab("True Positive Rate\n")+xlab("\nFalse Positive Rate") +
    scale_color_discrete("Model") +
    theme_bw()
}

roc(list(df$model_prediction,df$better_prediction),list(df$preterm,df$preterm),
    c("Causal","Predictive"))
