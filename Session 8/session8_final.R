###############################
#Solutions exercise session 8
###############################

#Exercise 1
###############################
blood.df <- BLOOD
blood.df <- subset(blood.df, blood.df$X.testost.<999)

#1)
blood.glm <- glm(blood.df$X.case.~blood.df$X.testost., family = binomial(link=logit))
summary(blood.glm)
  #a)
  exp(blood.glm$coefficients)
  
  #b)
  combine <- data.frame(cbind(blood.df$X.testost., blood.df$X.case., fitted(blood.glm)))
  colnames(combine) <- c("testosterone", "cancer", "fitted values")
  head(combine, 5)  
  par(mfrow = c(1,1))  
  plot(blood.df$X.testost., blood.df$X.case., type = "p", col = "red")  
  points(blood.df$X.testost., fitted(blood.glm), col = "black")  
  
  #c)
  table(blood.df$X.case., fitted(blood.glm)>0.5)
  
  #d)
  summary(blood.glm)
  #deviance = 621.05 while null deviance = 625.32, not very good model
  
  #e)
  library(ROCR)
  predict <- fitted(blood.glm)
  pred <- prediction(predict, blood.df$X.case.)
  perfb <- performance(pred, 
                       measure = "tpr",
                       x.measure = "fpr")
  plot(perfb, 
       main = "sensitivity vs FP",
       colorize = T,
       colorkey.relwidth = 0.5,
       lwd = 4.5)
  perf_auc <- performance(pred, measure = "auc")
  perf_auc
  
  #AUC value = 0.559 is close to 0.5 not very good
  
#2)
  blood.glm1 <- glm(blood.df$X.case.~blood.df$X.age., family = binomial(link=logit))
  summary(blood.glm1)
  #a)
  exp(blood.glm1$coefficients)
  
  #b)
  combine <- data.frame(cbind(blood.df$X.age., blood.df$X.case., fitted(blood.glm1)))
  colnames(combine) <- c("age", "cancer", "fitted values")
  head(combine, 5)  
  par(mfrow = c(1,1))  
  plot(blood.df$X.age., blood.df$X.case., type = "p", col = "red")  
  points(blood.df$X.age., fitted(blood.glm1), col = "black")  
  
  #c)
  table(blood.df$X.case., fitted(blood.glm1)>0.5)
  
  #d)
  summary(blood.glm1)
  #deviance = 621.05 while null deviance = 625.32, not very good model
  
  #e)
  library(ROCR)
  predict <- fitted(blood.glm1)
  pred <- prediction(predict, blood.df$X.case.)
  perfb <- performance(pred, 
                       measure = "tpr",
                       x.measure = "fpr")
  plot(perfb, 
       main = "sensitivity vs FP",
       colorize = T,
       colorkey.relwidth = 0.5,
       lwd = 4.5)
  perf_auc <- performance(pred, measure = "auc")
  perf_auc
  
  #AUC value = 0.5023 is close to 0.5 not very good, even worse than the previous model
  
#3)
  blood.glm2 <- glm(blood.df$X.case.~blood.df$X.testost.+blood.df$X.age., family = binomial(link=logit))
  summary(blood.glm2)
  #a)
  exp(blood.glm2$coefficients)
  
  #b)
  combine <- data.frame(cbind(blood.df$X.testost., blood.df$X.age., blood.df$X.case., fitted(blood.glm2)))
  colnames(combine) <- c("testosterone", "age", "cancer", "fitted values")
  head(combine, 5)  
  par(mfrow = c(1,1))  
  s3d <- scatterplot3d(blood.df$X.testost., blood.df$X.age., blood.df$X.case., main = "3D scatterplot")
  s3d$plane3d(blood.glm2, lty.box = "solid")
  
  #c)
  table(blood.df$X.case., fitted(blood.glm2)>0.5)
  
  #d)
  summary(blood.glm2)
  #deviance = 620.89 while null deviance = 625.32, not very good model
  
  #e)
  library(ROCR)
  predict <- fitted(blood.glm2)
  pred <- prediction(predict, blood.df$X.case.)
  perfb <- performance(pred, 
                       measure = "tpr",
                       x.measure = "fpr")
  plot(perfb, 
       main = "sensitivity vs FP",
       colorize = T,
       colorkey.relwidth = 0.5,
       lwd = 4.5)
  perf_auc <- performance(pred, measure = "auc")
  perf_auc
  
  #AUC value = 0.553 is close to 0.5 not very good

#4 Via step function beste model genereren 
  names(blood.df)
  slm.forward <- step(glm(blood.df$X.case.~1), 
                      scope=~blood.df$X.matchid.+blood.df$X.curpmh.+blood.df$X.age.+
                              blood.df$X.estradol.+blood.df$X.estrone.+blood.df$X.testost.+blood.df$X.prolactn., 
                      direction = "forward")
  library(ROCR)
  predict <- fitted(slm.forward)
  pred <- prediction(predict, blood.df$X.case.)
  perfb <- performance(pred, 
                       measure = "tpr",
                       x.measure = "fpr")
  plot(perfb, 
       main = "sensitivity vs FP",
       colorize = T,
       colorkey.relwidth = 0.5,
       lwd = 4.5)
  perf_auc <- performance(pred, measure = "auc")
  perf_auc
  