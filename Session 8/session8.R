#####################################
#Session 8

#Exercise 1
blood.df <- BLOOD
names(blood.df)
blood.df <- subset(blood.df, blood.df$X.testost. != 999)
summary(blood.df)
hist(blood.df$X.testost.)

glm <- glm(blood.df$X.case.~blood.df$X.testost., family = binomial(link="logit"))
summary(glm)

#a)
exp(glm$coefficients)
#Because odds 1.01, the odds of getting breast cancer increase by 1% if you increase one level of testosterone by one unit
#The levels of testosterone are therfore a good indicator for having breast cancer

#b)
combined <- data.frame(cbind(blood.df$X.testost., blood.df$X.case., fitted(glm)))
colnames(combined) <- c("Testosterone","Cancer","Fitted value")
head(combined, 5)
#fitted value -> probability, testosterone level of 8 -> probability lower then testosterone level of 25

par(mfrow = c(1,1))
plot(blood.df$X.testost, blood.df$X.case., type = "p", col = "red")
points(blood.df$X.testost., fitted(glm), col = "black")

#c)
table(blood.df$X.case., fitted(glm)>0.5)
table(blood.df$X.case., fitted(glm)>0.3)
table(blood.df$X.case., fitted(glm)>0.7)

#d) 
summary(glm)
#Deviance is very close to the null deviance so not a good model

#e)
library(ROCR)
predict <- fitted(glm)
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
#AUC value of 0.559 
#The curve is almost the diagonal 0.5, is almost the same as flipping a coin, very uncertain

#Exercise 2
blood.df <- BLOOD
glm <- glm(blood.df$X.case.~blood.df$X.age., family = binomial(link="logit"))
summary(glm)

#a)
exp(glm$coefficients)

#b)
combined <- data.frame(cbind(blood.df$X.case.,blood.df$X.age., fitted(glm)))
colnames(combined) <- c("Cancer", "Age", "Fitted value")
head(combined, 5)

par(mfrow = c(1,1))
plot(blood.df$X.age., blood.df$X.case., type = "p", col = "red")
points(blood.df$X.age., fitted(glm), col = "black")

#c)
table(blood.df$X.case., fitted(glm)>0.5)
table(blood.df$X.case., fitted(glm)>0.3)
table(blood.df$X.case., fitted(glm)>0.7)

#d) 
summary(glm)
#Deviance is very close to the null deviance so not a good model

#e)
library(ROCR)
predict <- fitted(glm)
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
#AUC value of 0.50

#Exercise 3
blood.df <- subset(blood.df, blood.df$X.testost. < "999")
glm <- glm(blood.df$X.case.~blood.df$X.age.+blood.df$X.testost., family = binomial(link="logit"))
summary(glm)

#a)
exp(glm$coefficients)

#b)
combined <- data.frame(cbind(blood.df$X.case.,blood.df$X.age., blood.df$X.testost., fitted(glm)))
colnames(combined) <- c("Cancer", "Age", "Testosterone", "Fitted value")
head(combined, 5)

par(mfrow = c(1,1))
plot(blood.df$X.testost+blood.df$X.age., blood.df$X.case., type = "p", col = "red")
points(blood.df$X.testost.+blood.df$X.age., fitted(glm), col = "black")
#Oplossing bezien, ander plot
#library(scatterplot3d)
#scatterplot3d(blood.df$X.testost,blood.df$X.age., fitted(glm), type = "p", col = "red")


#c)
table(blood.df$X.case., fitted(glm)>0.5)

#d) 
summary(glm)
#Deviance is very close to the null deviance so not a good model

#e)
library(ROCR)
predict <- fitted(glm)
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
#AUC value of 0.55
#The classification is actually worse now

#The difference between the models can be caculated based on the deviances
#the diviances is distributed as a chisquare
#0.69 value isn't better, null hypothesis rejected? Niet helemaal zeker van antwoord 

#It is very likely that there are other variables which are better for prediction

