###############################
#Solutions exercise session 6
###############################

#Exercise 1
###############################
senic.df <- SENIC
  
  #a)
  X1 <- ifelse(senic.df$reg=="1", 1,0)
  X2 <- ifelse(senic.df$reg=="2", 1,0)  
  X3 <- ifelse(senic.df$reg=="3", 1,0)  
  
  #b)
  senic.lm <- lm(risk~X1+X2+X3, data = senic.df)
  summary(senic.lm)  
  #p value only slightly lower then 0.05. This means we don't have equality of means in the different regions
  
  #c)
  #As explained on page 10-11 of the course notes: anova uses standardly treatment coding where mu = alfa + beta
  senic.lm$coefficients
  s.means <- c(0,0,0,0)
  s.means[1] <- senic.lm$coefficients[1]+senic.lm$coefficients[2]
  s.means[2] <- senic.lm$coefficients[1]+senic.lm$coefficients[3]
  s.means[3] <- senic.lm$coefficients[1]+senic.lm$coefficients[4]
  s.means[4] <- senic.lm$coefficients[1]
  
  s.sd <- by(senic.df$risk, senic.df$reg, sd)
  s.n <- c(28,32,37,16)
  
  matrix <- matrix(data = 0, nrow = 4, ncol = 5)
  
  library(BSDA) 
  for(i in 1:length(s.means)){
    y <- tsum.test(s.means[i], s.sd[i], s.n[i], conf.level = 0.95, var.equal = T)  
    matrix[i,4] <-y$conf.int[1]
    matrix[i,5] <-y$conf.int[2]
  }
  
  matrix[,1] <- c(1,2,3,4)
  matrix[,2] <- s.means  
  matrix[,3] <- s.n

  par(mfrow = c(1,1))
  dotchart(matrix[,2], labels = matrix[,1], col = "red", xlim = c(min(matrix[,4:5]), max(matrix[,4:5])))
  for(i in 1:4){
    lines(x = c(matrix[i,4], matrix[i,5]), y = c(matrix[i,1], matrix[i,1]))
  }
  
  #d)
  aov <- aov(risk ~ reg, data = senic.df)
  summary(aov) #pvalue is small so H0: mu1 = mu2 = mu3=...=mn rejected
  #there is much variation in the average between the different regions
  
  #e)
  diffs <- TukeyHSD(aov, which="reg", conf.level = 0.95)
  diffs  
  #You see here that the hypothesis H0 mu3= mu1 is the only one rejected
  #All the other H0'are not rejected. You see that we get a wrong image by not looking at the pairwise (TukeyHSD) test
  
  #f)
  #We cannot check this, depends on the design of the experiment
  
  library(car)
  leveneTest(risk~reg, data = senic.df)
  #p-value is > 0.05 so H0: homogenity of variances is not rejected
  
  lm <- lm(risk~reg, data = senic.df)
  shapiro.test(lm$residuals)  
  hist(lm$residuals)  
  #normally distributed
  
  plot(cooks.distance(lm))
  #no influential points  