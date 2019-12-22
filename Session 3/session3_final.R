###############################
#Solutions exercise session 3
###############################

#Exercise1
###############################
  iran.df <- c(128,125,133,104,146,132,125,118,129,124)
  belgium.df <- c(160,128,169,105,151,164,162,177,185,150,182,158,156,123,141,176,162,172)
  #a)
  #You have to test for normality first 
  shapiro.test(belgium.df)
  shapiro.test(iran.df) 
  #And, because we have two independent sample sets to compare, check for equality of variances
  var.test(belgium.df, iran.df) 
  #The null hypothesis is rejected, we have different variances
  #b)  
  #H0: mu_iran = mu_belgium or H1: mu_iran =/= mu_belgium
  #c) and d)
  t.test(belgium.df, iran.df, var.equal = F, conf.level = 0.9)
  #e)
  t.test(iran.df, belgium.df, var.equal = F, alternative = "less", conf.level = 0.9)
  #p value < 0.05 so the null hypothesis is rejected
  
#Exercise 2
###############################
  blood.df <- BLOOD
  #a)
  younger50 <- subset(blood.df, blood.df$X.age.<50)
  older68 <- subset(blood.df, blood.df$X.age.>68)  
  #b) 
  #H0: mu_younger_testost = mu_older_testost, H1: mu_younger_testost =/= mu_older_testost
  #c)
  nrow(younger50)
  nrow(older68)
  #Small sample set so we have to check for normality
  shapiro.test(younger50$X.testost.)
  shapiro.test(older68$X.testost.)
  var.test(younger50$X.testost., older68$X.testost.)
  
  t.test(younger50$X.testost., older68$X.testost.,
         alternative = "two.sided",
         var.equal = T,
         conf.level = 0.95)
  #p value > 0.05 so the null hypothesis is not rejected
  
  #In oplossingen wordt nog een Wilcoxon rank sum test gedaan
  #Mij lijkt dit niet nodig? 
  wilcox.test(younger50$X.testost., older68$X.testost., exact = F) 
  #exact should be put F because of ties in the data. 
  #Wilcox test laat hetzelfde zien als de t.test, H0 is not rejected
  
#Exercise 3
############################### 
  library(faraway)
  tvdoctor.df <- tvdoctor
  #a) b) and c)
  combine <- tvdoctor.df[-1]
  apply(combine, 2, FUN = shapiro.test) #2 indicates you do the function over the colums
  cor(combine, method = "spearman")
  #a)
  shapiro.test(tvdoctor.df$life)
  shapiro.test(tvdoctor.df$tv)
  cor.test(tvdoctor.df$life, tvdoctor.df$tv, method = "spearman", exact = F)
  #b)
  shapiro.test(tvdoctor.df$life)
  shapiro.test(tvdoctor.df$doctor)
  cor.test(tvdoctor.df$life, tvdoctor.df$doctor, method = "spearman", exact = F)
  #c)
  shapiro.test(tvdoctor.df$doctor)
  shapiro.test(tvdoctor.df$tv)
  cor.test(tvdoctor.df$doctor, tvdoctor.df$tv, method = "spearman", exact = F)
  #d)  
  pairs(tvdoctor.df)

#Exercise 4
###############################  
  senic.df <- SENIC
  
  #a)
  #descriptive statistics
  senic.sub <- data.frame(senic.df$length, senic.df$risk, senic.df$fac, senic.df$xray)
  
  summary(senic.sub)  
  #senic.var <- diag(var(senic.sub))  #Another option: diag takes only the diagonal = what you actually need
  apply(senic.sub, 2, FUN = var)
  apply(senic.sub, 2, FUN = sd) 
  
  par(mfrow = c(2,2))
  boxplot(senic.sub$senic.df.length, main = "length")
  boxplot(senic.sub$senic.df.risk, main = "risk")
  boxplot(senic.sub$senic.df.fac, main = "fac")  
  boxplot(senic.sub$senic.df.xray, main = "xray")  

  pairs(senic.sub)  
  
  plot(density(senic.sub$senic.df.length), main = "length")  
  plot(density(senic.sub$senic.df.risk), main = "risk")  
  plot(density(senic.sub$senic.df.fac), main = "fac")  
  plot(density(senic.sub$senic.df.xray), main = "xray") 
  
  #Weet niet of deze van heel veel belang zijn  
  plot(sort(senic.sub$senic.df.length), main ="length")
  plot(sort(senic.sub$senic.df.risk), main ="risk")
  plot(sort(senic.sub$senic.df.fac), main ="fac")
  plot(sort(senic.sub$senic.df.xray), main ="xray")
  
  #b
  apply(senic.sub, 2, FUN = shapiro.test)
  cor(senic.sub, method = "spearman")
  #length is not normally distributed
  #zit niet echt een duidelijke correlatie in, misschien tussen length en risk
  pairs(senic.sub)

  #c
  senic.lm <- lm(senic.sub$senic.df.risk~senic.sub$senic.df.length)  
  summary(senic.lm)  
  
  #H0: Beta_length = 0, H1: Beta_length =/= 0 
  #The p-value is < 0.05 so H0 is rejected in favor of H1
  #The estimated value for Beta_length is 0.37, this means that for an increase of the length variable by 1, the risk increases 0.37
  #The smaller the R² value the less variability in the data is explained, R² is here 0.2846, we can explain approximately 28,5 percent of the data
  
  par(mfrow = c(1,1))
  plot(senic.sub$senic.df.risk~senic.sub$senic.df.length, ylab = "risk", xlab = "length of stay")  
  abline(senic.lm, col = "red")  
  
  #d
  senic.lm <- lm(senic.sub$senic.df.risk~senic.sub$senic.df.fac)
  summary(senic.lm)  
  plot(senic.sub$senic.df.risk~senic.sub$senic.df.fac, ylab = "risk", xlab = "facilities")  
  abline(senic.lm, col = "red") 

  #e
  senic.lm <- lm(senic.sub$senic.df.risk~senic.sub$senic.df.xray)
  summary(senic.lm)  
  plot(senic.sub$senic.df.risk~senic.sub$senic.df.xray, ylab = "risk", xlab = "xray")  
  abline(senic.lm, col = "red") 

  #f
  #As already explored in b) does the best correlation occur between risk and length
  #When building a linear model it can be seen that the R² values of the different models confirm this discovery
  #To improve the R² value, one should move towards multiple linear regression or use other variables to build a simple linear regression model 
  
  #forward
  names(senic.df)
  senic.lm2 <- step(lm(risk~1, data = senic.df),
                   scope = ~length+age+cult+xray+beds+meds+reg+cen+nur+fac,
                   direction = "forward")
  senic.lm <- lm(senic.sub$senic.df.risk~senic.sub$senic.df.length)  
  
  AIC(senic.lm2)  
  AIC(senic.lm)  
  summary(senic.lm2)
  
  #So model 2 is an improvement, we now have an R² value of 0.5575 
  #model variables: risk ~ cult + length + fac + reg + xray + meds
  
  plot(risk ~ cult + length + fac + reg + xray + meds, data = senic.df)
    
  