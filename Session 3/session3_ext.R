#Exercise 4 of session 3
senic.df <- SENIC 
#a)
combine <-data.frame(senic.df$length, senic.df$risk, senic.df$fac, senic.df$xray)
pairs(combine)
#for graph 1,2 length on y axis, risk on x axis
#You can see a a correlation between risk and length

boxplot(senic.df$length)
boxplot(senic.df$risk)
boxplot(senic.df$fac)
boxplot(senic.df$xray)

boxplot(combine)

#b)
cor(combine, method = "spearman")
cor(combine, method = "pearson")

#c)
#F-test 
# For simple linear regression
#y = B + B1X + epsilon
# Ftest H0 : B1 = 0  else H1 : B1=/0
#y = B0 +B1X1 + B2X2 + epsilon
#Ftest H0: B1 = 0 and B2 = 0 vs H1: !H0

senic.lm<- lm(senic.df$risk~senic.df$length, data = senic.df)
summary(senic.lm)

plot(senic.df$risk~senic.df$length, data = senic.df)
abline(senic.lm)
#The value of the R squared metric indicates whether or not the fit is good. This isn't a good fit. 
#You can remove these points in order to make the fit better. There is a difference between influential points and outliers. 
#Are these two points outliers or high leverage points, leverage points follow the same trend but are further away from data.

#get an idea of the outliers
plot(cooks.distance(senic.lm))
cook_dist = cooks.distance(senic.lm)
sort(cook_dist, index.return = T, decreasing =  T)
senic.wo <-senic.df[c(1:46, 48:111, 113),]
senic.wolm <- lm(risk~length, data = senic.wo)
summary(senic.wolm)

plot(risk ~length, data = senic.wo)
abline(senic.wolm)
#The R squared value is better, we gained some performance. We actually gained much more then is shown by this value
#maffe gast die wat zit te brabbelen

#d) 
#The same for risk and fac
#The same for risk xray

senic.lm<- lm(senic.df$risk~senic.df$fac, data = senic.df)
summary(senic.lm)

plot(senic.df$risk~senic.df$fac, data = senic.df)
abline(senic.lm)

#summary: pr>|t| zijn de waardes van beta, p value duidt aan de H0 verwerpen of niet. 
#R squared zegt hoe goed je linear regression line fit

#e)
senic.lm<- lm(senic.df$risk~senic.df$xray, data = senic.df)
summary(senic.lm)

plot(senic.df$risk~senic.df$xray, data = senic.df)
abline(senic.lm)
