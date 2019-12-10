#Exercise1
senic.df <- SENIC
#a) One variable X4 has to be kept zero 
X1 <- ifelse(senic.df$reg=="1",1,0)
X2 <- ifelse(senic.df$reg=="2",1,0)
X3 <- ifelse(senic.df$reg=="3",1,0)
senic.df <- data.frame(senic.df, X1, X2, X3)
#b) fstatistic: at least one of the coefficients is not important H0 is rejected, 
#p value only slightly lower then 0.05 
senic.lm <- lm(risk~X1+X2+X3, data = senic.df)
summary(senic.lm)
#c)
#dotchart, means represented by a dot, makes it easier to do the confidence interval
#Value of the intercept is the mean value, taken as mean
#region three is midly safer 
#other means are changed mildly

#we cannot tell much from the confidence intervals 
#Not entirely sure if other regions play a role

#d) pvalue is the same as the linear model

#e) now multiple pairwise comparisons
# tukey test
#What does this plot show, the pairwise comparison between 4 minus 3 and compare the difference,
#you take all possible combinations an you try to see which are significantly different,
#Only in 3-1 is sign different The third region has a lower infection rate, because the difference is negative so, risk of 1 is higher. 

#f) 
#* cannot check this
#* can be checked, see solutions 
#* normality, you can do the shapiro test
#* no, you can do the regression model for that, you can also do the coocks plot, four columns because you have four values for the region variable




