#Exercise 5
#a) 
fev.df <- FEV
summary(fev.df)
fev.cor <- cor(fev.df[,1:3])
fev.cor #All very correclated, Fev in correlation with height is 0.86 so probably correlated 
pairs(fev.df[,1:3])

#Boxplots
#In Fev much more outliers, people excell a lot more then the others, also one person a lot older then the others

#Density plots, 
#Age most around 10, one outlier of 19
#Fev is a right skewed distribution

#Index plots

#b)
X1 <- as.vector(rep(0,nrow(fev.df)))
X2 <- as.vector(rep(0,nrow(fev.df)))
X1 <- ifelse(fev.df$Sex == "Male", 0, 1)  # ifelse function to create dummy variable
X2 <- ifelse(fev.df$Smoke == "No", 0, 1)
fev.df <- data.frame(fev.df, X1, X2)

#c)1)
par(mfrow = c(1,1))
boxplot(fev.df$Fev~fev.df$X2, data= fev.df)
#What you find is very counterintuitive, the median of Fev is larger for the people who smoke
#This can happen because other variables are in play (confoundment -> opzoeken in wikipedia) 

#2)
fev.lm <-lm(Fev~X2,data = fev.df)
summary(fev.lm)
plot(fev.df$Fev~fev.df$X2)
abline(fev.lm, col = "red" )

#3)
#p value is < alfa
#this model says smokers excell 0.71 liters fev compared to non smokers

#The plot line goes up this means better scores for fev for smokers

#d) subset is not used, we take everything 
#1) #NoteVery rarely you have to include a polynomial of an order higher then 2
fev.lm <- lm(Fev~Age+Height+X1+X2, data = fev.df)
summary(fev.lm)
#X2 is not significant because pvalue is above 0.05
#X1/gender value is highly significant 
#Substantial improvement for R² 
#When F statistic is very low this means one of your variables is contributing significantly 
fit <-fitted(fev.lm)
rs <- rstandard(fev.lm)
plot(rs~fit, ylim = c(-4.5, 4.5))
#Looks more or less like a linear relationship

fev.p <-lm(Fev~Age+Height+X1+X2+I(Age^2), data =fev.df)
summary(fev.p)

fev.p2 <-lm(Fev~Age+Height+X1+X2+I(Height^2)+I(X1*Age)+I(X2*Age) , data = fev.df)
summary(fev.p2)
#Polynomial term added, Height squared does add some significant 

#For future predictions normality is not that important
#2)
scatter.smooth(fev.df$Fev, fev.df$Height, col = "red")
scatter.smooth(fev.df$Fev, fev.df$Age, col = "red")

#What would you do next,
#Because you have many insignificant variables (see solution exercises) you have to remove them
#Backward(all variables insign dropped starting from all variables in model) or foward selection can be used with AIC
fev.r <- step(fev.p2, direction= "backward")
summary(fev.r)
#X2 is dropped
#in online solutions, 3 variables are dropped, much better
#R² has in fact increased a little bit

#What var now important, Height Age X1 and polys (see solutions) are significantly important
#Analysis would stop here, This would be the final model

#e)What do you think is wrong with the model
#The poly terms make the model wrong for prediction. A lot of things can go wrong
#Poly terms are good to explain what you have but are not good for prediction
#Simpler models perform best for prediction. 

