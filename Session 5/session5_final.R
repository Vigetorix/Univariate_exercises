###############################
#Solutions exercise session 5
###############################

#Exercise 1
###############################
data.df <- FEV
  
  #a)
  summary(data.df)
  apply(data.df, 2, FUN = var)
  apply(data.df, 2, FUN = sd)
  
  names <- names(data.df)
  for(i in 1:length(data.df[1:3])){
    dev.new()
    boxplot(data.df[,i], main = paste(names[i]))
  }
  #Most outliers in FEV variable
  
  cor(data.df[1:3])
  #High values for the corraltion coeff
  
  apply(data.df[1:3], 2, FUN = shapiro.test)
  for(i in 1:length(data.df[1:3])){
    dev.new()
    hist(data.df[,i], main = paste(names[i]))
  }
  #Given the histograms I assume normally distributed
  
  for(i in 1:length(data.df[1:3])){
    dev.new()
    plot(density(data.df[,i]), main = paste(names[i]))
  }
  #FEV a little skewed maybe? 
  
  for(i in 1:length(data.df[1:3])){
    dev.new()
    plot(sort(data.df[,i]), main = paste(names[i]))
  }
  #Again outliers in the Fev variable
  
  #b)
  X1 <- ifelse(data.df$Sex == "Male", 0, 1)
  X2 <- ifelse(data.df$Smoke == "No", 0, 1)
  data.df <- data.frame(data.df, X1, X2)
  
  #c)
  par(mfrow = c(1,1))
  boxplot(data.df$Fev~data.df$Smoke, main = "FEV in function of smoking")  
  #It is weird that the median of Smoking = Yes is higher compared to the median of Smoking = No
  #This would imply smokers can exhale more air in the first second of a forceful breath
  
  fev.lm <- lm(Fev~X2, data = data.df)
  summary(fev.lm)
  
  plot(data.df$Fev~data.df$X2)  
  abline(fev.lm, col = "red")  

  #checking for linearity
  fit <- fitted(fev.lm)
  rs <- rstandard(fev.lm)  
  plot(rs~ fit)  
  plot(rs~data.df$X2)  #Data should be add random, this is not the case here + summary R² = 6%, very low
  #checking normality
  shapiro.test(rs)
  hist(rs) #oke + CLT 
  #cooks distance
  par(mfrow = c(1,1))
  plot(cooks.distance(fev.lm))   
  
  #d)
  #Beste is om geen subset te nemen, alle data wordt gebruikt voor model te berkenen
  
  names(data.df)
  fev.lm <- lm(Fev ~ Height+Age+X1+X2+I(Height*X1)+I(Height*X2)+I(X1*X2)+I(Age*Height)+I(Age*X1)+I(Age*X2),
               data = data.df)
  summary(fev.lm)  
  #Drop X2
  fev.lm <- lm(Fev ~ Height+Age+X1+I(Height*X1)+I(Height*X2)+I(X1*X2)+I(Age*Height)+I(Age*X1)+I(Age*X2),
               data = data.df)
  summary(fev.lm)  
  #Drop age*X1
  fev.lm <- lm(Fev ~ Height+Age+X1+I(Height*X1)+I(Height*X2)+I(X1*X2)+I(Age*Height)+I(Age*X2),
               data = data.df)
  summary(fev.lm)  
  #Drop X1*X2 
  fev.lm <- lm(Fev ~ Height+Age+X1+I(Height*X1)+I(Height*X2)+I(Age*Height)+I(Age*X2),
               data = data.df)
  summary(fev.lm)
  #Oke now
  
  fit <- fitted(fev.lm)
  rs <- rstandard(fev.lm)  
  plot(fit~rs)  
  #Polynomial terms don't seem to be necessary
  scatter.smooth(data.df$Height, data.df$Fev, ylab = "Fev", xlab = "Height")
  scatter.smooth(data.df$Age, data.df$Fev, ylab = "Fev", xlab = "Age")
  #Height seems to be exponential
  fev.lm2 <- step(lm(Fev ~ Height+Age+X1+X2+I(Height*X1)+I(Height*X2)+I(X1*X2)+
                       I(Age+Height)+I(Age*X1)+I(Age*X2),
                     data = data.df),
                  direction = "backward")
  fev.lm.poly <- lm(Fev ~ Height+Age+X1+X2+I(Height^2)+I(Height*X1)+I(Height*X2)+I(Age*X2),
                    data = data.df)
  summary(fev.lm.poly)  
  par(mfrow = c(2,2))
  plot(fev.lm.poly)  #<- is nog wel een gemakkelijker manier om in 1 keer aan je Residual plot te komen
  
  summary(fev.lm.poly)
  #X1 seems to be the largest driving force, If all variables the same Fev decreases with 0.94 when X1 increases with 1
  #So men have a higher Fev then women
  
  #X2 is not significant for prediction so we can assume there is no difference in smoking vs non smoking
  
  #OPLOSSING ONLINE PRECIES WAT ANDERS? R Value komt wel min of meer overeen dus misschien meerdere oplossingen?
  
  #e)
  #Van notities van les: 
  #The poly terms make the model wrong for prediction. A lot of things can go wrong
  #Poly terms are good to explain what you have but are not good for prediction
  #Simpler models perform best for prediction.
  
    