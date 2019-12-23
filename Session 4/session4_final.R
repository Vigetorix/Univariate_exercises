###############################
#Solutions exercise session 4
###############################

#Exercise 1
###############################
  body.df <- body
  #a)
  body.sub <- body.df[,2:16]
  summary(body.sub)
  apply(body.sub, 2, var)    
  apply(body.sub, 2, sd)    
  
  body.names <- names(body.sub)
  for(i in 1:length(body.sub)){
    dev.new()
    boxplot(body.sub[,i], main = paste(body.names[i]))
  }
  #Age seems skewed to me, most of the people are around 30 and we have some outliers around 60
  #Thight seems to have some outliers as well
  #Same for knee and ankle
  
  for(i in 1:length(body.sub)){
    dev.new()
    plot(density(body.sub[,i]), main = paste(body.names[i]))
  }
  #Age is again skewed
  #The others seem normal
  for(i in 1:length(body.sub)){
    dev.new()
    plot(sort(body.sub[,i]), main = paste(body.names[i]))
  }
  
  apply(body.sub, 2, FUN = shapiro.test)
  #only calf seems to be approximately normally distributed
  #But CLT because observations >25
  cor(body.sub, method = "pearson")
  #A lot of correlations
  par(mfrow = c(1,1))
  pairs(body.sub, method = "pearson")
  
  #b)
  X1 <- ifelse(body.df$Sex == "Male", 0, 1)
  body.df <- data.frame(body.df, X1)
  
  #Y = Beta0 + Beta1*X + Beta2*X1
  #If the dummy variable increases 1 (Male vs Female) and X stays the same, Y will increase/decrease Beta1 depending on the sign of beta
  #quantitative
  
  #c)
  rows.sample <- sample(1:nrow(body.df), 50)
  body.sample <- body.df[rows.sample,] #Je neemt de indexen als random sample
  body.mod <- body.df[-rows.sample,]
  
  #d)
  names(body.mod)
  body.lm <- lm(Weight ~ Height+Shoulder+Chest+Waist+Abdo+Hip+Thigh+Bicep+Forearm+Knee+Calf+Ankle+Wrist+Age, data = body.mod )
  
  body.mod.names <- names(body.mod)
  #Comments zijn voor alle plots, zijn er wat veel
  for(i in 2:4){#length(body.mod)){
    #body.temp = body.mod[-i]
    #body.temp.names = names(body.temp)
    for(k in 5:6){#length(body.temp)){
      dev.new()
      
      plot(body.mod[,i]~body.mod[,k],
           xlab = paste(body.mod.names[i]), 
           ylab= paste(body.mod.names[k]),
           main = paste(body.mod.names[i],"vs",body.mod.names[k], sept = " "))
      abline(lm(body.mod[,i]~body.mod[,k]), col = "red")
    }
  }
  
  summary(body.lm)
  AIC(body.lm)
  #Wrist variable has the highest p-value -> removed
  body.lm <- lm(Weight ~ Height+Shoulder+Chest+Waist+Abdo+Hip+Thigh+Bicep+Forearm+Knee+Calf+Ankle+Age, data = body.mod )
  summary(body.lm)
  AIC(body.lm)
  #Ankle variable removed  
  body.lm <- lm(Weight ~ Height+Shoulder+Chest+Waist+Abdo+Hip+Thigh+Bicep+Forearm+Knee+Calf+Age, data = body.mod )
  summary(body.lm) 
  AIC(body.lm)
  #Bicep variable removed  
  body.lm <- lm(Weight ~ Height+Shoulder+Chest+Waist+Abdo+Hip+Thigh+Forearm+Knee+Calf+Age, data = body.mod )
  summary(body.lm)
  AIC(body.lm)
  #Abdo variable removed  
  body.lm <- lm(Weight ~ Height+Shoulder+Chest+Waist+Hip+Thigh+Forearm+Knee+Calf+Age, data = body.mod )
  summary(body.lm)
  AIC(body.lm)
  #All varaibles significant
  #Always smaller AIC value -> oke
  #AIC gebruiken om model verder te reduceren, niet gedaan is tijdverlies beter via automatische methode doen:
  body.step.lm <- step(lm(Weight ~ 1, data = body.mod),
                       scope =~Height+Shoulder+Chest+Waist+Abdo+Hip+Thigh+Bicep+Forearm+Knee+Calf+Ankle+Wrist+Age,
                       direction = "forward")
    
  summary(body.step.lm)
  
  #model misspecification and non linearity
  body.fit <- fitted(body.step.lm)
  body.rs <- rstandard(body.step.lm)  
  plot(body.rs~body.fit)  
  for(i in 1:length(body.mod[2:16])){
    dev.new()
    plot(body.rs~body.mod[,i])
  }
  #All random -> oke
  #model normality
  shapiro.test(body.rs) #not normal
  hist(body.rs) #plot is normal, so oke
  #In oplossingen staat nog iets over een QQ plot
  #https://data.library.virginia.edu/understanding-q-q-plots/
  #influential points
  plot(cooks.distance(body.step.lm))
  
  #R² value = explained variation/total variation, It is a measure for how good the data is fitted
  #R² adj value = R² value adjusted for the number of predictors in the model, allows you to compare models with a different number
  #of predictors. The R² value only increases if the new term improves the model more than would be expected by chance. It is always
  #lower than R²  
  summary(body.step.lm)
  #Based on the high values of both R² (close to 1 what is max value) I would suggest this model can be used for prediction
  
  library(leaps)
  leap.r2 <- leaps(x=cbind(body.mod$Height,body.mod$Shoulder,body.mod$Chest,
                        body.mod$Waist,body.mod$Abdo,body.mod$Hip,
                        body.mod$Thigh,body.mod$Bicep,body.mod$Forearm,
                        body.mod$Knee,body.mod$Calf,body.mod$Ankle,
                        body.mod$Wrist,body.mod$Age),
                y = body.mod$Weight, 
                method = c("r2"),
                nbest = 3) 
  
  leap.adjr2 <- leaps(x=cbind(body.mod$Height,body.mod$Shoulder,body.mod$Chest,
                        body.mod$Waist,body.mod$Abdo,body.mod$Hip,
                        body.mod$Thigh,body.mod$Bicep,body.mod$Forearm,
                        body.mod$Knee,body.mod$Calf,body.mod$Ankle,
                        body.mod$Wrist,body.mod$Age),
                y = body.mod$Weight, 
                method = c("adjr2"),
                nbest = 3) 
  
  leap.cp <- leaps(x=cbind(body.mod$Height,body.mod$Shoulder,body.mod$Chest,
                        body.mod$Waist,body.mod$Abdo,body.mod$Hip,
                        body.mod$Thigh,body.mod$Bicep,body.mod$Forearm,
                        body.mod$Knee,body.mod$Calf,body.mod$Ankle,
                        body.mod$Wrist,body.mod$Age),
                y = body.mod$Weight, 
                method = c("Cp"),
                nbest = 3) 
  combine <- cbind(leap$which, leap$size, leap.r2$r2, leap.adjr2$adjr2,leap.cp$Cp)
  dimnames(combine) <- list(1:length(combine[,1]),
                            c("Height","Shoulder","Chest",
                              "Waist","Abdo","Hip","Thigh",
                              "Bicep","Forearm","Knee","Calf",
                              "Ankle","Wrist","Age","size", "r2", "adjr2", "Cp"))
  round(combine, digits = 3)  
  
  plot(leap.cp$size, leap.cp$Cp, xlim = c(0,15), ylim = c(0,15))
  abline(a = 0, b = 1)  
  
  library(scatterplot3d)
  body.lm.3d <- lm(Weight~Height+Waist, data = body.mod)
  s3d <- scatterplot3d(body.mod$Height,body.mod$Waist,body.mod$Weight, main="3D Scatterplot")
  s3d$plane3d(body.lm.3d, lty.box = "solid")

  #e)
  summary(body.lm)
  body.pred <- predict(body.lm, body.sample, interval = "confidence", se.fit = TRUE)  
  body.pred    
  #discussion nog doen 
  
  #f)
  body.lm.f <- lm(Weight ~ Height+Shoulder+Chest+Waist+Abdo+Hip+Thigh+Bicep+Forearm+Knee+Calf+Ankle+Wrist+Age+X1+
                    I(X1*Height)+I(X1*Shoulder)+I(X1*Chest)+I(X1*Waist)+I(X1*Abdo)+I(X1*Hip)+I(X1*Thigh)+I(X1*Bicep)+
                    I(X1*Forearm)+I(X1*Knee)+I(X1*Calf)+I(X1*Ankle)+I(X1*Wrist)+I(X1*Age), data = body.mod )
  summary(body.lm.f)
  #X1 is significant, the variable X1 helps in reducing the unexplained variation
  #The interaction between X1 and Height is significant
  
  summary(body.lm.f) 
  summary(body.lm)
  #This one would perform better then the model without X1
  
  body.lm.reduced <- lm(Weight~Height, data = body.mod)
  plot(body.mod$Weight,body.mod$Height)
  abline(body.lm.reduced, col = "red") #Deze wilt niet om de een of andere reden
  
  #g)
  body.pred.f <- predict(body.lm.f, body.sample, interval = "confidence", se.fit = TRUE) 
  body.pred.f  
  #df en residual scale,
  #Predictions should normally be better compared to the model without X1
  
  