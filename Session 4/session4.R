#Session 4
#Exercise 1

#question a
body.df <- body[-1]
boxplot(body.df)

combine <- data.frame(body.df[-16])
cor(combine)

#question b
X1 <- as.vector(rep(0,nrow(body.df)))
X1 <- ifelse(body.df$Sex == "Male", 0, 1)  # ifelse function to create dummy variable
body.df <- data.frame(body.df, X1)

#Y = B0 + B1X + B2X1 + epsilon + B3XX1
#Y = B0 + B1X + B2 + epsilon + B3X if X1=1 
#Y = B0 + B1X + epsilon if X1=0

#question about 

#question c
body.s <- sample(1:nrow(body.df), 50, replace = F)
body.mod <- body.df[-body.s]
body.new <- body.df[body.s,]

#question d
#see solutions -> par mfrow is just to put some space between the labels 
#lines function is used to draw linear regression 
#consider polynomials? Not really necessary, no polynomial structure in the scatterplots 
#No correlation between weight and age, we have already seen this in question a

#notities vragen aan Joon doorgegaan 

