#Exercise 1
beta.df <- BETACAR2

#Exercise 2
colnames(beta.df)

#Exercise 3
chick.df <- ChickWeight

colnames(chick.df)

mean(chick.df[["weight"]])
median(chick.df[["weight"]])
var(chick.df[["weight"]])
quantile(chick.df[["weight"]])
sd(chick.df[["weight"]])
mean(chick.df[["chicken"]])

#this is just the enumeration of all the chickens,
#so if there are 72, the mean is 36, this makes no sense 
names(chick.df)[names(chick.df)=="chicken"] <- "No."

horsebean <- subset(chick.df, chick.df$feed=='horsebean')
mean(horsebean[["weight"]])
median(horsebean[["weight"]])
var(horsebean[["weight"]])
quantile(horsebean[["weight"]])
sd(horsebean[["weight"]])
?by #This can be used to get information about various commandos
by(chick.df$weight, chick.df$feed, summary) #deze geeft de meesten al
by(chick.df$weight, chick.df$feed, sd)
by(chick.df$weight, chick.df$feed, var)

table(chick.df$feed)

#Exercise 4
monica.df <- monica
by(monica.df$age, monica.df$sex, summary)
by(monica.df$age, monica.df$sex, sd)
by(monica.df$age, monica.df$sex, var)
boxplot(monica.df$age)
boxplot(subset(monica.df, monica.df$sex=='m')$age)
boxplot(subset(monica.df, monica.df$sex=='f')$age)
#Of alles tesamen kan ook:
boxplot(age~sex,monica.df)
dm<- density(subset(monica.df, monica.df$sex=='m')$age)
plot(dm)
df<- density(subset(monica.df, monica.df$sex=='f')$age)
plot(df, col = 'pink')
lines(dm, col='blue')


