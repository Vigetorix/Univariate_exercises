###############################
#Solutions exercise session 1
###############################

#Exercise1
###############################
#a)
#Ofwel eerst "Import Dataset(rechtsboven)", heading "YES" en dan gewoon:
beta.df <- BETACAR2
#Ofwel zo:
beta.df <- read.table(file=file.choose(), header=TRUE, sep="\t")

#Exercise2
###############################
#a)
names(beta.df)

#Exercise3
###############################
#a)
chick.df <- read.table(file=file.choose(), header=TRUE, sep="\t")
#b)
names(chick.df)
#c)
summary(chick.df$weight) #geeft alleen mean en median anders mean() en median()
var(chick.df$weight)
sd(chick.df$weight)
#d)
summary(chick.df$chicken)
var(chick.df$chicken)
sd(chick.df$chicken)
#Nonsense, these variable is just an enumeration
#e)
names(chick.df)[1] <- "No."
names(chick.df)
#f)
by(chick.df$weight, chick.df$feed, summary)
by(chick.df$weight, chick.df$feed, var)
by(chick.df$weight, chick.df$feed, sd)
#g)
table(chick.df$feed)

#Exercise4
###############################
#a)
monica.df <- read.table(file=file.choose(), header=TRUE, sep=";")
#b)
by(monica.df$age, monica.df$sex, summary)
by(monica.df$age, monica.df$sex, var)
by(monica.df$age, monica.df$sex, sd)
#c)
par(mfrow = c(1,1)) #Allows you to combine several plots in one view
boxplot(monica.df$age)
#d)
boxplot(monica.df$age~monica.df$sex)
#of wat je ook kan doen: 
boxplot(age~sex, data = monica.df)
#e
#f
monica.den <- by(monica.df$age, monica.df$sex, density) # calculate densities
par(mfrow=c(1,2)) # put two graphs next to each other in one row, two columns
plot(monica.den[[1]], main="Females") #Hoe komt het eerst females? 
plot(monica.den[[2]], main="Males")
#of kan ook anders, duidelijker welke female is en welke male, which(monica...)
par(mfrow=c(1,2))
hist(monica.df$age[which(monica.df$sex == 'm')], probability = TRUE, main = "Males", breaks = 36, ylim = c(0,0.08), xlab = "Age") #When probabilities true, relative frequecy, tov het totaal aantal, som is 1  
lines(density(monica.df$age[which(monica.df$sex == 'm')]))
hist(monica.df$age[which(monica.df$sex == 'f')], probability = TRUE, main = "Females", breaks = 36, ylim = c(0,0.08), xlab = "Age")
lines(density(monica.df$age[which(monica.df$sex == 'f')]))
