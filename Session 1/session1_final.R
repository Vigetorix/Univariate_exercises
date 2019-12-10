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
