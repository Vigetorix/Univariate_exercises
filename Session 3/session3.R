#EXERCISE 1
#Flow Chart bekijken pagina 23/76
#a) If the data is normally distributed
iran <- c(128,125,133,104,146,132,125,118,129,124)
belgium <- c(160,128,169,105,151,164,162,177,185,150,182,158,156,123,141,176,162, 172)

#To test whether normally distributed or not
shapiro.test(iran)
shapiro.test(belgium)

#Check the variances equal or not
var.test(iran,belgium)

#b) H0: avgI = avgB H1: avgI =/ avgB
#c & d) 
t.test(iran, belgium, var.equal = FALSE, conf.level = 0.9)

#e)
t.test(iran, belgium, var.equal = FALSE, conf.level = 0.9, alternative = c("less"))

#EXERCISE 2

blood.df <- read.table(file=file.choose(), header=TRUE, sep=",")

#a)
blood.Y <-subset(blood.df, X.age. <50)
blood.O <-subset(blood.df, X.age. >68)

nrow(blood.Y)
nrow(blood.O)

shapiro.test(blood.Y$X.testost.)
shapiro.test(blood.O$X.testost.)

var.test(blood.Y$X.testost.,blood.O$X.testost.)
 
t.test(blood.O$X.testost, blood.Y$X.testost., var.equal = FALSE, conf.level = 0.95)
#NAKIJKEN OP TOLEDO, WEET NIET OF DIT JUIST IS

#EXERCISE 3
#a)
life<-(tvdoctor$life)
nrtv<-(tvdoctor$tv)
doctor <-(tvdoctor$doctor)
combine <- data.frame(life, nrtv, doctor)

apply(combine, 2, FUN = shapiro.test)
cor(combine, method = "spearman")
cor(combine, method = "pearson")

#Difference in correlation(doctor, nrtv) between spearman and pearson
#Spearman picks up monotonic correlations (eg exponential)
#Pearson picks up linear correlations (eg x = y)
#This causes a difference you can see on the pairs plot:
pairs(tvdoctor)

#EXERCISE 4
senic.df <- SENIC 
#a)
combine <-data.frame(senic.df$length, senic.df$risk, senic.df$fac, senic.df$xray)
pairs(combine)
#for graph 1,2 length on y axis, risk on x axis
#b)
cor(combine, method = "spearman")
cor(combine, method = "pearson")

#NOTES FROM EXPLANATION
#



