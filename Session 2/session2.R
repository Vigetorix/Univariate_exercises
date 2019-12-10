#Exercise 1
#a) how most obvious random variable described? 
  #The number that is thrown by the dice, ranging from 1 till 6 
#b) Discrete or continuous?
  #Discrete, only 6 possibilities, 1 till 6
#c) Probability Density function
  #6 bars every bar possibility of 1/6
#d) Cummulative Distribution function
  # CDF: probabilty that x is less than or equal to a certain value, in this case 1-6 
  #Six bars, one for each value 1-6. Probability of bars 1/6 2/6 3/6 ...

#Exercise 2
#a) heads or tails
#b) discrete
#c) Binominal distribution 1/2 chance on heads, 1/2 chance on tails
#d) CDF distribution: P(X=<6) = pbinom(10,6,0.5)
#e) CDF distribution: P(X>7) = 1-P(X=<6) = 1 - pbinom(10,6,0.5)
#f) 5 since the probability is 5 
#g) p(X=<x) = 0,75 ...???

#Exercise 3
#a) Lambda, this value determines the poisson distribution = 2
#b) Poisson distribution 
#c) P(x=<0) = P(x=0) 
dpois(0,2)
#d) P(x>=1) = 1-P(x<1) = 1-P(x=0) 
1-ppois(1,2)
#e) 50 minutes -> 5*10 minutes lambda = 10 = 2*5
#Should be very low because you have the interval five times
#f) 
barplot(dpois(x=0:10, 4))

#Exercise 4
#a) continuous
#b) normally
#c)
dnorm(100, 85, sqrt(500))
#d)
pnorm(100, 85, sqrt(500))
#e)
pnorm(80, 85, sqrt(500))
#f)
1-pnorm(60, 85, sqrt(500))
#g)
#h) 
qnorm(0.975, 85, sqrt(500))

#Exercise 5
#a) 
1-pt(1,15)
#b) 0.95 because bigger then so all the area right of 0.05 
qt(0.95, 15)
#c)

#Exercise 6
#a) ??? niet helemaal zeker of dit juist is 
qchisq(0.95, 10)

#Exercise 7
#a) 
pf(10, 4, 9)- pf(5,4,9)
#b) 
qf(0.95, 4,9)

#Exercise 8
library(BSDA)
library(PropCIs)
#a) 
xmean <- 41.924
sigma <- 0.3
conf <- 0.95
n <- 10
alpha <- 1-conf
lcl <- xmean-qnorm(1-alpha/2)*sigma/sqrt(n)
ucl <- xmean+qnorm(1-alpha/2)*sigma/sqrt(n)
result <- list(mean=xmean, lcl = lcl, ucl = ucl)
result
#b)
zsum.test(41.924, 0.3, 10)

#Exercise 9
# omdat je met sample standard deviation bezig bent doe je tsum.test 
tsum.test(750, 30, 20, conf.level = 0.95)

#Exercise 10 
#a)
z.test(BLOOD$X.age.,sigma.x =5, conf.level = 0.9, alternative = "two.sided")
#b)
t.test(BLOOD$X.prolactn., alternative = "two.sided")
#c)
#d)
#e)
subs<- subset(BLOOD, BLOOD$X.age. = 50)
nrow(subs)


