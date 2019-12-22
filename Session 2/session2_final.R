###############################
#Solutions exercise session 2
###############################

#Exercise1
###############################
  #a) The most obvious variable is the result of a thrown dice, so 1, 2, 3, 4, 5, 6
  #b) Discrete, only a finite number of possibilities 1:6
  #c)
  x <- c(1:6)
  p <- rep(1/6,6)  
  plot(x, p, xlab = "x value", ylab = "probability", col = 2)
  #d)
  x <- c(1:6)
  p <- seq(from = 1/6, to=1, by=1/6)
  plot(x, p, xlab = "x value", ylab = "cummulative probability", col = 2)  

#Exercise2
###############################   
  #a) The result of a coin toss
  #b) Discrete
  #c) 1/2 and 1/2
  #d) P(X =< 6) -> pbinom
  pbinom(6, 10, 1/2)
  #e) 1 - P(X =< 6)
  1- pbinom(6, 10, 1/2)#or
  pbinom(6, 10, 1/2, lower.tail = F)
  #Lower tail, false is eigenlijk spiegelen, je had 1-pbinom dus dan neem je alleen groter dan 6. 
  #pbinom lower.tail false is hetzelfde als dit maar gespiegeld. 
  #Beste is wel op gewoon met 1 - pbinom wanneer > en pbinom wanneer < te werken
  #f)!!! median = quantile 50% => qbinom (quantile function)
  qbinom(1/2, 10, 1/2) 
  #g)
  qbinom(3/4, 10, 1/2)

#Exercise3
###############################  
  #a) The number of bugs in a timeframe, 10 minutes
  #b) Poisson distribution 
  #c)
  dpois(0, 2) 
  #d)
  1 - ppois(0,2)
  #e)
  dpois(0, 10)
  #f)
  threehours <- seq(0, 18, 1)
  dens <- dpois(threehours, 4)  
  plot(threehours, dens)  
  
#Exercise4
###############################   
  xmean <- 85
  var <- 500
  sd <-sqrt(var)  
  #a) continuous
  #b) normal distribution
  #c) 
  dnorm(100, xmean, sd)
  #d)
  pnorm(100, xmean, sd)
  #e)
  pnorm(80, xmean, sd)
  #f)
  1-pnorm(60, xmean, sd)
  #g)
  x.60 <- 60
  lim <- qnorm(0.975, 85, s.d)
  lim.u <- 170
  lim.l <- 0
  x <- seq(lim.l,lim.u,length=1000)
  i.60 <- x >= x.60 & x <= lim.u
  hx <- dnorm(x, mean = 85, sd = s.d)
  
  plot(x, hx, type="n", xlab="", ylab="",
       main=expression(mu ~ "= 85 kg," ~ sigma ~ "= 22.36 kg"),
       axes=FALSE)
  lines(x, hx)
  polygon(c(x.60,x[i.60],lim.u), c(0,hx[i.60],0), col="green")
  result <- "P(X > 60)"
  mtext(result,3)
  axis(1, at=c(lim.l, x.60, 85, lim.u), pos=0)
  
  #h)
  qnorm(0.975, xmean, sd)
  
#Exercise5
############################### 
  #a)
  1-pt(1,15)
  #b)
  qt(0.95,15)#or
  qt(0.05, 15, lower.tail = FALSE)
  
#Exercise 6
############################### 
  qchisq(0.95, 10)#or
  qchisq(0.05, 10, lower.tail = FALSE) 
  
#Exercise 7
############################### 
  #a)
  diff(pf(c(5,10), 4, 9))
  #b)
  qf(0.95, 4, 9)
  
#Exercise 8
###############################
library(BSDA)
library(PropCIs)  

  #a)
  conf <- 0.95
  sigma <- 0.3
  n <- 10  
  xmean <- 41.924  
  alpha <- 1-conf  
  lcl <- xmean-qnorm(1-alpha/2)*sigma/sqrt(n)  
  ucl <- xmean+qnorm(1-alpha/2)*sigma/sqrt(n)
  result <- list(mean=xmean, lcl = lcl, ucl = ucl)
  result  
  #b)
  zsum.test(mean.x = xmean, sigma.x = sigma, n.x = n, conf.level = conf)

#Exercise 9
###############################
  n <- 20
  conf <- 0.95    
  xmean <- 750  
  sigma <- 30  
  alpha <- 1-conf  
  zsum.test(mean.x = xmean, sigma.x = sigma, n.x = n, conf.level = conf)  

#Exercise 10
###############################
  blood.df <- BLOOD
  #a)
  xmean <- mean(blood.df$X.age.)    
  z.test(blood.df$X.age., mu = xmean, sigma.x = 5, conf.level = 0.90) 
  #b)
  t.test(blood.df$X.prolactn., conf.level = 0.95)
  #c)
  between50and60 <- subset(blood.df, blood.df$X.age.>50 & blood.df$X.age.<60)
  proportion50and60 <- nrow(between50and60)/nrow(blood.df)   
  proportion50and60      
  #d)
  prop.test(nrow(between50and60), nrow(blood.df))
  #e)  
  lower50 <- subset(blood.df, blood.df$X.age. < 50)  
  #f)
  nrow(lower50)
  shapiro.test(lower50$X.testost.) #normality check because the sample size is small 
  t.test(lower50$X.testost., conf.level = 0.99)  

    