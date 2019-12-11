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
  1- pbinom(6, 10, 1/2)
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
  