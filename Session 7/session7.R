#Exercise1
#a)
#answered throughout the exercise 
lab.df <- LAB

#b)
mean_conc <- rowMeans(lab.df[-1:-5])
lab.df <- data.frame(lab.df, mean_conc)

#c)
#we calculate the difference to work with
lab.df <- transform(lab.df, difference = Target-mean_conc)
lab.df
#What are the factors for the anova, 
#Lab and rep -> 2*12 = 24 factors, full anova model is needed, done in part d 
#Solutions are not, can be assumed to be indepent 

#d)
  #1
  dens <- by(lab.df$Sol,lab.df$Lab, density)
  #the number of each list is 7, so balanced
  #2
  interaction.plot(lab.df$Lab, lab.df$Rep, lab.df$difference, type="b", pch=c(18,24), col=c(1,2))
  #the interaction is parallel in some parts and non parallel in others so there is some interaction  
  #lab on horizontal axis, rep on lines, not that much difference only for L
  #based on this plot we can suggest H, the more positive the difference is the less good of a job the lab is doing
  #However, for secon it comes close to lab E, E is consitent for both 1 and 2 rep 
  #3
  lab.aov <- aov(lab.df$difference~lab.df$Lab+lab.df$Rep+lab.df$Lab*lab.df$Rep)
  summary(lab.aov)                   
  #interaction term is not significant, p=0.06473, This means the lines are parallel
  #hence drop the interaction term
  #
  lab.aov1 <- aov(lab.df$difference~lab.df$Lab+lab.df$Rep)
  summary(lab.aov1) 
  #only focussed on the main effects, they are high significant. 
  library(car)
  leveneTest(lab.df$difference~lab.df$Lab) #computes test for homogenity of variances, p<0.05 so H0: homogenity is accepted
  leveneTest(lab.df$difference~lab.df$Rep) #werkt niet om de 1 of andere reden
  #see for the rest of the solution in the solution file
  #There seems to be some difference between the labs and between the replications
  
  #Cooks distace, only two outliers, not necessary to remove these
  #If you remove these observations you need to change the linear model accordingly
  
  #shapiro test, normailty does not seem to work 
  
  #Mutiple comparison
  #You can now drop the replication factor, only focussed on lab factor
  #pgirmess kruskalmc
  #only few of the pairwise are significant
  #significant differences are extracted and plotted 
  #difference between H-L is positive so H is underestimating the calcium concentration more then L
  #Several labs are doing better then H but also some like A and B are doing worse, H somewere in the middle
  #You cannot tell what is the best lab here because you do not have all pairwise comparisons
  #G seems to be better???
  
  
  #
  