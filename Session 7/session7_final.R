###############################
#Solutions exercise session 7
###############################

#Exercise 1
###############################
lab.df <- LAB

x <- c(1,3,4) #Dit doen, anders problemen bij aov
for (i in 1:length(x)){
  lab.df[,x[i]] <- as.factor(lab.df[,x[i]])
}

#a) is answered in the other questions

#b)
lab.subs <- cbind(lab.df$M1, lab.df$M2, lab.df$M3, lab.df$M4)
mean_conc <- rowMeans(lab.subs)
#or mean_conc <- rowMeans(lab.df[,6:9])
lab.df <- data.frame(lab.df, mean_conc)

#c)
diff_conc <- lab.df$Target-lab.df$mean_conc
lab.df <- data.frame(lab.df, diff_conc)

#d)
library(tidyverse)
by_rep_sol <- group_by(lab.df, Sol, Rep)
summarise(by_rep_sol, number=n())
#balanced dataset, number is 12 for each solution and rep

interaction.plot(lab.df$Lab, lab.df$Rep, lab.df$diff_conc, type = "b", pch = c(18,24), col = c(1,2))
#Visualisation of the mean difference for each rep for each lab
#the interaction is parallel in some parts and non parallel in others, so there is some interaction  
#lab on horizontal axis, rep on lines, not that much difference only for L
#based on this plot we can suggest H, the more positive the difference is the less good of a job the lab is doing
#However, for Rep 2 it comes close to lab E, E is consitent for both 1 and 2 rep 

#two way anova
aov1 <- aov(diff_conc~Lab+Rep+Lab*Rep,
            contrasts= list(Lab = "contr.sum", Rep= "contr.sum"),
            data = lab.df)
summary(aov1)

#without interaction, because p(interaction)<0.05
aov2 <- aov(diff_conc~Lab+Rep,
            contrasts= list(Lab = "contr.sum", Rep= "contr.sum"),
            data = lab.df)
summary(aov2)

#diagnostics
library(car)
leveneTest(diff_conc~Lab,data = lab.df)
leveneTest(diff_conc~Rep, data = lab.df)
#There is an indication of unequal variances so
Anova(aov2, type= 'III', white.adjust = 'hc3')
#Indicates both are significant
shapiro.test(aov1$residuals)
hist(aov1$residuals)
#Not normally distributed
plot(cooks.distance(aov1))
# Using non-parametric test for differences between labs (One-Way ANOVA)
kruskal.test(diff_conc~Lab, data = lab.df)

#Van online oplossingen
# d.5
# Using non-parametric method for comparing means
# install.packages("pgirmess")
library(pgirmess)
kruskalmc(diff_conc~Lab, data = lab.df)

# Plotting significantly different means
# in easy to interpret dotchart
lab.kru <- data.frame(kruskalmc(diff_conc~Lab, data = lab.df))
lab.kru <- lab.kru[,3:5]
kru.sig <- as.data.frame(matrix(data = NA, nrow = nrow(lab.kru), ncol = ncol(lab.kru), dimnames = dimnames(lab.kru)))
for(i in 1:nrow(lab.kru)){
  if(lab.kru[i,3] == "TRUE"){
    kru.sig[i,] <- lab.kru[i,]
  }
}

kru.sig <- na.omit(kru.sig)

dotchart(kru.sig[,1], labels = rownames(kru.sig), col = "red", xlim = c(52, 90))
abline(v = 52.39141, lty = 2, col = "grey")

