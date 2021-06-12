source("tidy-data.R")




library("tidyverse")

#lets look how to relation between the Healthy life expectancy and the Total score:
tidy21 %>% ggplot(aes(y= score, x= health ))+
geom_point(color="deeppink")+ scale_y_log10()+
stat_smooth(method = "lm",color="black")

#The R-Squared is high-middle,  0.59, which means that around 60% of the variation in the dependent variable is explained by the independent variable. 
#Most important check the p-Value < 2.2e-16  because, We can consider a linear model to be statistically significant only when p-Value is less that the statistical significance level, which is 0.05. 
#To sum up this results we can determine which there is a linear corrolotaion between score and Healthy life expectancy.
ml = tidy21 %>% select(-c('region', 'isIsrael', 'year', 'country'))


tidy21_lm = lm(formula=score ~ . ,data=ml)
summary(tidy21_lm)

#scatteR.smooth(x=tidy21$`Ladder score`, y=tidy21$'Healthy life expectancy', main="score ~ Healthy") 

score.res = resid(tidy21_lm)

#plot(tidy21$"Healthy life expectancy", score.res, 
#ylab="score", xlab="Healthy", 
#main="Health~score")
#abline(0, 0) 

#we have two assumptions about the regretion model:
#1. residuals are homoscedastic.

tidy21_lm %>% ggplot(aes(x=.fitted,y=.resid, frame=FALSE)) +
geom_point(fill="white", shape=21, size=2) + geom_hline(yintercept=0) +
labs(title="Residual Plot")

 #tidy21_lm %>% ggplot(aes(sample=.resid)) +
  #geom_qq() + geom_qq_line(color="cyan4") +
  #labs(title="QQ Plot")

#2. residuals are distributed normally.
#it is a graphical display to compare a data set to a particular probability distribution. 
 qqnorm(tidy21$'Ladder score', pch = 1, frame = FALSE)
 qqline(tidy21$'Ladder score', col = "steelblue", lwd = 2)
 
 #conclusions:
 #The "Residuals Plot shows homoscedastic.
 #In addition, If the data are exactly normal, we expect the points to lie on a straight line.
 #our QQ plot shows that our residuals are distributed normal 
 #our model can predicting.