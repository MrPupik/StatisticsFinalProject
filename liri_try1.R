library("tidyverse")
data21 = readr::read_csv(file="data/2021.csv")
View(data21)

#lets look how to relation between the Healthy life expectancy and the Total score:
data21 %>% ggplot(aes(y= `Ladder score` , x= `Healthy life expectancy` ))+
geom_point(color="deeppink")+ scale_y_log10()+
stat_smooth(method = "lm",color="black")

#The R-Squared is high-middle,  0.59, which means that around 60% of the variation in the dependent variable is explained by the independent variable. 
#Most important check the p-Value < 2.2e-16  because, We can consider a linear model to be statistically significant only when p-Value is less that the statistical significance level, which is 0.05. 
#To sum up this results we can determine which there is a linear corrolotaion between score and Healthy life expectancy.

data21_lm = lm(formula =data21$`Ladder score` ~ data21$`Healthy life expectancy` ,data=data21)
summary(data21_lm)

#scatteR.smooth(x=data21$`Ladder score`, y=data21$'Healthy life expectancy', main="score ~ Healthy") 

score.res = resid(data21_lm)

#plot(data21$"Healthy life expectancy", score.res, 
#ylab="score", xlab="Healthy", 
#main="Health~score")
#abline(0, 0) 

#we have two assumptions about the regretion model:
#1. residuals are homoscedastic.

data21_lm %>% ggplot(aes(x=.fitted,y=.resid, frame=FALSE)) +
geom_point(alpha=0.1) + geom_hline(yintercept=0) +
labs(title="Residual Plot")

 #data21_lm %>% ggplot(aes(sample=.resid)) +
  #geom_qq() + geom_qq_line(color="cyan4") +
  #labs(title="QQ Plot")

#2. residuals are distributed normally.
#it is a graphical display to compare a data set to a particular probability distribution. 
 qqnorm(data21$'Ladder score', pch = 1, frame = FALSE)
 qqline(data21$'Ladder score', col = "steelblue", lwd = 2)
 
 #conclusions:
 #The "Residuals Plot shows homoscedastic.
 #In addition, If the data are exactly normal, we expect the points to lie on a straight line.
 #our QQ plot shows that our residuals are distributed normal 
 #our model can predicting.