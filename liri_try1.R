library("tidyverse")
data18 = readr::read_csv(file="data/2018.csv")
View(data18)
data18 %>% ggplot(mapping = aes(x=`Healthy life expectancy`, y= Score)) + ggplot2::geom_point() + geom_smooth()

data18 %>% ggplot(aes(y= Score , x= `Healthy life expectancy` ))+
geom_point(color="lightskyblue")+ scale_y_log10()+
stat_smooth(method = "lm",color="black")

data18_lm$residuals
data18_new = data18 %>%
  mutate(`Healthy life expectancy` =)

sum18 = lm(formula =data18$Score ~ data18$`Healthy life expectancy` ,data=data18)
summary(sum18)


# data18.av <- aov(data18)
# summary(data18.av)