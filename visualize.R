library("tidyverse")
data18 = readr::read_csv(file="data/2018.csv")
View(data18)
data18 %>% ggplot(mapping = aes(x=, y=Score)) + ggplot2::geom_point() + geom_smooth()
 


 # peaks %>% select(height_metres) %>% ggplot(mapping=aes(x=peaks$height_metres)) + ggplot2::geom_histogram(bins = 50)
mean(data18$Score)


lm(formula =data18$Score ~ data18$`GDP per capita` ,data=data18)
View(data18)
