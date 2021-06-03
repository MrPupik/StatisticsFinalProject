library(tidyverse)

#data collect: 

#data15 = readr::read_csv(file="data/2015.csv")
#data16 = readr::read_csv(file="data/2016.csv")
data17 = readr::read_csv(file="data/2017.csv")
data18 = readr::read_csv(file="data/2018.csv")
data19 = readr::read_csv(file="data/2019.csv")
data20 = readr::read_csv(file="data/2020.csv")
data21 = readr::read_csv(file="data/2021.csv")
allData = c(data17, data18, data19, data20, data21)

#read data:

glimpse(data20)
glimpse(data19)
view(data18)  
view(data20)
view(data19)

# we wantes to check if the coronavirus accured in 2020 have affected the world happiness.  
# h0 = world happiness was higher in 2019 than in 2020 after the corona 
#h1 = else

set.seed(0)
happ19 = data19 %>% sample_n(150) %>% select(Score) %>% mutate(year = 2019)
happ20 = data20 %>% sample_n(150) %>% select(`Ladder score`) %>% mutate(year = 2020) %>% rename(Score = `Ladder score`)
happ1920 = bind_rows(happ19, happ20)

t.test(formula = happ1920$Score ~ happ1920$year, data = happ1920, alternative = "greater")

av_2019 = mean(data19$Score)
av_2020 = mean(data20$`Ladder score`)

boxplot(data19$Score, data20$`Ladder score`, names = c("2019", "2020"))

#sample(1:6, 10, replace=TRUE) 

t.test(x = av_2019,
       y = av_2020,
       paired = TRUE, alternative = "greater")

lm(formula =data18$Score ~ data18$`GDP per capita` ,data=data18)

#רעיונות - אולי להניח נורמליות ולהפריח, מי המדינה הכי שמחה ל2020 (לשים במפה) התלפגות שמחה לפי יבשת, מי שעשיר יותר שמח יותר 

by_reg = data20 %>% select(`Regional indicator`, `Ladder score`)
view(by_reg)

try = aggregate(by_reg[2], list(by_reg$`Regional indicator`), mean)  %>% rename(Continent = Group.1)
#table = table(try)
#view(table)
view(try)
#by_reg %>% group_by(`Regional indicator`) %>% summarize_at(mean(`Ladder score`))
#geom_col(try[1], try[2], blues9)
# we want to check confidence interval, 
boxplot(try$Continent, try$`Ladder score`)
data_west = data20 %>% filter(`Regional indicator` %in% c("Western Europe", 
  "North America and ANZ"))
data_else = data20 %>% filter(!(`Regional indicator` %in% c("Western Europe", 
                                                          "North America and ANZ", "Latin America and Caribbean")))

ggplot(try, aes(`Ladder score`, Continent)) + geom_col()
ggplot(data_west, aes(`Ladder score`)) + geom_density()
ggplot(data_else, aes(`Ladder score`)) + geom_density()
ggplot(data20, aes(`Ladder score`)) + geom_density()

data_else %>% count(data_else$`Regional indicator`)
  
t.test(x = try$Continent,
       y = try$`Ladder score`,
       paired = TRUE, alternative = "greater")

data_west %>% count()
e_kova = mean(data_west$`Ladder score`)
chisq.test(x = data_west$`Ladder score`, p = c(1/25, 1/25, 1/25, 1/25, 1/25, 1/25, 1/25, 1/25, 1/25,
                                       1/25, 1/25, 1/25, 1/25, 1/25, 1/25, 1/25, 1/25, 1/25,
                                       1/25, 1/25, 1/25, 1/25, 1/25, 1/25, 1/25))


#מבחן חי בריבוע להתפלגות טי, ואז נבדוק רווח בר סמך 
#הפרש תוחלות ואז נגיע למסקנה שאם אתה גר במדינות מערביות אתה שמח יותר
#מסחן רווח בר סמך - אם אתה מעל 7, אז בככה וככה סיכוי אתה במדינות מאזור כלשהו



