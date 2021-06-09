library(tidyverse)

# data exploration

data15 = readr::read_csv(file="data/2015.csv")
data16 = readr::read_csv(file="data/2016.csv")
data17 = readr::read_csv(file="data/2017.csv")
data18 = readr::read_csv(file="data/2018.csv")
data19 = readr::read_csv(file="data/2019.csv")
data20 = readr::read_csv(file="data/2020.csv")
data21 = readr::read_csv(file="data/2021.csv")

# glimpse(data15)
# glimpse(data16)
# glimpse(data17)
# glimpse(data18)
# glimpse(data19)
# glimpse(data20)
# glimpse(data21)

# column names are not the same between different years.
# we are going to have to rename them.
# we wont to compare israel's data between years - so tidy data to make country column :

data21 = data21 %>% mutate(`Healthy life expectancy`=`Healthy life expectancy` / 100) # in 2021 it's in years
data20 = data20 %>% mutate(`Healthy life expectancy`=`Healthy life expectancy` / 100) # in 2021 it's in years

data21 = data21 %>% mutate(`Logged GDP per capita`=`Logged GDP per capita` / 10) # in 2021 it's in years
data20 = data20 %>% mutate(`Logged GDP per capita`=`Logged GDP per capita` / 10) # in 2021 it's in years


data18 = data18 %>% filter(!is.na(`Perceptions of corruption`)) %>% transform(`Perceptions of corruption`=as.double(data18$`Perceptions of corruption`))
AllData = list(data15, data16, data17, data18, data19, data20 ,data21)


# data21 = data21 %>% select(-contains('error')) %>% select(-contains('error')) %>%
#   select(-contains('Explained')) %>% select(-contains('Dystopia + residual'))
# data21 = rename(data21, dystopia=matches("Dystopia", ignore.case = TRUE))
# data21 = rename(data21,   country=contains("Country") ,score=matches("Score", ignore.case = TRUE),
#                       gdp=contains("GDP"), trust=contains("Corruption"),
#                       health=contains("Health"), freedom=contains("Freedom"),
#                       trust=matches("Corruption", ignore.case = TRUE),  dystopia=matches("Dystopia", ignore.case = TRUE))
# data21 = data21 %>% select(country, score, gdp, trust, health, freedom, trust, dystopia)

current_year = 15

score_yearly = data15 %>% select(Country, `Happiness Score`) %>% rename(score15=`Happiness Score`, country=Country)

for (i in c(1:length(AllData))) {
  AllData[[i]] = AllData[[i]] %>% select(-contains('error')) %>% select(-contains('error')) %>%
    select(-contains('Explained')) %>% select(-contains('Dystopia + residual'))
  AllData[[i]] = rename(AllData[[i]], dystopia=matches("Dystopia", ignore.case = TRUE))
  AllData[[i]] = rename(AllData[[i]],   country=contains("Country"),  
                  score=matches("Score", ignore.case = TRUE),
                  gdp=contains("GDP"), trust=contains("Corruption"),
                  health=contains("Health"), freedom=contains("Freedom"),
                  trust=matches("Corruption", ignore.case = TRUE),  dystopia=matches("Dystopia", ignore.case = TRUE))
  
  
  AllData[[i]] = AllData[[i]] %>% rename(region=matches("region", ignore.case = TRUE))
  AllData[[i]] = AllData[[i]] %>% mutate(year = current_year)
  
  if (any(names(AllData[[i]]) == 'region'))
  {
      AllData[[i]] = AllData[[i]] %>% select(country,score, gdp, trust, health, freedom, trust, year, region) 
  }
  else
  {
    AllData[[i]] = AllData[[i]] %>% select(country, score, gdp, trust, health, freedom, trust, year) %>% mutate(region = NA)
  }
  
  current_score_yearly = AllData[[i]] %>% select(country, score)
  current_score_yearly[[paste('score', current_year, sep = '')]] = current_score_yearly$score
  
  if (current_year > 15)
  {
    score_yearly = inner_join(score_yearly, current_score_yearly, by="country")
  }
  
  current_year = current_year + 1
  
}


 entire_data = bind_rows(AllData[[1]],AllData[[2]],AllData[[3]], AllData[[4]], AllData[[5]], AllData[[6]], AllData[[7]])
 some_data = bind_rows(AllData[[1]],AllData[[4]], AllData[[7]])
 
 # factor
 entire_data = entire_data %>% mutate(year = factor(year))
 some_data = some_data %>% mutate(year = factor(year))

 entire_data = entire_data %>% mutate(isIsrael = ifelse(country == "Israel", 1, 0))
 some_data = some_data %>% mutate(isIsrael = ifelse(country == "Israel", 1, 0))
#   filter(data15, Country == "Israel") %>% mutate(year=2015)

 entire_data %>% ggplot(aes(x=gdp, y=score, color=year))  + geom_point() # gdp - all

some_data %>% ggplot(aes(x=gdp, y=score, color=year, shape=factor(isIsrael)))  + geom_point(size=3) # gdp - some data
AllData[[7]] %>% ggplot(aes(x=gdp, y=score, color=year))  + geom_point() # gdp - only 21
some_data %>% ggplot(aes(y=score, x=factor(year) ,fill=year))  + geom_boxplot() # boxplot - compare var and mean


 
some_data %>% ggplot(aes(x=`score`, color=year)) + geom_density()


View(AllData[[6]])
# ggplot(data, aes(x=))

 entire_data %>% ggplot(aes(y=score, x=year))  + geom_boxplot()
 some_data %>% ggplot(aes(y=score, x=year,fill=year))  + geom_boxplot()
 
 

 score15 = AllData[[1]] %>% select(country, score) 
 score18 = AllData[[3]] %>% select (country, score) 
 score21 = AllData[[7]] %>% select (country, score) %>% rename(score.21 = score)

diff_data = inner_join(x=score15, y=score18, by="country", suffix = c(".15", ".18"))
diff_data = inner_join(x=diff_data, y=score21, by="country")
diff_data = diff_data %>% mutate(diff.18 = score.18-score.15) %>% mutate(diff.21 = score.21-score.18) %>%
          mutate(diff.total = score.21-score.15)
diff_total = diff_data %>% select(country, diff.total)
data21 = inner_join(AllData[[7]], diff_total, by="country")


data21 %>% ggplot(aes(x=score, y=diff.total)) + geom_point() + geom_line()

scoreLevel = function (scores){
 sapply(scores, function(scores){
   if (scores < 4){
     return('low')
   }
   if(scores<6){
     return('medium')
   }
   return('high')
 })
  
}

data21 = data21 %>% mutate(score_level = scoreLevel(data21$score)) %>%
        mutate(score_level = factor(score_level, levels = c('low', 'medium', 'high')))

data21 %>% ggplot(aes(x=score_level, y=diff.total)) + geom_boxplot()

data21 %>% group_by(score_level) %>% summarise(mean=mean(diff.total))
mean(filter(data21,score_level=='high')$diff.total)

# myworld = world %>% rename(country=region)
# myworld = inner_join(myworld, data21, by="country")
# library(maps)
# world = map_data('world') 
# 
# ggplot(world) + 
#   geom_map(mapping=aes(long, lat, map_id=country), map=world) + ggtitle("who gets better") 
# 
# View(myworld)
# View(world)
