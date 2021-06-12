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

# life expectancy is between 0-100 in 2020 and 20201, before that range is 0-1
data21 = data21 %>% mutate(`Healthy life expectancy`=`Healthy life expectancy` / 100) # in 2021 it's in years
data20 = data20 %>% mutate(`Healthy life expectancy`=`Healthy life expectancy` / 100) # in 2020 it's in years

# gdp is between 0-100 in 2020 and 20201, before that range is 0-1
data21 = data21 %>% mutate(`Logged GDP per capita`=`Logged GDP per capita` / 10) 
data20 = data20 %>% mutate(`Logged GDP per capita`=`Logged GDP per capita` / 10) # in 2021 it's in years


data18 = data18 %>% filter(!is.na(`Perceptions of corruption`)) %>% transform(`Perceptions of corruption`=as.double(data18$`Perceptions of corruption`))
arrAllData = list(data15, data16, data17, data18, data19, data20 ,data21)


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

for (i in c(1:length(arrAllData))) {
  arrAllData[[i]] = arrAllData[[i]] %>% select(-contains('error')) %>% select(-contains('error')) %>%
    select(-contains('Explained')) %>% select(-contains('Dystopia + residual'))
  arrAllData[[i]] = rename(arrAllData[[i]], dystopia=matches("Dystopia", ignore.case = TRUE))
  arrAllData[[i]] = rename(arrAllData[[i]],   country=contains("Country"),  
                        score=matches("Score", ignore.case = TRUE),
                        gdp=contains("GDP"), trust=contains("Corruption"),
                        health=contains("Health"), freedom=contains("Freedom"),
                        trust=matches("Corruption", ignore.case = TRUE),  dystopia=matches("Dystopia", ignore.case = TRUE))
  
  
  arrAllData[[i]] = arrAllData[[i]] %>% rename(region=matches(".*region.*", ignore.case = TRUE))
  
  arrAllData[[i]] = arrAllData[[i]] %>% mutate(year = current_year) %>% mutate(year = factor(year))
  
  if (any(names(arrAllData[[i]]) == 'region'))
  {
    arrAllData[[i]] = arrAllData[[i]] %>% select(country,score, gdp, trust, health, freedom, trust, year, region) 
  }
  else
  {
    arrAllData[[i]] = arrAllData[[i]] %>% select(country, score, gdp, trust, health, freedom, trust, year) %>% mutate(region = NA)
  }
  
  
  arrAllData[[i]] = arrAllData[[i]] %>% 
    mutate(isIsrael = ifelse(country == "Israel", 1, 0)) %>% mutate(isIsrael = factor(isIsrael))
  
  
  if (current_year > 15)
  {
    current_score_yearly = arrAllData[[i]] %>% select(country, score)
    current_score_yearly[[paste('score', current_year, sep = '')]] = current_score_yearly$score
    current_score_yearly = current_score_yearly[ ,!(colnames(current_score_yearly) == "score")]
    score_yearly = inner_join(score_yearly, current_score_yearly, by="country")
  }
  
  current_year = current_year + 1
  
}

# for (i in c(1:length(arrAllData))) {
#   arrAllData[[i]] = arrAllData[[i]]  %>%
#     mutate(year = factor(year))  %>% 
#     mutate(isIsrael = ifelse(country == "Israel", 1, 0)) %>% mutate(isIsrael = factor(isIsrael))
# }
# 

tidy15 = arrAllData[[1]] 
tidy16 = arrAllData[[2]] 
tidy17 = arrAllData[[3]] 
tidy18 = arrAllData[[4]] 
tidy19 = arrAllData[[5]] 
tidy20 = arrAllData[[6]] 
tidy21 = arrAllData[[7]] 

entire_data = bind_rows(tidy15 ,tidy16, tidy17, tidy18, tidy19, tidy20, tidy21)
some_data = bind_rows(tidy15,tidy18, tidy21)

