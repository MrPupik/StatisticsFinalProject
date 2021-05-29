library(tidyverse)
#data15 = readr::read_csv(file="data/2015.csv")
#data16 = readr::read_csv(file="data/2016.csv")
data17 = readr::read_csv(file="data/2017.csv")
data18 = readr::read_csv(file="data/2018.csv")
data19 = readr::read_csv(file="data/2019.csv")
data20 = readr::read_csv(file="data/2020.csv")
data21 = readr::read_csv(file="data/2021.csv")

glimpse(data20)
View(data20)


allData = c(data17, data18, data19, data20, data21)


IsraelData = filter(data17, country == "Israel") %>% mutate(year=year)

year = 18
for (data in alldata){
  data = filter(data.entry(),  == "Israel") %>% mutate(year=year)
  IsraelData = bind_rows(IsraelData, data)
  year = year + 1
  
}




glimpse(true)
glimpse(fake)

true %>% count(subject)
fake %>% count(subject)

fake = fake %>% mutate(true = 0)
true = true %>% mutate(true = 1)

minmize_subjects <- Vectorize(function(sub) {
  if (sub %in% c("politics", "politicsNews"))
  {
    return("politics")
  }
  else{
    return("non-politics")
  }
}
)
data = bind_rows(true, fake) %>% mutate(subject = minmize_subjects(subject))
data %>% count(subject, true)



