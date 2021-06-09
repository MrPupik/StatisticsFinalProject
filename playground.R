library(tidyverse)

source("tidy-data.R")


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
