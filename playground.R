library(tidyverse)

source("tidy-data.R")


#   filter(data15, Country == "Israel") %>% mutate(year=2015)

entire_data %>% ggplot(aes(x=gdp, y=score, color=year))  + geom_point() # gdp - all

some_data %>% ggplot(aes(x=gdp, y=score, color=year, shape=isIsrael))  + geom_point(size=2) # gdp - some data
entire_data %>% ggplot(aes(x=trust, y=score, color=year, shape=isIsrael))  + geom_point(size=2) # gdp - some data

tidy21 %>% ggplot(aes(x=gdp, y=score, )) + geom_point(color="blue") + labs(title="2021 only") # gdp - only 21
tidy16 %>% ggplot(aes(x=trust, y=score, )) + geom_point(color="blue") + labs(title="2021 only") # gdp - only 21
some_data %>% ggplot(aes(y=score, x=factor(year) ,fill=year))  + geom_boxplot() # boxplot - compare var and mean


 
some_data %>% ggplot(aes(x=`score`, color=year)) + geom_density()



entire_data %>% ggplot(aes(y=score, x=year))  + geom_boxplot()
some_data %>% ggplot(aes(y=score, x=year,fill=year))  + geom_boxplot()
 
 

 score15 = tidy15 %>% select(country, score) 
 score18 = tidy18 %>% select (country, score) 
 score21 = tidy21 %>% select (country, score) %>% rename(score.21 = score)

diff_data = inner_join(x=score15, y=score18, by="country", suffix = c(".15", ".18"))
diff_data = inner_join(x=diff_data, y=score21, by="country")
diff_data = diff_data %>% mutate(diff.18 = score.18-score.15) %>% mutate(diff.21 = score.21-score.18) %>%
          mutate(diff.total = score.21-score.15)
diff_total = diff_data %>% select(country, diff.total)
diff21 = inner_join(tidy21, diff_total, by="country")

diff21 = diff21 %>% mutate(diff.abs = abs(diff.total))

diff21 %>% ggplot(aes(x=diff.total)) + geom_density()

diff21 %>% ggplot(aes(x=score, y=diff.abs)) + geom_point() + geom_smooth()

diff21 %>% ggplot() + geom_qq(aes(sample=diff21$diff.total))



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

diff21 = diff21 %>% mutate(score_level = scoreLevel(diff21$score)) %>%
        mutate(score_level = factor(score_level, levels = c('low', 'medium', 'high')))

diff21 %>% ggplot(aes(x=score_level, y=diff.abs, color=score_level)) + geom_boxplot()

diff21 %>% group_by(score_level) %>% summarise(mean=mean(diff.total))
mean(filter(diff21,score_level=='high')$diff.total)

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
