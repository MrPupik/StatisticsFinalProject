library(tidyverse)
source("tidy-data.R")

view(tidy21)
view(tidy19)
view(score_yearly)
view(happ1920)
view(entire_data)
############################### covid ##################################

# we wanted to check if the corona virus happened in 2020 effected the world happiness. 
# our hypothesis ..... will be:
# h0 = the world happiness was the same during 2019 (before) and 2021 (after)
# h1 = world happiness was higher in 2019 than in 2021 - the corona virus caused the world happiness to descend

t.test(x = score_yearly$score21 , y = score_yearly$score19 , paired = TRUE, alternative = "less")

#p-value very high - we accept h0 , but lets look at the findings in these boxplots:

entire_data %>% filter(year %in% c(19,21)) %>% ggplot(aes(y= score, x=factor(year) ,fill=factor(year)), color = year) + geom_boxplot() +
  ggtitle("Corona Happiness 2019 and 2021") + xlab("year") + theme(legend.position="none") 

# we can see that the mean of the happiness score is higher in 2021 than in 2019
#  lets make a paird t-test to check it:

t.test(x = score_yearly$score21 , y = score_yearly$score19 , paired = TRUE, alternative = "greater")

###############################  end of covid ##################################

###############################  diff between west and else ##################################

# we want to check how the area you live at will effect the happiness score. 
# at first, we took only the data of the area and the score.
# we took the mean of each area and placed the means in a new table.
by_reg = tidy21 %>% select(region, score)
mean_by_reg = aggregate(by_reg[2], list(by_reg$region), mean)  %>% rename(Region = Group.1)

# now lets view the data:
ggplot(mean_by_reg, aes(Region, score)) + geom_col(aes(fill = score), width = 0.7) +
  ggtitle("Happiness Score By Area") + theme(legend.position="none") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))+ylab("Score") + 
  scale_fill_gradient(low = "orchid4", high = "plum2", space = "Lab",  guide = "colourbar",  aesthetics = "fill" )

# lets make a chi test, to see if we can assume normal distribution at the world happiness score in 2021:

# creating groups of happiness score, to see how many measurements are actually in each group
happy_new = tidy21 %>% 
  mutate(new_groups = cut(score, breaks = c(0, 4, 5, 6, 7, 8)))   
# we need to make sure that there are more than 5 in each group 
happy_new %>% count(new_groups)
# making a test shows whats the possibility to be in each of the groups (until 4, until 5, until 6....), according to normal distribution 
pnorm(q = c(4, 5, 6, 7, 8), mean = mean(tidy21$score), sd = sd(tidy21$score))
#multiple by the number of countries (153) we will get the expected cumulative 
expected_cum = pnorm(q = c(4, 5, 6, 7, 8), mean = mean(tidy21$score), sd = sd(tidy21$score)) *149
# to see the exact number of expediency we will have to subtract the wanted group with all of her prev
expected = expected_cum[1:5] - c(0, expected_cum[1:4])

by_group = happy_new %>% count(new_groups, name = "observed") %>% mutate(expected = expected) %>% mutate(chi = ((expected-observed)^2)/expected)
chi_sq = sum(by_group$chi)
chi_sq

#table value:
qchisq(p = 1-0.05, df = 5-2-1)

# we got a very small p-value ! also smaller than our table value, it means we are going to accept the assumption that 
# the world happiness distribution is normal.
# now lets check if the mean of the west is bigger than the others

west = tidy21 %>% filter(region %in% c("Western Europe", "North America and ANZ"))
mu = mean(tidy21$score)
t.test(x = west$score, alternative = "greater", mu = mu, conf.level = 0.99)

tidy20 %>% 
  ggplot(aes(y=score, x=factor(isWest) ,fill=(isWest), width = 0.5))  + geom_boxplot() +
  theme(legend.position="none", axis.text.x = element_text(color = "black", size = 15 )) + xlab("") + ylab("Score")# + geom_point()


# we get a really small p-value , so we can see we are right  in 99%!

###############################  end of diff between west and else ##################################

# we chose a sample of 150 countries from each year data, took their score and combined all into one table.
set.seed(0)
happ19 = data19 %>% sample_n(150) %>% select(Score) %>% mutate(year = 2019)
happ20 = data20 %>% sample_n(150) %>% select(`Ladder score`) %>% mutate(year = 2020) %>% rename(Score = `Ladder score`)
happ1920 = bind_rows(happ19, happ20)

t.test(formula = happ1920$Score ~ happ1920$year, data = happ1920, alternative = "less")

# paired t-test:
t.test(formula = happ1920$Score ~ happ1920$year, data = happ1920, paired = TRUE , alternative = "two.sided")


#QT \ PT ()

#p-value is more than 0.05 , try to do it paired 
# מדגם בין מדינות דומות, פיירד = טרו 
# יוצא פי ואליו ממש גדול - כלומר מקבלים את השארת ה 0 , אז האם להתייחס לזה? האם להמשיך לחקור את זה? או פשוט להגיד שזה לא נכון שהקורונה הורידה את השמחה ולעבור הלאה
# לעשות דו כיווני או חד?


# conclusion: because the p-value is .... we are going to ....
# we can see that not only that 2019 wasn't happier than 2020, but that 2020 was even happier than 2019.






happy_table = tidy20%>%
  mutate(happy_count = cut(score, breaks =  c(0, 4, 5, 6, 7, 8))) 
#happy_table$happy_count
view(happy_table)
mu = mean(happy_table$score)
sigma = sd(happy_table$score)

happy_chi_prep = happy_table %>%
  count(happy_count, name = "observed") %>%
  mutate(upper_bound = c(0, 4, 5, 6, 7, 8)[-1]) %>%
  mutate(lower_bound = c(0, 4, 5, 6, 7, 8)[1:5]) %>%
  mutate(expected_prob = pnorm(q = upper_bound, mean = mu, sd = sigma)-
           pnorm(q = lower_bound, mean = mu, sd = sigma)) %>%
  mutate(expected_prob = expected_prob/sum(expected_prob)) %>%
  mutate(expected = expected_prob*153) %>%
  mutate(chi_comp = (observed-expected)^2/expected)
chi2_0 = sum(happy_chi_prep$chi_comp)
chi2_0

view(happy_chi_prep)

#table value:
qchisq(p = 1-0.05, df = 5-2-1)

# another try:

view(tidy20)
view(by_group)


# we got a greater value than our table value, it means we are going to reject the assumption that 
# the world happiness distribution is normal.
# looking at the distribution graph, we noticed that it looks like two normal distributions combined together. 
ggplot(data20, aes(`Ladder score`)) + geom_density()

# we decided to check it. as you can see in the Happiness Score By Regoin plot, the average score of the 
# western countries, is much higher than the others.we assumed that the area of the country effects the
# happiness score of the country.
# to check it, we added a new column to our data table, distinguishing between west world countries
#(Western Europe and North America and ANZ), and the rest of the world.

tidy20 = tidy20 %>% mutate(isWest = ifelse(`Regional indicator` %in% c("Western Europe", "North America and ANZ") 
                                           , "west world country" , "rest of the world"))

# after that we decided to look again at the distribution graphs: 
# density of the west world:
ggplot((data20 %>% filter(isWest == "west world country")), aes(`Ladder score`)) + geom_density() + 
  ggtitle("west world countries")
# looks pretty normal!
# density of the rest of the world:
ggplot((data20 %>% filter(isWest == "rest of the world")),  aes(`Ladder score`)) + geom_density() +
  ggtitle("non west world countries")
# we can see that its still not very normal, we filtered the Latin America and Caribbean countries to help 
# our data to look more normal
ggplot((data20 %>% filter(isWest == "rest of the world") %>% filter(!(`Regional indicator` == "Latin America and Caribbean")))
       , aes(`Ladder score`)) + geom_density() + ggtitle("non west world countries")
# now its better :)

# so we got two graphs that looks pretty normal. 
# lets test it in a QQ test:
#QQ test for the west countries:
qqnorm((tidy20 %>% filter(isWest == "west world country"))$`Ladder score`, pch = 1, frame = FALSE,
       main = "Normal QQ Plot - west countries ")
qqline((tidy20 %>% filter(isWest == "west world country"))$`Ladder score`, col = "steelblue", lwd = 2)
#QQ test for the rest of the world:
qqnorm((tidy20 %>% filter(isWest == "rest of the world") %>% 
          filter(!(`Regional indicator` == "Latin America and Caribbean")))$`Ladder score`, pch = 1, frame = FALSE,
       main = "Normal QQ Plot - non west countries ")
qqline((tidy20 %>% filter(isWest == "rest of the world") %>% 
         filter(!(`Regional indicator` == "Latin America and Caribbean")))$`Ladder score`, col = "steelblue", lwd = 2)

# as we can see, this isn't perfectly normal, but for our research it's good enough. 
# we just need to understand that the results we are going to have are not exactly right
# to confirm our assumption, we will make a test of hefresh tohalot:
#lets look at the data

tidy20 %>% filter(!(region == "Latin America and Caribbean")) %>%
  ggplot(aes(y=`Ladder score`, x=factor(isWest) ,fill=(isWest), width = 0.5))  + geom_boxplot() +
  theme(legend.position="none", axis.text.x = element_text(color = "black", size = 15 )) + xlab("") + ylab("Score")# + geom_point()

tidy20 %>% 
  ggplot(aes(y=score, x=factor(isWest) ,fill=(isWest), width = 0.5))  + geom_boxplot() +
  theme(legend.position="none", axis.text.x = element_text(color = "black", size = 15 )) + xlab("") + ylab("Score")# + geom_point()


# to make the t-test we would like to take the same length of the data

######## PROBLEM 
# it is not working :( i wanted to count the number of countries of the west  and all the others to show that
# its not the same and we need to take a sample from the "else" countries
data20 %>% count(isWest == "west world country") #25
data20 %>% count(isWest == "rest of the world") %>% count((`Regional indicator` == "Latin America and Caribbean"))
#its sposed to be 107 and 25 
###### END OF PROBLEM

#and now - the t-test:
t.test(x = (data20 %>% filter(isWest == "west world country"))$`Ladder score`,
       y = ((data20 %>% filter(isWest == "rest of the world") %>% filter(!(`Regional indicator` == "Latin America and Caribbean")))
            %>% sample_n(25))$`Ladder score`, paired = TRUE, alternative = "greater")

## we can try to do paired = false, and than we wont have to take a sample of 25 countries 
# it worked!!!

t.test(x = (data20 %>% filter(isWest == "west world country"))$`Ladder score`,
       y = (data20 %>% filter(isWest == "rest of the world") %>% filter(!(`Regional indicator` == "Latin America and Caribbean")))$`Ladder score`, 
       paired = FALSE, alternative = "greater")

# conclusion ! 
# we can see that the p-value < 2.2e-16 --> this is really small,
# so we can confirm our assumption that living in west countries raieses your happiness score. 




