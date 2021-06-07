library(tidyverse)
library(wesanderson)

#data collect: 
data19 = readr::read_csv(file="data/2019.csv")
data20 = readr::read_csv(file="data/2020.csv")

# we wanted to check if the corona virus happend in 2020 effected the world happiness. 
# our hypothesis ..... will be:
# h0 = the world happiness was the same during 2019 and 2020
# h1 = world happiness was higher in 2019 than in 2020

# we chose a sample of 150 countries from each year data, took their score and combined all into one table.
set.seed(0)
happ19 = data19 %>% sample_n(150) %>% select(Score) %>% mutate(year = 2019)
happ20 = data20 %>% sample_n(150) %>% select(`Ladder score`) %>% mutate(year = 2020) %>% rename(Score = `Ladder score`)
happ1920 = bind_rows(happ19, happ20)

# lets look at the findings in this boxplots:

happ1920 %>% ggplot(aes(y= Score, x=factor(year) ,fill=(year)), color = year) + geom_boxplot() +
  ggtitle("Corona Happiness 2019 and 2020") + xlab("year") + theme(legend.position="none") 

###### i cant change the colors !!!!


# and lets make a t-test:
t.test(formula = happ1920$Score ~ happ1920$year, data = happ1920, alternative = "greater")

# conclusion: because the p-value is .... we are going to ....
# we can see that not only that 2019 wasn't happier than 2020, but that 2020 was even happier than 2019.


# now lets make another test.
# we want to check how the area you live at will effect the happiness score. 
# at first, we took only the data of the area and the score.
# we took the mean of each area and placed the means in a new table.
by_reg = data20 %>% select(`Regional indicator`, `Ladder score`)
mean_by_reg = aggregate(by_reg[2], list(by_reg$`Regional indicator`), mean)  %>% rename(Continent = Group.1)

# now lets view the data: 
ggplot(mean_by_reg, aes(Continent, `Ladder score`)) + geom_col(aes(fill = Continent), width = 0.7) +
  ggtitle("Happiness Score By Area") + theme(legend.position="none") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

# lets make a chi test, to see if we can assume normal distribution at the world happiness score:
interval_breaks = c(0, 1, 2, 3, 4, 5, 6, 7) # אולי להקטין את האינטרוולים בלי 0-1

happy_table = data20%>%
  mutate(happy_count = cut(`Ladder score`, breaks = interval_breaks)) %>%
  sample_n(153) # אולי להקטין גודל מדגם

mu = mean(happy_table$`Ladder score`)
sigma = sd(happy_table$`Ladder score`)

happy_chi_prep = happy_table %>%
  count(happy_count, name = "observed") %>%
  mutate(upper_bound = interval_breaks[-1]) %>%
  mutate(lower_bound = interval_breaks[1:7]) %>%
  mutate(expected_prob = pnorm(q = upper_bound, mean = mu, sd = sigma)-
           pnorm(q = lower_bound, mean = mu, sd = sigma)) %>%
  mutate(expected_prob = expected_prob/sum(expected_prob)) %>%
  mutate(expected = expected_prob*153) %>%
  mutate(chi_comp = (observed-expected)^2/expected)
chi2_0 = sum(happy_chi_prep$chi_comp)
chi2_0

#table value:
qchisq(p = 1-0.05, df = 7-2-1)

# we got a greater value than our table value, so we are going to reject the assumption that 
# the world happiness distribution isn't normal.
# looking at the distribution graph, we noticed that it looks like two normal distributions combaind together. 
ggplot(data20, aes(`Ladder score`)) + geom_density()

# so we decided to check it. we assumed that the area of the country effects the happiness score of the country.
# we added a new column to our data table, distinguishing between west world countries
#(Western Europe and North America and ANZ), and the rest of the world.

data20 = data20 %>% mutate(isWest = ifelse(`Regional indicator` %in% c("Western Europe", "North America and ANZ") 
                                           , "west world country" , "rest of the world"))

# after that we decided to look again at the distribution graphs: 
# density of the west world:
ggplot((data20 %>% filter(isWest == "west world countries")), aes(`Ladder score`)) + geom_density() + 
  ggtitle("west world countries")
# looks very normal!
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
qqnorm((data20 %>% filter(isWest == "west world country"))$`Ladder score`, pch = 1, frame = FALSE,
       main = "Normal QQ Plot - west countries ")
qqline((data20 %>% filter(isWest == "west world country"))$`Ladder score`, col = "steelblue", lwd = 2)
#QQ test for the rest of the world:
qqnorm((data20 %>% filter(isWest == "rest of the world") %>% 
          filter(!(`Regional indicator` == "Latin America and Caribbean")))$`Ladder score`, pch = 1, frame = FALSE,
       main = "Normal QQ Plot - non west countries ")
qqline((data20 %>% filter(isWest == "rest of the world") %>% 
         filter(!(`Regional indicator` == "Latin America and Caribbean")))$`Ladder score`, col = "steelblue", lwd = 2)

# as we can see, this isn't perfectly normal, but for our research it's good enough. 
# we just need to understand that the results we are going to have are not exactly right
# to confirm our assumption, we will make a test of hefresh tohalot:
#lets look at the data
data20 %>% filter(!(`Regional indicator` == "Latin America and Caribbean")) %>%
  ggplot(aes(y=`Ladder score`, x=factor(isWest) ,fill=(isWest)))  + geom_boxplot() # + geom_point()

# to make the t-test we would like to take the same length of the data

######## PROBLEM 
# it is not working :( i wanted to count the number of countries of the west  and all the others to show that
# its not the same and we need to take a sample from the "else" cpuntries
data20 %>% count(isWest == "west world country") #25
data20 %>% count(isWest == "rest of the world") %>% count((`Regional indicator` == "Latin America and Caribbean"))
#its sposed to be 107 and 25 
###### END OF PROBLEM

#and now - the t-test:
t.test(x = (data20 %>% filter(isWest == "west world country"))$`Ladder score`,
       y = ((data20 %>% filter(isWest == "rest of the world") %>% filter(!(`Regional indicator` == "Latin America and Caribbean")))
            %>% sample_n(25))$`Ladder score`,
       paired = TRUE, alternative = "greater")

# conclusion ! 
# we can see that the p-value is around 1e-07 ~ 9e-10 --> this is really small,
# so we can confirm our assumption that living in west countries rasis your happiness score. 





