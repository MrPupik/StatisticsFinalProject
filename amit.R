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

#view data if you want:

glimpse(data20)
glimpse(data19)
view(data18)  
view(data20)
view(data19)

#רעיונות - אולי להניח נורמליות ולהפריח, מי המדינה הכי שמחה ל2020 (לשים במפה) התלפגות שמחה לפי יבשת, מי שעשיר יותר שמח יותר 
#מבחן חי בריבוע להתפלגות טי, ואז נבדוק רווח בר סמך 
#הפרש תוחלות ואז נגיע למסקנה שאם אתה גר במדינות מערביות אתה שמח יותר
#מסחן רווח בר סמך - אם אתה מעל 7, אז בככה וככה סיכוי אתה במדינות מאזור כלשהו


# we want to check if the corona virus accrued in 2020 effected the world happiness. 
# מודל ההשארות שלנו יהיה:
# h0 = the world happiness was the same during 2019 and 2020
# h1 = world happiness was higher in 2019 than in 2020

#  צריך לברר מה קורה אם עשינו מבחן ימני או מה שזה לא יהיה ובסוף יצא בכלל הפוך לגמרי ממה שקיווינו

# we chose a sample of 150 countries from each year data, took their score and combined all into one table.

set.seed(0)
happ19 = data19 %>% sample_n(150) %>% select(Score) %>% mutate(year = 2019)
happ20 = data20 %>% sample_n(150) %>% select(`Ladder score`) %>% mutate(year = 2020) %>% rename(Score = `Ladder score`)
happ1920 = bind_rows(happ19, happ20)

# making a t test to the data, we found that not only that 2019 wastnt happier than 2020 - but 2020 was even happier than 2019.

t.test(formula = happ1920$Score ~ happ1920$year, data = happ1920, alternative = "greater")

# סתם שמישהו לעזזל יסביר לי מה המבחן הזה אומרררר לא היינו אמורים למצוא איזה ערך מסויים ולהגיד אם הוא גדול או קטן מהערך בטבלה וככה להסיק מסקנות?

#חישוב ממוצע מה זה למה צריך את זה בכלל ולמה זה לא יוצא אותו דבר כמו במבחן טי למעלה
av_2019 = mean(data19$Score)
av_2020 = mean(data20$`Ladder score`)

# but anyway, you can also see these findings in this boxplot: 

boxplot(data19$Score, data20$`Ladder score`, names = c("2019", "2020"))

# למה עשיתי את זה
lm(formula =data18$Score ~ data18$`GDP per capita` ,data=data18)

# now we want to check how will the area you live at is going to effect the happiness score. 
#at first, we took only the data of the area and the score.we took the mean of each area and placed the means in a new table.

by_reg = data20 %>% select(`Regional indicator`, `Ladder score`)
#view(by_reg)
mean_by_reg = aggregate(by_reg[2], list(by_reg$`Regional indicator`), mean)  %>% rename(Continent = Group.1)
#view(mean_by_reg)

# now lets take the data and view it:
ggplot(try, aes(`Ladder score`, Continent)) + geom_col()
# אם מצליחים לסובב את השמות של היבשות בציר האיקס אז הגרף הזה יותר טוב
ggplot(try, aes(Continent, `Ladder score`)) + geom_col()


# אני לא יודעת מה אלו 
by_reg %>% group_by(`Regional indicator`) %>% summarize_at(mean(`Ladder score`))
geom_col(try[1], try[2], blues9)
# we want to check confidence interval, 
data_else %>% count(data_else$`Regional indicator`)
t.test(x = try$Continent,
       y = try$`Ladder score`,
       paired = TRUE, alternative = "greater")
e_kova = mean(data_west$`Ladder score`)
chisq.test(x = data_west$`Ladder score`, p = c(1/25, 1/25, 1/25, 1/25, 1/25, 1/25, 1/25, 1/25, 1/25,
                                       1/25, 1/25, 1/25, 1/25, 1/25, 1/25, 1/25, 1/25, 1/25,
                                       1/25, 1/25, 1/25, 1/25, 1/25, 1/25, 1/25))
# נגמר הדברים שאני לא יודעת מה הם

# lets see if we can assume normal distribution at the world happiness score

interval_breaks <- c(0, 1, 2, 3, 4, 5, 6, 7) # אולי להקטין את האינטרוולים בלי 0-1

happy_table <- data20%>%
  mutate(happy_count = cut(`Ladder score`, breaks = interval_breaks)) %>%
  sample_n(153) # אולי להקטין גודל מדגם

mu <- mean(happy_table$`Ladder score`)
sigma <- sd(happy_table$`Ladder score`)

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

qchisq(p = 1-0.05, df = 7-2-1)

#בקיצור רואים שזה ממש לא התפלגות נורמלית, אבל היי תיראו את גרף ההתפלגות:

ggplot(data20, aes(`Ladder score`)) + geom_density()

# ניראה קצת כמו חיבור של 2 התפלגויוצ נורמליות.. בוא נפריד את הדאטה למדינות מערביות ולכל השאר, ונסתכל על הגרפים של הם:

data_west = data20 %>% filter(`Regional indicator` %in% c("Western Europe", 
                                                          "North America and ANZ"))
data_else = data20 %>% filter(!(`Regional indicator` %in% c("Western Europe", 
                                                            "North America and ANZ", "Latin America and Caribbean")))
ggplot(data_west, aes(`Ladder score`)) + geom_density()
ggplot(data_else, aes(`Ladder score`)) + geom_density()

data_west %>% count() #25
data_else %>% count() #107

# הגרפים ניראים די דומה להתפלגות נורמלית, איזה התפלגות אפשר להניח?
# נעשה מבחן הפקש תוחלות ואז נוכל להראות שאם אתה גר במדינה מערבית אתה שמח יותר

