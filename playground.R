library(tidyverse)
fake = readr::read_csv(file="C:/tmp/Fake.csv")
true = readr::read_csv(file="C:/tmp/True.csv")


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



