library(tidyverse)

raw.data <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621finalproject\\openpowerlifting.csv', guess_max=8836) %>%
  filter(!is.na(BestDeadliftKg) & !is.na(BestBenchKg) & !is.na(BestSquatKg)
         ) %>%
  select(MeetID, Sex, Equipment, Age, BodyweightKg, TotalKg) %>%
  mutate(Sex = factor(Sex),
         Equipment = factor(Equipment, levels(raw.data$Equipment)[c(2, 4, 3, 1)])
         )

meets.data <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621finalproject\\meets.csv') %>%
  select(MeetID, Federation, MeetCountry)

all.data <- inner_join(raw.data, meets.data) %>%
  filter(MeetCountry == 'USA') %>%
  select(-MeetID, -MeetCountry)


#missing too much age
all.data %>%
  map_dbl(~sum(is.na(.))/nrow(all.data))
  
all.data <- all.data %>%
  filter(!is.na(Age))

#focus on US
all.data %>%
  group_by(Federation) %>%
  count() %>%
  arrange(desc(n))

all.data <- all.data %>%
  filter(Federation %in% c('USPA', 'USAPL', 'SPF'))


all.data %>%
  group_by(Federation, Equipment) %>%
  count()


set.seed(123)
imputed.data <- mice(all.data[, -1], m=5, maxit=10, method='pmm', seed=500, printFlag=TRUE)
all.data.complete <- cbind(all.data[, 1], complete(imputed.data, 1))


all.data.complete %>%
  map_dbl(~sum(is.na(.))/nrow(all.data.complete))

write_csv(all.data.complete, 'all_data_complete.csv')










