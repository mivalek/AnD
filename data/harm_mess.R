

library(tidyverse)
set.seed(1)
df <- read_csv("analysing_data/data/harm.csv")
df <- df %>% select(-c(Age, Country, Ethnicity)) %>%
  rename(Age = AgeNew)


df$Gender[sample(nrow(df), 14)] <- NA


df$Age[sample(nrow(df), 5)] <- c(199, 223, 6, 118, "1y")

df[ , c("Experience", "Agency", "Consciousness", "Empathy")] <- lapply(
  df[ , c("Experience", "Agency", "Consciousness", "Empathy")], round)

write_csv(df, "analysing_data/data/harm_data.csv")

## include in a chunk with eval = T if you want to knit prac_03_basic_desc.Rmd
library(tidyverse)
library(knitr)

data <- read_csv("https://raw.githubusercontent.com/mivalek/AnD/master/data/harm_data.csv")
data <- data %>%
  filter(Age != "1y") %>%
  mutate(Age = as.numeric(Age),
         ID = factor(ID),
         Condition = factor(Condition, labels = c("Human_NoHarm", "Human_Harm", "Robot_NoHarm", "Robot_Harm")),
         Humanness = factor(Humanness, labels = c("Human", "Robot")),
         Harm = factor(Harm, labels = c("Unharmed", "Harmed")),
         Gender = factor(Gender, labels = c("Male", "Female")))

age_desc <- data %>%
  summarise(age_mean = mean(Age),
            age_sd = sd(Age),
            age_min = min(Age),
            age_max = max(Age)) %>%
  modify(round, 2)

gender_desc <- data %>%
  mutate(Gender = fct_explicit_na(Gender)) %>%
  group_by(Gender) %>%
  summarise(n = n(),
            perc = n()/nrow(data),
            age_mean = mean(Age),
            age_sd = sd(Age))