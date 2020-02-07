

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

