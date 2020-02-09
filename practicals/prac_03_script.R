

library(tidyverse)
library(english)

# read in
gensex <- read_csv("https://mivalek.github.io/adata/gen_sex_q.csv")


# Data inspection ---------------------------------------------------------

# structure of dataset
gensex %>% str()

# unique values of age
gensex %>%
  pull(Age) %>%
  table()


# Data cleaning -----------------------------------------------------------

### AGE

# recode age
gensex %>%
  mutate(Age = recode(Age, "18 years" = "18", "19 years old" = "19"))

# see that it worked
gensex %>%
  mutate(Age = recode(Age, "18 years" = "18", "19 years old" = "19")) %>%
  pull(Age) %>%
  table()

# turn age numeric
gensex %>%
  mutate(Age = recode(Age, "18 years" = "18", "19 years old" = "19"),
         Age = as.numeric(Age))

# see that it worked
gensex %>%
  mutate(Age = recode(Age, "18 years" = "18", "19 years old" = "19"),
         Age = as.numeric(Age)) %>%
  pull(Age) %>%
  class()

# WATCH OUT FOR unquoted numbers in recode()
# This results in age being coerced to numeric and results in NAs
gensex %>%
  mutate(Age = recode(Age, "18 years" = 18, "19 years old" = 19),
         Age = as.numeric(Age))

# when happy, reassign to keep changes
gensex <- gensex %>%
  mutate(Age = recode(Age, "18 years" = "18", "19 years old" = "19"),
         Age = as.numeric(Age))



### GENDER

# gender into factor
gensex <- gensex %>%
  mutate(Gender = factor(Gender))

# see that it worked
gensex %>% str()


# see rough summary
gensex %>% summary()

### IMPROBABLE VALUES

# count how many cases you're about to remove
age_removed <- gensex %>% filter(Age > 99) %>% nrow()

# remove
gensex <- gensex %>% filter(Age < 100)

### Removing rows with only `NA`s

# test for NA
is.na(gensex) %>% as.tibble()

# add up instances of NAs row-wise
rowSums(is.na(gensex))

# count how many you're about to remove
all_missing <- gensex %>% filter(rowSums(is.na(gensex)) > 10) %>% nrow()

# remove them
gensex <- gensex %>% filter(rowSums(is.na(gensex)) < 11)


# DESCRIPTIVES ------------------------------------------------------------

# desc for Age
age_desc <- gensex %>%
  summarise(mean = mean(Age),
            sd = sd(Age),
            min = min(Age),
            max = max(Age))

# round to 2 decimal places
age_desc <- age_desc %>% 
  modify(round, 2)


# Single pipeline
age_desc <- gensex %>%
  summarise(mean = mean(Age, na.rm = T),
            sd = sd(Age, na.rm = T),
            min = min(Age, na.rm = T),
            max = max(Age, na.rm = T)) %>%
  modify(round, 2)

# Break-down of gender with age stats
gen_age_desc <- gensex %>%
  group_by(Gender) %>%
  summarise(n = n(),
            perc = n()/nrow(gensex) * 100,
            mean_age = mean(Age, na.rm = T),
            sd_age = sd(Age, na.rm = T))


# Make missing values explicit
gen_age_desc <- gensex %>%
  mutate(Gender = fct_explicit_na(Gender)) %>%
  group_by(Gender) %>%
  summarise(n = n(),
            perc = n()/nrow(gensex) * 100,
            mean_age = mean(Age, na.rm = T),
            sd_age = sd(Age, na.rm = T))


# turn tibble into table
# library(kableExtra)
# library(knitr) works too but without kable_styling()
gen_age_desc %>% kable() %>% kable_styling()

# give column names (with markdown)
# give table a caption
# round to 2 decimal places
gen_age_desc %>%
  mutate(Gender = str_to_sentence(Gender)) %>%
  kable(col.names = c("Gender", "*N*", "%", "*M*~age~", "*SD*~age~"),
        caption = "Table 1 *Descriptive statistics by Gender*",
        digits = 2) %>%
  kable_styling()

