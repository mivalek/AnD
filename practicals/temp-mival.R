## ---- eval=F--------------------------------------------------------------------------------------
## read_csv("https://mivalek.github.io/adata/gen_sex_q.csv")


## ---- eval=F--------------------------------------------------------------------------------------
## library(tidyverse)
## gensex <- read_csv("https://mivalek.github.io/adata/gen_sex_q.csv")


## -------------------------------------------------------------------------------------------------
gensex %>% str()


## -------------------------------------------------------------------------------------------------
gensex %>%
  pull(Age) %>%
  table()


## -------------------------------------------------------------------------------------------------
gensex %>%
  mutate(Age = recode(Age, "18 years" = "18", "19 years old" = "19"))


## -------------------------------------------------------------------------------------------------
gensex %>%
  mutate(Age = recode(Age, "18 years" = "18", "19 years old" = "19")) %>%
  pull(Age) %>%
  table()


## -------------------------------------------------------------------------------------------------
gensex %>%
  mutate(Age = recode(Age, "18 years" = "18", "19 years old" = "19"),
         Age = as.numeric(Age))


## -------------------------------------------------------------------------------------------------
gensex %>%
  mutate(Age = recode(Age, "18 years" = "18", "19 years old" = "19"),
         Age = as.numeric(Age)) %>%
  pull(Age) %>%
  class()


## -------------------------------------------------------------------------------------------------
gensex %>%
  mutate(Age = recode(Age, "18 years" = 18, "19 years old" = 19),
         Age = as.numeric(Age))


## -------------------------------------------------------------------------------------------------
gensex <- gensex %>%
  mutate(Age = recode(Age, "18 years" = "18", "19 years old" = "19"),
         Age = as.numeric(Age))


## -------------------------------------------------------------------------------------------------
gensex <- gensex %>%
  mutate(Gender = factor(Gender))


## -------------------------------------------------------------------------------------------------
gensex %>% str()


## -------------------------------------------------------------------------------------------------
gensex %>% summary()


## -------------------------------------------------------------------------------------------------
age_removed <- gensex %>% filter(Age > 99) %>% nrow()
age_removed


## -------------------------------------------------------------------------------------------------
gensex <- gensex %>% filter(Age < 100)


## -------------------------------------------------------------------------------------------------
is.na(gensex) %>% as_tibble()


## -------------------------------------------------------------------------------------------------
rowSums(is.na(gensex))


## -------------------------------------------------------------------------------------------------
all_missing <- gensex %>% filter(rowSums(is.na(gensex)) > 10) %>% nrow()
all_missing


## -------------------------------------------------------------------------------------------------
gensex <- gensex %>% filter(rowSums(is.na(gensex)) < 11)


## -------------------------------------------------------------------------------------------------
age_desc <- gensex %>%
  summarise(mean = mean(Age),
            age = sd(Age),
            age = min(Age),
            age = max(Age))
age_desc


## -------------------------------------------------------------------------------------------------
age_desc <- age_desc %>% 
  modify(round, 2)
age_desc


## -------------------------------------------------------------------------------------------------
age_desc <- gensex %>%
  summarise(mean = mean(Age, na.rm = T),
            sd = sd(Age, na.rm = T),
            min = min(Age, na.rm = T),
            max = max(Age, na.rm = T)) %>%
  modify(round, 2)
age_desc


## ---- message=T, warning=TRUE---------------------------------------------------------------------
gen_age_desc <- gensex %>%
  group_by(Gender) %>%
  summarise(n = n(),
            perc = n()/nrow(gensex) * 100,
            mean_age = mean(Age, na.rm = T),
            sd_age = sd(Age, na.rm = T))
gen_age_desc


## -------------------------------------------------------------------------------------------------
gen_age_desc <- gensex %>%
  mutate(Gender = fct_explicit_na(Gender)) %>%
  group_by(Gender) %>%
  summarise(n = n(),
            perc = n()/nrow(gensex) * 100,
            mean_age = mean(Age, na.rm = T),
            sd_age = sd(Age, na.rm = T))
gen_age_desc


## -------------------------------------------------------------------------------------------------
library(kableExtra)
gen_age_desc %>% kable() %>% kable_styling()


## -------------------------------------------------------------------------------------------------
gen_age_desc %>%
  kable(col.names = c("Gender", "*N*", "%", "*M*~age~", "*SD*~age~"),
        caption = "Table 1 *Descriptive statistics by Gender*",
        digits = 2) %>%
  kable_styling()


## ---- echo=F--------------------------------------------------------------------------------------
gen_age_desc %>%
  kable(col.names = c("Gender", "*N*", "%", "*M*~age~", "*SD*~age~"),
        caption = "Table 1 *Descriptive statistics by Gender*") %>%
  kable_styling()


## ---- echo = F------------------------------------------------------------------------------------
data <- read_csv("https://raw.githubusercontent.com/mivalek/AnD/master/data/harm_data.csv")


## ---- eval=F, echo= solution, class="solution"----------------------------------------------------
## # add library() commands to load all packages you need for this document.
## 
## library(tidyverse)
## # if kableExtra doesn't work
## library(knitr)
## # otherwise
## library(kableExtra)


## ---- eval=F, echo= solution, class="solution"----------------------------------------------------
## # complete the line to read in the data
## 
## data <- read_csv("https://raw.githubusercontent.com/mivalek/AnD/master/data/harm_data.csv")


## ---- echo = solution, class = "solution"---------------------------------------------------------
summary(data)


## ---- echo = F------------------------------------------------------------------------------------
tibble(
  var_name = names(data),
  level = rep(c("nominal", "continuous", "ordinal"),
              c(5, 1, 6)),
  desc = c(
    "Unique participant ID",
    "Experimntal condition; combination of Humanness and Harm",
    "Avatar type: 1 = Human, 2 = Robot",
    "Avatar state: 1 = Unharmed, 2 = Harmed",
    "Participant gender: 1 = Male, 2 = Female",
    "Participant age in years",
    "Does avatar have capacity to feel pain? 1 = Strongly disagree, 7 = Strongly agree",
    "Does avatar have experience? 1 = Strongly disagree, 7 = Strongly agree",
    "Does avatar have agency? 1 = Strongly disagree, 7 = Strongly agree",
    "Does avatar have consciousness? 1 = Strongly disagree, 7 = Strongly agree",
    "Level of empathy for avatar: Higher number means more empathy felt",
    "Attractiveness of avatar: Higher number means more attractive")) %>%
  kable(col.names = c("Variable name", "Level", "Description"),
        caption = "Data set code book") %>%
  kable_styling()


## ---- include=solution, class = "solution"--------------------------------------------------------
data %>%
  pull(Age) %>%
  table()


## ---- include=solution, class = "solution"--------------------------------------------------------
data <- data %>%
  filter(Age != "1y") %>%
  mutate(Age = as.numeric(Age))
# check it worked
data


## ---- echo=solution, eval = F, class = "solution"-------------------------------------------------
## data <- data %>% mutate(
##   ID = factor(ID),
##   Condition = factor(Condition),
##   Humanness = factor(Humanness),
##   Harm = factor(Harm),
##   Gender = factor(Gender))




## ---- echo=solution, class = "solution"-----------------------------------------------------------
data <- data %>% mutate(
  ID = factor(ID),
  Condition = factor(Condition, labels = c("Human_NoHarm", "Human_Harm",
                                           "Robot_NoHarm", "Robot_Harm")),
  Humanness = factor(Humanness, labels = c("Human", "Robot")),
  Harm = factor(Harm, labels = c("Unharmed", "Harmed")),
  Gender = factor(Gender, labels = c("Male", "Female")))
# check it worked
data


## ---- include = solution, class = "solution"------------------------------------------------------
data %>%
  pull(ID) %>%
  table()


## ---- include = solution, class = "solution"------------------------------------------------------
data %>%
  pull(ID) %>%
  table() %>%
  all() == 1


## ---- echo=solution, class = "solution"-----------------------------------------------------------
too_young <- data %>%
  filter(Age < 17) %>%
  nrow()
too_old <- data %>%
  filter(Age > 90) %>%
  nrow()
data <- data %>% filter(Age > 18 | Age < 90)


## ---- echo=solution, class = "solution"-----------------------------------------------------------
age_desc <- data %>%
  summarise(mean = mean(Age),
            sd = sd(Age),
            min = min(Age),
            max = max(Age)) %>%
  modify(round, 2)
# let's see
age_desc


## ---- echo=solution, class = "solution"-----------------------------------------------------------
gender_desc <- data %>%
  mutate(Gender = fct_explicit_na(Gender)) %>%
  group_by(Gender) %>%
  summarise(n = n(),
            perc = n()/nrow(data) * 100,
            age_mean = mean(Age),
            age_sd = sd(Age))
# let's see
gender_desc


## ---- echo=solution, class = "solution"-----------------------------------------------------------
type_desc <- data %>%
  group_by(Humanness) %>%
  summarise(n = n(),
            pain_m = mean(Pain),
            exp_m = mean(Experience),
            agency_m = sd(Agency),
            consc_m = sd(Consciousness))
# let's see
type_desc


## ---- echo=solution, class = "solution"-----------------------------------------------------------
unharm_desc <- data %>%
  filter(Harm == "Unharmed") %>%
  group_by(Humanness) %>%
  summarise(n = n(),
            attr_m = mean(Attractiveness),
            emp_m = mean(Empathy))
# See the resul
unharm_desc


## ----table_1, echo = solution, class="solution"---------------------------------------------------
# provide tibble to push to kable() and fill in missing column names
gender_desc %>%
  kable(col.names = c("Gender", "*N*", "%", "*M*~age~", "*SD*~Age~"),
        caption = "Table 1 *Descriptive statistics by categories of gender*",
        digits = 2) %>%
  kable_styling()

