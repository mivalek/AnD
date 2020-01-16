
candidate_number <- 123465

red_data <- function(seed = candidate_number,
                     n = 60, age_m = 20.19, age_sd = 2.5, attr_m = 5.67, attr_sd = 1.34, d = 1) {
set.seed(seed)

age_m <- age_m + rnorm(1, 0, .2)
age_sd <- age_sd + rnorm(1, 0, .2)
attr_m <- attr_m  + rnorm(1, 0, .3)
attr_sd <- attr_sd  + rnorm(1, 0, .05)
diff <- d + rnorm(1, 0, .3)

data <- tibble::tibble(
  id = factor(rep(replicate(n, paste(sample(LETTERS, 4, replace = T), collapse = "")), each = 21)),
  age = rep(round(rnorm(n, age_m, age_sd)), each = 21),
  condition = rep(rep(0:1, c(11, 10)), n),
  rating = round(rnorm(n * 21, attr_m + condition * diff, attr_sd)))

data$rating[data$rating > 9] <- 9
data$rating[data$rating < 1] <- 1
data$condition <- factor(data$condition, labels = c("control", "experimental"))

return(data)
}

#########################
green_data <- function(seed = candidate_number,
                     n = 168, age_m = 20.19, age_sd = 2.5, es = .15) {
  set.seed(seed)
  
  age_m <- age_m + rnorm(1, 0, .2)
  age_sd <- age_sd + rnorm(1, 0, .2)
  
  data <- tibble::tibble(
    id = factor(rep(replicate(n, paste(sample(LETTERS, 4, replace = T), collapse = "")), each = 3)),
    age = rep(round(rnorm(n, age_m, age_sd)), each = 3),
    gender = factor(rep(rbinom(n, 2, c(0.613, 0.363, 0.024)), each = 3),
                    labels = c("male", "female", "other")),
    condition = rep(0:1, each = n/2*3),
    product = factor(rep(0:2, n), labels = c("car", "cleaner", "dishwasher")),
    selection = 0)
  
  for (i in unique(data$condition)) {
    for (j in levels(data$product)) {
      data$selection[data$condition == i & data$product == j] <- rbinom(n/2, 1, .37 + (es + rnorm(1, 0, .02)) * i)
    }
  }
  
  data$selection <- factor(data$selection, labels = c("luxury", "green"))
  data$condition <- factor(data$condition, labels = c("control", "experimental"))
  return(data)
}

######################
df <- red_data()

df %>% group_by(condition) %>% summarise(m_age = mean(age), sd_age = sd(age), m_attr = mean(rating), sd_attr = sd(rating), min = min(rating), max = max(rating))

df2 <- df %>% group_by(id, condition) %>% summarise(m_rating = mean(rating))
t.test(m_rating ~ condition, df2, paired = T)
