

library(tidyverse)
set.seed(1)
df <- read_csv("analysing_data/data/harm2.csv")

# get rid of unneeded columns
df <- df[, -c(1:4, 6:11)]

# discard withdrawn data
df <- filter(df, is.na(Debrief_1))
df <- df[df$Age != "17" & !is.na(df$Age), 1:13 ]

df <- rbind(df, df[sample(nrow(df), 17), ])

df$Gender[301:303] <- NA
df$Age[304:310] <- c(sample(150:540, 7))
df[311:317, 3:13] <- NA
df <- df[sample(nrow(df)), ]
names(df)[1] <- "Duration"
write_csv(df, "analysing_data/gen_sex_q.csv")

