
cand_no <- 143522

dthomas <-
  "Do not go gentle into that good night,
Old age should burn and rave at close of day;
Rage, rage against the dying of the light.

Though wise men at their end know dark is right,
Because their words have forked no lightning, they
Do not go gentle into that good night.

Good men, the last wave by, crying how bright
Their frail deeds might have danced in a green bay,
Rage, rage against the dying of the light.

Wild men who caught and sang the sun in flight,
And learn, too late, they grieved it on its way,
Do not go gentle into that good night.

Grave men, near death, who see with blinding sight
Blind eyes could blaze like meteors and be gay,
Rage, rage against the dying of the light.

And you, my father, there on the sad height,
Curse, bless me now with your fierce tears, I pray.
Do not go gentle into that good night.
Rage, rage against the dying of the light."

dt2 <- unlist(strsplit(dthomas, "\\n"))
dt2 <- dt2[dt2 != ""]
dt2 <- strsplit(dt2, "\\s+")

set.seed(cand_no)

line <- unlist(dt2[runif(1, 1, length(dt2))])


result1 <- 4.11

check_result <- function(task, result) {
  dthomas <-
    "Do not go gentle into that good night,
Old age should burn and rave at close of day;
Rage, rage against the dying of the light.

Though wise men at their end know dark is right,
Because their words have forked no lightning, they
Do not go gentle into that good night.

Good men, the last wave by, crying how bright
Their frail deeds might have danced in a green bay,
Rage, rage against the dying of the light.

Wild men who caught and sang the sun in flight,
And learn, too late, they grieved it on its way,
Do not go gentle into that good night.

Grave men, near death, who see with blinding sight
Blind eyes could blaze like meteors and be gay,
Rage, rage against the dying of the light.

And you, my father, there on the sad height,
Curse, bless me now with your fierce tears, I pray.
Do not go gentle into that good night.
Rage, rage against the dying of the light."
  
  dt2 <- unlist(strsplit(dthomas, "\\n+"))
  dt2 <- strsplit(dt2, "\\s+")
  
  set.seed(cand_no)
  
  line <- unlist(dt2[sample(length(dt2), 1)])
  
  set.seed(cand_no)
  
  df <- data.frame(x = sample(c("sweet", "savoury", "only tea", "only cofee", "only other drink", "both sweet and savoury", "nothing"), 500, replace = T))
  
  results <- c(round(chisq.test(table(df$x))$statistic, 2), FALSE)
  if (result == results[task]) {
    message(paste("That is correct! Your", ifelse(task == 1, "first", "next"), "word is:", line[task]))
  } else {message("Not quite correct. Check your code and try again!")}
}

check_result(1, 4.17)
debug(check_result)
