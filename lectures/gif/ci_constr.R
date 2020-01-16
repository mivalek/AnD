
bg_col <- "#fdfdfd"
default_col <- "#434b75"
theme_col <- "#77bd9d"
complement_col <- "#bd7797"
point_col <- paste0(default_col, "88")

set.seed(1)
library(tidyverse)
library(cowplot)

theme_set(theme_cowplot() +
            theme(line = element_line(colour = default_col),
                  plot.background = element_rect(fill = bg_col),
                  panel.background = element_rect(fill = bg_col),
                  text = element_text(colour = default_col),
                  title = element_text(colour = default_col),
                  axis.line = element_line(colour = default_col),
                  axis.ticks = element_line(colour = default_col),
                  axis.text = element_text(colour = default_col),
                  axis.title = element_text(colour = default_col),
            )
)
update_geom_defaults("bar", list(fill = bg_col, colour = default_col))

n <- 700
df <- tibble(x = runif(n, -10, 10),
             y = runif(n, -10, 10),
             size = sample(1:20, n, T))
df <- df[sqrt(df$x^2 + df$y^2) <= 10, ]
mu <- mean(df$size)
sigma <- sd(df$size)
sample <- as.data.frame(replicate(100, sample(1:nrow(df), 30)))

png("analysing_data/ci_gif/p_%04d.png", 700, 500)

p1 <- tibble(x = seq(mu - 3 * sigma, mu + 3 * sigma, length.out = 200), y = dnorm(x, mu, sigma)) %>%
  ggplot(aes(x, y)) + labs(x = "Sample mean", y = "Density") + xlim(c(mu - 3 * sigma, mu + 3 * sigma)) + ylim(c(0, .072))

print(p1)

ii <- seq(mu - 3 * sigma, mu + 3 * sigma, length.out = 100)
for (i in 1:20) {
  print(p1 + 
          stat_function(fun =dnorm, args = list(mean = mu, sd = sigma),
                        xlim = c(mu - 3 * sigma, ii[i]), geom = "line", position ="identity", colour = default_col)
  )
}

ii <- ii[(i + 1):100]
alpha <- as.character(as.hexmode(round(seq(1, 255, length.out = length(ii)))))
y_coord <- c(rep(0, 50), round(seq(0, .072, length.out = length(ii) - 50), 3))
for (i in seq_along(ii)) {
  print(p1 +
          stat_function(fun =dnorm, args = list(mean = mu, sd = sigma), xlim = c(-1.96 * sigma + mu, min(c(1.96 * sigma + mu, ii[i]))),
                        geom = "area", position ="identity", colour = paste0(theme_col, alpha[i]), fill = paste0(theme_col, alpha[i])) + 
          stat_function(fun =dnorm, args = list(mean = mu, sd = sigma),
                        xlim = c(mu - 3 * sigma, ii[i]), geom = "line", position ="identity", colour = default_col) +
          geom_line(data = tibble(x = c(mu, mu), y = c(0, .072)), aes(x, y), lty=2, colour = bg_col)
  )
}

dev.off()
system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 3 -loop 1 analysing_data\\ci_gif\\*.png analysing_data\\ci_constr01.gif')

file.remove(list.files("analysing_data/ci_gif/", full.names = T))

p1 <-  p1 +
  stat_function(fun =dnorm, args = list(mean = mu, sd = sigma), xlim = c(-1.96, 1.96) * sigma + mu,
                geom = "area", position ="identity", colour = theme_col, fill = theme_col) + 
  stat_function(fun =dnorm, args = list(mean = mu, sd = sigma),
                xlim = c(mu - 3 * sigma, mu + 3 * sigma), geom = "line", position ="identity", colour = default_col) +
  geom_line(data = tibble(x = c(mu, mu), y = c(0, .072)), aes(x, y), lty=2, colour = bg_col)


mid <- mu + 1.96/2 * sigma


png("analysing_data/ci_gif/p_%04d.png", 700, 500)

for (i in seq(0, .002, length.out = 10)) {
  print(p1 +
          geom_errorbarh(data=tibble(x = 1, y = .007), aes(y = y),
                         xmin = mid, xmax = mid, height = i, colour = default_col)
  )
}

for (i in seq(0, 1.96/2, length.out = 40)) {
  print(p1 +
          geom_errorbarh(data=tibble(x = 1, y = .007), aes(y = y),
                         xmin = mid - i * sigma, xmax = mid + i * sigma, height = .002, colour = default_col)
  )
}

p2 <- p1 +
  geom_errorbarh(data=tibble(x = 1, y = .007), aes(y = y),
                 xmin = mid - 1.96/2 * sigma, xmax = mid + 1.96/2 * sigma, height = .002, colour = default_col)

for (i in as.character(as.hexmode(round(seq(1, 255, length.out = 10))))) {
  print(p2 +
          annotate("text", x = mu + 1.96/2 * sigma, y = .01,
                   label = expression(1.96 %*% italic(SE)), colour = paste0(default_col, i), size = 7)
  )
}

dev.off()

rm(p2)
system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 3 -loop 1 analysing_data\\ci_gif\\*.png analysing_data\\ci_constr02.gif')

file.remove(list.files("analysing_data/ci_gif/", full.names = T))

p1 <- p1 +
  geom_errorbarh(data=tibble(x = 1, y = .007), aes(y = y), xmin = mu + 1.96 * sigma, xmax = mu, height = .002, colour = default_col) +
  annotate("text", x = mu + 1.96/2 * sigma, y = .01, label = expression(1.96 %*% italic(SE)), colour = default_col, size = 7)


png("analysing_data/ci_gif/p_%04d.png", 700, 500)

for (i in as.character(as.hexmode(round(seq(1, 255, length.out = 30))))) {
  print(p1  +
          geom_point(data = tibble(x= 14, y = .02), aes(x, y), shape = 21, colour = paste0(default_col, i), fill =  paste0(bg_col, i), size = 4)
        )
}

for (i in as.character(as.hexmode(round(seq(1, 255, length.out = 15))))) {
  print(p1  +
          geom_point(x= 14, y = .02, shape = 21, fill = bg_col, colour = default_col, size = 4) +
          geom_errorbarh(data=tibble(x = 1, y = .007), aes(y = y),
                         xmin = mu + 1.96 * sigma, xmax = mu, height = .002, colour = paste0(complement_col, i))
  )
}

x1 <- seq(0, 14 - mu, length.out = 50)
y1 <- seq(0, .02 - .007, length.out = length(x1))

for (i in seq_along(x1)) {
  print(p1 +
          geom_errorbarh(data=tibble(x = 1, y = .007), aes(y = y + y1[i]),
                         xmin = mu + 1.96 * sigma + x1[i], xmax = mu + x1[i], height = .002, colour = complement_col) +
          annotate("text", x = mu + 1.96/2 * sigma, y = .01, label = expression(1.96 %*% italic(SE)), colour = default_col, size = 7)  +
          geom_point(x= 14, y = .02, shape = 21, fill = bg_col, colour = default_col, size = 4)
  )
}

p2 <- p1  +
  geom_errorbarh(data=tibble(x = 1, y = .02), aes(y = y),
                 xmin = 14, xmax = 14 + 1.96 * sigma, height = .002, colour = complement_col)+
  geom_point(x= 14, y = .02, shape = 21, fill = bg_col, colour = default_col, size = 4)

for (i in 1:10) {
  print(p2)
}


p2 <- p1  +
  geom_segment(data=NULL, aes(x = 14, y = .02, xend = 14 + 1.96 * sigma, yend = .02),
               arrow = arrow(length = unit(0.015, "npc"), angle = 90), colour = default_col)

for (i in seq(1.96, -1.96, length.out = 30)) {
  print(p2 +
          geom_segment(data=NULL, aes(x = 14, y = .02, xend = 14 + i * sigma, yend = .02),
                       arrow = arrow(length = unit(0.015, "npc"), angle = 90), colour = complement_col) +
          geom_point(x= 14, y = .02, shape = 21, fill = bg_col, colour = default_col, size = 4)
        )
}

for (i in as.character(as.hexmode(round(seq(255, 1, length.out = 15))))) {
  print(p1 +
          geom_errorbarh(data=tibble(x = 1, y = .02), aes(y = y),
                         xmin = 14 - 1.96 * sigma, xmax = 14 + 1.96 * sigma, height = .002, colour = default_col) +
          geom_segment(data=tibble(x = 14, y = .02), aes(x = x, y = y, xend = x - 1.96 * sigma, yend = y),
                       arrow = arrow(length = unit(0.015, "npc"), angle = 90), colour = paste0(complement_col, i)) +
          geom_point(x= 14, y = .02, shape = 21, fill = bg_col, colour = default_col, size = 4)
  )
}

dev.off()

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 3 -loop 1 analysing_data\\ci_gif\\*.png analysing_data\\ci_constr03.gif')

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 3 -loop 0 analysing_data\\ci_gif\\*.png analysing_data\\ci_constr_full.gif')

file.remove(list.files("analysing_data/ci_gif/", full.names = T))

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" convert analysing_data\\ci_constr01.gif -fuzz 0% -layers Optimize analysing_data\\ci_constr01_small.gif')
system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" convert analysing_data\\ci_constr02.gif -fuzz 0% -layers Optimize analysing_data\\ci_constr02_small.gif')
system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" convert analysing_data\\ci_constr03.gif -fuzz 0% -layers Optimize analysing_data\\ci_constr03_small.gif')
system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" convert analysing_data\\ci_constr_full.gif -fuzz 0% -layers Optimize analysing_data\\ci_constr_small.gif')
