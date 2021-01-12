bg_col <- "#fdfdfd"
default_col <- "#434b75"
theme_col <- "#77bd9d"
complement_col <- "#bd7797"
point_col <- paste0(default_col, "44")

set.seed(14)
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
s <- 30
df <- tibble(x = runif(n, -10, 10),
             y = runif(n, -10, 10),
             size = round(rnorm(n, 10, 3)))
df <- df[sqrt(df$x^2 + df$y^2) <= 10, ]
df$size <- (df$size / max(df$size) * 19) + 1

d <- c()
samps <- tibble()
for (i in 1:500) {
  samp <- df[sample(nrow(df), 2 * s), ]
  samp$group <- factor(rep(1:2, s))
  samp$samp <- i
  samps <- rbind(samps, samp)
  d <- c(d,
         samp %>%
           mutate(size = scale(size)) %>%
           group_by(group) %>%
           summarise(m = mean(size)) %>%
           pull(m) %>%
           diff
  )
}

t_fun <- function(x, df) {
  dt(x / sd(d), df) * 80
}

dist <- tibble(x = seq(-1, 1, length.out = 100),
               y = t_fun(seq(-1, 1, length.out = 100), s-1))

p1 <- 
  tibble(x = d) %>% ggplot(aes(x)) +
  geom_vline(xintercept = 0, lty=2) +
  xlim(-1, 1) + ylim(0, 40) +
  labs(title=bquote(Difference~bar(AI)[climb]-bar(AI)[gen]), x = expression(italic(D)), y = "Frequency")

png("analysing_data/gif_pics/p_%04d.png", 750, 350)

alphas <- seq(0, 1, length.out = 100)
x_lim <- seq(-1, 1, length.out = 100)
for (j in seq(2, 100, by=2)) {
  i <- alphas[j]
  vanish <- paste0(default_col, sprintf("%02x", round(255 * (1-i))))
  print(p1 +
          geom_histogram(bins = 50,
                         colour = paste0(default_col, sprintf("%02x", round(255 * (1-i)))),
                         fill = default_col, alpha = (1-i)/4) +
          stat_function(fun =t_fun, args = list(df = s - 1), xlim = x_lim[c(1, j)],
                        geom = "area", position ="identity", colour = NA, fill = default_col, alpha = i/4) +
          geom_line(data=dist[1:j, ], aes(x, y), colour = paste0(default_col, sprintf("%02x", round(255 * i)))) +
          theme(axis.title.y = element_text(colour = vanish),
                axis.ticks.y = element_line(colour = vanish),
                axis.line.y = element_line(colour = vanish),
                axis.text.y = element_text(colour = vanish))
  )
}


dev.off()


system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 1 -loop 1 analysing_data\\gif_pics\\*.png analysing_data\\nhst1.gif')


p1 <- p1 +
  geom_area(data=dist,  aes(x, y), colour = NA, fill = default_col, alpha = .25) +
  theme(axis.title.y = element_text(colour = bg_col),
        axis.ticks.y = element_line(colour = bg_col),
        axis.line.y = element_line(colour = bg_col),
        axis.text.y = element_text(colour = bg_col))

crit <- qt(1-c(.25, .05, .025, .005), s-1)*sd(d)


png("analysing_data/gif_pics/p_%04d.png", 750, 350)

p2 <- tibble(x = d) %>% ggplot(aes(x)) +
  xlim(-1, 1) + ylim(0, 40) +
  labs(title=bquote(Difference~bar(AI)[climb]-bar(AI)[gen]), x = expression(italic(D)), y = "Frequency") + 
  stat_function(fun =t_fun, args = list(df = s - 1), xlim = c(-1, 1),
                geom = "area", position ="identity", colour = NA, fill = "#EEB23833") +
  annotate("text", x = 0, y = 39,
           label = bquote(atop(italic(p)(italic(D)>=0)==.("50%"),
                               italic(p)(italic(D)<0)==.("50%"))),
           colour = default_col, size = 5) +
  theme(axis.title.y = element_text(colour = bg_col),
        axis.ticks.y = element_line(colour = bg_col),
        axis.line.y = element_line(colour = bg_col),
        axis.text.y = element_text(colour = bg_col)) +
  coord_cartesian(clip = 'off')

for (i in crit) {
  p2 <- p2 + 
    stat_function(fun =t_fun, args = list(df = s - 1), xlim = c(-1, -i),
                  geom = "area", position ="identity", colour = NA, fill = "#EEB23833") +
    stat_function(fun =t_fun, args = list(df = s - 1), xlim = c(i, 1),
                  geom = "area", position ="identity", colour = NA, fill = "#EEB23833")
}
for (i in sort(c(seq(0, 1, length.out = 50), crit))) {
  if (i %in% crit) {
    p2 <- p2 +
      geom_line(data = tibble(x = rep(c(-i, i), each = 2), y = rep(c(0, t_fun(i, s-1) + 3), 2)),
                aes(x, y, group = factor(x)), colour = default_col, lty=2) +
            annotate("text", x = -i - .17, y = t_fun(i, s-1) + 5.5,
                     label = bquote(italic(p)(italic(D)<=phantom()-.(round(i, 2)))==.(paste0(round((1 - pt(i/sd(d), s-1)) * 100, 2), "%"))),
                     colour = default_col, size = 5) +
            annotate("text", x = i + .17, y = t_fun(i, s-1) + 5.5,
                     label = bquote(italic(p)(italic(D)>=.(round(i, 2)))==.(paste0(round((1 - pt(i/sd(d), s-1)) * 100, 2), "%"))),
                     colour = default_col, size = 5)
  }
  
  print( 
    p2 + 
      stat_function(fun =t_fun, args = list(df = s - 1), xlim = c(-1, -i),
                    geom = "area", position ="identity", colour = NA, fill = "#ced0db") +
      stat_function(fun =t_fun, args = list(df = s - 1), xlim = c(i, 1),
                    geom = "area", position ="identity", colour = NA, fill = "#ced0db") +
      geom_line(data=dist, aes(x, y), colour = default_col) +
      geom_line(data = tibble(x = c(0, 0), y = c(0, t_fun(0, s-1) + 3)), aes(x, y), colour = default_col, lty = 2)
  )
}

dev.off()

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 1 -loop 1 analysing_data\\gif_pics\\*.png analysing_data\\nhst2.gif')

p1 <- tibble(x = d) %>% ggplot(aes(x)) +
  xlim(-1, 1) + ylim(0, 40) +
  labs(title=bquote(Difference~bar(AI)[climb]-bar(AI)[gen]), x = expression(italic(D)), y = "Frequency") +
  theme(axis.title.y = element_text(colour = bg_col),
        axis.ticks.y = element_line(colour = bg_col),
        axis.line.y = element_line(colour = bg_col),
        axis.text.y = element_text(colour = bg_col)) +
  coord_cartesian(clip = 'off')


png("analysing_data/gif_pics/p_%04d.png", 750, 350)

for (a in seq(1, 0, length.out = 20)) {
  vanish <- paste0(default_col, sprintf("%02x", round(255 * a)))
  p2 <- p1 +
    stat_function(fun =t_fun, args = list(df = s - 1), xlim = c(-1, 1),
                  geom = "area", position ="identity", colour = NA, fill = "#EEB238", alpha = .2 * a) +
    annotate("text", x = 0, y = 39,
             label = bquote(atop(italic(p)(italic(D)>=0)==.("50%"),
                                 italic(p)(italic(D)<0)==.("50%"))),
             colour = vanish, size = 5) +
    geom_line(data = tibble(x = c(0, 0), y = c(0, t_fun(0, s-1) + 3) * a), aes(x, y), colour = default_col, lty = 2)
  
  for (i in crit) {
    p2 <- p2 + 
      stat_function(fun =t_fun, args = list(df = s - 1), xlim = c(-1, -i),
                    geom = "area", position ="identity", colour = NA, fill = "#EEB238", alpha = .2 * a) +
      stat_function(fun =t_fun, args = list(df = s - 1), xlim = c(i, 1),
                    geom = "area", position ="identity", colour = NA, fill = "#EEB238", alpha = .2 * a) +
      geom_line(data = tibble(x = rep(c(-i, i), each = 2), y = rep(c(0, t_fun(i, s-1) + 3) * a, 2)),
                aes(x, y, group = factor(x)), colour = default_col, lty=2) +
      annotate("text", x = -i - .17, y = t_fun(i, s-1) + 5.5,
               label = bquote(italic(p)(italic(D)<=phantom()-.(round(i, 2)))==.(paste0(round((1 - pt(i/sd(d), s-1)) * 100, 2), "%"))),
               colour = vanish, size = 5) +
      annotate("text", x = i + .17, y = t_fun(i, s-1) + 5.5,
               label = bquote(italic(p)(italic(D)>=.(round(i, 2)))==.(paste0(round((1 - pt(i/sd(d), s-1)) * 100, 2), "%"))),
               colour = vanish, size = 5)
  }
  
  print(
    p2 + geom_line(data=dist, aes(x, y), colour = default_col)
  )
}

p2 <- p1

d_obs <- .47
p_val <- sub("^0", "",  round(pt(d_obs / sd(d), s - 1, lower.tail = F) * 2, 3)) 
for (i in seq(0, 1, length.out = 20)) {
  print(p2 +
    annotate("text", x = d_obs + .12, y = 20,
             label = bquote(atop(italic(D)==.(round(d_obs, 2)),~italic(p)==.(p_val))), colour = paste0(default_col, sprintf("%02x", round(255 * i))), size = 5) + 
    stat_function(fun = t_fun, args = list(df = s - 1), xlim = c(-1, -d_obs),
                  geom = "area", position ="identity", colour = NA, fill = "#EEB238", alpha = .8 * i) +
    stat_function(fun = t_fun, args = list(df = s - 1), xlim = c(d_obs, 1),
                  geom = "area", position ="identity", colour = NA, fill = "#EEB238", alpha = .8 * i) +
    geom_line(data=tibble(x = rep(d_obs, 2), y = c(0, 23) * i), aes(x, y), colour = default_col, lty = 2) +
    geom_line(data=dist, aes(x, y), colour = default_col)
  )
}
  
dev.off()

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 1 -loop 1 analysing_data\\gif_pics\\*.png analysing_data\\nhst3.gif')


png("analysing_data/gif_pics/p_%04d.png", 750, 350)

for (i in seq(0, 1, length.out = 30)) {
  print(
    p1 + 
      stat_function(fun =t_fun, args = list(df = s - 1), xlim = c(-1, qt(.025, s-1)*sd(d)),
                    geom = "area", position ="identity", colour = NA, fill = theme_col, alpha = i) +
      stat_function(fun =t_fun, args = list(df = s - 1), xlim = c(qt(.975, s-1)*sd(d), 1),
                    geom = "area", position ="identity", colour = NA, fill = theme_col, alpha = i) +
      geom_line(data=dist, aes(x, y), colour = default_col)
  )
}

dev.off()


system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 1 -loop 1 analysing_data\\gif_pics\\*.png analysing_data\\nhst4.gif')

