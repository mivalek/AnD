bg_col <- "#fdfdfd"
default_col <- "#434b75"
theme_col <- "#77bd9d"
complement_col <- "#bd7797"
point_col <- paste0(default_col, "AA")

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
             size = round(rnorm(n, 9, 2.5)))
df <- df[sqrt(df$x^2 + df$y^2) <= 10, ]
df$group <- 1
df$group[df$x >= -df$y] <- 2
df$size[df$group == 2] <- round(rnorm(nrow(df[df$group == 2, ]), 11, 2.5))
df$size <- df$size - 1
df$group <- factor(df$group)

means <- df %>% mutate(size = scale(size)) %>% group_by(group) %>% summarise(m = round(mean(size), 2))
p1 <- df %>%
  ggplot(aes(x, y, size = size)) +
  geom_point(shape = 21, aes(color = group, fill = group)) +
  geom_line(data = tibble(x = c(-9, 9), y = -x), aes(x, y), lwd = 1, color = default_col, inherit.aes = F) +
  theme(axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank(), legend.position="none") +
  labs(title = expression("Population"), x = "", y = "") +
  annotate("text", x = -7.5, y = -10,
           label = bquote(mu[gen]==.(means$m[1])), size = 5, color = default_col)  +
  annotate("text", x = 7.5, y = 10,
           label = bquote(mu[climb]==.(means$m[2])), size = 5, color = default_col)   +
  theme(plot.title = element_text(vjust=0)) +
  scale_color_manual(values = c("#6E4C1E77", "#EEB23877")) +
  scale_fill_manual(values = c("#6E4C1E77", "#EEB23877"))



d <- c()
samps <- tibble()
for (i in 1:500) {
  gen <- df$group == 1
  n_gen <- sum(gen)
  samp <- rbind(
    df[gen, ][sample(n_gen, s), ],
    df[!gen, ][sample(nrow(df) - n_gen, s), ]
  )
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

p2 <- 
  tibble(x = d) %>% ggplot(aes(x)) +
  geom_vline(xintercept = 0, lty=2) +
  # geom_vline(xintercept = diff(means$m), col = "#EEB238") +
  xlim(-1.5, 1.5) + ylim(0, 65) +
  labs(title=bquote(Difference~bar(AI)[climb]-bar(AI)[gen]), x = expression(italic(D)), y = "Frequency")

png("analysing_data/gif_pics/p_%04d.png", 750, 350)

plot_grid(p1, p2, rel_widths = c(1, 1.5))

k <- 1
for (i in c(1:50, seq(51, 100, by = 2), seq(101, 200, by = 3), seq(201, 430, by = 4), seq(431, 470, by = 2), 471:500)) {
  
  if (k == i) {
    p <- plot_grid(
      p1 + geom_point(data = samps[samps$samp %in% i, ], aes(x, y), fill = default_col, color = default_col, shape = 21),
      p2 + geom_histogram(data = tibble(x = d[1:i]), bins = 50, fill = paste0(default_col, 33)),
      rel_widths = c(1,1.5)
  )
  } else {
    p <- plot_grid(
      p1 + geom_point(data = samps[samps$samp %in% k:i, ],
                      aes(x, y, alpha = factor(samp)), fill = default_col, color = default_col, shape = 21) +
        scale_alpha_manual(values=c(rep(.33, i-k), 1)),
      p2 + geom_histogram(data = tibble(x = d[1:i]), bins = 50, fill = paste0(default_col, 33)),
      rel_widths = c(1,1.5)
    )
  }
  
  if (i < 10) {
    for (j in 1:10) print(p)
  } else if (i < 15) {
    for (j in 1:5) print(p)
  } else if (i < 20) {
    for (j in 1:2) print(p)
  } else if (i > 485) {
    for (j in 1:2) print(p)
  } else if (i > 492) {
    for (j in 1:5) print(p)
  } else if (i > 495) {
    for (j in 1:10) print(p)
  } else print(p)
  
  k <- i + 1
}


for(i in seq(0, 65, length.out = 20)) {
  print(
  plot_grid(
    p1,
    p2 + geom_histogram(data = tibble(x = d), bins = 50, fill = paste0(default_col, 33)) +
      geom_line(data = tibble(x = rep(mean(d), 2), y = c(0, i)), aes(x, y), col = "#EEB238", lwd = 1),
    rel_widths = c(1,1.5)
  ))
}

for (i in sprintf("%02x", round(seq(1, 255, length.out = 15)))) {
  print(
    plot_grid(
      p1,
      p2 + geom_histogram(data = tibble(x = d), bins = 50, fill = paste0(default_col, 33)) +
        geom_vline(xintercept = mean(d), col = "#EEB238", lwd = 1) +
        annotate("text", 1.1, 65, label = bquote(Delta==.(round(mean(d), 2))), size = 5, color = paste0("#6E4C1E", i)),
      rel_widths = c(1,1.5)
    ))
}

for (i in 1:30) {
  print(
    plot_grid(
      p1,
      p2 + geom_histogram(data = tibble(x = d), bins = 50, fill = paste0(default_col, 33)) +
        geom_vline(xintercept = mean(d), col = "#EEB238", lwd = 1) +
        annotate("text", 1.1, 65, label = bquote(Delta==.(round(mean(d), 2))), size = 5, color = "#6E4C1E"),
      rel_widths = c(1,1.5)
    ))
}
dev.off()

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 1 -loop 0 analysing_data\\gif_pics\\*.png analysing_data\\diff.gif')

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" convert analysing_data\\diff.gif -fuzz 10% -layers Optimize analysing_data\\diff_small.gif')

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 1 -loop 1 analysing_data\\gif_pics\\*.png analysing_data\\diff_1.gif')

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" convert analysing_data\\diff_1.gif -fuzz 10% -layers Optimize analysing_data\\diff_1_small.gif')
