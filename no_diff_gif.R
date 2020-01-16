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

p1 <- df %>%
  ggplot(aes(x, y, size = size)) +
  geom_point(shape = 21, color = point_col, fill = point_col) +
  theme(axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank(), legend.position="none") +
  labs(title = expression("Population"), x = "", y = "") +
  theme(plot.title = element_text(vjust=0))



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

p2 <- 
  tibble(x = d) %>% ggplot(aes(x)) +
  geom_vline(xintercept = 0, lty=2) +
  xlim(-1, 1) + ylim(0, 40) +
  labs(title=bquote(Difference~bar(AI)[climb]-bar(AI)[gen]), x = expression(italic(D)), y = "Frequency")

png("analysing_data/gif_pics/p_%04d.png", 750, 350)

plot_grid(p1, p2, rel_widths = c(1, 1.5))

k <- 1
for (i in c(1:50, seq(51, 100, by = 2), seq(101, 200, by = 3), seq(201, 430, by = 4), seq(431, 470, by = 2), 471:500)) {
  
  if (k == i) {
    p <- plot_grid(
      p1 + geom_point(data = samps[samps$samp %in% i, ], aes(x, y, fill = group, color = group), shape = 21) +
        scale_color_manual(values = c("#6E4C1EAA", "#EEB238AA")) +
        scale_fill_manual(values = c("#6E4C1EAA", "#EEB238AA")),
      p2 + geom_histogram(data = tibble(x = d[1:i]), bins = 50, fill = paste0(default_col, 33)),
      rel_widths = c(1,1.5)
  )
  } else {
    p <- plot_grid(
      p1 + geom_point(data = samps[samps$samp %in% k:i, ], aes(x, y, fill = group, color = group, alpha = factor(samp)), shape = 21) +
        scale_color_manual(values = c("#6E4C1E", "#EEB238")) +
        scale_fill_manual(values = c("#6E4C1E", "#EEB238")) +
        scale_alpha_manual(values=c(rep(.33, i-k), .66)),
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


for(i in 1:20) print(
  plot_grid(
    p1,
    p2 + geom_histogram(data = tibble(x = d), bins = 50, fill = paste0(default_col, 33)),
    rel_widths = c(1,1.5)
))

dev.off()

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 1 -loop 0 analysing_data\\gif_pics\\*.png analysing_data\\no_diff.gif')

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" convert analysing_data\\no_diff.gif -fuzz 10% -layers Optimize analysing_data\\no_diff_small.gif')

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 1 -loop 1 analysing_data\\gif_pics\\*.png analysing_data\\no_diff_1.gif')

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" convert analysing_data\\no_diff_1.gif -fuzz 10% -layers Optimize analysing_data\\no_diff_1_small.gif')
