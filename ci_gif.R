
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


s <- sample[ , 1]
cis <- tibble(x = mean(df$size[s]), y = 1,
              ci = qt(0.975, df =length(s) - 1)*sd(df$size[s])/sqrt(length(s)))
for (i in 2:ncol(sample)) {
  s <- sample[ , i]
  cis[i, ] <- tibble(x = mean(df$size[s]), y = i,
                     ci = qt(0.975, df =length(s) - 1)*sd(df$size[s])/sqrt(length(s)))
}

cis$out <- cis$ci + mu < mu + abs(cis$x - mu)

p1 <- df %>%
  ggplot(aes(x, y, size = size)) +
  geom_point(shape = 21, color = point_col, fill = point_col) +
  theme(axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank(), legend.position="none") +
  labs(title = "Population", x = "", y = "")

p2 <- df[s, ] %>%
  ggplot(aes(x = size)) +
  labs(title = expression("Sample" ~ (italic(N) == 30)), x = "Size", y = "N") +
  scale_x_continuous(breaks = seq(0, 20, by = 5), limits = c(0, 20)) + ylim(0, 8)


p3 <- cis %>%
  ggplot(aes(x, y)) +
  geom_vline(xintercept = mu, color = "#dc151588") +
  xlim(floor(min(cis$x - cis$ci)), ceiling(max(cis$x + cis$ci))) +
  labs(title=expression(italic(M)[size] %+-% 95~"% CIs"), x = "Mean size", y = "Sample") +
  scale_y_continuous(breaks = c(1, seq(10, ncol(sample), 10)), limits = c(1, 100))

p3b <- p3

null_left <- plot_grid(p1, p2, ncol = 1)
null_plot <- plot_grid(null_left, p3)
png("analysing_data/ci_gif/p_%04d.png", 450, 500)

print(null_plot)

for (i in (1:ncol(sample))[-seq(71, ncol(sample), 2)]) {
  s <- sample[ , i]
  
  p1b <- p1 + geom_point(data = df[s, ], shape = 21, color = "#dc151588", fill = "#dc151588")
  
  p2b <- df[s, ] %>%
    ggplot(aes(x = size)) +
    geom_histogram(bins = 10, fill = "#dc1515bb", color = "#dc1515") +
    geom_vline(xintercept = mean(df$size[s]), size = 1, color = default_col) +
    labs(title = expression("Sample" ~ (italic(N) == 30)), x = "Size", y = "N") +
    scale_x_continuous(breaks = seq(0, 20, by = 5), limits = c(0, 20)) + ylim(0, 8)
  
  
  
  left <- plot_grid(p1b, p2b, ncol = 1)
  if (i < 20) print(plot_grid(left, p3b))
  
  p3b <- cis[1:i, ] %>%
    ggplot(aes(x, y)) +
    geom_vline(xintercept = mu, color = "#dc151588") +
    geom_errorbarh(aes(xmax = x + ci, xmin = x - ci, color = out),
                   height = 0) +
    geom_point(shape = 21, aes(color = out), fill = "#dc1515") +
    xlim(floor(min(cis$x - cis$ci)), ceiling(max(cis$x + cis$ci))) +
    labs(title=expression(italic(M)[size] %+-% 95~"% CIs"), x = "Mean size", y = "Sample") +
    theme(legend.position = "none") + scale_color_manual(values = c(default_col, "#dc1515")) +
    scale_y_continuous(breaks = c(1, seq(10, ncol(sample), 10)), limits = c(1, 100))
  
  print(plot_grid(left, p3b))
  if (i < 40) print(plot_grid(left, p3b))
}

for (i in 1:15) print(plot_grid(null_left, p3b))

for (i in 1:5) {
  p3c <- cis[!cis$out, ] %>%
    ggplot(aes(x, y)) +
    geom_vline(xintercept = mu, color = "#dc151588") +
    geom_errorbarh(aes(xmax = x + ci, xmin = x - ci),
                   height = 0, color = default_col) +
    geom_point(shape = 21, color = point_col, fill = "#dc1515") +
    xlim(floor(min(cis$x - cis$ci)), ceiling(max(cis$x + cis$ci))) +
    labs(title=expression(italic(M)[size] %+-% 95~"% CIs"), x = "Mean size", y = "Sample") +
    scale_y_continuous(breaks = c(1, seq(10, ncol(sample), 10)), limits = c(1, 100))
  
  print(plot_grid(null_left, p3c))
  print(plot_grid(null_left, p3c))
  
  print(plot_grid(null_left, p3b))
  print(plot_grid(null_left, p3b))
}
dev.off()

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 12 -loop 1 analysing_data\\ci_gif\\*.png analysing_data\\ci.gif')

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" convert analysing_data\\ci.gif -fuzz 10% -layers Optimize analysing_data\\ci_small.gif')


system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" identify -format "%n\n" analysing_data\\lectures\\pics\\huck.gif')

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" convert analysing_data\\lectures\\pics\\huck.gif[0,25-98] analysing_data\\lectures\\pics\\huck_small.gif')

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" convert analysing_data\\lectures\\pics\\huck_small.gif -fuzz 5% -layers Optimize analysing_data\\lectures\\pics\\huck_small.gif')

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" convert analysing_data\\lectures\\pics\\puppyattack.gif -fuzz 10% -layers Optimize analysing_data\\lectures\\pics\\puppyattack_small.gif')
