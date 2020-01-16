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
df <- tibble(x = runif(n, -10, 10),
             y = runif(n, -10, 10),
             size = sample(1:20, n, T))
df <- df[sqrt(df$x^2 + df$y^2) <= 10, ]
mu <- mean(df$size)
sigma <- sd(df$size)

p1 <- df %>%
  ggplot(aes(x, y, size = size)) +
  geom_point(shape = 21, color = point_col, fill = point_col) +
  theme(axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank(), legend.position="none") +
  labs(title = expression("Population"), x = "", y = "")

samp <- df[sample(nrow(df), 2), ]
samp$group <- factor(1:2)

means <- samp %>% group_by(group) %>% summarise(x = n(), y = mean(size), ci = NA)


for (i in 2:100) {
  samp <- rbind(samp[ , 1:3], df[sample(nrow(df), 2), ])
  samp$group <- factor(rep(1:2, i))
  means <- rbind(means, samp %>% group_by(group) %>% summarise(x = n(), y = mean(size), ci = qt(.975, n()-1) * sd(size)/sqrt(n())))
}

p2 <- 
  means %>% ggplot(aes(x, y, color = group)) +
  scale_color_manual(values = c("#6E4C1E", "#EEB238")) +
  scale_fill_manual(values = rep(paste0(default_col, "33"), 2)) + 
  coord_cartesian(ylim=c(0, 22), xlim = c(1, 100)) +
  labs(title=bquote(Estimated~mean%+-%95*"% CI"), x = expression(italic(N)), y = expression(hat(mu))) +
  theme(axis.title.y = element_text(angle = 0, vjust = .5), legend.position = "none")


png("analysing_data/samp_gif/p_%04d.png", 900, 350)

plot_grid(p1, p2, rel_widths = 1:2)
for (i in c(rep(1:20, each = 5), rep(21:40, each = 3), rep(41:60, each = 2), 61:95, rep(96:100, each = 5), rep(100, 10))) {
  print(plot_grid(
    p1 +
      geom_point(data = samp[1:(2*i), ], aes(x, y, fill = group, color = group), shape = 21) +
      scale_color_manual(values = c("#6E4C1E", "#EEB238")) +
      scale_fill_manual(values = c("#6E4C1EAA", "#EEB238AA")),
    p2 + 
      geom_ribbon(data=means[1:(2*i),], aes(ymin = y - ci, ymax = y + ci, fill = group), color = NA) +
      geom_line(data=means[1:(2*i),]) +
      geom_errorbar(data = means[c(i*2, i*2-1), ], aes(ymin = y - ci, ymax = y + ci), color = default_col) +
      geom_point(data = means[c(i*2, i*2-1), ], aes(x, y, color = group)),
    rel_widths = 1:2
  ))
}

dev.off()

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 8 -loop 0 analysing_data\\samp_gif\\*.png analysing_data\\samp.gif')

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" convert analysing_data\\samp.gif -fuzz 10% -layers Optimize analysing_data\\samp_small.gif')

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 8 -loop 1 analysing_data\\samp_gif\\*.png analysing_data\\samp_1.gif')

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" convert analysing_data\\samp_1.gif -fuzz 10% -layers Optimize analysing_data\\samp_1_small.gif')
