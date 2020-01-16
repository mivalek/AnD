
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

dfs <- c(2, 5, 1000)
colours <- c(theme_col, default_col, complement_col)
p1 <- tibble(x = rep(seq(-5, 5, length.out = 200), 3), df = rep(dfs, each = 200), y = dt(x, df)) %>%
  mutate(df = as.factor(df)) %>%
  ggplot(aes(x, y, group = df, color = df)) +
  geom_line() +
  scale_color_manual(values = colours) +
  labs(x = expression(italic(t)-score), y = "Density") +
  theme(legend.position = c(.75, .7))


png("analysing_data/t_gif/p_%02d.png", 700, 400)


for (i in seq_along(dfs)) {
  crit <- sprintf("%.02f", round(qt(.975, dfs[i]), 2))
  print(p1 + 
          stat_function(fun = dt, args = list(df = dfs[i]),
                        xlim = c(-1, 1) * as.numeric(crit),
                        geom = "area", fill = colours[i], color = colours[i]) +
          annotate("text", x = -.05, y = .07, size = 7,
                   label = bquote(group("[", list(-.(crit),.(crit)), "]")), colour=bg_col)
  )
}

dev.off()

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 90 -loop 0 analysing_data\\t_gif\\*.png analysing_data\\t.gif')

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" convert analysing_data\\t.gif -fuzz 10% -layers Optimize analysing_data\\t_small.gif')

