
library(plotly)
library(reshape2)
library(magrittr)
library(colortools)
library(dplyr)


set.seed(123)

my_df <- tibble(x1 = rnorm(100, 100, 15), x2 = rnorm(100, 50, 10), y = -10 + .4 * x1 - .5 * x2 + rnorm(100, 0, 10))
m1 <- lm(y ~ x1 + x2,data = my_df)


#Graph Resolution (more important for more complex shapes)
graph_reso <- 100

#Setup Axis
axis_x <- seq(floor(min(my_df$x1)), ceiling(max(my_df$x1)), length.out = graph_reso)
axis_y <- seq(floor(min(my_df$x2)), ceiling(max(my_df$x2)), length.out = graph_reso)

#Sample points
m1_surface <- expand.grid(x1 = axis_x,x2 = axis_y,KEEP.OUT.ATTRS = F)
m1_surface$y <- predict.lm(m1, newdata = m1_surface)
m1_surface <- acast(m1_surface, x2 ~ x1, value.var = "y")


# hcolors=c("red","blue","green")[my_df$Species]
plot_3d <- plot_ly(my_df,
                     x = ~x1,
                     y = ~x2,
                     z = ~y,
                     type = "scatter3d",
                     mode = "markers",
                     marker = list(color = "#ba3e8a",
                                   size = 5),
                     opacity = .5,
                     showscale = F,
                     showlegend=F) %>%
  add_trace(z = m1_surface,
            x = axis_x,
            y = axis_y,
            type = "surface",
            # opacity = .8,
            colorscale = list(c(0, 1), rep(complementary("#ba3e8a99", plot = F)[2], 2))) %>%
  layout(paper_bgcolor="#fdfdfd", 
         margin = list(b = 0, l = 0, r = 0, t = 0, pad = 0, autoexpand = TRUE),
         scene = list(
           xaxis = list(title = "Predictor 1"),
           yaxis = list(title = "Predictor 2"),
           zaxis = list(title = "Outcome")
         ))  %>%
  config(displayModeBar = F)


plot_3d

htmlwidgets::saveWidget(plot_3d,'plot_3d.html',
  background = "fdfdfd")

# <div class="plot_ly">
# <iframe frameborder="0" seamless='seamless' scrolling=no src="test_plot.html"></iframe>
# </div>

