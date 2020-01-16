
# library(here)
# here("teaching/analysing_data/lectures/")
setwd("N:/teaching/analysing_data/lectures/gif")
dnorminv <- function(y) sqrt(-2 * log(sqrt(2 * pi) * y))

x <- seq(-8, 8, by = .1)

png("plot%02d.png", 800, 500)
bg_col <- "#fdfdfd"
default_col <- "#434b75"
theme_col <- "#77bd9d"
complement_col <- "#bd7797"
other_col <- "#eee8d5"
par(bg = bg_col, fg = default_col, col = default_col, col.axis = default_col, col.lab = default_col, col.main = default_col, col.sub = default_col, mar = c(5.1, 4.1, 1.1, 2.1))

plot(x, dnorm(x, 0, 1), xlim = c(-8, 8), ylim = c(0, .6),
     type = "l", bty = "n", xlab = "x", ylab = "Density")
polygon(c(-3, seq(-3, 3, by = .3), 3), c(0, dnorm(seq(-3, 3, by = .3), 0, 1), 0), col = default_col, border = NA)
polygon(c(-2, seq(-2, 2, by = .2), 2), c(0, dnorm(seq(-2, 2, by = .2), 0, 1), 0), col = other_col, border = NA)
polygon(c(-1, seq(-1, 1, by = .1), 1), c(0, dnorm(seq(-1, 1, by = .1), 0, 1), 0), col = theme_col, border = NA)
j <- c(.23, .14, .05)
k <- c("68.2%", "95.4%", "99.7%")
for (i in 1:3) {
  if (i == 1) {
    lines(c(-i,i), rep(j[i], 2), lwd = 2, col = default_col)
    lines(c(-i, -i), j[i] + c(-.005, .005), lwd = 2, col = default_col)
    lines(c(i, i), j[i] + c(-.005, .005), lwd = 2, col = default_col)
  } else {
    lines(c(-i, -i), c(dnorm(i, 0, 1), j[i]), lty = 2, col = default_col)
    lines(c(i, i), c(dnorm(i, 0, 1), j[i]), lty = 2, col = default_col)
    
    lines(c(-i,i), rep(j[i], 2), lwd = 2, col = default_col)
    lines(c(-i, -i), j[i] + c(-.005, .005), lwd = 2, col = default_col)
    lines(c(i, i), j[i] + c(-.005, .005), lwd = 2, col = default_col)
    # lines(dnorminv(j[i]) * c(-1, 1), rep(j[i], 2), lwd = 2, col = default_col)
  }
  text(0, j[i], labels = k[i], pos = 3, col = default_col)
}

a <- as.character(as.hexmode(round(seq(255, 0, length.out = length(x)))))
for (p in seq(1,length(x), by = 3)) {
  plot(x, dnorm(x, 0, 1), xlim = c(-8, 8), ylim = c(0, .6),
       type = "l", bty = "n", xlab = "x", ylab = "Density")
  polygon(c(-3, seq(-3, 3, by = .3), 3), c(0, dnorm(seq(-3, 3, by = .3), 0, 1), 0), col = paste0(default_col, a[p]), border = NA)
  polygon(c(-2, seq(-2, 2, by = .2), 2), c(0, dnorm(seq(-2, 2, by = .2), 0, 1), 0), col = paste0(other_col, a[p]), border = NA)
  polygon(c(-1, seq(-1, 1, by = .1), 1), c(0, dnorm(seq(-1, 1, by = .1), 0, 1), 0), col = paste0(theme_col, a[p]), border = NA)
  j <- c(.23, .14, .05)
  k <- c("68.2%", "95.4%", "99.7%")
  for (i in 1:3) {
    if (i == 1) {
      lines(c(-i,i), rep(j[i], 2), lwd = 2, col = paste0(default_col, a[p]))
      lines(c(-i, -i), j[i] + c(-.005, .005), lwd = 2, col = paste0(default_col, a[p]))
      lines(c(i, i), j[i] + c(-.005, .005), lwd = 2, col = paste0(default_col, a[p]))
    } else {
      lines(c(-i, -i), c(dnorm(i, 0, 1), j[i]), lty = 2, col = paste0(default_col, a[p]))
      lines(c(i, i), c(dnorm(i, 0, 1), j[i]), lty = 2, col = paste0(default_col, a[p]))
      
      lines(c(-i,i), rep(j[i], 2), lwd = 2, col = paste0(default_col, a[p]))
      lines(c(-i, -i), j[i] + c(-.005, .005), lwd = 2, col = paste0(default_col, a[p]))
      lines(c(i, i), j[i] + c(-.005, .005), lwd = 2, col = paste0(default_col, a[p]))
      # lines(dnorminv(j[i]) * c(-1, 1), rep(j[i], 2), lwd = 2, col = default_col)
    }
    text(0, j[i], labels = k[i], pos = 3, col = paste0(default_col, a[p]))
  }
  
  x2 <- x[1:p]
  lines(x2, dnorm(x2), lwd = 2, col = theme_col)
}
plot(x, dnorm(x, 0, 1), xlim = c(-8, 8), ylim = c(0, .6),
     type = "l", lwd = 2, col = theme_col,
     bty = "n", xlab = "x", ylab = "Density")
legend(5.1, .55, col = theme_col, lwd = 2, legend = expression(mu == 0), bty = "n", cex = 2, seg.len = 1)
dev.off()

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 3 -loop 1 *.png norm.gif')


png("%02d.png", 800, 500)
par(bg = bg_col, fg = default_col, col = default_col, col.axis = default_col, col.lab = default_col, col.main = default_col, col.sub = default_col, mar = c(5.1, 4.1, 1.1, 2.1))

for (i in seq(0, 6, length.out = 20)) {
  plot(x, dnorm(x, 0, 1), xlim = c(-8, 8), ylim = c(0, .6),
       type = "l", lwd = 2, col = theme_col,
       bty = "n", xlab = "x", ylab = "Density")
  legend(5.1, .55, col = theme_col, lwd = 2, legend = expression(mu == 0),
         bty = "n", cex = 2, seg.len = 1)
  lines(x, dnorm(x, 0 - i, 1), lwd = 2, col = paste0(theme_col, "55"))
}
plot(x, dnorm(x, 0, 1), xlim = c(-8, 8), ylim = c(0, .6),
     type = "l", lwd = 2, col = theme_col,
     bty = "n", xlab = "x", ylab = "Density")
legend(5.1, .55, col = c(theme_col, default_col), lwd = 2,
       legend = c(expression(mu == 0), expression(mu == -6)), bty = "n", cex = 2, seg.len = 1)
lines(x, dnorm(x, 0 - i, 1), lwd = 2, col = default_col)
dev.off()
system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 5 -loop 1 *.png mean1.gif')

png("%02d.png", 800, 500)
par(bg = bg_col, fg = default_col, col = default_col, col.axis = default_col, col.lab = default_col, col.main = default_col, col.sub = default_col, mar = c(5.1, 4.1, 1.1, 2.1))

for (i in seq(0, 10, length.out = 35)) {
  plot(x, dnorm(x, 0, 1), xlim = c(-8, 8), ylim = c(0, .6),
       type = "l", lwd = 2, col = theme_col,
       bty = "n", xlab = "x", ylab = "Density")
  lines(x, dnorm(x, -6, 1), lwd = 2, col = default_col)
  lines(x, dnorm(x, -6 + i, 1), lwd = 2, col = paste0(complement_col, "55"))
  legend(5.1, .55, col = c(theme_col, default_col), lwd = 2, legend = c(expression(mu == 0), expression(mu == -6)), bty = "n", cex = 2, seg.len = 1)
}
plot(x, dnorm(x, 0, 1), xlim = c(-8, 8), ylim = c(0, .6),
     type = "l", lwd = 2, col = theme_col,
     bty = "n", xlab = "x", ylab = "Density")
lines(x, dnorm(x, -6, 1), lwd = 2, col = default_col)
lines(x, dnorm(x, -6 + i, 1), lwd = 2, col = complement_col)
legend(5.1, .55, col = c(theme_col, default_col, complement_col), lwd = 2, legend = c(expression(mu == 0), expression(mu == -6),expression(mu == 4)), bty = "n", cex = 2, seg.len = 1)
dev.off()
system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 5 -loop 1 *.png mean2.gif')

png("%02d.png", 800, 500)
par(bg = bg_col, fg = default_col, col = default_col, col.axis = default_col, col.lab = default_col, col.main = default_col, col.sub = default_col, mar = c(5.1, 4.1, 1.1, 2.1))
# 
# plot(x, dnorm(x, 0, 1), xlim = c(-8, 8), ylim = c(0, .6),
#      type = "l", , lwd = 2, col = theme_col,
#      bty = "n", xlab = "x", ylab = "Density")
# legend(4.8, .55, col = theme_col, lwd = 2, legend = expression(sigma == 1), bty = "n", cex = 2, seg.len = 1)

for (i in seq(1, 3, length.out = 20)) {
  plot(x, dnorm(x, 0, 1), xlim = c(-8, 8), ylim = c(0, .6),
       type = "l", lwd = 2, col = theme_col,
       bty = "n", xlab = "x", ylab = "Density")
  lines(x, dnorm(x, 0, i), lwd = 2, col = paste0(theme_col, "55"))
  legend(4.8, .55, col = theme_col, lwd = 2, legend = expression(sigma == 1), bty = "n", cex = 2, seg.len = 1)
}
plot(x, dnorm(x, 0, 1), xlim = c(-8, 8), ylim = c(0, .6),
     type = "l", lwd = 2, col = theme_col,
     bty = "n", xlab = "x", ylab = "Density")
lines(x, dnorm(x, 0, 3), lwd = 2, col = default_col)
legend(4.8, .55, col = c(theme_col, default_col), lwd = 2, legend = c(expression(sigma == 1), expression(sigma == 3)), bty = "n", cex = 2, seg.len = 1)
dev.off()
system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 5 -loop 1 *.png sigma1.gif')

png("%02d.png", 800, 500)
par(bg = bg_col, fg = default_col, col = default_col, col.axis = default_col, col.lab = default_col, col.main = default_col, col.sub = default_col, mar = c(5.1, 4.1, 1.1, 2.1))


for (i in seq(3, .65, length.out = 35)) {
  plot(x, dnorm(x, 0, 1), xlim = c(-8, 8), ylim = c(0, .6),
       type = "l", lwd = 2, col = theme_col,
       bty = "n", xlab = "x", ylab = "Density")
  lines(x, dnorm(x, 0, 3), lwd = 2, col = default_col)
  lines(x, dnorm(x, 0, i), lwd = 2, col = paste0(complement_col, "55"))
  
  legend(4.8, .55, col = c(theme_col, default_col), lwd = 2, legend = c(expression(sigma == 1), expression(sigma == 3)), bty = "n", cex = 2, seg.len = 1)
}
plot(x, dnorm(x, 0, 1), xlim = c(-8, 8), ylim = c(0, .6),
     type = "l", lwd = 2, col = theme_col,
     bty = "n", xlab = "x", ylab = "Density")
lines(x, dnorm(x, 0, 3), lwd = 2, col = default_col)
lines(x, dnorm(x, 0, .65), lwd = 2, col = complement_col)
legend(4.8, .55, col = c(theme_col, default_col, complement_col), lwd = 2, legend = c(expression(sigma == 1), expression(sigma == 3),expression(sigma == 0.65)), bty = "n", cex = 2, seg.len = 1)
dev.off()
system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 3 -loop 1 *.png sigma2.gif')
