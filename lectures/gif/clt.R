
setwd("N:/teaching/analysing_data/lectures/gif")
set.seed(1)

bg_col <- "#fdfdfd"
default_col <- "#434b75"
theme_col <- "#77bd9d"
complement_col <- "#bd7797"
other_col <- bg_col

rbimod <- function(x, alpha, beta) c(rbeta(x/2, alpha, beta), rbeta(x/2, beta, alpha))
x <- seq(0, 1, .01)
y <- dbeta(x, 2, 10) + dbeta(x, 10, 2)

s <- replicate(1000, rbimod(20, 2, 10)) # N = 20
max_y <- max(apply(s, 2, function(x) max(hist(x, plot = F)$counts)))
mu <- mean(s)
x_bar <- apply(s, 2, mean)
se <- sd(x_bar)
x2 <- seq(range(x_bar)[1] - .05, range(x_bar)[2] + .05, length.out = 100)
y2 <- dnorm(x2, mu, se)
sampling_p <- hist(x_bar, breaks = 100, plot = F)
png("sample%04d.png", 500, 500)
# par(mfrow = 2:1)
## replace colours: grey = #93a1a1, darkgreen = #eee8d5, purple = #ffa733
## remove , bty="l" from all plots
par(mfrow = 2:1, bg = bg_col, col = default_col, col.axis = default_col, col.lab = default_col, col.main = default_col, col.sub = default_col)
sample_mean <- NA

### to get density plots and histograms on the same y scale
# trial and error
scale_y <- max(y)/5
scale_y2 <- max(y2)/40

for (i in 1:40) {
  plot(x, y, type = "n", ylim = c(0, max_y), xlab = "", ylab = "", main = "Sample (N = 20)", bty="l")
  polygon(x, scale(y, F, scale = scale_y), col = paste0(theme_col, "aa"), border = NA)
  plot(NULL, main = "Sampling distribution of the mean", xlim = c(.4, .6),
       ylim = c(0, max(sampling_p$counts)), xlab = "", ylab = "", bty="l")
  polygon(x2, scale(y2, F, scale = scale_y2), col = paste0(default_col, "aa"), border = NA)
}

for(j in 1:10) {
  p <- hist(s[ , j], breaks = 20, plot = F)
  p_count <- p$counts
  p$counts <- rep(0, 20)
  sample_mean <- c(sample_mean, mean(s[ , j]))
  
  for (i in seq(0, 20, each = 4)) {
    p$counts[i] <- p_count[i]
    
    plot(x, y, type = "n", ylim = c(0, max_y), xlab = "", ylab = "", main = "Sample (N = 20)", bty="l")
    polygon(x, scale(y, F, scale = scale_y), col = paste0(theme_col, "aa"), border = NA)
    plot(p, add = T, col = bg_col, border = default_col)
    plot(NULL, main = "Sampling distribution of the mean", xlim = c(.4, .6),
         ylim = c(0, max(sampling_p$counts)), xlab = "", ylab = "", bty="l")
    polygon(x2, scale(y2, F, scale = scale_y2), col = paste0(default_col, "aa"), border = NA)
    hist(sample_mean[1:j], add = T, col = theme_col, border = theme_col, breaks = sampling_p$breaks)
  }
  for (i in 1:25) {
    plot(x, y, type = "n", ylim = c(0, max_y), xlab = "", ylab = "", main = "Sample (N = 20)", bty="l")
    polygon(x, scale(y, F, scale = scale_y), col = paste0(theme_col, "aa"), border = NA)
    plot(p, add = T, col = bg_col, border = default_col)
    abline(v = sample_mean[j+1], col = theme_col)
    
    plot(NULL, main = "Sampling distribution of the mean", xlim = c(.4, .6),
         ylim = c(0, max(sampling_p$counts)), xlab = "", ylab = "", bty="l")
    polygon(x2, scale(y2, F, scale = scale_y2), col = paste0(default_col, "aa"), border = NA)
    abline(v = sample_mean[j+1], col = theme_col)
    hist(sample_mean[-1], add = T, col = theme_col, border = theme_col, breaks = sampling_p$breaks)
  }
}

sample_mean <- apply(s, 2, mean)
for(j in 11:15) {
  p <- hist(s[ , j], breaks = 20, plot = F)
  p_count <- p$counts
  p$counts <- rep(0, 20)
  
  for (i in 0:20) {
    p$counts[i] <- p_count[i]
    
    plot(x, y, type = "n", ylim = c(0, max_y), xlab = "", ylab = "", main = "Sample (N = 20)", bty="l")
    polygon(x, scale(y, F, scale = scale_y), col = paste0(theme_col, "aa"), border = NA)
    plot(p, add = T, col = bg_col, border = default_col)
    plot(NULL, main = "Sampling distribution of the mean", xlim = c(.4, .6),
         ylim = c(0, max(sampling_p$counts)), xlab = "", ylab = "", bty="l")
    polygon(x2, scale(y2, F, scale = scale_y2), col = paste0(default_col, "aa"), border = NA)
    hist(sample_mean[1:(j-1)], add = T, col = theme_col, border = theme_col, breaks = sampling_p$breaks)
  }
  for (i in 1:5) {
    plot(x, y, type = "n", ylim = c(0, max_y), xlab = "", ylab = "", main = "Sample (N = 20)", bty="l")
    polygon(x, scale(y, F, scale = scale_y), col = paste0(theme_col, "aa"), border = NA)
    plot(p, add = T, col = bg_col, border = default_col)
    abline(v = sample_mean[j], col = theme_col)
    
    plot(NULL, main = "Sampling distribution of the mean", xlim = c(.4, .6),
         ylim = c(0, max(sampling_p$counts)), xlab = "", ylab = "", bty="l")
    polygon(x2, scale(y2, F, scale = scale_y2), col = paste0(default_col, "aa"), border = NA)
    abline(v = sample_mean[j], col = theme_col)
    hist(sample_mean[1:j], add = T, col = theme_col, border = theme_col, breaks = sampling_p$breaks)
  }
}

for(j in 16:20) {
  p <- hist(s[ , j], breaks = 20, plot = F)
  p_count <- p$counts
  p$counts <- rep(0, 20)
  
  for (i in seq(2, 20, by = 2)) {
    p$counts[1:i] <- p_count[1:i]
    
    plot(x, y, type = "n", ylim = c(0, max_y), xlab = "", ylab = "", main = "Sample (N = 20)", bty="l")
    polygon(x, scale(y, F, scale = scale_y), col = paste0(theme_col, "aa"), border = NA)
    plot(p, add = T, col = bg_col, border = default_col)
    plot(NULL, main = "Sampling distribution of the mean", xlim = c(.4, .6),
         ylim = c(0, max(sampling_p$counts)), xlab = "", ylab = "", bty="l")
    polygon(x2, scale(y2, F, scale = scale_y2), col = paste0(default_col, "aa"), border = NA)
    hist(sample_mean[1:(j-1)], add = T, col = theme_col, border = theme_col, breaks = sampling_p$breaks)
  }
  plot(x, y, type = "n", ylim = c(0, max_y), xlab = "", ylab = "", main = "Sample (N = 20)", bty="l")
  polygon(x, scale(y, F, scale = scale_y), col = paste0(theme_col, "aa"), border = NA)
  plot(p, add = T, col = bg_col, border = default_col)
  abline(v = sample_mean[j], col = theme_col)
  
  plot(NULL, main = "Sampling distribution of the mean", xlim = c(.4, .6),
       ylim = c(0, max(sampling_p$counts)), xlab = "", ylab = "", bty="l")
  polygon(x2, scale(y2, F, scale = scale_y2), col = paste0(default_col, "aa"), border = NA)
  abline(v = sample_mean[j], col = theme_col)
  hist(sample_mean[1:j], add = T, col = theme_col, border = theme_col, breaks = sampling_p$breaks)
}

for(j in 21:30) {
  p <- hist(s[ , j], breaks = 20, plot = F)
  p_count <- p$counts
  p$counts <- rep(0, 20)
  
  for (i in seq(4, 20, by = 4)) {
    p$counts[1:i] <- p_count[1:i]
    
    plot(x, y, type = "n", ylim = c(0, max_y), xlab = "", ylab = "", main = "Sample (N = 20)", bty="l")
    polygon(x, scale(y, F, scale = scale_y), col = paste0(theme_col, "aa"), border = NA)
    plot(p, add = T, col = bg_col, border = default_col)
    plot(NULL, main = "Sampling distribution of the mean", xlim = c(.4, .6),
         ylim = c(0, max(sampling_p$counts)), xlab = "", ylab = "", bty="l")
    polygon(x2, scale(y2, F, scale = scale_y2), col = paste0(default_col, "aa"), border = NA)
    hist(sample_mean[1:(j-1)], add = T, col = theme_col, border = theme_col, breaks = sampling_p$breaks)
  }
  plot(x, y, type = "n", ylim = c(0, max_y), xlab = "", ylab = "", main = "Sample (N = 20)", bty="l")
  polygon(x, scale(y, F, scale = scale_y), col = paste0(theme_col, "aa"), border = NA)
  plot(p, add = T, col = bg_col, border = default_col)
  abline(v = sample_mean[j], col = theme_col)
  
  plot(NULL, main = "Sampling distribution of the mean", xlim = c(.4, .6),
       ylim = c(0, max(sampling_p$counts)), xlab = "", ylab = "", bty="l")
  polygon(x2, scale(y2, F, scale = scale_y2), col = paste0(default_col, "aa"), border = NA)
  abline(v = sample_mean[j], col = theme_col)
  hist(sample_mean[1:j], add = T, col = theme_col, border = theme_col, breaks = sampling_p$breaks)
}

for(j in 31:40) {
  p <- hist(s[ , j], breaks = 20, plot = F)
  p_count <- p$counts
  p$counts <- rep(0, 20)
  
  for (i in seq(5, 20, by = 5)) {
    p$counts[1:i] <- p_count[1:i]
    
    plot(x, y, type = "n", ylim = c(0, max_y), xlab = "", ylab = "", main = "Sample (N = 20)", bty="l")
    polygon(x, scale(y, F, scale = scale_y), col = paste0(theme_col, "aa"), border = NA)
    plot(p, add = T, col = bg_col, border = default_col)
    plot(NULL, main = "Sampling distribution of the mean", xlim = c(.4, .6),
         ylim = c(0, max(sampling_p$counts)), xlab = "", ylab = "", bty="l")
    polygon(x2, scale(y2, F, scale = scale_y2), col = paste0(default_col, "aa"), border = NA)
    hist(sample_mean[1:(j-1)], add = T, col = theme_col, border = theme_col, breaks = sampling_p$breaks)
  }
  plot(x, y, type = "n", ylim = c(0, max_y), xlab = "", ylab = "", main = "Sample (N = 20)", bty="l")
  polygon(x, scale(y, F, scale = scale_y), col = paste0(theme_col, "aa"), border = NA)
  plot(p, add = T, col = bg_col, border = default_col)
  abline(v = sample_mean[j], col = theme_col)
  
  plot(NULL, main = "Sampling distribution of the mean", xlim = c(.4, .6),
       ylim = c(0, max(sampling_p$counts)), xlab = "", ylab = "", bty="l")
  polygon(x2, scale(y2, F, scale = scale_y2), col = paste0(default_col, "aa"), border = NA)
  abline(v = sample_mean[j], col = theme_col)
  hist(sample_mean[1:j], add = T, col = theme_col, border = theme_col, breaks = sampling_p$breaks)
}

for(j in 41:60) {
  p <- hist(s[ , j], breaks = 20, plot = F)
  p_count <- p$counts
  p$counts <- rep(0, 20)
  
  for (i in c(10, 20)) {
    p$counts[1:i] <- p_count[1:i]
    
    plot(x, y, type = "n", ylim = c(0, max_y), xlab = "", ylab = "", main = "Sample (N = 20)", bty="l")
    polygon(x, scale(y, F, scale = scale_y), col = paste0(theme_col, "aa"), border = NA)
    plot(p, add = T, col = bg_col, border = default_col)
    plot(NULL, main = "Sampling distribution of the mean", xlim = c(.4, .6),
         ylim = c(0, max(sampling_p$counts)), xlab = "", ylab = "", bty="l")
    polygon(x2, scale(y2, F, scale = scale_y2), col = paste0(default_col, "aa"), border = NA)
    hist(sample_mean[1:(j-1)], add = T, col = theme_col, border = theme_col, breaks = sampling_p$breaks)
  }
  plot(x, y, type = "n", ylim = c(0, max_y), xlab = "", ylab = "", main = "Sample (N = 20)", bty="l")
  polygon(x, scale(y, F, scale = scale_y), col = paste0(theme_col, "aa"), border = NA)
  plot(p, add = T, col = bg_col, border = default_col)
  abline(v = sample_mean[j], col = theme_col)
  
  plot(NULL, main = "Sampling distribution of the mean", xlim = c(.4, .6),
       ylim = c(0, max(sampling_p$counts)), xlab = "", ylab = "", bty="l")
  polygon(x2, scale(y2, F, scale = scale_y2), col = paste0(default_col, "aa"), border = NA)
  abline(v = sample_mean[j], col = theme_col)
  hist(sample_mean[1:j], add = T, col = theme_col, border = theme_col, breaks = sampling_p$breaks)
}

for(j in 61:100) {
  p <- hist(s[ , j], breaks = 20, plot = F)
  
  plot(x, y, type = "n", ylim = c(0, max_y), xlab = "", ylab = "", main = "Sample (N = 20)", bty="l")
  polygon(x, scale(y, F, scale = scale_y), col = paste0(theme_col, "aa"), border = NA)
  plot(p, add = T, col = bg_col, border = default_col)
  
  plot(NULL, main = "Sampling distribution of the mean", xlim = c(.4, .6),
       ylim = c(0, max(sampling_p$counts)), xlab = "", ylab = "", bty="l")
  polygon(x2, scale(y2, F, scale = scale_y2), col = paste0(default_col, "aa"), border = NA)
  hist(sample_mean[1:(j-1)], add = T, col = theme_col, border = theme_col, breaks = sampling_p$breaks)
    
  plot(x, y, type = "n", ylim = c(0, max_y), xlab = "", ylab = "", main = "Sample (N = 20)", bty="l")
  polygon(x, scale(y, F, scale = scale_y), col = paste0(theme_col, "aa"), border = NA)
  plot(p, add = T, col = bg_col, border = default_col)
  abline(v = sample_mean[j], col = theme_col)
  
  plot(NULL, main = "Sampling distribution of the mean", xlim = c(.4, .6),
       ylim = c(0, max(sampling_p$counts)), xlab = "", ylab = "", bty="l")
  polygon(x2, scale(y2, F, scale = scale_y2), col = paste0(default_col, "aa"), border = NA)
  abline(v = sample_mean[j], col = theme_col)
  hist(sample_mean[1:j], add = T, col = theme_col, border = theme_col, breaks = sampling_p$breaks)
}

for(j in seq(101, ncol(s), by = 3)) {
  p <- hist(s[ , j], breaks = 20, plot = F)

  plot(x, y, type = "n", ylim = c(0, max_y), xlab = "", ylab = "", main = "Sample (N = 20)", bty="l")
  polygon(x, scale(y, F, scale = scale_y), col = paste0(theme_col, "aa"), border = NA)
  plot(p, add = T, col = bg_col, border = default_col)
  abline(v = sample_mean[j], col = theme_col)
  abline(v = sample_mean[j+1], col = theme_col)
  abline(v = sample_mean[j+2], col = theme_col)
  
  plot(NULL, main = "Sampling distribution of the mean", xlim = c(.4, .6),
       ylim = c(0, max(sampling_p$counts)), xlab = "", ylab = "", bty="l")
  polygon(x2, scale(y2, F, scale = scale_y2), col = paste0(default_col, "aa"), border = NA)
  abline(v = sample_mean[j], col = theme_col)
  abline(v = sample_mean[j+1], col = theme_col)
  abline(v = sample_mean[j+2], col = theme_col)
  hist(sample_mean[1:j], add = T, col = theme_col, border = theme_col, breaks = sampling_p$breaks)
}

j <- ncol(s)
for(i in 1:40) {
  p <- hist(s[ , j], breaks = 20, plot = F)
  
  plot(x, y, type = "n", ylim = c(0, max_y), xlab = "", ylab = "", main = "Sample (N = 20)", bty="l")
  polygon(x, scale(y, F, scale = scale_y), col = paste0(theme_col, "aa"), border = NA)
  plot(p, add = T, col = bg_col, border = default_col)
  abline(v = sample_mean[j], col = theme_col)
  
  plot(NULL, main = "Sampling distribution of the mean", xlim = c(.4, .6),
       ylim = c(0, max(sampling_p$counts)), xlab = "", ylab = "", bty="l")
  polygon(x2, scale(y2, F, scale = scale_y2), col = paste0(default_col, "aa"), border = NA)
  hist(sample_mean[1:j], add = T, col = theme_col, border = theme_col, breaks = sampling_p$breaks)
}
  
dev.off()
# system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 2 *.png clt.gif')

system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" -delay 2 -loop 1 *.png clt_slide.gif')
system('"C:\\Program Files\\ImageMagick-7.0.9-Q16\\magick.exe" convert clt_slide.gif -fuzz 10% -layers Optimize clt_small.gif')
file.remove(list.files(pattern = ".png"))
