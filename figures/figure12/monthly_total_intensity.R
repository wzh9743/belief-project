df <- read.csv("monthly_total_intensity.csv", stringsAsFactors = FALSE)
df$year <- as.Date(df$year)

x <- df[[1]]

cols <- c("green", "darkorange")

setEPS()
postscript("monthly_intensity.eps", width=10, height=7)

plot(
  x, df[[2]],
  type = "l", lwd = 2, col = "green", lty = "solid",
  ylim =c(0, 90),
  xlab = "Year", cex.lab = 1, ylab = "",
  main = "",
  xaxt = "n"
)

lines(x, df[[3]], col = "darkorange",   lwd = 2, lty = "dashed")

years <- seq(
  from = as.Date(paste0(format(min(x), "%Y"), "-01-01")),
  to   = as.Date(paste0(format(max(x), "%Y"), "-01-01")),
  by   = "2 years"
)

axis(1, at = years, labels = format(years, "%Y"))


legend(
  "topleft",
  legend = c("positive events", "negative events"),
  col = c("green", "darkorange"),
  lty = c("solid", "dashed"),
  lwd = 2,
  bty = "o",
  cex = 1.2
)
dev.off()
