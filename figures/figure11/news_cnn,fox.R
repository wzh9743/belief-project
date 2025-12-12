df <- read.csv("news_cnn,fox.csv", stringsAsFactors = FALSE)
df$year <- as.Date(df$year)

x <- df[[1]]

cols <- c("blue", "red")

setEPS()
postscript("climate_news_coverage_data.eps", width=10, height=7)

plot(
  x, df[[2]],
  type = "l", lwd = 2, col = "blue", lty = "solid",
  ylim =c(0, 350),
  xlab = "Year", cex.lab = 1, ylab = "",
  main = "",
  xaxt = "n"
)

lines(x, df[[3]], col = "red",   lwd = 2, lty = "dashed")

years <- seq(
  from = as.Date(paste0(format(min(x), "%Y"), "-01-01")),
  to   = as.Date(paste0(format(max(x), "%Y"), "-01-01")),
  by   = "2 years"
)

axis(1, at = years, labels = format(years, "%Y"))


legend(
  "topleft",
  legend = c("CNN", "FOX"),
  col = c("blue", "red"),
  lty = c("solid", "dashed"),
  lwd = 2,
  bty = "o",
  cex = 1.2
)
dev.off()
