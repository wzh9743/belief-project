df <- read.csv("sub_imp_year.csv", stringsAsFactors = FALSE)

x <- df[[1]]

cols <- c("blue", "darkgreen", "red", "orange")
circles <- c("(1)", "(2)", "(3)", "(4)")

setEPS()
postscript("sub_imp_year.eps", width=10, height=7)

plot(
  x, df[[2]],
  type = "l", lwd = 2, col = "blue", lty = "solid",
  ylim =c(0.3, 1),
  xlab = "", ylab = "",
  main = ""
)
mtext("Year", side = 1, line = 3, cex = 2)

lines(x, df[[3]], col = "darkgreen",   lwd = 2, lty = "dashed")
lines(x, df[[4]], col = "red",  lwd = 2, lty = "dotdash")
lines(x, df[[5]], col = "orange", lwd = 2, lty = "dotted")

x_point <- x[12]
y_point <- df[12, 2:5]

text(x_point, y_point[1], labels = circles[1], col = cols[1], cex = 1, pos = 1)
text(x_point, y_point[2], labels = circles[2], col = cols[2], cex = 1, pos = 3)
text(x_point, y_point[3], labels = circles[3], col = cols[3], cex = 1, pos = 1)
text(x_point, y_point[4], labels = circles[4], col = cols[4], cex = 1, pos = 3)

legend(
  "topleft",
  legend = c(
    "(1) Dem (positive news)",
    "(2) Dem (negative news)",
    "(3) Rep (positive news)",
    "(4) Rep (negative news)"
  ),
  text.col = cols,
  bty = "o",
  lty = NA,
  lwd = NA,
  pch = NA,
  col = NA,
  pt.cex = 0,
  x.intersp = 0,
  cex = 1.2
)
dev.off()
