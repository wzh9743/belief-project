df <- read.csv("coe_div.csv", stringsAsFactors = FALSE)

x <- df[[1]]

cols <- c("orange", "darkgreen", "blue")
circles <- c("(1)", "(2)", "(3)")

setEPS()
postscript("coe_div.eps", width=10, height=7)

plot(
  x, df[[2]],
  type = "l", lwd = 2, col = "orange", lty = "solid",
  ylim =c(40, 85),
  xlab = "", ylab = "",
  main = ""
)

lines(x, df[[3]], col = "darkgreen",   lwd = 2, lty = "dashed")
lines(x, df[[4]], col = "blue",  lwd = 2, lty = "dotdash")

x_point_1 <- x[6]
y_point_1 <- df[6, 2]

x_point_2 <- x[15]
y_point_2 <- df[15, 3]

x_point_3 <- x[27]
y_point_3 <- df[27, 4]

text(x_point_1, y_point_1, labels = circles[1], col = cols[1], cex = 1, pos = 1)
text(x_point_2, y_point_2, labels = circles[2], col = cols[2], cex = 1, pos = 1)
text(x_point_3, y_point_3, labels = circles[3], col = cols[3], cex = 1, pos = 4)

legend(
  "topleft",
  legend = c(
    "(1) political color",
    "(2) education level",
    "(3) education diversity"
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
