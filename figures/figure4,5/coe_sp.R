df <- read.csv("coe_sp.csv", stringsAsFactors = FALSE)

x <- df[[1]]

cols <- c("purple", "red")
circles <- c("(1)", "(2)")

setEPS()
postscript("coe_sp.eps", width=10, height=7)

plot(
  x, df[[2]],
  type = "l", lwd = 2, col = "purple", lty = "dotted",
  ylim =c(40, 85),
  xlab = "", ylab = "",
  main = ""
)

lines(x, df[[3]], col = "red",   lwd = 2, lty = "twodash")

x_point_1 <- x[25]
y_point_1 <- df[25, 2]

x_point_2 <- x[18]
y_point_2 <- df[18, 3]

text(x_point_1, y_point_1, labels = circles[1], col = cols[1], cex = 1, pos = 1)
text(x_point_2, y_point_2, labels = circles[2], col = cols[2], cex = 1, pos = 2)

legend(
  "topleft",
  legend = c(
    "(1) news importance",
    "(2) social pressure"
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
