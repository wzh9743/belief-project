df <- read.csv("gallup_survey.csv", stringsAsFactors = FALSE)

str(df)

df$year <- as.numeric(df$year)

ylim_all <- range(as.matrix(df[ , -1]), na.rm = TRUE)

setEPS()
postscript("gallup_survey.eps", width=10, height=7)

plot(
  df$year, df$Galluphappen,
  type = "n",
  ylim = ylim_all,
  xlab = "", ylab = ""
)
mtext("Year", side = 1, line = 3, cex = 1)

add_line <- function(x, y, col, lwd = 2, lty = "solid") {
  ok <- !is.na(y)
  if (any(ok)) {
    lines(x[ok], y[ok], col = col, lwd = lwd, lty = lty)
  }
}

add_line(df$year, df$Galluphappen, col = "blue", lwd = 2, lty = "solid")
add_line(df$year, df$Gallupcause,   col = "orange",    lwd = 2, lty = "dashed")
add_line(df$year, df$Gallupworry,   col = "darkgreen",     lwd = 2, lty = "dotdash")
add_line(df$year, df$Gallupharmper, col = "red", lwd = 2, lty = "dotted")

cols    <- c("blue", "orange", "darkgreen", "red")
circles <- c("(1)", "(2)", "(3)", "(4)")
vars    <- c("Galluphappen","Gallupcause","Gallupworry","Gallupharmper")

for (i in 1:4) {
  y <- df[[vars[i]]]
  ok <- which(!is.na(y))
  first_idx <- ok[1]
  
  text(df$year[first_idx] + 0.3,
       y[first_idx],
       labels = circles[i],
       col = cols[i],
       cex = 1,
       pos = 2)
}

legend(
  "bottomright",
  legend = c("(1) happen", "(2) anthropogenic", "(3) worry", "(4) threat"),
  text.col = cols,
  bty = "o",
  lty = NA,
  lwd = NA,
  pch = NA,
  col = cols,
  cex = 1.2
)
dev.off()