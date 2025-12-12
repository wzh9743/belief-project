library(ggplot2)
library(lubridate)
dat <- read.csv("aggregate_time_series.csv", header = T, stringsAsFactors = F)
dat$Month <- as.Date(dat$Month, format = "%Y/%m/%d")

beta_0 <- 119.4
beta_1 <- 1.40; se_beta_1 <- 0.55
beta_2 <- -0.14; se_beta_2 <- 0.16
beta_3 <- 0.27; se_beta_3 <- 0.04
beta_4 <- -0.90; se_beta_4 <- 0.05
beta_5 <- -282.2; se_beta_5 <- 37.1
beta_6 <- 0.02; se_beta_6 <- 0.04
beta_7 <- -15.57; se_beta_7 <- 2.89

#first quartile, third quartile
beta_1_q1 <- beta_1 - 0.6745 * se_beta_1
beta_1_m <- beta_1
beta_1_q3 <- beta_1 + 0.6745 * se_beta_1

dat$opinion_mon.1_q1 <- beta_0 * dat$constant + beta_1_q1 * dat$news_county_mean_ob_imp +
  beta_2 * dat$news_county_num + beta_3 * dat$voteturn +
  beta_4 * dat$unem + beta_5 * dat$socialpressure +
  beta_6 * dat$edulevel + beta_7 * dat$pc

dat$opinion_mon.1_m <- beta_0 * dat$constant + beta_1_m * dat$news_county_mean_ob_imp +
  beta_2 * dat$news_county_num + beta_3 * dat$voteturn +
  beta_4 * dat$unem + beta_5 * dat$socialpressure +
  beta_6 * dat$edulevel + beta_7 * dat$pc

dat$opinion_mon.1_q3 <- beta_0 * dat$constant + beta_1_q3 * dat$news_county_mean_ob_imp +
  beta_2 * dat$news_county_num + beta_3 * dat$voteturn +
  beta_4 * dat$unem + beta_5 * dat$socialpressure +
  beta_6 * dat$edulevel + beta_7 * dat$pc

dat$Month.1 <- dat$Month %m+% months(1)

#autocorrelation
cor(dat$opinion_mon.1_m[-1], dat$opinion_mon.1_m[-nrow(dat)])

ggplot(dat, aes(x = Month.1)) +
geom_line(aes(y = opinion_mon.1, color = "Observed"), linetype = "solid", linewidth = 1) +
geom_line(aes(y = opinion_mon.1_m, color = "Median"), linetype = "dotdash", linewidth = 1) +
geom_ribbon(aes(ymin = opinion_mon.1_q1, ymax = opinion_mon.1_q3),
              fill = "pink", alpha = 0.2) +
  labs(
    x = "Year",
    y = NULL,
    color = NULL,
    title = NULL
  ) +
  scale_color_manual(
    values = c(
      "Observed" = "black",
      "Median" = "red"
    )
  )+
  coord_cartesian(ylim = c(30, 90)) +
  theme_linedraw(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    legend.text = element_text(family = "Helvetica", size = 11 * 1.5),
    axis.title.x = element_text(family = "Helvetica", size = 11 * 2),
    legend.position = c(0.01, 0.99),
    legend.justification = c(0, 1),
    legend.background = element_rect(
      color = "black", fill = "white",
      linewidth = 0.4),
    plot.title = element_text(hjust = 0.5)
  )