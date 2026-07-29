library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(readr)

#Dem vs Rep, baseline
Ainv <- read.csv("Ainv.csv", stringsAsFactors = FALSE)
fips <- Ainv[[1]]
Ainv_mat <- as.matrix(Ainv[,-1])
storage.mode(Ainv_mat) <- "double"

metrics <- c("constant_mon.csv",
             "news_county_mean_ob_imp_mon.csv",
             "news_county_num_mon.csv",
             "voteturn_mon.csv",
             "unem_mon.csv",
             "edudiv_mon.csv",
             "edulevel_mon.csv",
             "pc_mon.csv")
for (f in metrics) {
  df <- read.csv(f, stringsAsFactors = FALSE)
  df$Month <- format(as.Date(df$Month), "%Y/%m/%d")
  write.csv(df, f, row.names = FALSE)
}

beta_1 <- -65.3; beta_2 <- 1.59; beta_3 <- -0.19; beta_4 <- 0.23
beta_5 <- -0.79; beta_6 <- 149.9; beta_7 <- 0.36; beta_8 <- -19.62
beta <- c(beta_1, beta_2, beta_3, beta_4, beta_5, beta_6, beta_7, beta_8)

read_metric_matrix <- function(file) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  df[[1]] <- format(as.Date(df[[1]], format = "%Y/%m/%d"), "%Y/%m/%d")
  months <- df[[1]]
  X <- as.matrix(df[,-1])
  storage.mode(X) <- "double"
  list(months = months, X = X)
}

processed <- lapply(metrics, function(f) {
  dat <- read_metric_matrix(f)
  M <- t(Ainv_mat %*% t(dat$X))
  list(months = dat$months, M = M)
})

stopifnot(all(sapply(processed, function(x) identical(processed[[1]]$months, x$months))))
months_all <- processed[[1]]$months
T_len <- length(months_all)

arr <- simplify2array(lapply(processed, `[[`, "M"))
opinion_mat <- apply(arr, c(1,2), function(...) sum(beta * c(...)))

opinion_df <- data.frame(
  Month = rep(months_all, each = ncol(opinion_mat)),
  FIPS  = rep(fips, times = T_len),
  opinion = as.vector(t(opinion_mat))
)

opinion_df <- opinion_df %>%
  mutate(Month = ymd(Month) %m+% months(1)) %>%
  filter(Month <= ymd("2024-12-01")) %>%
  mutate(Month = format(Month, "%Y/%m/%d"))

#extract one specific county
target_fips <- "49049"
df_one <- opinion_df %>%
  filter(FIPS == target_fips) %>%
  mutate(Month = as.Date(Month, format = "%Y/%m/%d")) %>%
  arrange(Month)
ggplot(df_one, aes(x = Month, y = opinion)) +
  geom_line() +
  labs(title = NULL,
       x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

write.csv(opinion_df, "county_opinion_all_months.csv", row.names = FALSE)

if(FALSE){
  #check outliers
  subset(opinion_df, opinion >= 100 | opinion <= 0)
  sum(opinion_df$opinion >= 100 | opinion_df$opinion <= 0, na.rm = TRUE)
}

#group by party
pc_long <- read_csv("pc_county_mon.csv") %>%
  pivot_longer(cols = -Month, names_to = "FIPS", values_to = "pc") %>%
  mutate(FIPS = as.character(FIPS),
         Month = ymd(Month)) %>%
  filter(Month >= ymd("1990-02-01")) %>%
  mutate(Month = format(Month, "%Y/%m/%d"))

opinion_df <- read_csv("county_opinion_all_months.csv") %>%
  mutate(FIPS = as.character(FIPS),
         Month = format(as.Date(Month, "%Y/%m/%d"), "%Y/%m/%d"))

merged_df <- left_join(opinion_df, pc_long, by = c("FIPS", "Month"))

merged_df <- merged_df %>%
  mutate(party = ifelse(pc < 0, "Dem", "Rep"))

median_opinion_by_party <- merged_df %>%
  group_by(Month, party) %>%
  summarise(
    q25 = quantile(opinion, 0.25, na.rm = TRUE),
    median_opinion = median(opinion, na.rm = TRUE),
    q75 = quantile(opinion, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

median_opinion_by_party %>%
  mutate(Month = as.Date(Month, "%Y/%m/%d")) %>%
  ggplot(aes(x = Month, color = party, fill = party)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.15, color = NA) +
  geom_line(aes(y = median_opinion, linetype = party), linewidth = 1) +
  scale_color_manual(values = c("Dem" = "blue", "Rep" = "red")) +
  scale_fill_manual(values = c("Dem" = "blue", "Rep" = "red")) +
  scale_linetype_manual(values = c("Dem" = "solid", "Rep" = "solid")) +
  labs(title = NULL, x = "Year", y = "%", color = NULL, fill = NULL) +
  theme_linedraw(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_text(
      family = "Helvetica",
      size = 11,
      angle = 0,
      hjust = 0.5,
      vjust = 1,
      margin = margin(r = -15)
    ),
    axis.text.x = element_text(
      family = "Helvetica",
      size = 11
    ),
    axis.text.y = element_text(
      family = "Helvetica",
      size = 11
    ),
    axis.title.x = element_text(
      family = "Helvetica",
      size = 11 * 1.2
    ),
    legend.position = "none"
  ) +
  annotate(
    "text",
    x = median(as.Date(median_opinion_by_party$Month)),
    y = 65,
    label = "Dem",
    color = "blue",
    size = 6
  ) +
  annotate(
    "text",
    x = median(as.Date(median_opinion_by_party$Month)),
    y = 42,
    label = "Rep",
    color = "red",
    size = 6
  )

#value of lines
if(FALSE){
  median_opinion_by_party %>%
    filter(party == "Dem") %>%
    summarise(
      min_value = min(median_opinion, na.rm = TRUE),
      max_value = max(median_opinion, na.rm = TRUE)
    )
}

#figure comparison, after generating two opinion files
median_by_party <- function(opinion_csv, scenario_label) {
  read_csv(opinion_csv) %>%
    mutate(
      FIPS  = as.character(FIPS),
      Month = format(as.Date(Month, "%Y/%m/%d"), "%Y/%m/%d")
    ) %>%
    left_join(pc_long, by = c("FIPS","Month")) %>%
    mutate(party = ifelse(pc < 0, "Dem", "Rep")) %>%
    group_by(Month, party) %>%
    summarise(median_opinion = median(opinion, na.rm = TRUE), .groups = "drop") %>%
    mutate(scenario = scenario_label)
}

median_base <- median_by_party("county_opinion_all_months.csv",    "Baseline")
median_new   <- median_by_party("county_opinion_all_months_unem-25quar.csv", "New strategy")

median_both <- bind_rows(median_base, median_new) %>%
  mutate(Month = as.Date(Month, "%Y/%m/%d"),
         group = paste(party, scenario, sep = "_"))

custom_colors <- c(
  "Dem_Baseline"       = "blue",
  "Rep_Baseline"       = "red",
  "Dem_New strategy"   = "skyblue",
  "Rep_New strategy"   = "pink"
)

label_pos <- median_both %>%
  filter(scenario == "Baseline") %>%
  group_by(party) %>%
  summarise(
    x = median(Month),
    y = median(median_opinion),
    .groups = "drop"
  ) %>%
  mutate(y = y + 7)

ggplot(
  median_both,
  aes(Month, median_opinion,
      color = group, linetype = group)
) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = c(
    "Dem_Baseline"       = "solid",
    "Rep_Baseline"       = "solid",
    "Dem_New strategy"   = "dotdash",
    "Rep_New strategy"   = "dotdash"
  )) +
  labs(
    x = "Year",
    y = "%"
  ) +
  theme_linedraw(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_text(
      family = "Helvetica",
      size = 11 * 1.5,
      angle = 0,
      hjust = 0.5,
      vjust = 1,
      margin = margin(r = -20)
    ),
    axis.text.x = element_text(
      family = "Helvetica",
      size = 11 * 1.5
    ),
    axis.text.y = element_text(
      family = "Helvetica",
      size = 11 * 1.5
    ),
    axis.title.x = element_text(
      family = "Helvetica",
      size = 11 * 2
    ),
    legend.position = "none"
  ) +
  
  geom_text(
    data = label_pos,
    aes(x = x, y = y, label = party),
    color = c("Dem" = "blue", "Rep" = "red")[label_pos$party],
    size = 11 * 0.8,
    inherit.aes = FALSE
  )

#mean difference
if(FALSE){
  diff_by_month_party <- median_both %>%
    select(Month, party, scenario, median_opinion) %>%
    pivot_wider(names_from = scenario, values_from = median_opinion) %>%
    mutate(
      diff     = `New strategy` - Baseline
    )
  
  mean_per_party_overall <- diff_by_month_party %>%
    group_by(party) %>%
    summarise(
      mean_diff     = mean(diff, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(mean_per_party_overall)
}