library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(readr)

#figure comparison, after generating two opinion files
pc_long <- read_csv("pc_county_mon.csv") %>%
  pivot_longer(cols = -Month, names_to = "FIPS", values_to = "pc") %>%
  mutate(FIPS = as.character(FIPS),
         Month = ymd(Month)) %>%
  filter(Month >= ymd("1990-02-01")) %>%
  mutate(Month = format(Month, "%Y/%m/%d"))

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
median_new   <- median_by_party("county_opinion_all_months_edulevel+25quar.csv", "New strategy")

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
    x = "Solid lines refer to baseline cases, dotdash lines refer to policy effects",
    y = NULL
  ) +
  theme_linedraw(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.title.x = element_text(family = "Helvetica", size = 11*2)
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