library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(readr)
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
  labs(title = NULL, x = "Year", y = NULL, color = NULL, fill = NULL) +
  theme_linedraw(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.title.x = element_text(family = "Helvetica", size = 11 * 1.2)
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