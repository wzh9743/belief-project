library(dplyr)
library(stargazer)
library(car)

df <- read.csv("aggregate_time_series.csv")
df$Month <- NULL
df <- df %>%
  mutate(
    edulevel_c = scale(edulevel, center = TRUE, scale = FALSE),
    pc_c       = scale(pc, center = TRUE, scale = FALSE)
  )

#correlation matrix
correlation <- df %>%
  select(-opinion_mon.1, -constant, -edulevel_c, - pc_c)

cor_matrix <- cor(correlation, use = "pairwise.complete.obs")
print(cor_matrix)
cor_df <- as.data.frame(round(cor_matrix, 2))
cor_df$Variable <- rownames(cor_df)
stargazer(cor_df, summary = FALSE, rownames = FALSE,
          title = "Correlation Matrix", digits = 2)

#model estimation
#model 7 (baseline)
model <- lm(opinion_mon.1 ~ . -racediv -politicaldiv -popden
            -agediv -edulevel_c -pc_c -housemedianprice -medianinc
            -1, data = df)
summary(model)
logLik(model)
stargazer(model,
          type = "latex",
          title = "OLS regression of opinion",
          dep.var.labels = "opinion",
          digits = 2)

#VIF check for model 7 (baseline)
model_vif <- lm(opinion_mon.1 ~ .  -racediv -politicaldiv -popden
                -agediv -edulevel_c -pc_c -housemedianprice -medianinc
                -constant, data = df)
vif_values <- vif(model_vif)
print(vif_values)
vif_df <- data.frame(
  Variable = names(vif_values),
  VIF = as.numeric(vif_values)
)
stargazer(vif_df, summary = FALSE, rownames = FALSE,
          title = "VIF",
          label = "")