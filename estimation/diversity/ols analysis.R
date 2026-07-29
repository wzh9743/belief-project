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
#model 1
model <- lm(opinion_mon.1 ~ . -politicaldiv -popden
            -agediv -edulevel_c -pc_c -housemedianprice
            -edulevel -pc
            -1, data = df)
summary(model)
#model 2
model <- lm(opinion_mon.1 ~ . -politicaldiv -popden
            -racediv -edulevel_c -pc_c -housemedianprice
            -edulevel -pc
            -1, data = df)
summary(model)
#model 3
model <- lm(opinion_mon.1 ~ . -racediv -popden
            -agediv -edulevel_c -pc_c -housemedianprice
            -edulevel -pc
            -1, data = df)
summary(model)
#model 4
model <- lm(opinion_mon.1 ~ . -politicaldiv -racediv
            -agediv -edulevel_c -pc_c -housemedianprice
            -edulevel -pc
            -1, data = df)
summary(model)
#model 5
model <- lm(opinion_mon.1 ~ . -politicaldiv -popden
            -agediv -edulevel_c -pc_c -racediv
            -edulevel -pc
            -1, data = df)
summary(model)
#model 6
model <- lm(opinion_mon.1 ~ . -racediv -politicaldiv -popden
            -agediv -edulevel_c -pc_c -housemedianprice
            +edulevel_c:pc_c
            -1, data = df)
summary(model)
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
#standardized coefficients of model 7 (baseline)
model_clean <- lm(
  opinion_mon.1 ~ . -racediv -politicaldiv -popden -agediv -edulevel_c
  -pc_c -housemedianprice -medianinc -constant,
  data=df
)
summary(model_clean)

beta_raw <- coef(model_clean)
vars <- names(beta_raw)
vars <- vars[vars != "(Intercept)"]
mf <- model.frame(model_clean)
y <- model.response(mf)
X <- model.matrix(model_clean)
sd_y <- sd(y)
scale_factor <- sapply(
  vars,
  function(v){
    sd(X[, v]) / sd_y
  }
)
std_coef <- beta_raw[vars] * scale_factor
ci_raw <- confint(
  model_clean,
  level = 0.95
)
ci_raw <- ci_raw[vars, ]
ci_std <- ci_raw
ci_std[,1] <- ci_raw[,1] * scale_factor
ci_std[,2] <- ci_raw[,2] * scale_factor
std_table <- data.frame(
  Variable = vars,
  Standardized_Coefficient = as.numeric(std_coef),
  CI_lower = as.numeric(ci_std[,1]),
  CI_upper = as.numeric(ci_std[,2])
)
print(std_table)

stargazer(
  std_table,
  type = "latex",
  summary = FALSE,
  rownames = FALSE,
  digits = 2,
  title = "Comparable effect sizes"
)

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