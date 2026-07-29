# baseline model
# observed opinion information
library(dplyr)
library(stargazer)
library(car)

df3 <- read.csv(
  "ob_div.csv",
  check.names = TRUE
)
df3$year <- NULL
print(names(df3))

cat(
  "Number of observed opinion years:",
  nrow(df3),
  "\n"
)

ob_model2 <- lm(
  opinion_mon.1 ~ . -racediv -politicaldiv -popden
  -agediv -housemedianprice -medianinc
  -1, data = df3
)

summary(ob_model2)

stargazer(
  ob_model2,
  type = "latex",
  title = "Regression using observed opinion",
  dep.var.labels = "Opinion",
  digits = 3,
  add.lines = list(
    c("Opinion data", "Actually observed"),
    c("Observations", nobs(ob_model2))
  ),
  notes = paste(
    "The regression only uses actually observed opinion."
  )
)

#standardized coefficient
model_clean <- lm(
  opinion_mon.1 ~ . -racediv -politicaldiv -popden
  -agediv -housemedianprice -medianinc -constant,
  data = df3
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

# Newey-west specification
library(sandwich)
library(lmtest)
df4 <- read.csv("aggregate_time_series_div.csv")
df4$Month <- NULL
model <- lm(opinion_mon.1 ~ . -racediv -politicaldiv -popden
             -agediv -housemedianprice -medianinc
            -1, data = df4)
# Monthly data: allow serial correlation up to 12 months
nw12_vcov <- NeweyWest(
  model,
  lag = 12,
  prewhite = FALSE,
  adjust = TRUE
)

nw24_vcov <- NeweyWest(
  model,
  lag = 24,
  prewhite = FALSE,
  adjust = TRUE
)

nw36_vcov <- NeweyWest(
  model,
  lag = 36,
  prewhite = FALSE,
  adjust = TRUE
)
# OLS coefficients with Newey-West standard errors
nw12_result <- coeftest(model, vcov. = nw12_vcov)
nw24_result <- coeftest(model, vcov. = nw24_vcov)
nw36_result <- coeftest(model, vcov. = nw36_vcov)
print(nw12_result)
print(nw24_result)
print(nw36_result)
# Extract Newey-West standard errors and p-values
nw12_se_div <- sqrt(diag(nw12_vcov))
nw24_se_div <- sqrt(diag(nw24_vcov))
nw36_se_div <- sqrt(diag(nw36_vcov))
nw12_p_div <- nw12_result[,4]
nw24_p_div <- nw24_result[,4]
nw36_p_div <- nw36_result[,4]

names(coef(ob_model2))
names(coef(model))

stargazer(
  model,
  model,
  model,
  type="latex",
  column.labels=c(
    "NW(12)",
    "NW(24)",
    "NW(36)"
  ),
  se=list(
    nw12_se_div,
    nw24_se_div,
    nw36_se_div
  ),
  p=list(
    nw12_p_div,
    nw24_p_div,
    nw36_p_div
  ),
  digits=2
)