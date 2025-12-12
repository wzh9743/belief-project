library(dplyr)
library(stargazer)
library(car)

df <- read.csv("aggregate_time_series.csv")
df$Month <- NULL

#model estimation
model <- lm(opinion_mon.1 ~ . -popden -housemedianprice -medianinc
            -1, data = df)
summary(model)
logLik(model)
stargazer(model,
          type = "latex",
          title = "OLS regression of opinion",
          dep.var.labels = "opinion",
          digits = 2)

#VIF check
model_vif <- lm(opinion_mon.1 ~ . -popden -housemedianprice -medianinc
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