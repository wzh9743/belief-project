import pandas as pd
import matplotlib.pyplot as plot
import matplotlib as mpl

mpl.rcParams['font.family'] = 'Helvetica'

df = pd.read_csv("disaster_information_cost_after1989.csv")

df["End Date"] = pd.to_datetime(df["End Date"], format="mixed", errors="coerce")

df_filtered = df[df["End Date"].dt.year.between(1990, 2024)]

df_filtered = df_filtered.assign(
    Year=df_filtered["End Date"].dt.year,
    Month=df_filtered["End Date"].dt.month
)

years = list(range(1990, 2025))
months = list(range(1, 13))
index = pd.MultiIndex.from_product([years, months], names=["Year", "Month"])

monthly_sum = (
    df_filtered.groupby(["Year", "Month"])
    .agg(
        Disaster_Number=("End Date", "count"),
        Total_Cost=("CPI-Adjusted Cost", "sum")
    )
    .reindex(index, fill_value=0)
    .reset_index()
)

monthly_sum.to_csv("monthly_disaster_cost,num.csv", index=False, float_format="%.1f")

print(monthly_sum.head(15))

import matplotlib.pyplot as plot

yearly_sum = (
    df_filtered.groupby("Year")
    .agg(
        Disaster_Number=("End Date", "count"),
        Total_Cost=("CPI-Adjusted Cost", "sum")
    )
    .reset_index()
)

fig, ax1 = plot.subplots(figsize=(10, 7))

ax1.set_xlabel("Year", fontsize = 12)
ax1.set_ylabel("Number of Disasters", color="black")
line1, = ax1.plot(yearly_sum["Year"], yearly_sum["Disaster_Number"],
         label="Disaster Number", color="black", linestyle="-")
ax1.tick_params(axis='y', labelcolor="black")

ax2 = ax1.twinx()
ax2.set_ylabel("CPI-Adjusted Cost ($)", color="black")
line2, = ax2.plot(yearly_sum["Year"], yearly_sum["Total_Cost"],
         label="CPI-Adjusted Cost ($)", color="black", linestyle="--")
ax2.tick_params(axis='y', labelcolor="black")

lines = [line1, line2]
labels = [line.get_label() for line in lines]
plot.legend(lines, labels, loc="upper left", fontsize=14.4)

fig.tight_layout()
ax1.grid(False)
plot.savefig("disaster_number_cost_yearly.eps", format='eps')
plot.show()