# belief-project

## Introduction
This repository includes cleaned data [^1], data documentation, code, and online appendix for the paper:
**News exposure, local diversity, and U.S. public opinion on climate change: A model-based analysis using county-level covariates**.
All the estimation results, sensitivity tests, policy effects, robustness checks, and figures can be replicated following this instruction.
[^1]: Due to the file size limits on GitHub, we are not able to present raw data and the process we compile raw data here.

## estimation
### 1. diversity
In this subfolder, there is a data file and a code file, namely `aggregate_time_series.csv` and `ols analysis.R`, respectively. The data file contains aggregated monthly national data from January 1990 to December 2024, covering one-period lag public opinion, current diversity indices (race, age, education, political), number of news, average objective importance of news, as well as political, demographic, and economic characteristics. The code file lists seven potential models, where the seventh model is the baseline model, corresponding to model (5) in Table 3 in the main text.
### 2. social pressure
Similarly, there is a data file named `aggregate_time_series.csv` and a code file named `ols analysis.R` in this subfolder. However, we introduce low-diversity index (interpreted cautiously as a proxy for social pressure) to replace those four diversity indicies. And the results obtained by the code file corresponds to model (6) in Table 3.

## sensitivity
We conduct a series of sensitivity tests by deviating from the baseline model in one direction each time. All these tests change the number of news and average objective importance of news that flow to the public. Thus, the data files in the subfolders share the same file name, which is `aggregate_time_series.csv`, but they differ in the values of news-related variables. On the other hand, the code files `ols analysis.R` are exactly the same.

## policy
We experiment with a series of policies in the code file `policy_effects.R`. By replacing the code "county_opinion_all_months_unem-25quar.csv" in line 189 with "county_opinion_all_months_0.9posrel.csv", "county_opinion_all_months_edulevel+25quar_edudiv-5quar.csv", "county_opinion_all_months_edulevel+25quar_edudiv-10quar.csv", "county_opinion_all_months_edulevel+25quar.csv", "county_opinion_all_months_low_thr.csv", "county_opinion_all_months_only_dem.csv", "county_opinion_all_months_only_rep.csv", "county_opinion_all_months_op.csv", and "county_opinion_all_months_polar.csv", all policy effects can be visualized. [^2]
[^2]: Note that the CSV file `Ainv.csv` used in the code is too large to upload to the online repository. We therefore provide it as a compressed ZIP file, `Ainv.csv.zip`. Before running the code, please unzip `Ainv.csv.zip` to obtain `Ainv.csv`.

## robustness
The results of the robustness checks reported in Table 5 can be replicated using `rob.R`.
