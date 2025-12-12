# belief-project

## Introduction
This repository includes cleaned data [^1], data documentation, code, and online appendix for the paper:
**The role of news and social pressure in shaping U.S. public opinion on climate change**.
All the estimation results, sensitivity tests, policy effects, and figures in the main text can be replicated following this instruction.
[^1]: Due to the file size limits on GitHub, we are not able to present raw data and the process we compile raw data here.

## estimation
### 1. diversity
In this subfolder, there is a data file and a code file, namely `aggregate_time_series.csv` and `ols analysis.R`, respectively. The data file contains aggregated monthly national data from January 1990 to December 2024, covering one-period lag public opinion, current diversity indices (race, age, education, political), number of news, average objective importance of news, as well as political, demographic, and economic characteristics. The code file lists seven potential models, where the seventh model is the baseline model, corresponding to model (5) in Table 3 in the main text.
### 2. social pressure
Similarly, there is a national-level monthly data file named `aggregate_time_series.csv` and a code file named `ols analysis.R` in this subfolder. However, we introduce social pressure to replace those four diversity indicies. And the results obtained by the code file corresponds to model (6) in Table 3.

## sensitivity
We conduct a series of sensitivity tests by deviating from the baseline model in one direction each time. All these tests change the number of news and average objective importance of news that flow to the public. Thus, the data files in the subfolders share the same file name, which is `aggregate_time_series.csv`, but they differ in the values of news-related variables. On the other hand, the code files `ols analysis.R` are exactly the same. The subfolder `sensitivity/gamma_0` corresponds to Table 4, `sensitivity/national thresholds` corresponds to Table 5, while `sensitivity/positivity` and `sensitivity/reliability` correspond to Table 6.

## figures
- `figures/figure1,2/political color experiment.R`: political color of national channels, as well as their political distance
- `figures/figure3/sub_imp_pb.R` & `figures/figure3/sub_imp_year.R`: subjective importance by power balance and year
- `figures/figure4,5/cov_div.R` & `figures/figure4,5/cov_sp.R`: dependence of public opinion on selected variables
- `figures/figure6/coe_quartile_sim.R`: predicted public opinion with interquartile range
- `figures/figure7/all_months_county_opinion.R`: county-level median opinion by party affiliation
- `figures/figure8,9/policy_effects.R`: effects of policy instruments (replace the code `county_opinion_all_months_edulevel+25quar.csv` in line 29 by `county_opinion_all_months_edulevel+25quar_edudiv-10quar.csv`, `county_opinion_all_months_only_dem.csv`, or `county_opinion_all_months_only_rep.csv` to examine the effects of different policies)
- `figures/figure10/disaster_summary_plot.py`: number and losses of the U.S. billion-dollar disasters since 1990
- `figures/figure11/news_cnn,fox.R`: monthly news coverage of climate change on CNN and FOX
- `figures/figure12/monthly_total_intensity.R`: monthly climate-related events intensity
- `figures/figure13/gallup_survey.R`: trends in public opinion on climate change in the U.S.
