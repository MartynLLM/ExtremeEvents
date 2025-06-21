# ExtremeEvents
R code for extreme precipitation simulations

The "stretch" code implements the precipitation stretch algorithm described in [Laguna Marin (2022)](https://stud.epsilon.slu.se/18471/3/Laguna-Marin-c-20230119.pdf)

The drought code implements the algorithms presented in Kuehn et al. in prep.

All calculations work on daily precipitation values. If the input time series is at a sub-daily resolution, it can be aggregated to daily values using createDailyPrecip.r

Once a daily precipitation time series is available, it can be stretched using stretch_mass_balance.r

Finally, daily stretch values can be applied to sub-daily using StretchSubDailyPrecipitation.r


