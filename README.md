# Time series analysis: Hyndman's wineind dataset

**Documentation and analysis in progress**

Dataset available [in the forecast package for R](https://cran.r-project.org/web/packages/forecast/forecast.pdf).
Analysis follows methods and examples from Rob J Hyndman and George Athanasopoulos, [Forecasting: Principles and Practice](https://otexts.org/fpp2/); Robert H. Shumway and David S. Stoffer, [Time Series Analysis and Its Applications](https://www.stat.pitt.edu/stoffer/tsa4/tsa4.pdf); Penn State, [Stat 510: Applied Time Series Analysis](https://onlinecourses.science.psu.edu/stat510/node/47/); and Optum DSU head Marc Paradis (scripts).

### Problem definition
The dataset tracks wine sales by bottle (one liter or larger) in Australia, January 1980-August 1994. Potential forecasting use cases include projections to inform wine producers determining how many acres to put under vines, or wine distributors or retailers planning purchases. Forecasts will estimate numbers of bottles sold in the near future, relative to the dataset.

### Exploratory analysis
There are 176 rows in the dataset, indicating that the data represent monthly totals. Initial plots show a strong seasonal component, and an increasing trend over the period covered. At a glance, it looks like a cyclic behavior causing a slight increase and decrease over each 10-year period may be present, but there's not enough data to be sure. Seasonal plots show sales increasing over the year, with a peak in the summer (vacation time), a slight dip in early fall (beginning of the school year), and the highest sales in December (religious holidays and New Year). Sales seem to be consistently low in January, possibly reflecting New Year's resolutions to lose weight, drink less alcohol, or adopt generally healthier habits. Lag plots reinforce this, with a strong positive relationship at lag 12. The max for the time series, 40,226 bottles, occurs in December. There are no obvious outliers.

### Checking assumptions
The correlogram shows an almost perfect pattern of significant auto-correlation at multiples of six and 12, with larger spikes at 12-month intervals. This is consistent with the highest annual sales observed in December, and increases in the summer. The upward trend and obvious seasonal pattern violate the assumptions for weak stationarity.
