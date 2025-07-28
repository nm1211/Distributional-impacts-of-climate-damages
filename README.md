# Files-for-Distributional-impacts-of-climate-damages

This file explains files used for our analysis.
# Data used and their sources
- gcd_raw: HH_consumption_GCD.csv, Household consumption from the Global Consumption Database, available in: https://datatopics.worldbank.org/consumption/detail
- price_vec: cons_consumptionpatterns_increases_in_prices_through_system_world_market.xlsx, Price changes for GTAP sectors
- gcd_mapper: Appendix_mapping_GCD_GTAP.csv, Mapper to match GTAP sectors to GCD data
- CP_elast_ low and CP_elast middle: Cross price elasticity based on Cornelsen et al. (2015), available in https://pubmed.ncbi.nlm.nih.gov/25236930/
- GDP_country: GDP per capita in PPP for the year 2019 from WDI, available in https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.CD
- gini_coefficient: Gini coefficient from the WDI (latest estimate for each country), available in: https://data.worldbank.org/indicator/SI.POV.GINI
- latitude: Latitude from the CSGNetwork (2021), available in: http://www.csgnetwork.com/llinfotable.html
# File(s) for analysis:
Main.R: main r script includes:
- Data cleaning, structuring and merge.
- Computation of Consumer surplus contribution and Consumption elasticity
- Analysis: correlations, regressions and plots
# Maps:
mapped using QGIS based on CS_change_exp.csv
