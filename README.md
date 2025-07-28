# Files-for-Distributional-impacts-of-climate-damages

This file explains files used for our analysis.
# Data used and their sources
## Main Analysis
- gcd_raw: HH_consumption_GCD.csv, household consumption from the Global Consumption Database, available in: https://datatopics.worldbank.org/consumption/detail
- price_vec: cons_consumptionpatterns_increases_in_prices_through_system_world_market.xlsx, price changes for GTAP sectors based on MRIO
- gcd_mapper: Appendix_mapping_GCD_GTAP.csv, mapper to match GTAP sectors to GCD data
- per_capita_consumption: per_capita_consumption.csv
- CP_elast_ low and CP_elast middle: Cross price elasticity based on Cornelsen et al. (2015), available in https://pubmed.ncbi.nlm.nih.gov/25236930/
- GDP_country: GDP per capita in PPP for the year 2019 from WDI, available in https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.CD
- gini_coefficient: Gini coefficient from the WDI (latest estimate for each country), available in: https://data.worldbank.org/indicator/SI.POV.GINI
- latitude: Latitude from the CSGNetwork (2021), available in: http://www.csgnetwork.com/llinfotable.html
## Senstivity Analysis
  - total_value_capture_per_sector_change_in_Mio_USD: total_value_capture_per_sector_change_in_Mio_USD.xlsx, Total value captured in million USD per each sector
  - own_price_elasticity: own price elasticity.xlsx, own prices elasticities based on Muhammad et al (2011), available in https://www.ers.usda.gov/publications/pub-details?pubid=47581 

# File(s) for analysis:
Main.R: main r script includes:
- Data cleaning, structuring and merge.
- Computation of Consumer surplus contribution and Consumption elasticity
- Analysis: correlations, regressions and plots
SA_income effects.R
- follows Main.R
- Computation of Income effects 
# Maps:
mapped using QGIS based on CS_change_exp.csv
