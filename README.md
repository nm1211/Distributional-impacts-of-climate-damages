# Files-for-Distributional-impacts-of-climate-damages

This file explains files used for our analysis.
# Data used and their sources
## Main Analysis
- gcd_raw: household consumption from the Global Consumption Database (GCD), available in: https://datatopics.worldbank.org/consumption/detail
- gcd_mapper: Appendix_mapping_GCD_GTAP.csv, mapper to match GTAP sectors (GTAP version 10) to GCD data.
  The matching procedure is based on Dorband et al. (2019), who matched GTAP sectors (version 9) with GCD data. Available in: https://www.sciencedirect.com/science/article/pii/S0305750X18304212#s0130 
- per_capita_consumption: from the GCD, available in: https://datatopics.worldbank.org/consumption/detail 
- CP_elast_ low and CP_elast middle: Cross price elasticity based on Cornelsen et al. (2015), available in https://pubmed.ncbi.nlm.nih.gov/25236930/
- GDP_country: GDP per capita in PPP for the year 2019 from WDI, available in https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.CD
- gini_coefficient: Gini coefficient from the WDI (latest estimate for each country), available in: https://data.worldbank.org/indicator/SI.POV.GINI
- latitude: Latitude from the CSGNetwork (2021), available in: http://www.csgnetwork.com/llinfotable.html
- country codes: To merge data files from GCD countries to GTAP countries, country codes are used from Aguiar et al. (2019), Table A.3. Available in: https://lpvageojs02.it.purdue.edu/ojs/index.php/jgea/article/view/77/96

Data is available upon request.

## Senstivity Analysis
  - total_value_capture_per_sector_change_in_Mio_USD: total_value_capture_per_sector_change_in_Mio_USD.xlsx, Total value captured in million USD per each sector
  - own_price_elasticity: own price elasticity.xlsx, own prices elasticities based on Muhammad et al (2011), available in https://www.ers.usda.gov/publications/pub-details?pubid=47581 

# Files for Data:
## GTAP_food_price_increases_country_consumption_weighted_clean.py: Provides price changes for GTAP sector based MRIO 
- Python code to diffuse the price shocks throughout global supply chain system considering consumption relations/shares
-  Output file is used in main analysis (Main.R)
## Shock_demand_overall_for_production_for_C3_clean.py
- Python code for approximating the overall production impacts and global share shifts for C3, it includes:
- Output file is isued in the Sensitivity Analysis: Income Effects (SA_income effects.R)
  
# File(s) for analysis:
## Main.R: main r script includes:
- Data cleaning, structuring and merge.
- Computation of Consumer surplus contribution and Consumption elasticity
- Analysis: correlations, regressions and plots

## SA_own_price_elast.R: r script for sensitivity analysis using own price elasticities, it includes:
- Data cleaning, structuring and merge.
- Computation of Consumer surplus contribution based on compensation variation
- Analysis: correlations, regressions and plots

## SA_income effects.R: r script for sensitivity analysis for income effects, it includes:
- follows Main.R
- Computation of Income effects
  
# Maps:
mapped using QGIS based on CS_change_exp.csv
