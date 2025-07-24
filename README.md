# Rscripts-for-Distributional-impacts-of-climate-damages

This file explains steps and files used for our analysis.
# Data used and their sources
- Household consumption from the Global Consumption Database
- Price changes for GTAP sectors
- GCD mapper: to match GTAP sectors to GCD data
- Cross price elasticity based on Cornelsen et al. 
- Per capita consumption
- GDP per capita in PPP for the year 2019
- Gini coefficient
- Latitude 
- World Bank classification for countries income levels to match countries to cross-price elasticities
# Data preparation and restructuring
- Transpose the cross-price elasticties data such that we have cross-price elasticities in terms of consumption increase
- Map cross-price elasticites products (goods) categories to that of the GCD to be able to match them
- Set the diagonal of the cross price elasticity matrix to be own-price elasticity
 - Merge datasets
- Create 3 separate datasets on consumption, prices and elasticities for the computation of consumer surplus
    
# Computation of Consumer surplus contribution with cross-price elasticities
For this we need 3 data files: consumption, price increase of goods, and elasticities. 
The three files have to follow the following structure and all countries and goods need to match
## Consumption file needs to be have the following columns: Country, Good, lowest,	low,	medium,	high
 First column (Country) identifies country name (Malaysia, Burkina Faso, ...)
 Second column (Good) identifies expenditure category (rice, vegetables, ...)
 Third to sixth column (lowest, low, medium, high) contain expenditure of the respective country on respective good by respective consumption group
## Prices file: has the following columns: Country, Good, Price
 First column (Country) identifies country name (Malaysia, Burkina Faso, ...)
 Second column (Good) identifies expenditure category (rice, vegetables, ...)
Third column is the price of the respective country and good. This has to be relative so that every value here is >=1 
## Elasticity file: elasticity by country and good X good combination this file needs to have the following columns: Country, Good, N Goods
 First column: (Country) identifies country name (Malaysia, Burkina Faso, ...)
 Second column: (Good) identifies expenditure category (rice, vegetables, ...) whose DEMAND increases as response to price increase from the thrid to 3+N column
 Third to 3+N columns: (Goods) reports own- and cross-price elasticities, column name identifies expenditure category (rice, vegetables, ...) whose price increases 
 A value epsilon in the ith row and j column means that the demand of the good in column two  and row i increases by epsilon percent as a response to a 1% price increase of the good in the jth column 
 Own price elasticities are given when good in the second column and column-name in the 3+N columns match
## Output is CSfinal, a dataframe this file has the following columns: 
Country, Good, Group, Cons, CSfactor, CScontribution
First column (Country) identifies country name (Malaysia, Burkina Faso, ...)
Second column (Good) identifies expenditure category (rice, vegetables, ...) that is consumed in the country
Third column (Group) identifies the consumption group in the respective country
Fourth column (Cons) is expenditure value (in Dollar) of the respective group in the respective country on the respective group 
Fifth column is the factor in the formula for the consumer surplus contribution of that expenditure item in that country
CSfactor=(p_1^eps_k1)*(p_2^eps_k2)*...*(p_k-1^eps_kk-1)*1/(1+eps_kk)*(1-p_k^eps_kk)
Sixth column is the contribution to the consumer surplus of that good in that country and for that consumption group
CScontribution=CSfactor*Cons represents, the total consumer surplus loss of a consumption group can be found by summing the CScontribution over all goods of that particular group in that particular country

# Prepare/compute sub datasets for analysis
- Total expenditure before price increase
- change in consumer surplus
- share of good on the change of consumer surplus for the lowest income segment
- Share of good in the change of consumer surplus for the highest income segment
- Compute food group driver of consumer surplus loss for the lowest consumption segment for each country 


