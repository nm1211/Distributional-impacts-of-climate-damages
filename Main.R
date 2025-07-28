setwd("")

##Load Libararies
library(readxl)
library(mosaic)
library(dplyr)
library(sqldf)
library(gsubfn)
library(proto)
library(RSQLite)
library(dplyr, warn.conflicts = FALSE)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(writexl)
library(Rmisc)
library(collapse)
library(reshape2)
library(tibble)
library(ggpmisc)
library(ggpubr)
library(countrycode)
library(zoo)
library(PerformanceAnalytics)
library(corrplot)
library(Hmisc)
library(kableExtra)
library(haven)
library(doBy)
library(stargazer)
library(data.table)

# Load in relevant data

gcd_raw=read.csv(file="HH_consumption_GCD.csv", sep=";")
price_vec=cons_consumptionpatterns_increases_in_prices_through_system_world_market <- read_excel("cons_consumptionpatterns_increases_in_prices_through_system_world_market.xlsx")
price_vec = t(price_vec)
colnames(price_vec) <- price_vec[1,]
price_vec<-price_vec[-1,]
price_vec<- as.data.frame(price_vec)
price_vec$i..GTAPsector<- row.names(price_vec)
names(price_vec)[names(price_vec) == "i..GTAPsector"] <- "GTAPsector"
gcd_mapper <- read.csv(file="Appendix_Mapping_GCD_GTAP.csv", sep=";")
country_code_GTAP <- read.csv(file = "Codes_Countries.csv", sep=";")
CP_elast_low<-read_xlsx("cross_price_elasticities.xlsx",  sheet = "low_uncompensated")
CP_elast_middle<-read_xlsx("cross_price_elasticities.xlsx",  sheet = "medium_uncompensated")

###load additional datasets for later
per_capita_consumption <- read.csv(file = "per_capita_consumption_aggregated.csv", sep=";")

GDP_country <- read.csv(file = "GDP_per_capita_PPP.csv", sep = ";")
GDP_country$X2019 <- as.numeric(sub(",", ".", GDP_country$X2019))
GDP_country$iso3c<-countrycode(GDP_country$Country, origin = 'country.name', destination = 'iso3c')

gini_coefficient <- read_excel("data/gini_coefficient.xls")
names(gini_coefficient)[names(gini_coefficient) == "country"] <- "Country"
gini_coefficient$iso3c<-countrycode(gini_coefficient$Country, origin = 'country.name', destination = 'iso3c')


latitude <- read.csv(file= "Latitude.csv", sep=";")
latitude$Latitude <- as.numeric(sub(",", ".", latitude$Latitude))
latitude$iso3c<-countrycode(latitude$Country, origin = 'country.name', destination = 'iso3c', custom_match=c('Rawanda' = 'RWA'))
latitude2 <- read.csv(file= "average-latitude-longitude-countries.csv")
latitude2$iso3c<- countrycode(latitude2$Country, origin = 'country.name', destination = 'iso3c')
##############################################################################################################################
# Transpose such that we have consumption increase not price increase

CP_elast_low<-t(CP_elast_low)
colnames(CP_elast_low)<-CP_elast_low[1,]
CP_elast_low<- CP_elast_low[-1,]
CP_elast_low<-as.data.frame(CP_elast_low)
CP_elast_low$Product_or_Service<-row.names(CP_elast_low)
CP_elast_low$country_income<-"low"


CP_elast_middle<-t(CP_elast_middle)
colnames(CP_elast_middle)<-CP_elast_middle[1,]
CP_elast_middle<- CP_elast_middle[-1,]
CP_elast_middle<-as.data.frame(CP_elast_middle)
CP_elast_middle$Product_or_Service<-row.names(CP_elast_middle)
CP_elast_middle$country_income<-"middle"
CP_elast_low$`fruit & veg` = as.numeric(sub(",", ".", CP_elast_low$`fruit & veg`, fixed = TRUE))
CP_elast_low$meat = as.numeric(sub(",", ".", CP_elast_low$meat, fixed = TRUE))
CP_elast_low$fish = as.numeric(sub(",", ".", CP_elast_low$fish, fixed = TRUE))
CP_elast_low$dairy = as.numeric(sub(",", ".", CP_elast_low$dairy, fixed = TRUE))
CP_elast_low$cereals = as.numeric(sub(",", ".", CP_elast_low$cereals, fixed = TRUE))
CP_elast_low$`fats & oil` = as.numeric(sub(",", ".", CP_elast_low$`fats & oil`, fixed = TRUE))
CP_elast_low$sweets = as.numeric(sub(",", ".", CP_elast_low$sweets, fixed = TRUE))
CP_elast_middle$`fruit & veg` = as.numeric(sub(",", ".", CP_elast_middle$`fruit & veg`, fixed = TRUE))
CP_elast_middle$meat = as.numeric(sub(",", ".", CP_elast_middle$meat, fixed = TRUE))
CP_elast_middle$fish = as.numeric(sub(",", ".", CP_elast_middle$fish, fixed = TRUE))
CP_elast_middle$dairy = as.numeric(sub(",", ".", CP_elast_middle$dairy, fixed = TRUE))
CP_elast_middle$cereals = as.numeric(sub(",", ".", CP_elast_middle$cereals, fixed = TRUE))
CP_elast_middle$`fats & oil` = as.numeric(sub(",", ".", CP_elast_middle$`fats & oil`, fixed = TRUE))
CP_elast_middle$sweets = as.numeric(sub(",", ".", CP_elast_middle$sweets, fixed = TRUE))
own_low<-as.numeric(diag(as.matrix(CP_elast_low)))
own_mid<-as.numeric(diag(as.matrix(CP_elast_middle)))
summary(own_low)
summary(own_mid)
own_all<-append(own_low, own_mid)
summary(own_all)
################################################################################################################################################
#Map cross price elasticities GCD categories

product_mapping <- rep(1:7, times = c(5, 5, 2, 4, 2, 2, 9))  
elasticities_7x7_low <- CP_elast_low[, 1:7]  
elasticities_29x29_low <- matrix(0, nrow = 29, ncol = 29)  

# Fill the 29x29 matrix based on the 7x7 elasticities
for (i in 1:29) {
  for (j in 1:29) {
    category_i <- product_mapping[i]
    category_j <- product_mapping[j]
    elasticities_29x29_low[i, j] <- elasticities_7x7_low[category_i, category_j]
  }
}

# Zero out cross-elasticities WITHIN THE SAME GROUP (but keep diagonal)
for (i in 1:29) {
  for (j in 1:29) {
    if (i == j) next  # Skip diagonal (keep own-price elasticity)
    if (product_mapping[i] == product_mapping[j]) {
      elasticities_29x29_low[i, j] <- 0  # Set same-group cross-elasticity to 0
    }
  }
}

# Preserve diagonal (redundant but safe)
diag(elasticities_29x29_low) <- diag(elasticities_29x29_low)  

colnames(elasticities_29x29_low)<-c("Fresh or Chilled Fruit", "Fresh or Chilled Potatoes" , "Fresh or Chilled Vegetables Other than Potatoes" ,"Frozen, Preserved or Processed Fruit and Fruit-based Product" , "Frozen, Preserved or Processed Vegetables and Vegetable-based Product", 
                                    "Beef and Veal", "Lamb, Mutton and Goat" , "Other Meats and Meat Preparations", "Pork",  "Poultry",
                                    "Fresh, Chilled or Frozen Fish and Seafood", "Preserved or Processed Fish and Seafood",
                                    "Cheese","Preserved Milk and Other Milk Products","Eggs and Egg-Based Products" ,"Fresh Milk",
                                    "Other Cereals, Flour and Other Products", "Rice",
                                    "Butter and Margarine" , "Other Edible Oil and Fats",
                                    "Beer" ,   "Bread" , "Coffee, Tea and Cocoa", "Mineral Waters, Soft Drinks, Fruit and Vegetable Juices", "Other Bakery Products"  ,"Spirits"    , "Sugar" ,"Tobacco" , "Wine")

row.names(elasticities_29x29_low)<-c("Fresh or Chilled Fruit", "Fresh or Chilled Potatoes" , "Fresh or Chilled Vegetables Other than Potatoes" ,"Frozen, Preserved or Processed Fruit and Fruit-based Product" , "Frozen, Preserved or Processed Vegetables and Vegetable-based Product", 
                                     "Beef and Veal", "Lamb, Mutton and Goat" , "Other Meats and Meat Preparations", "Pork",  "Poultry",
                                     "Fresh, Chilled or Frozen Fish and Seafood", "Preserved or Processed Fish and Seafood",
                                     "Cheese","Preserved Milk and Other Milk Products","Eggs and Egg-Based Products" ,"Fresh Milk",
                                     "Other Cereals, Flour and Other Products", "Rice",
                                     "Butter and Margarine" , "Other Edible Oil and Fats",
                                     "Beer" ,   "Bread" , "Coffee, Tea and Cocoa", "Mineral Waters, Soft Drinks, Fruit and Vegetable Juices", "Other Bakery Products"  ,"Spirits"    , "Sugar" ,"Tobacco" , "Wine")

# Ensure diagonal is preserved (own-price elasticities)

elasticities_7x7_mid<-CP_elast_middle[,c(1:7)]
elasticities_29x29_mid <- matrix(0, nrow = 29, ncol = 29)

for (i in 1:29) {
  for (j in 1:29) {
    category_i <- product_mapping[i]
    category_j <- product_mapping[j]
    elasticities_29x29_mid[i, j] <- elasticities_7x7_mid[category_i, category_j]
  }
}

# Zero out cross-elasticities WITHIN THE SAME GROUP (but keep diagonal)
for (i in 1:29) {
  for (j in 1:29) {
    if (i == j) next  # Skip diagonal (keep own-price elasticity)
    if (product_mapping[i] == product_mapping[j]) {
      elasticities_29x29_mid[i, j] <- 0  # Set same-group cross-elasticity to 0
    }
  }
}

# Set diagonal to own-price elasticities (if needed)
diag(elasticities_29x29_mid) <- diag(elasticities_29x29_mid)

colnames(elasticities_29x29_mid)<-c("Fresh or Chilled Fruit", "Fresh or Chilled Potatoes" , "Fresh or Chilled Vegetables Other than Potatoes" ,"Frozen, Preserved or Processed Fruit and Fruit-based Product" , "Frozen, Preserved or Processed Vegetables and Vegetable-based Product", 
                                    "Beef and Veal", "Lamb, Mutton and Goat" , "Other Meats and Meat Preparations", "Pork",  "Poultry",
                                    "Fresh, Chilled or Frozen Fish and Seafood", "Preserved or Processed Fish and Seafood",
                                    "Cheese","Preserved Milk and Other Milk Products","Eggs and Egg-Based Products" ,"Fresh Milk",
                                    "Other Cereals, Flour and Other Products", "Rice",
                                    "Butter and Margarine" , "Other Edible Oil and Fats",
                                    "Beer" ,   "Bread" , "Coffee, Tea and Cocoa", "Mineral Waters, Soft Drinks, Fruit and Vegetable Juices", "Other Bakery Products"  ,"Spirits"    , "Sugar" ,"Tobacco" , "Wine")

row.names(elasticities_29x29_mid)<-c("Fresh or Chilled Fruit", "Fresh or Chilled Potatoes" , "Fresh or Chilled Vegetables Other than Potatoes" ,"Frozen, Preserved or Processed Fruit and Fruit-based Product" , "Frozen, Preserved or Processed Vegetables and Vegetable-based Product", 
                                     "Beef and Veal", "Lamb, Mutton and Goat" , "Other Meats and Meat Preparations", "Pork",  "Poultry",
                                     "Fresh, Chilled or Frozen Fish and Seafood", "Preserved or Processed Fish and Seafood",
                                     "Cheese","Preserved Milk and Other Milk Products","Eggs and Egg-Based Products" ,"Fresh Milk",
                                     "Other Cereals, Flour and Other Products", "Rice",
                                     "Butter and Margarine" , "Other Edible Oil and Fats",
                                     "Beer" ,   "Bread" , "Coffee, Tea and Cocoa", "Mineral Waters, Soft Drinks, Fruit and Vegetable Juices", "Other Bakery Products"  ,"Spirits"    , "Sugar" ,"Tobacco" , "Wine")

elasticities_29x29_low<-as.data.frame(elasticities_29x29_low)
elasticities_29x29_low$country_income<-"low"
elasticities_29x29_mid<-as.data.frame(elasticities_29x29_mid)
elasticities_29x29_mid$country_income<-"middle"
CP_elast<-rbind(elasticities_29x29_low,elasticities_29x29_mid)
################################################################################################################
# Create the table of matching for paper

elasticities_match<-c("Fresh or Chilled Fruit", "Fresh or Chilled Potatoes" , "Fresh or Chilled Vegetables Other than Potatoes" ,"Frozen, Preserved or Processed Fruit and Fruit-based Product" , "Frozen, Preserved or Processed Vegetables and Vegetable-based Product", 
                      "Beef and Veal", "Lamb, Mutton and Goat" , "Other Meats and Meat Preparations", "Pork",  "Poultry",
                      "Fresh, Chilled or Frozen Fish and Seafood", "Preserved or Processed Fish and Seafood",
                      "Cheese","Preserved Milk and Other Milk Products","Eggs and Egg-Based Products" ,"Fresh Milk",
                      "Other Cereals, Flour and Other Products", "Rice",
                      "Butter and Margarine" , "Other Edible Oil and Fats",
                      "Beer" ,   "Bread" , "Coffee, Tea and Cocoa", "Mineral Waters, Soft Drinks, Fruit and Vegetable Juices", "Other Bakery Products"  ,"Spirits"    , "Sugar" ,"Tobacco" , "Wine")


CP_match<-c("Fruit & Veg.", "Fruit & Veg." , "Fruit & Veg." ,"Fruit & Veg." , "Fruit & Veg.", 
            "Meats", "Meats" , "Meats", "Meats",  "Meats",
            "Fish", "Fish",
            "Dairy","Dairy","Dairy" ,"Dairy",
            "Cereals", "Cereals",
            "Fats & Oils" , "Fats & Oils",
            "Sweets" ,   "Sweets" , "Sweets", "Sweets", "Sweets"  ,"Sweets"    , "Sweets" ,"Sweets" , "Sweets")
elasticities_match <- cbind(elasticities_match, CP_match)
write.csv(elasticities_match, "elasticities_matching.csv")
####################################################################################################################################
# Merge data sets: 

GCD <- merge(gcd_raw,gcd_mapper, by = "Product.or.Service")
GCD_GTAP <- merge(GCD, country_code_GTAP, by = "Country")
result <- merge(GCD_GTAP, price_vec, by = "GTAPsector")
result$priceincrease=result[cbind(seq_len(nrow(result)),match(result$Country_Code, colnames(result)))]
final <- data.frame(result$Country, result$GTAPsector, result$Consumption.Segment,result$Product.or.Service, result$X.PPP, result$priceincrease)
colnames(final)<- c("Country","GTAP_Sector","Consumption_Segment","Product_or_Service","PPP","Price_Increase") 
final <- final[order(final$Product_or_Service),]

final$elast.prod2 <- NA
final$elast.prod2[which(final$GTAP_Sector == "omt")]<- "Meats"
final$elast.prod2[which(final$GTAP_Sector == "cmt")]<- "Meats"
final$elast.prod2[which(final$GTAP_Sector == "mil")]<- "Dairy"
final$elast.prod2[which(final$Product_or_Service == "Fresh, Chilled or Frozen Fish and Seafood")]<-"Fish"
final$elast.prod2[which(final$Product_or_Service =="Preserved or Processed Fish and Seafood")]<-"Fish"
final$elast.prod2[which(final$GTAP_Sector == "v_f")] <-"Fruits & vegetables"
final$elast.prod2[which(final$Product_or_Service =="Fresh or Chilled Potatoes")]<-"Fruits & vegetables"
final$elast.prod2[which(final$Product_or_Service =="Frozen, Preserved or Processed Fruit and Fruit-based Product")]<-"Fruits & vegetables"
final$elast.prod2[which(final$Product_or_Service =="Frozen, Preserved or Processed Vegetables and Vegetable-based Product")] <-"Fruits & vegetables"
final$elast.prod2[which(final$GTAP_Sector == "vol")]<- "Oils & fats"
final$elast.prod2[which(final$GTAP_Sector == "b_t")]<-"Food other"
final$elast.prod2[which(final$GTAP_Sector == "oap")]<- "Dairy"
final$elast.prod2[which(final$GTAP_Sector == "pdr")]<- "Cereals"
final$elast.prod2[which(final$GTAP_Sector == "sgr")]<- "Food other"
final$elast.prod2[which(final$Product_or_Service == "Other Cereals, Flour and Other Products")] <- "Cereals"
final$elast.prod2[which(final$GTAP_Sector == "ofd" & final$elast.prod =="")] <-"Food other"
final$elast.prod2[which(final$Product_or_Service == "Bread")] <- "Food other"
final$elast.prod2[which(final$Product_or_Service == "Other Bakery Products")] <- "Food other"

final2<-final

## separate here 2 datasets one for price and one for consumption and transpose consumption , good name will be elat.prod. 

low_income<- c("Cote d’Ivoire", "Tanzania", "Uganda", "Bangladesh", "Indonesia", "Pakistan", "Philippines", "Bolivia", "Ecuador", "Paraguay", "Lesotho",  "Afghanistan" ,"Yemen, Rep." , "India", "Lao PDR" , "Mozambique" ,  "Swaziland" ,  "Nepal",  "Tajikistan", "Niger" ,"Zambia",  "Kenya", "Guinea", "Congo, Rep."  ,"Cabo Verde","Senegal" , "Benin", "Rwanda" , "Congo, Dem. Rep." ,"Madagascar", "Mauritania", "Jordan", "Liberia" ,"Chad" ,  "Sierra Leone" ,  "Cambodia"  , "Burkina Faso" ,"Ghana" ,"Vietnam"  ,"Morocco"  ,"Bhutan", "Sri Lanka", "Timor Leste" ,  "Mali", "Malawi",  "Papua New Guinea"  , "Ethiopia"  , "Sao Tome and Principe" , "Uganda" ,  "Burundi",  "Egypt, Arab Rep." , "Kyrgyz Republic", "Gambia, The" , "Nicaragua" , "Cameroon","Honduras", "Djibouti", "Togo" , "Nigeria" )

middle_income<- c("South Africa", "Turkey", "Saudia-Arabia", "Bulgaria", "Hungary", "Latvia", "Lithuania", "Romania", "Slovenia", "Mexico", "Brazil", "El Salvador", "Bosnia and Herzegovina", "Macedonia, FYR", "Thailand" ,"Azerbaijan", "Iraq" , "Moldova", "Russian Federation"  , "Belarus" ,"Gabon" , "Maldives", "Colombia", "Mauritius" ,"Serbia",   "Mongolia",  "Fiji" , "Ukraine",  "Jamaica", "Albania" , "Peru", "Kazakhstan", "Montenegro" ,  "Guatemala" , "Namibia" , "Armenia" , "China" )


final2$country_income<-ifelse(final2$Country %in% low_income, "low", "middle")

CP_elast$Product_or_Service<-rownames(CP_elast)
CP_elast$Product_or_Service <- gsub("[0-9]", "", CP_elast$Product_or_Service)

final3<-left_join( CP_elast,final2, by = c( "country_income","Product_or_Service"))

final3$PPP = as.numeric(sub(",", ".", final3$PPP, fixed= TRUE))
final3$Price_Increase = as.numeric(sub(",", ".", final3$Price_Increase, fixed = TRUE))

price_increase<- final3[,c(30:32,34,36)]
consumption<- final3[,c(30:32,34,35)]
consumption_w<-consumption %>%  pivot_wider(names_from = Consumption_Segment, values_from = PPP)

#price_increase<-price_increase[,-c(1,4)]
prices <- price_increase %>%
  group_by(Country, Product_or_Service) %>%
  dplyr::summarize(price= mean(Price_Increase, na.rm = TRUE))
prices$country_income<-ifelse(prices$Country %in% low_income, "low", "middle")
elast<- CP_elast
elast<-left_join(prices, elast, by=c("country_income", "Product_or_Service"))
elast<-elast[,-c(3,4)]
consumption_w<-consumption_w[,-1]
prices<-prices[,-4]
consumption_w<- consumption_w[, c(2,1,7,6,3,4,5)]

write.csv(elast, "CP_elast.csv")
write.csv(prices, "prices.csv")
write.csv(consumption_w, "consumption.csv")
##################################################################################################################
# Compute consumer surplus loss 
compute_CS <- function(Consumption, Prices,Elas)
{
Consumption=consumption_w
Prices= prices
Elas=elast
  
  price_sorted=Prices[
    with(Prices, order(Country, Product_or_Service)),
  ]
  
  elas_sorted=Elas[
    with(Elas, order(Country, Product_or_Service)),
  ]
  
  #for columns in elasticities
  elas_sorted=elas_sorted[ , order(c("A","AA",names(elas_sorted[3:ncol(elas_sorted)])))]
  
  #### square price data so that we have prices also for cross-elasticities and in the same format as elasticites:
  #make transposed vector of prices:
  price_wide=price_sorted %>% 
    pivot_wider(names_from = Product_or_Service, values_from = price)
  #delete price column in sorted prices as for each good we will write down price increases for all goods
  price_sorted$price<-NULL
  #merge both data frames:
  prices_final=left_join(price_sorted,price_wide, by=c("Country"))
  #### for each combination of goodxgood, compute possible products in factor to consumer surplus, eliminate unneccessary ones later
  #compute component for cross-price elasticities (p^elasticity) by goodxgood combination
  #this will also use own-price elasticities to compute, which we will get rid of in second step
  crossM=mapply(function(x,y) x**y, prices_final[3:ncol(elas_sorted)], elas_sorted[3:ncol(elas_sorted)])
  #create index matrix with matching good=good entries:
  #create this matrix for only one country:
  m=diag(length(3:ncol(elas_sorted)))
  #now replicate for all countries:
  ownM_index=do.call(rbind, replicate(length(unique(Elas$Country)), m, simplify=FALSE)) 
  #replace all calculations from above at own price spot with zero:
  crossM=crossM-mapply(function(x,y) x*y, crossM,ownM_index)
  #calculate own price contribution 1/(1+e)*(1-p^(1+e)) or -log(x)
  ownM=mapply(function(x,y) ifelse(y==-1,-log(x),1/(1+y)*(1-x**(1+y))) , prices_final[3:ncol(elas_sorted)], elas_sorted[3:ncol(elas_sorted)])
  #remove calculations at cross price indices:
  ownM=ownM*0+mapply(function(x,y) x*y, ownM,ownM_index)
  #combine contributions before calculating product:
  bothM=ownM+crossM
  #create matrix with only lower half with ones, zeros otherwise --> this gives the places that enter the product
  #creates this matrix only for one country:
  m=lower.tri(diag(length(3:ncol(elas_sorted))), diag = TRUE)*1
  #replicate with number of countries
  lowerM=do.call(rbind, replicate(length(unique(Elas$Country)), m, simplify=FALSE))
  #leave only contributions in lower triangle,
  bothM=bothM*0+mapply(function(x,y) x*y, bothM,lowerM)
  #now calculate the product  
  contributions=prices_final
  #put in contributions but fill upper triangle with ones to calculate product
  #creates upper triangle matrix only for one country:
  m=upper.tri(diag(length(3:ncol(elas_sorted))), diag = FALSE)*1
  #replicate with number of countries
  upperM=do.call(rbind, replicate(length(unique(Elas$Country)), m, simplify=FALSE))
  bothM=bothM+upperM
  #compute the CSfactors as the product
  contributions[,3:ncol(elas_sorted)]=bothM
  contributions$CSfactor=apply(bothM,1,prod)
  #remove product components (kept just for checking)
  CSfactors=contributions %>%
    select(Country, Product_or_Service, CSfactor)
  
  #now merge with consumption data    
  #make consumption long format
  
  cons_long <- melt(setDT(consumption_w), id.vars = c("Country","Product_or_Service"), variable.name = "Consumption_Segment")
  colnames(cons_long)[4]<-"Cons"
  
  CSfinal=merge(cons_long,CSfactors, by=c("Country","Product_or_Service"))
  
  CSfinal=CSfinal %>%
    mutate(CScontribution=Cons*CSfactor)
  
  return(CSfinal)
}
CSfinal=compute_CS(Consumption, Prices,Elas)

write.csv(CSfinal, "CP_CSfinal_good.csv")
CSfinal_all<-filter(CSfinal, Consumption_Segment=="All")
write.csv(CSfinal_all, "CP_CSfinal_all.csv")
CSfinal_all_factor<-CSfinal_all[,-c(4,6)]
write.csv(CSfinal_all_factor, "CP_CSfinal_factor.csv")

CSfinal_agg <- CSfinal %>%
  group_by(Country, Consumption_Segment) %>%
  dplyr::summarise(CS_facto_agg = sum(CScontribution, na.rm = T) / sum(Cons, na.rm = T)
  ) %>% 
  ungroup()
write.csv(CSfinal_agg, "CP_CSfinal2.csv")
CSfinal_exp=CSfinal_agg %>% 
  pivot_wider(names_from = Consumption_Segment, values_from = CS_facto_agg)
write.csv(CSfinal_exp, "CP_CSfinal_exp2.csv")

CSfinal_change=CSfinal %>% 
  select(Country,Product_or_Service, Consumption_Segment,CScontribution) %>% 
  pivot_wider(names_from = Consumption_Segment, values_from = CScontribution)

Expenditure_before_priceinc=CSfinal %>% 
  select(Country,Product_or_Service, Consumption_Segment,Cons) %>% 
  pivot_wider(names_from = Consumption_Segment, values_from = Cons)


final4<-left_join(CSfinal,final3, by=c("Product_or_Service", "Country", "Consumption_Segment"))
######################################################################################################
options(scipen = 999) 
# Create subsets for analysis

countries_gcd<-unique(final4$Country)
nr_products <- unique(final4$Product_or_Service)
nr_products2 <- unique(final4$elast.prod2)


Total_expenditures_before_price_change=matrix(NA, nrow=length(countries_gcd), ncol=4)
colnames(Total_expenditures_before_price_change)<- c("Lowest", "Low", "Middle", "Higher")
row.names(Total_expenditures_before_price_change)<-countries_gcd


change_CS=matrix(NA, nrow=length(countries_gcd), ncol=4)
colnames(change_CS)<- c("Lowest", "Low", "Middle", "Higher")
row.names(change_CS)<- countries_gcd

CS_change_exp=matrix(NA, nrow=length(countries_gcd), ncol=6)
colnames(CS_change_exp)<- c("Lowest", "Low", "Middle", "Higher", "National", "Regressivity")
row.names(CS_change_exp)<- countries_gcd


Share_good_on_change_CS_lowest=matrix(NA, ncol = length(nr_products), nrow = length(countries_gcd))
row.names(Share_good_on_change_CS_lowest) <- countries_gcd
colnames(Share_good_on_change_CS_lowest) <- nr_products

Share_good_on_change_CS_higher=matrix(NA, ncol = length(nr_products), nrow = length(countries_gcd))
row.names(Share_good_on_change_CS_higher) <- countries_gcd
colnames(Share_good_on_change_CS_higher) <-nr_products

for (j in 1:length(countries_gcd))
{
  ## find all data entries for country j
  vec_country=which(final4$Country==countries_gcd[j])
  
  ## Household consumption of lowest segment for the country
  vec_lowest=which(final4$Consumption_Segment=="Lowest");
  lowest_match=match(vec_country,vec_lowest, FALSE)>0;
  consumption_country_lowest=final4[vec_country[lowest_match],];
  consumption_country_lowest=as.data.frame(consumption_country_lowest)
  ## Household consumption of low segment
  vec_low=which(final4$Consumption_Segment=="Low");
  low_match=match(vec_country,vec_low, FALSE)>0;
  consumption_country_low=final4[vec_country[low_match],];
  consumption_country_low=as.data.frame(consumption_country_low)
  
  ## Household consumption of middle segment
  vec_middle=which(final4$Consumption_Segment=="Middle");
  middle_match=match(vec_country,vec_middle, FALSE)>0;
  consumption_country_middle=final4[vec_country[middle_match],];
  consumption_country_middle=as.data.frame(consumption_country_middle)
  
  ## Household consumption of higher segment
  vec_higher=which(final4$Consumption_Segment=="Higher");
  higher_match=match(vec_country,vec_higher, FALSE)>0;
  consumption_country_higher=final4[vec_country[higher_match],];
  consumption_country_higher=as.data.frame(consumption_country_higher)
  
##################################
## Calculate/ attribute data for total expenditures - before and after and total additional expenditures
##################################
  
  ### Total expenditures for each income group and country BEFORE the price change
  Total_expenditures_before_price_change[j,1] <-sum(consumption_country_lowest$PPP)
  Total_expenditures_before_price_change[j,2] <-sum(consumption_country_low$PPP)
  Total_expenditures_before_price_change[j,3] <-sum(consumption_country_middle$PPP)
  Total_expenditures_before_price_change[j,4] <-sum(consumption_country_higher$PPP)
  ### Change in Consumer surplus for each group and country
  change_CS[j,1] <-sum(consumption_country_lowest$CScontribution,na.rm=T)
  change_CS[j,2] <-sum(consumption_country_low$CScontribution, na.rm=T)
  change_CS[j,3] <-sum(consumption_country_middle$CScontribution, na.rm=T)
  change_CS[j,4] <-sum(consumption_country_higher$CScontribution, na.rm=T)
  #Calculate for every household segment and country the respective change in CS as share of initial expenditure share
  ### Attribute data to the belonging matrices
  CS_change_exp[j,1] <- (change_CS[j,1] / Total_expenditures_before_price_change[j,1])
  CS_change_exp[j,2] <- (change_CS[j,2] / Total_expenditures_before_price_change[j,2])
  CS_change_exp[j,3] <- (change_CS[j,3] / Total_expenditures_before_price_change[j,3])
  CS_change_exp[j,4] <- (change_CS[j,4] / Total_expenditures_before_price_change[j,4])
  #additional expenditure share of the respective nation - total CS change of each consumption segment are aggregated
  CS_change_exp[j,5] <- (sum(change_CS[j,])/sum(Total_expenditures_before_price_change[j,]))
  #regressivity-indicator: displays regressive effects between change in CS as a share of total expenditures between lowest and higher group
  CS_change_exp[j,6] <- (CS_change_exp[j,1] / CS_change_exp[j,4])
  
  # Calculate the share of each consumption item on change in CS - will be used for the sectoral analysis to examine which sector drives the regressivity
  Share_good_on_change_CS_lowest[j,] <-  consumption_country_lowest[,6]/change_CS[j,1]
  Share_good_on_change_CS_higher[j,] <-  consumption_country_higher[,6]/change_CS[j,4]
}
#############################################################################################################################
## Compute food group driver of consumer surplus loss for the lowest consumption segment for each country
driver_max_lowest=matrix(NA, nrow = 2, ncol = length(countries_gcd))
colnames(driver_max_lowest) <- countries_gcd
row.names(driver_max_lowest) <- c("Consumption_Item", "Share(in %)")


for (j in 1:length(countries_gcd)) 
{
  Share_good_on_change_CS_lowest[is.na(Share_good_on_change_CS_lowest)] <- -9999
  driver_lowest <- which.max(Share_good_on_change_CS_lowest[j,])
  good_value <- max(Share_good_on_change_CS_lowest[j,])
  
  driver_max_lowest[1,j] <-driver_lowest
  driver_max_lowest[2,j] <-good_value*100
}
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="1", "Beef and Veal")
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="2", "Beer" )
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="3", "Bread")
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="4", "Butter and Margarine")
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="5", "Cheese")
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="6", "Coffee, Tea and Cocoa")
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="7", "Eggs and Egg-Based Products")
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="8", "Fresh Milk")
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="9", "Fresh or Chilled Fruit" )
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="10", "Fresh or Chilled Potatoes")
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="11", "Fresh or Chilled Vegetables Other than Potatoes")
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="12", "Fresh, Chilled or Frozen Fish and Seafood")
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="13", "Frozen, Preserved or Processed Fruit and Fruit-based Product" )
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="14", "Frozen, Preserved or Processed Vegetables and Vegetable-based Product")
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="15", "Lamb, Mutton and Goat")
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest== "16", "Mineral Waters, Soft Drinks, Fruit and Vegetable Juices")    
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="17", "Other Bakery Products")
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="18", "Other Cereals, Flour and Other Products")
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="19", "Other Edible Oil and Fats")
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="20", "Other Meats and Meat Preparations")
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="21", "Pork")
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="22", "Poultry")
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="23", "Preserved Milk and Other Milk Products" )
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="24", "Preserved or Processed Fish and Seafood" )
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="25", "Rice" )
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="26", "Spirits" )
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="27", "Sugar")
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="28", "Tobacco")
driver_max_lowest <- replace(driver_max_lowest, driver_max_lowest=="29", "Wine")

driver_max_lowest_t<-t(driver_max_lowest)
driver_max_lowest_t<-as.data.frame(driver_max_lowest_t)
driver_max_lowest_t$`Share(in %)`<-ifelse(driver_max_lowest_t$`Share(in %)`==-999900, NA, driver_max_lowest_t$`Share(in %)`)
########################################################################################################################
# Compute consumption elasticity 
## clean and restructure consumer surplus loss data for analysis: compute absolute value to be able to log the values for analysis+ convert relevant variables to numeric
CS_change_exp<-abs(CS_change_exp[,1:6])
convert_ln <-log(CS_change_exp[,1:6])
ln_CS_change_exp <- convert_ln
ln_CS_change_exp[order(rownames(ln_CS_change_exp)),]
ln_CS_change_exp_t <- t(ln_CS_change_exp)
ln_CS_change_exp_t <- data.frame(ln_CS_change_exp_t)
ln_CS_change_exp<-as.data.frame(ln_CS_change_exp)
ln_CS_change_exp$Country<- rownames(ln_CS_change_exp)
ln_CS_change_exp<- as.data.frame(ln_CS_change_exp)
ln_CS_change_exp$Lowest<- as.numeric(ln_CS_change_exp$Lowest)
ln_CS_change_exp$Low<- as.numeric(ln_CS_change_exp$Low)
ln_CS_change_exp$Middle<- as.numeric(ln_CS_change_exp$Middle)
ln_CS_change_exp$Higher<- as.numeric(ln_CS_change_exp$Higher)
ln_CS_chng<-melt(ln_CS_change_exp, id.vars=c("Country"))
names(ln_CS_chng)[names(ln_CS_chng) == "variable"] <- "Consumption_Segment"
names(ln_CS_chng)[names(ln_CS_chng) == "value"] <- "ln_CS_change_exp"
ln_exp$iso3c<-countrycode(ln_exp$Country, origin = 'country.name', destination = 'iso3c')
ln_CS_chng$iso3c<-countrycode(ln_CS_chng$Country, origin = 'country.name', destination = 'iso3c')

### Clean per capita consumption data file and make sure all relevant values are numeric
df2 <- per_capita_consumption[!(per_capita_consumption$Sector=="Food and beverages" | per_capita_consumption$Sector=="Clothing and footwear" | per_capita_consumption$Sector=="Housing" | per_capita_consumption$Sector=="Energy" | per_capita_consumption$Sector=="Transport" | per_capita_consumption$Sector=="Water" | per_capita_consumption$Sector=="Education sector" | per_capita_consumption$Sector=="Health" | per_capita_consumption$Sector=="ICT" | per_capita_consumption$Sector=="Financial services" | per_capita_consumption$Sector=="Other goods and services" | per_capita_consumption$Sector=="Appliances\n "),]
df3 <- df2[!(row.names(df2) %in% c("13","26","39","52","65","78","91","104","117","130","143","156","169","182","195","208","221","234","247","260","273","286","299","312","325","338","351","364","377","390","403","416","429","442","455","468","481","494","507","520","533","546","559","572","585","598","611","624","637","650","663","676","689","702","715","728","741","754","767","780","793","806","819","832","845","858","871","884","897","910","923","936","949","962","975","988","1001","1014","1027","1040","1053","1066","1079","1092","1105","1118","1131","1144","1157","1170","1183")), ]
#df4 shows only per capita expenditures for every country and income group #not the log of it
df4 <- subset(df3, select = -c(Sector, Alles))
#delete row for Paraguay - Data for Paraguay is only accessible for aggregated data (per capita in PPP), not for HH consumption in PPP 
df4 <- df4[-c(68), ]
df4$Lowest = as.numeric(sub(",", ".", df4$Lowest))
df4$Low = as.numeric(sub("," ,".", df4$Low))
df4$Middle = as.numeric(sub("," ,".", df4$Middle))
df4$Higher = as.numeric(sub("," ,".", df4$Higher))
df3$iso3c<-countrycode(df3$Country, origin = 'country.name', destination = 'iso3c')
df4$iso3c<-countrycode(df4$Country, origin = 'country.name', destination = 'iso3c')
CSfinal_exp$iso3c<-countrycode(CSfinal_exp$Country, origin = 'country.name', destination = 'iso3c')
write.csv(CSfinal_exp,"CSfinal_exp.csv")
final_countries<-unique(CSfinal_exp$iso3c)
df3<- df3[which(df3$iso3c %in% final_countries),]
df4<- df4[which(df4$iso3c %in% final_countries),]

## Compute log for each income group and restructure dataset
ln_expenditure<-data.frame(df4$Country, log(df4$Lowest), log(df4$Low), log(df4$Middle), log(df4$Higher))
row.names(ln_expenditure) <- c("Afghanistan","Albania","Armenia", "Azerbaijan", "Bangladesh", "Belarus", "Benin", "Bhutan", "Bolivia", "Bosnia.and.Herzegovina", "Brazil", "Bulgaria", "Burkina.Faso", "Burundi", "Cambodia", "Cameroon", "Cabo.Verde", "Chad", "China", "Colombia", "Congo..Dem..Rep.", "Congo..Rep.", "Cote.d.Ivoire", "Djibouti", "Egypt..Arab.Rep.", "El.Salvador", "Ethiopia","Fiji","Gabon", "Gambia..The", "Ghana", "Guatemala", "Guinea", "Honduras", "India", "Indonesia", "Iraq", "Jamaica", "Jordan", "Kazakhstan", "Kenya", "Kyrgyz.Republic", "Lao.PDR", "Latvia", "Lesotho", "Liberia", "Lithuania", "Macedonia..FYR", "Madagascar", "Malawi", "Maldives", "Mali", "Mauritania", "Mauritius", "Mexico", "Moldova", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Namibia", "Nepal", "Nicaragua", "Niger", "Nigeria", "Pakistan", "Papua.New.Guinea", "Peru", "Philippines", "Romania", "Russian.Federation", "Rwanda", "Sao.Tome.and.Principe", "Senegal", "Serbia", "Sierra.Leone", "South.Africa", "Sri.Lanka", "Swaziland", "Tajikistan", "Tanzania", "Thailand", "Timor.Leste", "Togo", "Turkey", "Uganda", "Ukraine", "Vietnam", "Yemen..Rep.", "Zambia")
colnames(ln_expenditure) <- c("Country", "Lowest", "Low", "Middle", "Higher")
ln_expenditure <- ln_expenditure[order(rownames(ln_expenditure)),]
ln_expenditure<-ln_expenditure[,-1]
ln_expenditure_t <- t(ln_expenditure)
ln_expenditure_t <- data.frame(ln_expenditure_t)
ln_expenditure<- cbind(Country= rownames(ln_expenditure), ln_expenditure)
ln_exp<-melt(ln_expenditure, id.vars=c("Country"))
names(ln_exp)[names(ln_exp) == "variable"] <- "Consumption_Segment"
names(ln_exp)[names(ln_exp) == "value"] <- "ln_expenditure"

### Merge expenditure and CS data for analysis
trial<- left_join(ln_exp, ln_CS_chng, by= c("iso3c", "Consumption_Segment"))
trial$ln_percapita_CS <- trial$ln_CS_change_exp+trial$ln_expenditure

### Plot both variables and compute the consumption elasticity

ggplot(trial, aes(x=ln_expenditure, y= ln_percapita_CS, color=Consumption_Segment ))+ 
  geom_point() + labs(x = "Ln Expenditure", y = "ln per capita CS change") +geom_smooth(method=lm, se=FALSE)+ theme_minimal()+stat_poly_eq(aes(label = paste(after_stat(eq.label)))) 

lm_fit <- lm(ln_percapita_CS ~ ln_expenditure, data=trial)
summary(lm_fit)
predicted_df <- data.frame(alpha_pred = predict(lm_fit, trial), hp=trial$ln_percapita_CS)

ggplot(trial, aes(x=ln_expenditure, y= ln_percapita_CS,  colour= Consumption_Segment))+ geom_point() + labs(x = "Ln per capita expenditure", y = "Ln per capita CS change") + theme_minimal() + scale_colour_grey(end=0.6)+  theme_minimal(base_size=18)+theme(legend.position = "bottom")+geom_abline(slope = coef(lm_fit)[["ln_expenditure"]], intercept= coef(lm_fit)[["(Intercept)"]])

trial_reg<- trial
trial_reg$lowest<- ifelse(trial$Consumption_Segment=="Lowest",1,0)
trial_reg$low<- ifelse(trial$Consumption_Segment=="Low",1,0)
trial_reg$highest<- ifelse(trial$Consumption_Segment=="Higher",1,0)

regression<- lm(trial_reg, formula= ln_percapita_CS ~ ln_expenditure+ lowest+low+highest)
summary(regression)

stargazer(regression, type= "html", out="regression_1.html")
### Check if estimate is formally less than 1
coef_main <- coef(summary(regression))["ln_expenditure", "Estimate"]
se_main <- coef(summary(regression))["ln_expenditure", "Std. Error"]
#### Test the hypothesis that the coefficient is less than 1
t_stat <- (coef_main - 1) / se_main
p_value <- pt(t_stat, df = df.residual(regression))  # df.residual(model) gives the degrees of freedom
print(paste("t-statistic:", t_stat))
print(paste("p-value:", p_value))

#### Interpretation at 5%

if (p_value < 0.05) {
  print("Reject the null hypothesis: The coefficient is significantly less than 1")
} else {
  print("Fail to reject the null hypothesis: The coefficient is not significantly less than 1")
}
##"Reject the null hypothesis: The coefficient is significantly less than 1"
library(sandwich)
library(lmtest)
### Cluster standard errors by country 
regression_clustered <-  lm(trial_reg, formula= ln_percapita_CS ~ ln_expenditure+ lowest+low+highest + factor(Country.x))
clustered_se <- vcovCL(regression_clustered, cluster = ~Country.x)
coeftest(regression_clustered, vcov. = clustered_se)
stargazer(regression_clustered, type= "html", out="regression_1C.html")
###############################################################################################################################################
# Macro-economic variables and consumer surplus loss analysis

CS_change_exp<-as.data.frame(CS_change_exp)
CS_change_exp$Lowest<- abs(CS_change_exp$Lowest)
CS_change_exp$Low<- abs(CS_change_exp$Low)
CS_change_exp$Middle<- abs(CS_change_exp$Middle)
CS_change_exp$Higher<- abs(CS_change_exp$Higher)
CS_change_exp$National<- abs(CS_change_exp$National)
CS_change_exp$Country<-row.names(CS_change_exp)
CS_change_exp$iso3c<-countrycode(CS_change_exp$Country, origin = 'country.name', destination = 'iso3c')
write.csv(CS_change_exp, "CS_change_exp.csv")
## Merge with GDP, plot correlation and compute regression on national and lowest income group 
trial2<- inner_join(GDP_country, CS_change_exp , by= c("iso3c"))
ggplot(trial2, aes(x=X2019, y= abs(National),label= iso3c))+  geom_point() + labs(x = "GDP per capita (in $PPP 2019)", y = "Damage") + theme_minimal(base_size=18)+geom_text(hjust=0, vjust=0, size=5)+geom_smooth(method = "lm", se = FALSE)

lm_GDP_national <-lm(formula = abs(trial2$National) ~ trial2$X2019)
summary(lm_GDP_national)
lm_GDP_lowest <-lm(formula = abs(trial2$Lowest) ~ trial2$X2019)
summary(lm_GDP_lowest)

## Merge with Gini, plot correlation and compute regression on national and lowest income group 

gini_CS<- right_join(gini_coefficient, CS_change_exp, by="iso3c")
lm_gini_CS <- lm(formula= abs(gini_CS$National) ~ gini_CS$giniWB)
summary(lm_gini_CS)
plot_gini_CS<-ggplot(gini_CS, aes(x=giniWB, y=abs(National))) +
  geom_point() +  geom_text(label=gini_CS$iso3c, size=4)+xlab("Gini coefficient")+ylab("change in Consumer surplus (% of Exp)")+geom_smooth()+stat_poly_eq(aes(label = paste(after_stat(eq.label)))) 

plot_gini_CS+theme_minimal()+xlim(min(gini_CS$giniWB), max(gini_CS$giniWB))

lm_gini_CS <- lm(formula= abs(gini_CS$Lowest) ~ gini_CS$giniWB)
summary(lm_gini_CS)
plot_gini_CS<-ggplot(gini_CS, aes(x=giniWB, y=abs(Lowest))) +
  geom_point() +  geom_text(label=gini_CS$iso3c, size=4)+xlab("Gini coefficient")+ylab("change in Consumer surplus (% of Exp)")+geom_smooth() 

plot_gini_CS+theme_minimal()
## Merge with latitude (indicators for latitude), plot correlation and compute regression on national and lowest income group 

latitude_without_higher_group <- latitude[-c(7,14,33,46,63,74,84), ]
lat_CS<- full_join(latitude, CS_change_exp, by="iso3c")

plot_lat<-ggplot(lat_CS, aes(x=Latitude, y=abs(National))) + geom_point() +  geom_text(label=lat_CS$iso3c, size=4)+xlab("Latitude")+ylab("change in Consumer surplus (% of Exp)")
plot_lat+theme_minimal()+geom_smooth()

lm_latitude = lm(formula=  abs(lat_CS$National) ~ lat_CS$Latitude)

summary(lm_latitude)
# Split by hemisphere (North and sourth)
latitude_s <- left_join(lat_CS, latitude2, by= "iso3c")
latitude_s<- latitude_s[-latitude_s$Latitude.y>0,]
plot_lat<-ggplot(latitude_s, aes(x=Latitude.x, y=abs(National))) + geom_point() +  geom_text(label=latitude_s$iso3c, size=4)+xlab("Latitude")+ylab("change in Consumer surplus (% of Exp)")
plot_lat+theme_minimal()+geom_smooth()

lm_latitude = lm(formula=  abs(latitude_s$National) ~ latitude_s$Latitude.y)
summary(lm_latitude)

latitude_n <- left_join(lat_CS, latitude2, by= "iso3c")
latitude_n<- latitude_n[-latitude_n$Latitude.y<0,]
plot_lat<-ggplot(latitude_n, aes(x=Latitude.x, y=abs(National))) + geom_point() +  geom_text(label=latitude_n$iso3c, size=4)+xlab("Latitude")+ylab("change in Consumer surplus (% of Exp)")
plot_lat+theme_minimal()+geom_smooth()

lm_latitude = lm(formula=  abs(latitude_n$National) ~ latitude_n$Latitude.y)
summary(lm_latitude)

plot_lat_n<-ggplot(lat_CS, aes(x=Latitude, y=abs(National))) + geom_point() +  geom_text(label=lat_CS$iso3c, size=4)+xlab("Latitude")+ylab("change in Consumer surplus (% of Exp)")
plot_lat_n+theme_minimal()+geom_smooth()

lm_latitude_n = lm(formula=  abs(lat_CS$National) ~ lat_CS$Latitude)
summary(lm_latitude_n)
#################################################################################################################
# Summarize data by consumption segment to get the percentage borne of CS loss by each income segment

data_sum_CS<- ddply(final4,.(Consumption_Segment), summarise,  sum_CS = sum(CScontribution, na.rm=T), mean_CS = mean(CScontribution, na.rm =T), PPP=sum(PPP, na.rm =T))
data_sum_CS<-data_sum_CS[-1,]
data_sum_CS$Consumption_Segment<- factor(data_sum_CS$Consumption_Segment, levels=c("Lowest","Low","Middle","Higher"))
data_sum_CS$CS_exp<- data_sum_CS$sum_CS/data_sum_CS$PPP
data_sum_CS$total<- sum(data_sum_CS$sum_CS)
data_sum_CS$perc<- round(data_sum_CS$sum_CS/data_sum_CS$total,4)*100
data_sum_CS
write.csv(data_sum_CS, "data_sum_CS.csv")
ln_CS_change_exp_t<-ln_CS_change_exp_t[-c(5,6),]
ln_CS_change_exp_t[is.na(ln_CS_change_exp_t) | ln_CS_change_exp_t=="-Inf"] = -9999


## restructure data to compute consumption elasticity per country (including plots and checks)

df3$Alles = as.numeric(sub(",", ".", df3$Alles))
df3$iso3c<-countrycode(df3$Country, origin = 'country.name', destination = 'iso3c')
ln_CS_percapita <-reshape2::dcast(trial, Country.x ~ Consumption_Segment)
ln_CS_percapita$Country<- ln_CS_percapita$Country.x
ln_CS_percapita<-left_join(ln_expenditure, ln_CS_percapita, by= "Country")
ln_CS_percapita<-ln_CS_percapita[,c(1, 7:10)]
colnames(ln_CS_percapita) <- c("Country","Lowest", "Low", "Middle", "Higher")
ln_CS_percapita_t<-t(ln_CS_percapita)
colnames(ln_CS_percapita_t)<-ln_CS_percapita_t[1,]
ln_CS_percapita_t<-ln_CS_percapita_t[-1,]
ln_CS_percapita_t<-data.frame(ln_CS_percapita_t)

cons_elasticity <- matrix(NA, nrow = length(ln_CS_percapita$Country), ncol = 2)
colnames(cons_elasticity) <- c("alpha", "R squared")
rownames(cons_elasticity) <-names(ln_CS_percapita_t)    
segments<-c("Lowest", "Low", "Middle", "Higher")

for (j in names(ln_CS_percapita_t))
{
  lm_expenditure <-lm(formula = ln_CS_percapita_t[,j] ~ ln_expenditure_t[,j])
  
  R_squared <- summary(lm(ln_CS_percapita_t[,j] ~ ln_expenditure_t[,j]))$r.squared
  country_slope <- coefficients(lm_expenditure)
  country_slope <- data.frame(country_slope)
  
  cons_elasticity[j,1] <- country_slope[2,]
  cons_elasticity[j,2] <- R_squared
}
cons_elasticity <- data.frame(cons_elasticity)
cons_elasticity$alpha2<-ifelse(cons_elasticity$alpha== 1.000, NA, cons_elasticity$alpha )
cons_elasticity<- cbind(Country= rownames(cons_elasticity), cons_elasticity)
cons_elasticity$category <- 0
cons_elasticity$category <- ifelse(cons_elasticity$alpha2>1, "disproportionate effect on higher-consumption groups", "disproportionate effect on lower-consumption groups")
write.csv(cons_elasticity, "cons_elasticitcy.csv")
cons_elasticity$subregioncode<- countrycode(cons_elasticity$Country, origin = 'country.name', destination = 'region')
cons_elasticity$iso3c<- countrycode(cons_elasticity$Country, origin = 'country.name', destination = 'iso3c')

data<- left_join(cons_elasticity, df3, by="iso3c")
plot<-ggplot(data, aes(x=Alles, y=alpha2, color=category)) +
  geom_point() +  geom_text(label=data$iso3c, size=5, hjust=0, vjust=0)+xlab("Per capita expenditure")+ylab("Consumption elasticity")
plot+theme_minimal(base_size=18)+labs(colour = "")+theme(legend.position = "bottom")+scale_colour_grey(end=0.6)
lm_cons_elasticity<-lm(formula = data$alpha2 ~ data$Alles)
summary(lm_cons_elasticity)

## Plot of r squared for each country
plot(data$alpha2, data$R.squared)
plot2<-ggplot(data, aes(x=data$alpha2, y=data$R.squared)) +
  geom_point() + 
  geom_text(label=data$iso3c, size=4, nudge_x = 0.01)
plot2+theme_minimal()+ xlab("Consumption elasticity")+ ylab("R squared")
ln_CS_capita<-  melt(ln_CS_percapita, id.vars = c("Country"))
ln_CS_capita <- ln_CS_capita[which(ln_CS_capita$variable!="National"&ln_CS_capita$variable!="Regressivity"),]
colnames(ln_CS_capita)<- c("Country", "Consumption_Segment", "lnCScapita")

low_rsq<-cons_elasticity[which(cons_elasticity$R.squared < 0.99),]
low_rsq <- melt(low_rsq, id.vars = c("Country"))
low_rsq<-low_rsq[which(low_rsq$variable =="R.squared"),]

colnames(low_rsq)<- c("Country", "var", "Rsquared")

data_rsq<-inner_join(ln_CS_capita, low_rsq, by="Country")
data_rsq<-inner_join(data_rsq, ln_exp, by=c("Country", "Consumption_Segment"))
data_rsq<-data_rsq[which(data_rsq$lnCScapita!=-Inf),]
ln_CS_capita<-  melt(ln_CS_percapita, id.vars = c("Country"))
ln_CS_capita <- ln_CS_capita[which(ln_CS_capita$variable!="National"&ln_CS_capita$variable!="Regressivity"),]
colnames(ln_CS_capita)<- c("Country", "Consumption_Segment", "lnCScapita")

low_rsq<-cons_elasticity[which(cons_elasticity$R.squared < 0.99),]
low_rsq <- melt(low_rsq, id.vars = c("Country"))
low_rsq<-low_rsq[which(low_rsq$variable =="R.squared"),]

colnames(low_rsq)<- c("Country", "var", "Rsquared")

data_rsq<-inner_join(ln_CS_capita, low_rsq, by="Country")
data_rsq<-inner_join(data_rsq, ln_exp, by=c("Country", "Consumption_Segment"))
data_rsq<-data_rsq[which(data_rsq$lnCScapita!=-Inf),]

data_rsq$Rsquared<-as.numeric(data_rsq$Rsquared)
plot_rsq<- ggplot(data = data_rsq, aes(x= ln_expenditure, y= lnCScapita))+geom_point()+ geom_smooth()+facet_wrap(vars(Country))+geom_text(label=ifelse(data_rsq$Consumption_Segment=="Middle",round(data_rsq$Rsquared,3),''), parse = T, nudge_y = 2, nudge_x = 2, size = 3)

plot_rsq+theme_minimal()+ xlab("Ln Expenditure")+ ylab("ln per capita CS")

data_allrsq<-inner_join(trial, cons_elasticity, by="iso3c")

plot_allrsq<- ggplot(data = data_allrsq, aes(x= ln_expenditure, y= ln_percapita_CS))+geom_point()+ geom_smooth()+facet_wrap(vars(Country))+geom_text(label=ifelse(data_allrsq$Consumption_Segment=="Middle",round(data_allrsq$R.squared,4),''), parse = T, nudge_y = 2, nudge_x = 2, size = 3)+xlab("per capita expenditure")+ylab("per  capita change in CS")+theme_minimal(base_size = 12)


plot_allrsq
############################################################################################
# Food sectors driving loss in CS in lowest and highest income groups

Share_good_on_change_CS_higher <- as.data.frame(Share_good_on_change_CS_higher)
Share_good_on_change_CS_higher$Country<- rownames(Share_good_on_change_CS_higher)
#Share_good_on_change_CS_higher<-Share_good_on_change_CS_higher[which(Share_good_on_change_CS_higher$iso3c %in% final_countries),]
Share_good_on_change_CS_higher<-Share_good_on_change_CS_higher[,-which(names(Share_good_on_change_CS_higher) %in% c("Beef and Veal","Beer","Bread","Butter and Margarine","Cheese","Coffee, Tea and Cocoa","Eggs and Egg-Based Products","Fresh Milk", "Fresh, Chilled or Frozen Fish and Seafood", "Mineral Waters, Soft Drinks, Fruit and Vegetable Juices", "Spirits", "Tobacco","Wine", "Preserved or Processed Fish and Seafood","Preserved Milk and Other Milk Products", "Other Meats and Meat Preparations" , "Frozen, Preserved or Processed Vegetables and Vegetable-based Product", "Frozen, Preserved or Processed Fruit and Fruit-based Product","Lamb, Mutton and Goat","Other Meats and Meat Preparations","Pork","Poultry", "Other Bakery Products" , "Sugar"))]
Share_good_on_change_CS_higher_long <- melt(Share_good_on_change_CS_higher, id.vars = c("Country"))
names(Share_good_on_change_CS_higher_long)[names(Share_good_on_change_CS_higher_long) == "variable"] <- "Sector"

Share_good_on_change_CS_higher_long$iso3c<-countrycode(Share_good_on_change_CS_higher_long$Country, origin = 'country.name', destination = 'iso3c')
Share_good_on_change_CS_higher_long$region<-countrycode(Share_good_on_change_CS_higher_long$Country, origin = 'country.name', destination = 'region')

Share_good_on_change_CS_higher_long<- left_join(Share_good_on_change_CS_higher_long, cons_elasticity, by="iso3c")
Share_good_on_change_CS_higher_long<-Share_good_on_change_CS_higher_long[-which(Share_good_on_change_CS_higher_long$value==0),]

Share_good_on_change_CS_higher_long_plot<-ggplot(Share_good_on_change_CS_higher_long, aes(x=value, y=alpha2, color= Sector)) +
  geom_point()+xlab("Contribution of sector on CS loss (H)")+ylab("Consumption elasticity")+geom_smooth(method=lm, se=FALSE)
Share_good_on_change_CS_higher_long_plot<-ggplot(Share_good_on_change_CS_higher_long, aes(x=value, y=alpha2, color= Sector)) +
  geom_point()+xlab("Contribution of sector on CS loss (H)")+ylab("Consumption elasticity")+geom_smooth(method=lm, se=FALSE)+theme_minimal()+scale_color_manual(labels = c("Fruit", "Potatoes", "Vegetables", "Cereals and Flour", "Oils & Fats", "Rice", "Sugar"), values = c("#FF33CC", "#996633", "#339900", "#FFCC00", "#000000", "#0099FF","#FF6633"))+labs(colour = "Sector")+theme(legend.position = "bottom")
Share_good_on_change_CS_higher_long_plot

Share_good_on_change_CS_lowest <- as.data.frame(Share_good_on_change_CS_lowest)
Share_good_on_change_CS_lowest$Country<- rownames(Share_good_on_change_CS_lowest)
Share_good_on_change_CS_lowest<-Share_good_on_change_CS_lowest[,-which(names(Share_good_on_change_CS_lowest) %in% c("Beef and Veal","Beer","Bread","Butter and Margarine","Cheese","Coffee, Tea and Cocoa","Eggs and Egg-Based Products","Fresh Milk", "Fresh, Chilled or Frozen Fish and Seafood", "Mineral Waters, Soft Drinks, Fruit and Vegetable Juices", "Spirits", "Tobacco","Wine", "Preserved or Processed Fish and Seafood","Preserved Milk and Other Milk Products", "Other Meats and Meat Preparations" , "Frozen, Preserved or Processed Vegetables and Vegetable-based Product", "Frozen, Preserved or Processed Fruit and Fruit-based Product","Lamb, Mutton and Goat","Other Meats and Meat Preparations","Pork","Poultry", "Other Bakery Products" , "Sugar"))]
Share_good_on_change_CS_lowest_long <- melt(Share_good_on_change_CS_lowest, id.vars = c("Country"))
names(Share_good_on_change_CS_lowest_long)[names(Share_good_on_change_CS_lowest_long) == "variable"] <- "Sector"
Share_good_on_change_CS_lowest_long$iso3c<-countrycode(Share_good_on_change_CS_lowest_long$Country, origin = 'country.name', destination = 'iso3c')
Share_good_on_change_CS_lowest_long$region<-countrycode(Share_good_on_change_CS_lowest_long$Country, origin = 'country.name', destination = 'region')
Share_good_on_change_CS_lowest_long<- left_join(Share_good_on_change_CS_lowest_long, cons_elasticity, by="iso3c")
Share_good_on_change_CS_lowest_long<-Share_good_on_change_CS_lowest_long[-which(Share_good_on_change_CS_lowest_long$value==0),]
## Plot for lowest income group

Share_good_on_change_CS_lowest_long_plot<-ggplot(Share_good_on_change_CS_lowest_long, aes( x=value, y=alpha2, color= Sector)) +
  geom_point()+xlab("Contribution of sector in CS loss (L)")+ylab("Consumption elasticity")+geom_smooth(method=lm, se=FALSE)

Share_good_on_change_CS_lowest_long_plot+theme_minimal()+ scale_color_manual(labels = c("Fruit", "Potatoes", "Vegetables", "Cereals and Flour", "Oils & Fats", "Rice", "Sugar"), values = c("#FF33CC", "#996633", "#339900", "#FFCC00", "#000000", "#0099FF","#FF6633"))+labs(colour = "Sector")+theme(legend.position = "bottom")

share_cs<- ddply(Share_good_on_change_CS_lowest_long,.(Sector), summarise, sum_sector =sum(value, na.rm=T))
share_cs$total<- sum(share_cs$sum_sector)
share_cs <- share_cs %>% 
  arrange(desc(Sector)) %>%
  mutate(prop = sum_sector / sum(share_cs$sum_sector) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
plot_cs<-ggplot(share_cs, aes( x="",  y=prop, fill=Sector)) + geom_bar(stat="identity", width=1)+  xlab("")+ylab("share of CS change")+coord_polar("y",start=0)+  theme_void() +  theme(legend.position="none") +  geom_text(aes(y=ypos, label = round(prop,2)), color = "white", size=4) 
plot_cs+theme_minimal()+theme(legend.position = "bottom")+scale_fill_manual(labels = c("Fruit", "Potatoes", "Vegetables", "Cereals and Flour", "Oils & Fats", "Rice", "Sugar"), values = c("#FF33CC", "#996633", "#339900", "#FFCC00", "#000000", "#0099FF","#FF6633"))+labs(colour = "Sector")+theme(legend.position = "bottom")

plot_cs<-ggplot(share_cs, aes( x="",  y=prop, fill=Sector)) + geom_bar(stat="identity", width=1)+  xlab("")+ylab("share of CS change")+  theme_void() +  theme(legend.position="none") +  geom_text(aes(y=ypos, label = round(prop,2)), color = "white", size=4) 
plot_cs+theme_minimal()+theme(legend.position = "bottom")+scale_fill_manual(labels = c("Fruit", "Potatoes", "Vegetables", "Cereals and Flour", "Oils & Fats", "Rice", "Sugar"), values = c("#FF33CC", "#996633", "#339900", "#FFCC00", "#000000", "#0099FF","#FF6633"))+labs(colour = "Sector")+theme(legend.position = "bottom")
## Focus on rice and vegetables as main contributors
share_cs_rice<- Share_good_on_change_CS_lowest_long[which((Share_good_on_change_CS_lowest_long$Sector=="Rice")), ]
write.csv(share_cs_rice, file= "share_rice_CS_lowest.csv")
share_cs_veg<- Share_good_on_change_CS_lowest_long[which((Share_good_on_change_CS_lowest_long$Sector=="Fresh or Chilled Vegetables Other than Potatoes")), ]

## Plot for highest income group

Share_good_on_change_CS_higher_long[Share_good_on_change_CS_higher_long==-Inf] = NA
share_cs2<- ddply(Share_good_on_change_CS_higher_long,.(Sector), summarise, sum_sector =sum(value, na.rm=T))
share_cs2$total<- sum(share_cs2$sum_sector)
share_cs2 <- share_cs2 %>% 
  arrange(desc(Sector)) %>%
  mutate(prop = sum_sector / sum(share_cs2$sum_sector) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
plot_cs<-ggplot(share_cs2, aes( x="",  y=prop, fill=Sector)) + geom_bar(stat="identity", width=1)+  xlab("")+ylab("share of CS change")+coord_polar("y",start=0)+  theme_void() +  theme(legend.position="none") +  geom_text(aes(y=ypos, label = round(prop,2)), color = "white", size=4) 
plot_cs+theme_minimal()+theme(legend.position = "bottom")+scale_fill_manual(labels = c("Fruit", "Potatoes", "Vegetables", "Cereals and Flour", "Oils & Fats", "Rice", "Sugar"), values = c("#FF33CC", "#996633", "#339900", "#FFCC00", "#000000", "#0099FF","#FF6633"))+labs(colour = "Sector")+theme(legend.position = "bottom")
## Focus on rice and vegetables as main contributors
share_cs2_rice<- Share_good_on_change_CS_higher_long[which((Share_good_on_change_CS_higher_long$Sector=="Rice")), ]
write.csv(share_cs2_rice, file= "share_rice_CS_higher.csv")
share_cs2_veg<- Share_good_on_change_CS_higher_long[which((Share_good_on_change_CS_higher_long$Sector=="Fresh or Chilled Vegetables Other than Potatoes")), ]

plot_csH<-ggplot(share_cs2, aes( x="",  y=prop, fill=Sector)) + geom_bar(stat="identity", width=1)+  xlab("")+ylab("share of CS change")+  theme_void() +  theme(legend.position="none") +  geom_text(aes(y=ypos, label = round(prop,2)), color = "white", size=4) 
plot_csH+theme_minimal()+theme(legend.position = "bottom")+scale_fill_manual(labels = c("Fruit", "Potatoes", "Vegetables", "Cereals and Flour", "Oils & Fats", "Rice", "Sugar"), values = c("#FF33CC", "#996633", "#339900", "#FFCC00", "#000000", "#0099FF","#FF6633"))+labs(colour = "Sector")+theme(legend.position = "bottom")

## Plot into a combined bar plot combined
share_cs$Consumption_Segment<-"Lowest"
share_cs2$Consumption_Segment<-"Higher"
shares_cs_both<-bind_rows(share_cs,share_cs2)
plot_cs<-ggplot(shares_cs_both, aes( x=Consumption_Segment,  y=prop, fill=Sector)) + geom_bar(stat="identity", width=0.5)+  xlab("")+ylab("share of CS change")+   theme(legend.position="none") +  geom_text(aes(y=ypos, label = round(prop,2)), color = "white", size=4) 
plot_cs+theme_minimal()+theme(legend.position = "bottom")+ scale_fill_manual(labels = c("Fruit", "Potatoes", "Vegetables", "Cereals and Flour", "Oils & Fats", "Rice", "Sugar"), values = c("#FF33CC", "#996633", "#339900", "#FFCC00", "#000000", "#0099FF","#FF6633"))+labs(colour = "Sector")+theme(legend.position = "bottom")
####################################################################
# Regress all sectors on alpha (consumption elasticity)

Share_good_on_change_CS_lowest$iso3c<-countrycode(Share_good_on_change_CS_lowest$Country, origin = 'country.name', destination = 'iso3c')
sectors_share<-left_join(Share_good_on_change_CS_lowest, cons_elasticity, by="iso3c")
lm_all <-lm(formula= sectors_share$alpha2 ~ sectors_share$`Other Cereals, Flour and Other Products`+ sectors_share$`Fresh or Chilled Fruit`+sectors_share$`Fresh or Chilled Potatoes`+ sectors_share$`Fresh or Chilled Vegetables Other than Potatoes`+ sectors_share$Rice+ sectors_share$`Other Edible Oil and Fats`)
summary(lm_all)
sectors_share<-left_join(Share_good_on_change_CS_lowest, cons_elasticity, by="iso3c")
names(sectors_share)[names(sectors_share) == "Country.x"] <- "Country"
names(sectors_share)[names(sectors_share) == "Country.y"] <- "Country2"
#############################################################################
# Compute the per capita food share of expenditure

per_capita_food_share<- per_capita_consumption[!(per_capita_consumption$Sector=="Clothing and footwear" | per_capita_consumption$Sector=="Housing" | per_capita_consumption$Sector=="Energy" | per_capita_consumption$Sector=="Transport" | per_capita_consumption$Sector=="Water" | per_capita_consumption$Sector=="Education sector" | per_capita_consumption$Sector=="Health" | per_capita_consumption$Sector=="ICT" | per_capita_consumption$Sector=="Financial services" | per_capita_consumption$Sector=="Other goods and services" | per_capita_consumption$Sector=="Appliances\n "),]
per_capita_food_share$Lowest = as.numeric(sub(",", ".", per_capita_food_share$Lowest))
per_capita_food_share$Low = as.numeric(sub("," ,".", per_capita_food_share$Low))
per_capita_food_share$Middle = as.numeric(sub("," ,".", per_capita_food_share$Middle))
per_capita_food_share$Higher = as.numeric(sub("," ,".", per_capita_food_share$Higher))
per_capita_food_share$Country <- with(per_capita_food_share, ave(replace(Country, !nzchar(Country), NA),
                                                                 FUN = function(x){ x1 <- na.locf(x, na.rm=FALSE)
                                                                 replace(x1, is.na(x1), setdiff(unique(na.omit(x1)), x1[!is.na(x1)][1]))}))

per_capita_food_share$Alles<- as.numeric(sub(",", ".",per_capita_food_share$Alles))
per_capita_food_share$Lowest<- as.numeric(sub(",", ".",per_capita_food_share$Lowest))
per_capita_food_share$Low<- as.numeric(sub(",", ".",per_capita_food_share$Low))
per_capita_food_share$Middle<- as.numeric(sub(",", ".",per_capita_food_share$Middle))
per_capita_food_share$Higher<- as.numeric(sub(",", ".",per_capita_food_share$Higher))

per_capita_food_share_f<-per_capita_food_share[per_capita_food_share$Sector=="Food and beverages",]
per_capita_food_share_total<- per_capita_food_share[per_capita_food_share$Sector=="All sectors",]
#delete row for Paraguay - Data for Paraguay is only accessible for aggregated data (per capita in PPP), not for HH consumption in PPP 
per_capita_food_share_f <- per_capita_food_share_f[-c(68), ]
per_capita_food_share_total <- per_capita_food_share_total[-c(68), ]

data_share_food_exp=matrix(NA, nrow=length(countries_gcd), ncol=6)
for (j in 1:length(countries_gcd))
{
  ## find all data entries for country j
  data_share_food_exp[j,1]= per_capita_food_share_f[j,1]
  data_share_food_exp[j,2] = per_capita_food_share_f[j,3] /per_capita_food_share_total[j,3]
  data_share_food_exp[j,3] =per_capita_food_share_f[j,4]/per_capita_food_share_total[j,4]
  data_share_food_exp[j,4] =per_capita_food_share_f[j,5]/per_capita_food_share_total[j,5]
  data_share_food_exp[j,5] =per_capita_food_share_f[j,6]/per_capita_food_share_total[j,6]
  data_share_food_exp[j,6] =per_capita_food_share_f[j,7]/per_capita_food_share_total[j,7]
  
}
colnames(data_share_food_exp) <-c("Country","Alles", "Lowest","Low", "Middle","Higher")
data_share_food_exp<-data.frame(data_share_food_exp)
data_share_food_exp$iso3c<- countrycode(data_share_food_exp$Country,origin = 'country.name', destination = 'iso3c')

collapse1 <- summaryBy(PPP ~ Consumption_Segment +Country, FUN=c(sum), data=final3)
#########################################################################################
# Compute food share per country and consumption segment from total consumption
Food_products<- unique(final3$elast.prod2)
Food_products2<- unique(final3$Product_or_Service)
final$food_sector<- ifelse(final$Product_or_Service%in% Food_products2, 1,0)
final$PPP <- as.numeric(sub(",", ".",final$PPP))
collapse_all <- summaryBy(PPP ~ Consumption_Segment +Country, FUN=c(sum), data=final)
food_share<- final[which(final$food_sector==1),]
collapse_food <- summaryBy(PPP ~ Consumption_Segment +Country, FUN=c(sum), data=food_share)
collapse_food$food_PPP= collapse_food$PPP.sum
collapse_food<- collapse_food[,-3]
food_share<- left_join(collapse_all, collapse_food, by=c("Country", "Consumption_Segment"))
food_share$exp_share<-food_share$food_PPP/food_share$PPP.sum

food_share <- food_share[order(food_share$Country, -food_share$exp_share),]

share_of_food=matrix(NA, nrow=length(countries_gcd), ncol=6)
colnames(share_of_food)<- c("Lowest_f", "Low_f", "Middle_f", "Higher_f", "National_f", "Ratio_f")
row.names(share_of_food)<- countries_gcd

for (j in 1:length(countries_gcd))
{
  ## find all data entries for country j
  vec_country=which(food_share$Country==countries_gcd[j])
  
  
  vec_lowest=which(food_share$Consumption_Segment=="Lowest");
  lowest_match=match(vec_country,vec_lowest, FALSE)>0;
  food_share_lowest=food_share[vec_country[lowest_match],];
  
  vec_low=which(food_share$Consumption_Segment=="Low");
  low_match=match(vec_country,vec_low, FALSE)>0;
  food_share_low=food_share[vec_country[low_match],];
  vec_middle=which(food_share$Consumption_Segment=="Middle");
  middle_match=match(vec_country,vec_middle, FALSE)>0;
  food_share_middle=food_share[vec_country[middle_match],];
  vec_higher=which(food_share$Consumption_Segment=="Higher");
  higher_match=match(vec_country,vec_higher, FALSE)>0;
  food_share_higher=food_share[vec_country[higher_match],];
  
  vec_all=which(food_share$Consumption_Segment=="All");
  all_match=match(vec_country,vec_all, FALSE)>0;
  food_share_all=food_share[vec_country[all_match],];
  
  share_of_food[j,1] <-food_share_lowest$exp_share
  share_of_food[j,2] <-food_share_low$exp_share
  share_of_food[j,3] <-food_share_middle$exp_share
  share_of_food[j,4] <-food_share_higher$exp_share
  
  share_of_food[j,5] <-food_share_all$exp_share
  #Ratio of highest to lowest
  share_of_food[j,6] <-share_of_food[j,4]/share_of_food[j,1]
}
share_of_food<- data.frame(share_of_food)
share_of_food$Country<- row.names(share_of_food)
share_of_food$iso3c<- countrycode(share_of_food$Country, origin = 'country.name', destination = 'iso3c')
share_of_food_N<-share_of_food[,c(5,7,8)]
share_of_food_N<-data.frame(share_of_food_N)
final$food_sector<- ifelse(final$Product_or_Service%in% Food_products2, 1,0)


Food_products3<-c("Fresh or Chilled Fruit","Fresh or Chilled Vegetables Other than Potatoes",	"Fresh or Chilled Potatoes", "Other Cereals, Flour and Other Products", "Other Edible Oil and Fats", "Rice" )
final3$PPP <- as.numeric(sub(",", ".",final3$PPP))
final5<-final3[which(final3$Product_or_Service %in% Food_products3),]
collapse2 <- summaryBy(PPP ~ Consumption_Segment +Country, FUN=c(sum), data=final5)

## Focus on Rice and vegetables as main contributors: compute share of rice and vegetables in total expenditure and in food expenditure 
### Rice share in total expenditure
rice<- final3[which(final3$Product_or_Service=="Rice"),]
rice<- rice[,c(31, 32,34:36)]
rice_share<- left_join(collapse1, rice, by=c("Country", "Consumption_Segment"))
rice_share$rice_share<- rice_share$PPP/rice_share$PPP.sum
rice_share<-rice_share[,c(1,2,7)]
rice_share <- rice_share[order(rice_share$Country, -rice_share$rice_share),]
rice_share_t<-melt(rice_share)
countries<-unique(rice_share$Country)
share_of_rice_f=matrix(NA, nrow=length(countries_gcd), ncol=6)
colnames(share_of_rice_f)<- c("Lowest_rf", "Low_rf", "Middle_rf", "Higher_rf", "National_rf", "Ratio_rf")
row.names(share_of_rice_f)<- countries_gcd 
for (j in 1:length(countries))
{
  ## find all data entries for country j
  vec_country=which(rice_share_t$Country==countries[j])
  
  
  vec_lowest=which(rice_share_t$Consumption_Segment=="Lowest");
  lowest_match=match(vec_country,vec_lowest, FALSE)>0;
  share_of_rice_lowest=rice_share_t[vec_country[lowest_match],];
  
  
  ## Household consumption of low segment
  vec_low=which(rice_share_t$Consumption_Segment=="Low");
  low_match=match(vec_country,vec_low, FALSE)>0;
  share_of_rice_low=rice_share_t[vec_country[low_match],];
  
  ## Household consumption of middle segment
  vec_middle=which(rice_share_t$Consumption_Segment=="Middle");
  middle_match=match(vec_country,vec_middle, FALSE)>0;
  share_of_rice_middle=rice_share_t[vec_country[middle_match],];
  
  ## Household consumption of higher segment
  vec_higher=which(rice_share_t$Consumption_Segment=="Higher");
  higher_match=match(vec_country,vec_higher, FALSE)>0;
  share_of_rice_higher=rice_share_t[vec_country[higher_match],];
  
  vec_all=which(rice_share_t$Consumption_Segment=="All");
  all_match=match(vec_country,vec_all, FALSE)>0;
  share_of_rice_all=rice_share_t[vec_country[all_match],];
  
  share_of_rice_f[j,1] <-share_of_rice_lowest$value
  share_of_rice_f[j,2] <-share_of_rice_low$value
  share_of_rice_f[j,3] <-share_of_rice_middle$value
  share_of_rice_f[j,4] <-share_of_rice_higher$value
  
  share_of_rice_f[j,5] <-share_of_rice_all$value
  #Ratio of highest to lowest
  share_of_rice_f[j,6] <-share_of_rice_f[j,4]/share_of_rice_f[j,1]
}
share_of_rice_f<- data.frame(share_of_rice_f)
share_of_rice_f$Country<- row.names(share_of_rice_f)
share_of_rice_f$iso3c<- countrycode(share_of_rice_f$Country, origin = 'country.name', destination = 'iso3c')
### Vegetables share in total expenditure

veg<- final3[which(final3$Product_or_Service=="Fresh or Chilled Vegetables Other than Potatoes"),]
veg<- veg[,c(31, 32,34:36)]
veg_share<- left_join(collapse1, veg, by=c("Country", "Consumption_Segment"))
veg_share$veg_share<- veg_share$PPP/veg_share$PPP.sum
veg_share<-veg_share[,c(1,2,7)]
veg_share <- veg_share[order(veg_share$Country, -veg_share$veg_share),]
veg_share_t<-melt(veg_share)
share_of_veg_f=matrix(NA, nrow=length(countries), ncol=6)
colnames(share_of_veg_f)<- c("Lowest_vf", "Low_vf", "Middle_vf", "Higher_vf", "National_vf", "Ratio_vf")
row.names(share_of_veg_f)<-countries
for (j in 1:length(countries_gcd))
{
  vec_country=which(veg_share_t$Country==countries[j])
  vec_lowest=which(veg_share_t$Consumption_Segment=="Lowest");
  lowest_match=match(vec_country,vec_lowest, FALSE)>0;
  share_of_veg_lowest=veg_share_t[vec_country[lowest_match],];
  vec_low=which(veg_share_t$Consumption_Segment=="Low");
  low_match=match(vec_country,vec_low, FALSE)>0;
  share_of_veg_low=veg_share_t[vec_country[low_match],];
  vec_middle=which(veg_share_t$Consumption_Segment=="Middle");
  middle_match=match(vec_country,vec_middle, FALSE)>0;
  share_of_veg_middle=veg_share_t[vec_country[middle_match],];
  vec_higher=which(veg_share_t$Consumption_Segment=="Higher");
  higher_match=match(vec_country,vec_higher, FALSE)>0;
  share_of_veg_higher=veg_share_t[vec_country[higher_match],];
  vec_all=which(veg_share_t$Consumption_Segment=="All");
  all_match=match(vec_country,vec_all, FALSE)>0;
  share_of_veg_all=veg_share_t[vec_country[all_match],];
  share_of_veg_f[j,1] <-share_of_veg_lowest$value
  share_of_veg_f[j,2] <-share_of_veg_low$value
  share_of_veg_f[j,3] <-share_of_veg_middle$value
  share_of_veg_f[j,4] <-share_of_veg_higher$value
  share_of_veg_f[j,5] <-share_of_veg_all$value
  #Ratio of highest to lowest
  share_of_veg_f[j,6] <-share_of_veg_f[j,4]/share_of_veg_f[j,1]
}
share_of_veg_f<- data.frame(share_of_veg_f)
share_of_veg_f$Country<- row.names(share_of_veg_f)
share_of_veg_f$iso3c<- countrycode(share_of_veg_f$Country, origin = 'country.name', destination = 'iso3c')

rice_share<- final[which(final$Product_or_Service=="Rice"),]
rice_share<- left_join(collapse_all, rice, by=c("Country", "Consumption_Segment"))
rice_share$exp_share<-rice_share$PPP/rice_share$PPP.sum
rice_share <- rice_share[order(rice_share$Country, -rice_share$exp_share),]

share_of_rice=matrix(NA, nrow=length(countries_gcd), ncol=6)
colnames(share_of_rice)<- c("Lowest_r", "Low_r", "Middle_r", "Higher_r", "National_r", "Ratio_r")
row.names(share_of_rice)<- countries
for (j in 1:length(countries_gcd))
{
  ## find all data entries for country j
  vec_country=which(rice_share$Country==countries_gcd[j])
  
  
  vec_lowest=which(rice_share$Consumption_Segment=="Lowest");
  lowest_match=match(vec_country,vec_lowest, FALSE)>0;
  share_of_rice_lowest=rice_share[vec_country[lowest_match],];
  
  
  ## Household consumption of low segment
  vec_low=which(rice_share$Consumption_Segment=="Low");
  low_match=match(vec_country,vec_low, FALSE)>0;
  share_of_rice_low=rice_share[vec_country[low_match],];
  
  ## Household consumption of middle segment
  vec_middle=which(rice_share$Consumption_Segment=="Middle");
  middle_match=match(vec_country,vec_middle, FALSE)>0;
  share_of_rice_middle=rice_share[vec_country[middle_match],];
  
  ## Household consumption of higher segment
  vec_higher=which(rice_share$Consumption_Segment=="Higher");
  higher_match=match(vec_country,vec_higher, FALSE)>0;
  share_of_rice_higher=rice_share[vec_country[higher_match],];
  
  vec_all=which(rice_share$Consumption_Segment=="All");
  all_match=match(vec_country,vec_all, FALSE)>0;
  share_of_rice_all=rice_share[vec_country[all_match],];
  
  share_of_rice[j,1] <-share_of_rice_lowest$exp_share
  share_of_rice[j,2] <-share_of_rice_low$exp_share
  share_of_rice[j,3] <-share_of_rice_middle$exp_share
  share_of_rice[j,4] <-share_of_rice_higher$exp_share
  
  share_of_rice[j,5] <-share_of_rice_all$exp_share
  #Ratio of highest to lowest
  share_of_rice[j,6] <-share_of_rice[j,4]/share_of_rice[j,1]
}
share_of_rice<- data.frame(share_of_rice)
share_of_rice$Country<- row.names(share_of_rice)
share_of_rice$iso3c<- countrycode(share_of_rice$Country, origin = 'country.name', destination = 'iso3c')
veg<- final[which(final$Product_or_Service=="Fresh or Chilled Vegetables Other than Potatoes"),]
veg_share<- left_join(collapse_all, veg, by=c("Country", "Consumption_Segment"))
veg_share$exp_share<-veg_share$PPP/veg_share$PPP.sum

veg_share<- veg_share[order(veg_share$Country, -veg_share$exp_share),]
share_of_veg=matrix(NA, nrow=length(countries_gcd), ncol=6)
colnames(share_of_veg)<- c("Lowest_v", "Low_v", "Middle_v", "Higher_v", "National_v", "Ratio_v")
row.names(share_of_veg)<- countries_gcd 
for (j in 1:length(countries_gcd))
{
  ## find all data entries for country j
  vec_country=which(veg_share$Country==countries_gcd[j])
  
  
  vec_lowest=which(veg_share$Consumption_Segment=="Lowest");
  lowest_match=match(vec_country,vec_lowest, FALSE)>0;
  share_of_veg_lowest=veg_share[vec_country[lowest_match],];
  
  
  ## Household consumption of low segment
  vec_low=which(veg_share$Consumption_Segment=="Low");
  low_match=match(vec_country,vec_low, FALSE)>0;
  share_of_veg_low=veg_share[vec_country[low_match],];
  
  ## Household consumption of middle segment
  vec_middle=which(veg_share$Consumption_Segment=="Middle");
  middle_match=match(vec_country,vec_middle, FALSE)>0;
  share_of_veg_middle=veg_share[vec_country[middle_match],];
  
  ## Household consumption of higher segment
  vec_higher=which(veg_share$Consumption_Segment=="Higher");
  higher_match=match(vec_country,vec_higher, FALSE)>0;
  share_of_veg_higher=veg_share[vec_country[higher_match],];
  
  vec_all=which(veg_share$Consumption_Segment=="All");
  all_match=match(vec_country,vec_all, FALSE)>0;
  share_of_veg_all=veg_share[vec_country[all_match],];
  
  share_of_veg[j,1] <-share_of_veg_lowest$exp_share
  share_of_veg[j,2] <-share_of_veg_low$exp_share
  share_of_veg[j,3] <-share_of_veg_middle$exp_share
  share_of_veg[j,4] <-share_of_veg_higher$exp_share
  
  share_of_veg[j,5] <-share_of_veg_all$exp_share
  #Ratio of highest to lowest
  share_of_veg[j,6] <-share_of_veg[j,4]/share_of_veg[j,1]
}
share_of_veg<- data.frame(share_of_veg)
share_of_veg$Country<- row.names(share_of_veg)
share_of_veg$iso3c<- countrycode(share_of_veg$Country, origin = 'country.name', destination = 'iso3c')
### Rice share in food expenditure and as contributor to CS
share_ricef_N <- share_of_rice_f[,c(5,7,8)]
share_ricef_N<-data.frame(share_ricef_N)
share_rice_N<- share_of_rice[,c(5,7,8)]
share_rice_N<-data.frame(share_rice_N)
share_cs_rice_N<- share_cs_rice[,c(1,3,4)]
colnames(share_cs_rice_N)<-c("Country", "CS_rice","iso3c")
share_cs_rice_N<-data.frame(share_cs_rice_N)
### Vegetables share in food expenditure and as contributor to CS
share_vegf_N <- share_of_veg_f[,c(5,7,8)]
share_vegf_N<-data.frame(share_vegf_N)
share_veg_N<- share_of_veg[,c(5,7,8)]
share_veg_N<-data.frame(share_veg_N)
share_cs_veg_N<- share_cs_veg[,c(1,3,4)]
colnames(share_cs_veg_N)<-c("Country", "CS_veg","iso3c")
share_cs_veg_N<-data.frame(share_cs_veg_N)

## Compute correlations between share of rice in food expenditure and that of vegetables

correl_data<- left_join(share_ricef_N, share_rice_N, by= c("iso3c"))
colnames(correl_data)<-c("National_rf", "Country" ,  "iso3c"   ,  "National_r" , "Country.y"  )
correl_data<- full_join(share_cs_rice_N, correl_data, by=  c("Country","iso3c"))
correl_data<-correl_data[,-6]
correl_datav<- left_join( share_vegf_N, share_veg_N,by=  c("iso3c"))
colnames(correl_datav)<-c("National_vf", "Country" ,  "iso3c"   ,    "National_v" , "Country.y"  )
correl_datav<-correl_datav[,-5]
correl_datav<- full_join(correl_datav,share_cs_veg_N , by=  c("Country","iso3c"))
correl_data<- full_join(correl_data, correl_datav, by=  c("Country","iso3c"))
correl_data<- full_join(correl_data, share_of_food_N,by=  c("Country","iso3c"))

correl_data<-correl_data[,c(1,3,4,5,2,6,7,8,9)]

mat_national<- correl_data[,c(3:9)]

cor.mat<-rcorr(as.matrix(mat_national), type = "pearson")

M <- cor.mat$r

kable(round(M,3), "simple")
p_mat <- cor.mat$P
corrplot(M, type = "lower", order = "hclust",  addCoef.col = "black", tl.col = "darkblue",tl.cex = 0.9,
         p.mat = p_mat, sig.level = 0.05, diag = FALSE )


chart.Correlation(mat_national, histogram = TRUE, pch = 19)
# Get correlations but for lowest income group
## Rice share in lowest income group 
share_ricef_L <- share_of_rice_f[,c(1,7,8)]
share_ricef_L<-data.frame(share_ricef_L)
share_rice_L<- share_of_rice[,c(1,7,8)]
share_rice_L<-data.frame(share_rice_L)
## Vegetables share in lowest income group 
share_vegf_L <- share_of_veg_f[,c(1,7,8)]
share_vegf_L<-data.frame(share_vegf_L)
share_veg_L<- share_of_veg[,c(1,7,8)]
share_veg_L<-data.frame(share_veg_L)
## Food share in lowest income group
share_of_food_L<-share_of_food[,c(1,7,8)]
share_of_food_L<-data.frame(share_of_food_L)
Lcorrel_data<- left_join(share_ricef_L, share_rice_L, by= c("iso3c"))
colnames(Lcorrel_data)<-c("Lowest_rf", "Country" ,  "iso3c"   ,    "Lowest_r" , "Country.y"  )
Lcorrel_data<-Lcorrel_data[,-5]
Lcorrel_datav<- left_join( share_vegf_L, share_veg_L,by=  c("iso3c"))
colnames(Lcorrel_datav)<-c("Lowest_vf", "Country" ,  "iso3c"   ,    "Lowest_v" , "Country.y"  )
Lcorrel_datav<-Lcorrel_datav[,-5]
Lcorrel_data<- full_join(Lcorrel_data, Lcorrel_datav, by=  c("Country","iso3c"))
Lcorrel_data<- full_join(Lcorrel_data, share_of_food_L,by=  c("Country","iso3c"))
Lcorrel_data<-Lcorrel_data[,c(2,3,1,4,5,6,7)]
mat_lowest<- Lcorrel_data[,c(3:7)]
cor.mat<-rcorr(as.matrix(mat_lowest), type = "pearson")
M <- cor.mat$r
kable(round(M,3), "simple")
p_mat <- cor.mat$P
corrplot(M, type = "lower", order = "hclust",  addCoef.col = "black", tl.col = "darkblue",tl.cex = 0.9,
         p.mat = p_mat, sig.level = 0.05, diag = FALSE )

#####################################################################################################################
# Regression Analysis
## Clean and structure data for analysis
reg_data<- right_join(gini_coefficient, GDP_country, by=c("iso3c"))
names(reg_data)[names(reg_data) == "Country.y"] <- "Country2"
names(reg_data)[names(reg_data) == "Country.x"] <- "Country"
reg_data$Country[reg_data$Country== "Northern Macedonia"] <- "Macedonia, FYR"
reg_data<-reg_data[,-9]
reg_data<-left_join(reg_data, correl_data, by= c("iso3c"))
names(reg_data)[names(reg_data) == "Country.y"] <- "Country"
names(reg_data)[names(reg_data) == "Country.x"] <- "Country2"
reg_data<-reg_data[,-1]
reg_data<-left_join(reg_data, Lcorrel_data, by= c("iso3c"))
reg_data<-left_join(reg_data, sectors_share, by= c("iso3c"))
reg_data$SSA <- ifelse(reg_data$subregioncode == 'Sub-Saharan Africa', 1, 0)
reg_data$EAP <- ifelse(reg_data$subregioncode == 'East Asia & Pacific', 1, 0)
reg_data$ECA <- ifelse(reg_data$subregioncode == 'Europe & Central Asia', 1, 0)
reg_data$LAC <- ifelse(reg_data$subregioncode == 'Latin America & Caribbean', 1, 0)
reg_data$MENA <- ifelse(reg_data$subregioncode == 'Middle East & North Africa', 1, 0)
reg_data$SA <- ifelse(reg_data$subregioncode == 'South Asia', 1, 0)
reg_data<- left_join(reg_data, data_share_food_exp, by=c("iso3c"))
reg_data<- inner_join(reg_data, latitude, by=c("iso3c"))
names(reg_data)[names(reg_data) == "Country.y"] <- "Country"
reg_data$Alles<- as.numeric(sub(",", ".",reg_data$Alles))
reg_data$Lowest<- as.numeric(sub(",", ".",reg_data$Lowest))
reg_data$Low<- as.numeric(sub(",", ".",reg_data$Low))
reg_data$Middle<- as.numeric(sub(",", ".",reg_data$Middle))
reg_data$Higher<- as.numeric(sub(",", ".",reg_data$Higher))
reg_data$X2019= reg_data$X2019/1000

## Regression analysis with consumption elasticity as dependent variable
### National level
regression1<- lm(reg_data, formula = alpha2~  X2019+National_rf+National_vf+giniWB+ SSA+EAP+ ECA+ LAC+ MENA  )
summary(regression1)
stargazer(regression1, type= "html", out="regression_a1.html")
regression2<- lm(reg_data, formula = alpha2~  X2019+ National_r+National_v+ giniWB+ SSA+EAP+ ECA+ LAC+ MENA  )
summary(regression2)
stargazer(regression2, type= "html", out="regression_2.html")
regression3<- lm(reg_data, formula = alpha2~ X2019+CS_rice+CS_veg+ giniWB+ SSA+EAP+ ECA+ LAC+ MENA  )
summary(regression3)
stargazer(regression3, type= "html", out="regression_3.html")
### Lowest income group
regression1L<- lm(reg_data, formula = alpha2~  X2019+Lowest_rf+ Lowest_vf+ giniWB+ SSA+EAP+ ECA+ LAC+ MENA  )
summary(regression1L)
stargazer(regression1L, type= "html", out="regression1L.html")
regression2L<- lm(reg_data, formula = alpha2~ X2019+Lowest_r+ Lowest_v+ giniWB+ SSA+EAP+ ECA+ LAC+ MENA  )
summary(regression2L)
stargazer(regression2L, type= "html", out="regression2L.html")
regression3L<- lm(reg_data, formula = alpha2~ X2019+ CS_rice+CS_veg+ giniWB+ SSA+EAP+ ECA+ LAC+ MENA  )
summary(regression3L)
stargazer(regression3L, type= "html", out="regression3L.html")

## Regression analysis with CS as share of expenditure as dependent variable:

# Create data frame to use for regression

colnames(CS_change_exp)<-c("Lowest_CS.exp", "Low_CS.exp", "Middle_CS.exp", "Higher_CS.exp", "National_CS.exp", "Regressivity", "Country","iso3c")
reg_data<-reg_data[,-c(17,29,42,48)]
reg_data2<-inner_join(reg_data, CS_change_exp, by= c("iso3c"))
names(reg_data)[names(reg_data) == "Country.x"] <- "Country"

### National level
regression1<- lm(reg_data2, formula = National_CS.exp~  X2019+National_vf+National_rf+
                   giniWB+ SSA+EAP+ ECA+ LAC+ MENA  )
summary(regression1)
stargazer(regression1, type= "html", out="regression1N.html")
regression2<- lm(reg_data2, formula =National_CS.exp~  X2019+ National_v+National_r+
                   giniWB+ SSA+EAP+ ECA+ LAC+ MENA  )
summary(regression2)
stargazer(regression2, type= "html", out="regression2N.html")
regression3<- lm(reg_data2, formula = National_CS.exp~  X2019 +CS_rice+CS_veg+
                   giniWB+ SSA+EAP+ ECA+ LAC+ MENA  )
summary(regression3)
stargazer(regression3, type= "html", out="regression3N.html")

### Lowest income group
regression1_L<- lm(reg_data2, formula = Lowest_CS.exp~  X2019+ Lowest_vf+Lowest_rf+
                     giniWB+ SSA+EAP+ ECA+ LAC+ MENA  )
summary(regression1_L)
stargazer(regression1_L, type= "html", out="regression1_L.html")
regression2_L<- lm(reg_data2, formula = Lowest_CS.exp~  X2019+ Lowest_v+Lowest_r+
                     giniWB+ SSA+EAP+ ECA+ LAC+ MENA  )
summary(regression2_L)
regression3_L<- lm(reg_data2, formula = Lowest_CS.exp~  X2019+CS_veg+CS_rice+
                     giniWB+ SSA+EAP+ ECA+ LAC+ MENA  )
summary(regression3_L)
stargazer(regression3_L, type= "html", out="regression3_L.html")

## Latitude: split by hemisphere 

reg_data$latitudeN<-ifelse(reg_data$iso3c %in% latitude_n$iso3c, 1,0)
reg_data_latitudeN<-filter(reg_data, latitudeN==1)
reg_data_latitudeS<-filter(reg_data, latitudeN==0)
regression1_latN<- lm(reg_data_latitudeN, formula = alpha2~  X2019+National_rf+National_vf+ giniWB+ SSA+EAP+ ECA+ LAC+ MENA +Latitude )
summary(regression1_latN)
stargazer(regression1_latN, type= "html", out="regression_1latN.html")
regression1_latS<- lm(reg_data_latitudeS, formula = alpha2~  X2019+National_rf+National_vf+ giniWB+ SSA+LAC+ EAP+ ECA+ MENA+ Latitude  )
summary(regression1_latS)
stargazer(regression1_latS, type= "html", out="regression_1latS.html")

reg_data2$latitudeN<-ifelse(reg_data2$iso3c %in% latitude_n$iso3c, 1,0)
reg_data2_latitudeN<-filter(reg_data2, latitudeN==1)
reg_data2_latitudeS<-filter(reg_data2, latitudeN==0)

regression_latN<- lm(reg_data2_latitudeN, formula = National_CS.exp~  X2019+National_vf+National_rf+
                       giniWB+ SSA+EAP+ ECA+ LAC+ MENA +Latitude )
summary(regression_latN)
stargazer(regression_latN, type= "html", out="regressionN_latN.html")

regression_latS<- lm(reg_data2_latitudeS, formula = National_CS.exp~  X2019+National_vf+National_rf+
                       giniWB+ SSA+LAC+EAP+ ECA+ MENA+Latitude  )
summary(regression_latS)
stargazer(regression_latS, type= "html", out="regressionN_latS.html")
##############################################################################################
#Plots

collapse_food <- summaryBy(PPP ~ Consumption_Segment +Product_or_Service, FUN=c(sum), data=final5)
names(collapse_food)[names(collapse_food) == "PPP.sum"] <- "Expenditure_ppp"
collapse4 <- summaryBy(PPP ~ Consumption_Segment, FUN=c(sum), data=final5)

collapse_share<- left_join(collapse4, collapse_food, by ="Consumption_Segment")
collapse_share$sector_share= collapse_share$Expenditure_ppp/collapse_share$PPP.sum
share_all<-collapse_share[which(collapse_share$Consumption_Segment=="All"),]
share_lowest<-collapse_share[which(collapse_share$Consumption_Segment=="Lowest"),]
share_higher<-collapse_share[which(collapse_share$Consumption_Segment=="Higher"),]

share_all <- share_all %>% 
  arrange(desc(Product_or_Service)) %>%
  mutate(prop = sector_share *100) %>%
  mutate(ypos = cumsum(prop)-0.5*prop )

plot_share<-ggplot(share_all, aes( x="",  y=prop, fill=Product_or_Service)) + geom_bar(stat="identity", width=1)+  xlab("")+ylab("sectoral share of food expenditure")+coord_polar("y",start=0)+  theme_void() +  theme(legend.position="none") +  geom_text(aes(y=ypos, label = round(prop,2)), color = "white", size=5) 
plot_share+theme_minimal()+theme(legend.position = "bottom")+ scale_fill_manual(labels = c("Fruit", "Potatoes", "Vegetables", "Cereals and Flour", "Oils & Fats", "Rice"), values = c("#FF33CC", "#996633", "#339900", "#FFCC00", "#000000", "#0099FF"), name= "Sector")+labs(colour = "Sector")+theme(legend.position = "bottom")


plot_share<-ggplot(share_all, aes( x="",  y=prop, fill=Product_or_Service)) + geom_bar(stat="identity", width=0.5)+  xlab("")+ylab("sectoral share of food expenditure")+  theme(legend.position="none") +  geom_text(aes(y=ypos, label = round(prop,2)), color = "white", size=5) 
plot_share+theme_minimal()+theme(legend.position = "bottom")+ scale_fill_manual(labels = c("Fruit", "Potatoes", "Vegetables", "Cereals and Flour", "Oils & Fats", "Rice"), values = c("#FF33CC", "#996633", "#339900", "#FFCC00", "#000000", "#0099FF"), name= "Sector")+labs(colour = "Sector")+theme(legend.position = "bottom")


share_lowest <- share_lowest %>% 
  arrange(desc(Product_or_Service)) %>%
  mutate(prop = sector_share *100) %>%
  mutate(ypos = cumsum(prop)-0.5*prop )

plot_sharel<-ggplot(share_lowest, aes( x="",  y=prop, fill=Product_or_Service)) + geom_bar(stat="identity", width=1)+  xlab("")+ylab("sectoral share of food expenditure (lowest)")+coord_polar("y",start=0)+  theme_void() +  theme(legend.position="none") +  geom_text(aes(y=ypos, label = round(prop,2)), color = "white", size=5) 
plot_sharel+theme_minimal()+theme(legend.position = "bottom")+ scale_fill_manual(labels = c("Fruit", "Potatoes", "Vegetables", "Cereals and Flour", "Oils & Fats", "Rice"), values = c("#FF33CC", "#996633", "#339900", "#FFCC00", "#000000", "#0099FF"), name= "Sector")+labs(colour = "Sector")+theme(legend.position = "bottom")


plot_sharel<-ggplot(share_lowest, aes( x="",  y=prop, fill=Product_or_Service)) + geom_bar(stat="identity", width=1)+  xlab("")+ylab("sectoral share of food expenditure (lowest)") +  theme(legend.position="none") +  geom_text(aes(y=ypos, label = round(prop,2)), color = "white", size=5) 
plot_sharel+theme_minimal()+theme(legend.position = "bottom")+ scale_fill_manual(labels = c("Fruit", "Potatoes", "Vegetables", "Cereals and Flour", "Oils & Fats", "Rice"), values = c("#FF33CC", "#996633", "#339900", "#FFCC00", "#000000", "#0099FF"), name= "Sector")+labs(colour = "Sector")+theme(legend.position = "bottom")

share_higher <- share_higher %>% 
  arrange(desc(Product_or_Service)) %>%
  mutate(prop = sector_share *100) %>%
  mutate(ypos = cumsum(prop)-0.5*prop )

plot_shareh<-ggplot(share_higher, aes( x="",  y=prop, fill=Product_or_Service)) + geom_bar(stat="identity", width=1)+  xlab("")+ylab("sectoral share of food expenditure (higher)")+coord_polar("y",start=0)+  theme_void() +  theme(legend.position="none") +  geom_text(aes(y=ypos, label = round(prop,2)), color = "white", size=5) 
plot_shareh+theme_minimal()+theme(legend.position = "bottom")+ scale_fill_manual(labels = c("Fruit", "Potatoes", "Vegetables", "Cereals and Flour", "Oils & Fats", "Rice"), values = c("#FF33CC", "#996633", "#339900", "#FFCC00", "#000000", "#0099FF"), name= "Sector")+labs(colour = "Sector")+theme(legend.position = "bottom")


plot_shareh<-ggplot(share_higher, aes( x="",  y=prop, fill=Product_or_Service)) + geom_bar(stat="identity", width=1)+  xlab("")+ylab("sectoral share of food expenditure (higher)")+  theme(legend.position="none") +  geom_text(aes(y=ypos, label = round(prop,2)), color = "white", size=5) 
plot_shareh+theme_minimal()+theme(legend.position = "bottom")+ scale_fill_manual(labels = c("Fruit", "Potatoes", "Vegetables", "Cereals and Flour", "Oils & Fats", "Rice"), values = c("#FF33CC", "#996633", "#339900", "#FFCC00", "#000000", "#0099FF"), name= "Sector")+labs(colour = "Sector")+theme(legend.position = "bottom")


share_all2<-share_all[,c(1:7)]
shares<-bind_rows(share_all2, share_lowest,share_higher)
shares$Sector<-shares$Product_or_Service
plot_shares<-ggplot(shares, aes( x=Consumption_Segment,  y=prop, fill=Product_or_Service)) + geom_bar(stat="identity", width=0.5)+  xlab("")+ylab("share of expenditure")+   theme(legend.position="none") +  geom_text(aes(y=ypos, label = round(prop,2)), color = "white", size=4) 
plot_shares+theme_minimal()+theme(legend.position = "bottom")+ scale_fill_manual(labels = c("Fruit", "Potatoes", "Vegetables", "Cereals and Flour", "Oils & Fats", "Rice"), values = c("#FF33CC", "#996633", "#339900", "#FFCC00", "#000000", "#0099FF"), name= "Sector")+labs(colour = "Sector")+theme(legend.position = "bottom")
##################################################################################################
#Extras:

Food_products2<-c("Fresh or Chilled Fruit","Fresh or Chilled Vegetables Other than Potatoes",	"Fresh or Chilled Potatoes", "Other Cereals, Flour and Other Products", "Other Edible Oil and Fats", "Rice" )
final6<-final4[which(final4$Product_or_Service %in% Food_products2),]
means <- aggregate(Price_Increase ~  Product_or_Service,final6, mean)
final6<-left_join(final6, means, by="Product_or_Service")
final6$Price_Increase.x= final6$Price_Increase.x-1
final6$Price_Increase.y= final6$Price_Increase.y-1

plot_prices<-ggplot(final6, aes(x=Product_or_Service, y=Price_Increase.x)) +
  geom_boxplot()+xlab("")+ylab("Price increase")+
  stat_summary(fun="mean", color="blue", shape=15)
plot_prices+theme_minimal()+ scale_x_discrete(labels=c("Fruits","Potatoes", "Vegetables", "Cereals", "Oils and Fats", "Rice"))+  geom_text(data = final6, aes(label=ifelse(final6$Consumption_Segment=="Middle"& Country=="Nepal",round(final6$Price_Increase.y,3),''), y = Price_Increase.y), nudge_y= 0.02, nudge_x=0.1,  color= "blue")+theme(axis.text=element_text(size=12))

rice_all<-rice[which(rice$Consumption_Segment=="All"),]
rice_all$Price_Increase= (rice_all$Price_Increase-1)*100
rice_all<-rice_all%>%arrange(desc(Price_Increase))
your_order <- order(-rice_all$Price_Increase)
rice_all$Country <- factor(rice_all$Country, levels = rice_all$Country[your_order])
plot_rice_prices<-ggplot(rice_all, aes(x=Country, y=Price_Increase)) +  geom_bar(stat = "identity")+xlab("Country")+ylab("Price increase")
plot_rice_prices+theme_minimal()+ geom_text(data = rice_all, aes(label=ifelse(rice_all$Price_Increase>0.06,round(rice_all$Price_Increase,3),"")), y = rice_all$Price_Increase-0.01, color= "white", size =4, angle = 90)+theme(axis.text=element_text(size=10, angle= 90))+geom_hline(yintercept = 4.78, color= "blue")

veg_all<-veg[which(veg$Consumption_Segment=="All"),]
veg_all$Price_Increase=as.numeric(veg_all$Price_Increase)
veg_all$Price_Increase= as.numeric((veg_all$Price_Increase-1)*100)

veg_all<-veg_all%>%arrange(desc(Price_Increase))
your_order <- order(-veg_all$Price_Increase)
veg_all$Country <- factor(veg_all$Country, levels = veg_all$Country[your_order])
plot_rice_prices<-ggplot(veg_all, aes(x=Country, y=Price_Increase)) +  geom_bar(stat = "identity")+xlab("Country")+ylab("Price increase")
plot_rice_prices+theme_minimal()+ geom_text(data = veg_all, aes(label=ifelse(veg_all$Price_Increase>0.06,round(veg_all$Price_Increase,3),"")), y = veg_all$Price_Increase-0.008, color= "white", size =4, angle = 90)+theme(axis.text=element_text(size=10, angle= 90))+geom_hline(yintercept = 4.83, color= "blue")

