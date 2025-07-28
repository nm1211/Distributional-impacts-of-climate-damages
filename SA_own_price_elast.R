remove(list = ls())
setwd("")

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

gcd_raw=read.csv(file="HH_consumption_GCD.csv", sep=";")
price_vec=cons_consumptionpatterns_increases_in_prices_through_system_world_market <- read_excel("cons_consumptionpatterns_increases_in_prices_through_system_world_market.xlsx")
price_vec = t(price_vec)
colnames(price_vec) <- price_vec[1,]
price_vec<-price_vec[-1,]
price_vec<- as.data.frame(price_vec)
price_vec$i..GTAPsector<- row.names(price_vec)
names(price_vec)[names(price_vec) == "i..GTAPsector"] <- "GTAPsector"
own_price_elasticity <- read_excel("own price elasticity.xlsx", 
                                   sheet = "Slutsky")
gcd_mapper <- read.csv(file="Appendix_Mapping_GCD_GTAP.csv", sep=";")
country_code_GTAP <- read.csv(file = "Codes_Countries.csv", sep=";")
pop_GCD <- read_excel( "GCD_pop_perc.xlsx")
pop_GCD<-pop_GCD[,-c(1,3)]
pop_GCD<-melt(pop_GCD, id.vars=c("Country"))
names(pop_GCD)[names(pop_GCD) == "variable"] <- "Consumption.Segment"

own_price_elasticity<- own_price_elasticity[, c(1:2)]
names(own_price_elasticity)[names(own_price_elasticity) == "...1"] <- "Country"
elasticity <- melt(own_price_elasticity, id.vars = c("Country"))
names(elasticity)[names(elasticity) == "value"] <- "elasticity"
names(elasticity)[names(elasticity) == "variable"] <- "elast.prod"
elasticity$elasticity<-gsub("a","",as.character(elasticity$elasticity))
elasticity$elasticity<-gsub("b","",as.character(elasticity$elasticity))
elasticity$elasticity<- as.numeric(elasticity$elasticity)
elasticity<-elasticity[which(elasticity$elast.prod!= "Per capita food"),]
summary(elasticity$elasticity)

GCD <- merge(gcd_raw,gcd_mapper, by = "Product.or.Service")
GCD_GTAP <- merge(GCD, country_code_GTAP, by = "Country")
result <- merge(GCD_GTAP, price_vec, by = "GTAPsector")
result <- left_join(result, pop_GCD, by =c("Country", "Consumption.Segment"))

result$priceincrease=result[cbind(seq_len(nrow(result)),match(result$Country_Code, colnames(result)))]
final <- data.frame(result$Country, result$GTAPsector, result$Consumption.Segment,result$Product.or.Service, result$X.PPP, result$priceincrease)
colnames(final)<- c("Country","GTAP_Sector","Consumption_Segment","Product_or_Service","PPP","Price_Increase") 
final <- final[order(final$Product_or_Service),]

final$elast.prod <-NA
final$elast.prod[which(final$GTAP_Sector == "omt")]<-"Food, beverages, & tobacco"
final$elast.prod[which(final$GTAP_Sector == "cmt")]<- "Food, beverages, & tobacco"
final$elast.prod[which(final$GTAP_Sector == "mil")]<-"Food, beverages, & tobacco"
final$elast.prod[which(final$Product_or_Service == "Fresh, Chilled or Frozen Fish and Seafood")]<-"Food, beverages, & tobacco"
final$elast.prod[which(final$Product_or_Service =="Preserved or Processed Fish and Seafood")]<-"Food, beverages, & tobacco"
final$elast.prod[which(final$GTAP_Sector == "v_f")] <-"Food, beverages, & tobacco"
final$elast.prod[which(final$Product_or_Service =="Frozen, Preserved or Processed Fruit and Fruit-based Product")]<-"Food, beverages, & tobacco"
final$elast.prod[which(final$Product_or_Service =="Frozen, Preserved or Processed Vegetables and Vegetable-based Product")] <-"Food, beverages, & tobacco"
final$elast.prod[which(final$GTAP_Sector == "vol")]<-"Food, beverages, & tobacco"
final$elast.prod[which(final$GTAP_Sector == "b_t")]<-"Food, beverages, & tobacco"
final$elast.prod[which(final$GTAP_Sector == "oap")]<- "Food, beverages, & tobacco"
final$elast.prod[which(final$GTAP_Sector == "pdr")]<- "Food, beverages, & tobacco"
final$elast.prod[which(final$GTAP_Sector == "sgr")]<- "Food, beverages, & tobacco"
final$elast.prod[which(final$Product_or_Service == "Other Cereals, Flour and Other Products")] <- "Food, beverages, & tobacco"
final$elast.prod[which(final$GTAP_Sector == "ofd" & final$elast.prod =="")] <-"Food, beverages, & tobacco"
final$elast.prod[which(final$Product_or_Service == "Bread")] <- "Food, beverages, & tobacco"
final$elast.prod[which(final$Product_or_Service == "Other Bakery Products")] <- "Food, beverages, & tobacco"
final2<-left_join(final, elasticity, by = c("Country", "elast.prod"))
final2<-final2[complete.cases(final2[,7]),]

per_capita_consumption <- read.csv(file = "per_capita_consumption_aggregated.csv", sep=";")

GDP_country <- read.csv(file = "GDP_per_capita_PPP.csv", sep = ";")
GDP_country$X2019 <- as.numeric(sub(",", ".", GDP_country$X2019))
GDP_country$iso3c<-countrycode(GDP_country$Country, origin = 'country.name', destination = 'iso3c')

gini_coefficient <- read_excel("gini_coefficient.xls")
names(gini_coefficient)[names(gini_coefficient) == "country"] <- "Country"
gini_coefficient$iso3c<-countrycode(gini_coefficient$Country, origin = 'country.name', destination = 'iso3c')


latitude <- read.csv(file= "Latitude.csv", sep=";")
latitude$Latitude <- as.numeric(sub(",", ".", latitude$Latitude))
latitude$iso3c<-countrycode(latitude$Country, origin = 'country.name', destination = 'iso3c', custom_match=c('Rawanda' = 'RWA'))

options(scipen = 999) 
final2$PPP = as.numeric(sub(",", ".", final2$PPP, fixed= TRUE))
final2$Price_Increase = as.numeric(sub(",", ".", final2$Price_Increase, fixed = TRUE))
final2$elasticity = as.numeric(sub(",", ".", final2$elasticity, fixed = TRUE))
final2["Addl_expenditure_share"]<-NA
final2$Price_Increase <- final2$Price_Increase-1.0
hist(final2$Price_Increase)
final2$change_in_demand= (final2$elasticity) * final2$Price_Increase
hist(final2$change_in_demand)
final2$Addl_expenditure_D = final2$PPP * (final2$change_in_demand)
hist(final2$Addl_expenditure_D)
final2$Addl_expenditure_P= final2$PPP * (final2$Price_Increase)
hist(final2$Addl_expenditure_P)
options(scipen = 999) 
final2$Total_expenditure_after_price= final2$PPP * (final2$Price_Increase+1)*(final2$change_in_demand+1)
hist(final2$Total_expenditure_after_price)
final2$change_expenditure = (final2$Total_expenditure_after_price- final2$PPP)
final2$Addl_expenditure_share = (final2$change_expenditure/final2$PPP)
hist(final2$change_expenditure)
hist(final2$Addl_expenditure_share)
final2$change_CS = (final2$PPP/(1+final2$elasticity))*(1- (final2$Price_Increase+1)^(final2$elasticity+1)) 
hist(final2$change_CS)
final2$regressivity= final2$change_CS/ final2$PPP                                             
hist(final2$regressivity)
countries_gcd<-unique(final2$Country)
nr_products <- unique(final2$Product_or_Service)


Total_expenditures_before_price_change=matrix(NA, nrow=length(countries_gcd), ncol=4)
colnames(Total_expenditures_before_price_change)<- c("Lowest", "Low", "Middle", "Higher")
row.names(Total_expenditures_before_price_change)<-countries_gcd


Total_expenditures_after_price_change=matrix(NA, nrow=length(countries_gcd), ncol=4)
colnames(Total_expenditures_after_price_change)<- c("Lowest", "Low", "Middle", "Higher")
row.names(Total_expenditures_after_price_change)<-countries_gcd


Total_addl_expenditures=matrix(NA, nrow=length(countries_gcd), ncol=4)
colnames(Total_addl_expenditures)<- c("Lowest", "Low", "Middle", "Higher")
row.names(Total_addl_expenditures)<- countries_gcd 


change_CS=matrix(NA, nrow=length(countries_gcd), ncol=4)
colnames(change_CS)<- c("Lowest", "Low", "Middle", "Higher")
row.names(change_CS)<- countries_gcd

regressivity=matrix(NA, nrow=length(countries_gcd), ncol=4)
colnames(regressivity)<- c("Lowest", "Low", "Middle", "Higher")
row.names(regressivity)<- countries_gcd

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
  vec_country=which(final2$Country==countries_gcd[j])
  
  ## Household consumption of lowest segment for the country
  vec_lowest=which(final2$Consumption_Segment=="Lowest");
  lowest_match=match(vec_country,vec_lowest, FALSE)>0;
  consumption_country_lowest=final2[vec_country[lowest_match],];
  
  ## Household consumption of low segment
  vec_low=which(final2$Consumption_Segment=="Low");
  low_match=match(vec_country,vec_low, FALSE)>0;
  consumption_country_low=final2[vec_country[low_match],];
  
  ## Household consumption of middle segment
  vec_middle=which(final2$Consumption_Segment=="Middle");
  middle_match=match(vec_country,vec_middle, FALSE)>0;
  consumption_country_middle=final2[vec_country[middle_match],];
  
  ## Household consumption of higher segment
  vec_higher=which(final2$Consumption_Segment=="Higher");
  higher_match=match(vec_country,vec_higher, FALSE)>0;
  consumption_country_higher=final2[vec_country[higher_match],];
  
##################################
###Calculate/ attribute data for total expenditures - before and after and total additional expenditures
##################################
  
  ### Total expenditures for each income group and country BEFORE the price change
  Total_expenditures_before_price_change[j,1] <-sum(consumption_country_lowest$PPP)
  Total_expenditures_before_price_change[j,2] <-sum(consumption_country_low$PPP)
  Total_expenditures_before_price_change[j,3] <-sum(consumption_country_middle$PPP)
  Total_expenditures_before_price_change[j,4] <-sum(consumption_country_higher$PPP)
  
  ### Total additional expenditures for each group and country
  Total_addl_expenditures[j, 1]=sum(consumption_country_lowest$change_expenditure, na.rm=T) 
  Total_addl_expenditures[j, 2]=sum(consumption_country_low$change_expenditure, na.rm=T)
  Total_addl_expenditures[j, 3]=sum(consumption_country_middle$change_expenditure, na.rm=T)
  Total_addl_expenditures[j, 4]=sum(consumption_country_higher$change_expenditure, na.rm=T)
  
  ### Total expenditures for each income group AFTER price change
  Total_expenditures_after_price_change[j,1] <-sum(consumption_country_lowest$Total_expenditure_after_price,na.rm=T)
  Total_expenditures_after_price_change[j,2] <-sum(consumption_country_low$Total_expenditure_after_price, na.rm=T)
  Total_expenditures_after_price_change[j,3] <-sum(consumption_country_middle$Total_expenditure_after_price, na.rm=T)
  Total_expenditures_after_price_change[j,4] <-sum(consumption_country_higher$Total_expenditure_after_price, na.rm=T)
  
  ### Change in Consumer surplus for each group and country
  change_CS[j,1] <-sum(consumption_country_lowest$change_CS,na.rm=T)
  change_CS[j,2] <-sum(consumption_country_low$change_CS, na.rm=T)
  change_CS[j,3] <-sum(consumption_country_middle$change_CS, na.rm=T)
  change_CS[j,4] <-sum(consumption_country_higher$change_CS, na.rm=T)
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
  Share_good_on_change_CS_lowest[j,] <- consumption_country_lowest[,15]/change_CS[j,1]
  Share_good_on_change_CS_higher[j,] <- consumption_country_higher[,15]/change_CS[j,4]
}

CS_change_exp <- data.frame(CS_change_exp[order(rownames(CS_change_exp)),])
CS_change_exp$Country<-row.names(CS_change_exp)
CS_change_exp$iso3c<-countrycode(CS_change_exp$Country, origin = 'country.name', destination = 'iso3c')

#some parts of the analysis will exclude countries without higher income group 
remove_countries <- c("Benin", "Burundi", "Guinea", "Liberia", "Nicaragua", "Senegal", "Togo")

Total_addl_expenditures <- data.frame(Total_addl_expenditures[order(rownames(Total_addl_expenditures)),])
Share_good_on_change_CS_lowest <- data.frame(Share_good_on_change_CS_lowest)
Share_good_on_change_CS_higher <- data.frame(Share_good_on_change_CS_higher)

CS_change_exp$difference<- ifelse(CS_change_exp$Higher>CS_change_exp$Lowest,1,0)
CS_change_exp$difference2<- ifelse(CS_change_exp$Middle>CS_change_exp$Lowest,1,0)
CS_change_exp$difference3<- ifelse(CS_change_exp$Low>CS_change_exp$Lowest,1,0)
CS_dataframe<-data.frame(CS_change_exp)

CS_dataframe<- CS_change_exp[order(CS_dataframe$National),]
CS_dataframe
CS_change_exp$Lowest<- abs(CS_change_exp$Lowest)
CS_change_exp$Low<- abs(CS_change_exp$Low)
CS_change_exp$Middle<- abs(CS_change_exp$Middle)
CS_change_exp$Higher<- abs(CS_change_exp$Higher)
CS_change_exp$National<- abs(CS_change_exp$National)

CS_change_exp
summary(CS_change_exp$National)
write.csv(CS_change_exp, "SlutskyCS_change_exp.csv")
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

convert_ln <-log(CS_change_exp[,1:6])
ln_CS_change_exp <- convert_ln
ln_CS_change_exp[order(rownames(ln_CS_change_exp)),]
ln_CS_change_exp_t <- t(ln_CS_change_exp)

df2 <- per_capita_consumption[!(per_capita_consumption$Sector=="Food and beverages" | per_capita_consumption$Sector=="Clothing and footwear" | per_capita_consumption$Sector=="Housing" | per_capita_consumption$Sector=="Energy" | per_capita_consumption$Sector=="Transport" | per_capita_consumption$Sector=="Water" | per_capita_consumption$Sector=="Education sector" | per_capita_consumption$Sector=="Health" | per_capita_consumption$Sector=="ICT" | per_capita_consumption$Sector=="Financial services" | per_capita_consumption$Sector=="Other goods and services" | per_capita_consumption$Sector=="Appliances\n "),]
df3 <- df2[!(row.names(df2) %in% c("13","26","39","52","65","78","91","104","117","130","143","156","169","182","195","208","221","234","247","260","273","286","299","312","325","338","351","364","377","390","403","416","429","442","455","468","481","494","507","520","533","546","559","572","585","598","611","624","637","650","663","676","689","702","715","728","741","754","767","780","793","806","819","832","845","858","871","884","897","910","923","936","949","962","975","988","1001","1014","1027","1040","1053","1066","1079","1092","1105","1118","1131","1144","1157","1170","1183")), ]
df4 <- subset(df3, select = -c(Sector, Alles))
df4 <- df4[-c(68), ]
df4$Lowest = as.numeric(sub(",", ".", df4$Lowest))
df4$Low = as.numeric(sub("," ,".", df4$Low))
df4$Middle = as.numeric(sub("," ,".", df4$Middle))
df4$Higher = as.numeric(sub("," ,".", df4$Higher))

ln_expenditure <- data.frame(log(df4$Lowest), log(df4$Low), log(df4$Middle), log(df4$Higher))
#country names checked and all is correct
row.names(ln_expenditure) <- c("Afghanistan","Albania","Armenia", "Azerbaijan", "Bangladesh", "Belarus", "Benin", "Bhutan", "Bolivia", "Bosnia.and.Herzegovina", "Brazil", "Bulgaria", "Burkina.Faso", "Burundi", "Cambodia", "Cameroon", "Cabo.Verde", "Chad", "China", "Colombia", "Congo..Dem..Rep.", "Congo..Rep.", "Cote.d.Ivoire", "Djibouti", "Egypt..Arab.Rep.", "El.Salvador", "Ethiopia","Fiji","Gabon", "Gambia..The", "Ghana", "Guatemala", "Guinea", "Honduras", "India", "Indonesia", "Iraq", "Jamaica", "Jordan", "Kazakhstan", "Kenya", "Kyrgyz.Republic", "Lao.PDR", "Latvia", "Lesotho", "Liberia", "Lithuania", "Macedonia..FYR", "Madagascar", "Malawi", "Maldives", "Mali", "Mauritania", "Mauritius", "Mexico", "Moldova", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Namibia", "Nepal", "Nicaragua", "Niger", "Nigeria", "Pakistan", "Papua.New.Guinea", "Peru", "Philippines", "Romania", "Russian.Federation", "Rwanda", "Sao.Tome.and.Principe", "Senegal", "Serbia", "Sierra.Leone", "South.Africa", "Sri.Lanka", "Swaziland", "Tajikistan", "Tanzania", "Thailand", "Timor.Leste", "Togo", "Turkey", "Uganda", "Ukraine", "Vietnam", "Yemen..Rep.", "Zambia")
colnames(ln_expenditure) <- c( "Lowest", "Low", "Middle", "Higher")
ln_expenditure <- ln_expenditure[order(rownames(ln_expenditure)),]
ln_expenditure_t <- t(ln_expenditure)

ln_expenditure_t <- data.frame(ln_expenditure_t)
ln_CS_change_exp_t <- data.frame(ln_CS_change_exp_t)

ln_expenditure<- cbind(Country= rownames(ln_expenditure), ln_expenditure)
ln_exp<-melt(ln_expenditure, id.vars=c("Country"))
names(ln_exp)[names(ln_exp) == "variable"] <- "Consumption_Segment"
names(ln_exp)[names(ln_exp) == "value"] <- "ln_expenditure"

ln_CS_change_exp<- cbind( ln_CS_change_exp, unique(ln_exp$Country))
names(ln_CS_change_exp)[names(ln_CS_change_exp) == "unique(ln_exp$Country)"] <- "Country"
ln_CS_change_exp<- as.data.frame(ln_CS_change_exp)
ln_CS_change_exp$Lowest<- as.numeric(ln_CS_change_exp$Lowest)
ln_CS_change_exp$Low<- as.numeric(ln_CS_change_exp$Low)
ln_CS_change_exp$Middle<- as.numeric(ln_CS_change_exp$Middle)
ln_CS_change_exp$Higher<- as.numeric(ln_CS_change_exp$Higher)
names(ln_CS_change_exp)[names(ln_CS_change_exp) == "V7"] <- "Country"

ln_CS_chng<-melt(ln_CS_change_exp, id.vars=c("Country"))
names(ln_CS_chng)[names(ln_CS_chng) == "variable"] <- "Consumption_Segment"
names(ln_CS_chng)[names(ln_CS_chng) == "value"] <- "ln_CS_change_exp"

trial<- merge(ln_exp, ln_CS_chng, by= c("Country", "Consumption_Segment"))
trial$ln_percapita_CS <- trial$ln_CS_change_exp+trial$ln_expenditure
trial<-trial[-which(trial$ln_CS_change_exp=="-Inf"),]
trial$iso3c<-countrycode(trial$Country, origin = 'country.name', destination = 'iso3c')
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


coef_main <- coef(summary(regression))["ln_expenditure", "Estimate"]
se_main <- coef(summary(regression))["ln_expenditure", "Std. Error"]

# Test the hypothesis that the coefficient is less than 1
t_stat <- (coef_main - 1) / se_main

p_value <- pt(t_stat, df = df.residual(regression))  # df.residual(model) gives the degrees of freedom
print(paste("t-statistic:", t_stat))
print(paste("p-value:", p_value))

if (p_value < 0.05) {
  print("Reject the null hypothesis: The coefficient is significantly less than 1")
} else {
  print("Fail to reject the null hypothesis: The coefficient is not significantly less than 1")
}

CS_change_exp$Country<-row.names(CS_change_exp)
CS_change_exp$iso3c<-countrycode(CS_change_exp$Country, origin = 'country.name', destination = 'iso3c')
trial2<- left_join(GDP_country, CS_change_exp , by= c("iso3c","Country"))
trial2<-trial2[-which(trial2$National==0),]
ggplot(trial2, aes(x=X2019, y= National,label= iso3c))+  geom_point() + labs(x = "GDP per capita (in $PPP 2019)", y = "Damage") + theme_minimal(base_size=18)+geom_text(hjust=0, vjust=0, size=5)+geom_smooth(method = "lm", se = FALSE)

lm_GDP_national <-lm(formula = trial2$National ~ trial2$X2019)
summary(lm_GDP_national)
lm_GDP_lowest <-lm(formula = trial2$Lowest ~ trial2$X2019)
summary(lm_GDP_lowest)
data_sum_CS<- ddply(final2,.(Consumption_Segment), summarise, mean_elast =mean(elasticity, na.rm=T), sum_CS = sum(change_CS, na.rm=T), mean_CS = mean(change_CS, na.rm =T), PPP=sum(PPP, na.rm =T))
data_sum_CS<-data_sum_CS[-1,]
data_sum_CS$Consumption_Segment<- factor(data_sum_CS$Consumption_Segment, levels=c("Lowest","Low","Middle","Higher"))
data_sum_CS$CS_exp<- data_sum_CS$sum_CS/data_sum_CS$PPP
data_sum_CS$total<- sum(data_sum_CS$sum_CS)
data_sum_CS$perc<- round(data_sum_CS$sum_CS/data_sum_CS$total,4)*100
write.csv(data_sum_CS, "SlutskyCS_segments_perc.csv")
data_sum_CS
ln_CS_change_exp_t<-ln_CS_change_exp_t[-c(5,6),]
ln_CS_change_exp_t[is.na(ln_CS_change_exp_t) | ln_CS_change_exp_t=="-Inf"] = -9999

df3$Alles = as.numeric(sub(",", ".", df3$Alles))
df3$iso3c<-countrycode(df3$Country, origin = 'country.name', destination = 'iso3c')
df3 <- df3[-c(68), ]

trial <- trial[, c(1,6,2,3,4,5)]
ln_CS_percapita <-dcast(trial, Country ~ Consumption_Segment)
#Using ln_percapita_CS as value column: use value.var to override.

ln_CS_percapita<-left_join(ln_expenditure, ln_CS_percapita, by= "Country")
ln_CS_percapita<-ln_CS_percapita[,c(1, 6:9)]
colnames(ln_CS_percapita) <- c("Country","Lowest", "Low", "Middle", "Higher")
ln_CS_percapita_t<-t(ln_CS_percapita)
colnames(ln_CS_percapita_t)<-ln_CS_percapita_t[1,]
ln_CS_percapita_t<-ln_CS_percapita_t[-1,]
ln_CS_percapita_t<-data.frame(ln_CS_percapita_t)
ln_CS_percapita_t[is.na(ln_CS_percapita_t) | ln_CS_percapita_t=="    -Inf"]=0

cons_elasticity <- matrix(NA, nrow = length(countries_gcd), ncol = 2)
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
write.csv(cons_elasticity, "Slutskycons_elasticitcy.csv")
cons_elasticity$subregioncode<- countrycode(cons_elasticity$Country, origin = 'country.name', destination = 'region')
cons_elasticity$iso3c<- countrycode(cons_elasticity$Country, origin = 'country.name', destination = 'iso3c')

data<- left_join(cons_elasticity, df3, by="iso3c")
data<-data[-which(data$alpha2==0),]
plot<-ggplot(data, aes(x=Alles, y=alpha2, color=category)) +
  geom_point() +  geom_text(label=data$iso3c, size=5, hjust=0, vjust=0)+xlab("Per capita expenditure")+ylab("Consumption elasticity")
plot+theme_minimal(base_size=18)+labs(colour = "")+theme(legend.position = "bottom")+scale_colour_grey(end=0.6)
lm_cons_elasticity<-lm(formula = data$alpha2 ~ data$Alles)
summary(lm_cons_elasticity)

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

data_rsq$Rsquared<-as.numeric(data_rsq$Rsquared)
plot_rsq<- ggplot(data = data_rsq, aes(x= ln_expenditure, y= lnCScapita))+geom_point()+ geom_smooth()+facet_wrap(vars(Country))+geom_text(label=ifelse(data_rsq$Consumption_Segment=="Middle",round(data_rsq$Rsquared,3),''), parse = T, nudge_y = 2, nudge_x = 2, size = 3)

plot_rsq+theme_minimal()+ xlab("Ln Expenditure")+ ylab("ln per capita CS")

data_allrsq<-inner_join(trial, cons_elasticity, by="Country")

plot_allrsq<- ggplot(data = data_allrsq, aes(x= ln_expenditure, y= ln_percapita_CS))+geom_point()+ geom_smooth()+facet_wrap(vars(Country))+geom_text(label=ifelse(data_allrsq$Consumption_Segment=="Middle",round(data_allrsq$R.squared,4),''), parse = T, nudge_y = 2, nudge_x = 2, size = 3)+xlab("per capita expenditure")+ylab("per  capita change in CS")+theme_minimal(base_size = 12)


plot_allrsq

trial2$category <- ifelse(trial2$Regressivity>1, "diproportionately affect lower-income", "diproportionately affect higher-income")
trial2$iso3c <- countrycode(trial2$Country, origin = 'country.name', destination = 'iso3c')
trial3<-left_join(trial2, df3, by=c("iso3c","Country"))
trial3<- trial3[-which(is.na(trial3$Regressivity)),]
plotreg<-ggplot(trial3, aes(x=Alles, y=Regressivity, color=category)) +geom_point() +  geom_text(label=trial3$iso3c, size=4,hjust=0, vjust=0)+xlab("Per capita expenditure")+ylab("Regressivity")
plotreg+theme_minimal(base_size = 12)+labs(colour = "")+theme(legend.position = "bottom")
lm_regressivity<-lm(formula = trial3$Regressivity ~ trial3$Alles)
summary(lm_regressivity)

percapitaCS_region <- data.frame(df3$Country,log(df3$Alles))
row.names(percapitaCS_region) <- c("Afghanistan","Albania","Armenia", "Azerbaijan", "Bangladesh", "Belarus", "Benin", "Bhutan", "Bolivia", "Bosnia.and.Herzegovina", "Brazil", "Bulgaria", "Burkina.Faso", "Burundi", "Cambodia", "Cameroon", "Cabo.Verde", "Chad", "China", "Colombia", "Congo..Dem..Rep.", "Congo..Rep.", "Cote.d.Ivoire", "Djibouti", "Egypt..Arab.Rep.", "El.Salvador", "Ethiopia","Fiji","Gabon", "Gambia..The", "Ghana", "Guatemala", "Guinea", "Honduras", "India", "Indonesia", "Iraq", "Jamaica", "Jordan", "Kazakhstan", "Kenya", "Kyrgyz.Republic", "Lao.PDR", "Latvia", "Lesotho", "Liberia", "Lithuania", "Macedonia..FYR", "Madagascar", "Malawi", "Maldives", "Mali", "Mauritania", "Mauritius", "Mexico", "Moldova", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Namibia", "Nepal", "Nicaragua", "Niger", "Nigeria", "Pakistan", "Papua.New.Guinea", "Peru", "Philippines", "Romania", "Russian.Federation", "Rwanda", "Sao.Tome.and.Principe", "Senegal", "Serbia", "Sierra.Leone", "South.Africa", "Sri.Lanka", "Swaziland", "Tajikistan", "Tanzania", "Thailand", "Timor.Leste", "Togo", "Turkey", "Uganda", "Ukraine", "Vietnam", "Yemen..Rep.", "Zambia")                     
colnames(percapitaCS_region) <-  c("Country","Alles")
percapitaCS_region <- percapitaCS_region[order(rownames(percapitaCS_region)),]
percapitaCS_region$region<- countrycode(percapitaCS_region$Country, origin = 'country.name', destination = 'region')
percapitaCS_region$Alles= ln_CS_change_exp$National+percapitaCS_region$Alles
plot_region_percapita<-ggplot(percapitaCS_region, aes(x=region, y=Alles)) +
  geom_boxplot()+xlab("")+ylab("Damages per capita")
plot_region_percapita+theme_minimal()

Share_good_on_change_CS_lowest[Share_good_on_change_CS_lowest==-9999] = NA
Share_good_on_change_CS_lowest$iso3c<- countrycode(row.names(Share_good_on_change_CS_lowest), origin = 'country.name', destination = 'iso3c')

Share_good_on_change_CS_higher$Country<- row.names(Share_good_on_change_CS_higher)
Share_good_on_change_CS_higher<-Share_good_on_change_CS_higher[,-which(names(Share_good_on_change_CS_higher) %in% c("Beef.and.Veal","Beer","Bread","Butter.and.Margarine","Cheese","Coffee..Tea.and.Cocoa","Eggs.and.Egg.Based.Products","Fresh.Milk", "Fresh..Chilled.or.Frozen.Fish.and.Seafood", "Mineral.Waters..Soft.Drinks..Fruit.and.Vegetable.Juices", "Spirits", "Tobacco","Wine", "Preserved.or.Processed.Fish.and.Seafood","Preserved.Milk.and.Other.Milk.Products", "Other.Meats.and.Meat.Preparations" , "Frozen..Preserved.or.Processed.Vegetables.and.Vegetable.based.Product", "Frozen..Preserved.or.Processed.Fruit.and.Fruit.based.Product","Lamb..Mutton.and.Goat","Other.Meats.and.Meat.Preparations","Pork","Poultry", "Other.Bakery.Products" , "Sugar"))]
Share_good_on_change_CS_higher_long <- melt(Share_good_on_change_CS_higher, id.vars = c("Country"))
names(Share_good_on_change_CS_higher_long)[names(Share_good_on_change_CS_higher_long) == "Variable"] <- "Sector"

Share_good_on_change_CS_higher_long$iso3c<-countrycode(Share_good_on_change_CS_higher_long$Country, origin = 'country.name', destination = 'iso3c')
Share_good_on_change_CS_higher_long$region<-countrycode(Share_good_on_change_CS_higher_long$Country, origin = 'country.name', destination = 'region')

Share_good_on_change_CS_higher_long<- left_join(Share_good_on_change_CS_higher_long, cons_elasticity, by="iso3c")
Share_good_on_change_CS_higher_long<-Share_good_on_change_CS_higher_long[-which(Share_good_on_change_CS_higher_long$alpha2==0),]
Share_good_on_change_CS_higher_long<-Share_good_on_change_CS_higher_long[-which(Share_good_on_change_CS_higher_long$value==0),]

Share_good_on_change_CS_higher_long_plot<-ggplot(Share_good_on_change_CS_higher_long, aes(x=value, y=alpha2, color= variable)) +
  geom_point()+xlab("Contribution of sector on CS loss (H)")+ylab("Consumption elasticity")+geom_smooth(method=lm, se=FALSE)
Share_good_on_change_CS_higher_long_plot+theme_minimal()+scale_color_manual(labels = c("Fruit", "Potatoes", "Vegetables", "Cereals and Flour", "Oils & Fats", "Rice", "Sugar"), values = c("#FF33CC", "#996633", "#339900", "#FFCC00", "#000000", "#0099FF","#FF6633"))+labs(colour = "Sector")+theme(legend.position = "bottom")

Share_good_on_change_CS_lowest$Country<- row.names(Share_good_on_change_CS_lowest)
Share_good_on_change_CS_lowest<-Share_good_on_change_CS_lowest[,-which(names(Share_good_on_change_CS_lowest) %in% c("Beef.and.Veal","Beer","Bread","Butter.and.Margarine","Cheese","Coffee..Tea.and.Cocoa","Eggs.and.Egg.Based.Products","Fresh.Milk", "Fresh..Chilled.or.Frozen.Fish.and.Seafood", "Mineral.Waters..Soft.Drinks..Fruit.and.Vegetable.Juices", "Spirits", "Tobacco","Wine", "Preserved.or.Processed.Fish.and.Seafood","Preserved.Milk.and.Other.Milk.Products", "Other.Meats.and.Meat.Preparations" , "Frozen..Preserved.or.Processed.Vegetables.and.Vegetable.based.Product", "Frozen..Preserved.or.Processed.Fruit.and.Fruit.based.Product","Lamb..Mutton.and.Goat","Other.Meats.and.Meat.Preparations","Pork","Poultry", "Other.Bakery.Products", "iso3c" , "Sugar"))]
Share_good_on_change_CS_lowest_long <- melt(Share_good_on_change_CS_lowest, id.vars = c("Country"))
names(Share_good_on_change_CS_lowest_long)[names(Share_good_on_change_CS_lowest_long) == "Variable"] <- "Sector"
#Share_good_on_change_CS_higher_long
Share_good_on_change_CS_lowest_long$iso3c<-countrycode(Share_good_on_change_CS_lowest_long$Country, origin = 'country.name', destination = 'iso3c')
Share_good_on_change_CS_lowest_long$region<-countrycode(Share_good_on_change_CS_lowest_long$Country, origin = 'country.name', destination = 'region')
Share_good_on_change_CS_lowest_long<- left_join(Share_good_on_change_CS_lowest_long, cons_elasticity, by="iso3c")
Share_good_on_change_CS_lowest_long<-Share_good_on_change_CS_lowest_long[-which(Share_good_on_change_CS_lowest_long$alpha2==0),]
Share_good_on_change_CS_lowest_long<-Share_good_on_change_CS_lowest_long[-which(Share_good_on_change_CS_lowest_long$value==0),]

Share_good_on_change_CS_lowest_long_plot<-ggplot(Share_good_on_change_CS_lowest_long, aes( x=value, y=alpha2, color= variable)) +
  geom_point()+xlab("Contribution of sector in CS loss (L)")+ylab("Consumption elasticity")+geom_smooth(method=lm, se=FALSE)

Share_good_on_change_CS_lowest_long_plot+theme_minimal()+ scale_color_manual(labels = c("Fruit", "Potatoes", "Vegetables", "Cereals and Flour", "Oils & Fats", "Rice", "Sugar"), values = c("#FF33CC", "#996633", "#339900", "#FFCC00", "#000000", "#0099FF","#FF6633"))+labs(colour = "Sector")+theme(legend.position = "bottom")

share_cs<- ddply(Share_good_on_change_CS_lowest_long,.(variable), summarise, sum_sector =sum(value, na.rm=T))
share_cs$total<- sum(share_cs$sum_sector)
share_cs <- share_cs %>% 
  arrange(desc(variable)) %>%
  mutate(prop = sum_sector / sum(share_cs$sum_sector) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
plot_cs<-ggplot(share_cs, aes( x="",  y=prop, fill=variable)) + geom_bar(stat="identity", width=1)+  xlab("")+ylab("share of CS change")+coord_polar("y",start=0)+  theme_void() +  theme(legend.position="none") +  geom_text(aes(y=ypos, label = round(prop,2)), color = "white", size=4) 
plot_cs+theme_minimal()+theme(legend.position = "bottom")+ scale_fill_manual(labels = c("Fruit", "Potatoes", "Vegetables", "Cereals and Flour", "Oils & Fats", "Rice", "Sugar"), values = c("#FF33CC", "#996633", "#339900", "#FFCC00", "#000000", "#0099FF","#FF6633"))+labs(colour = "Sector")+theme(legend.position = "bottom")
share_cs_rice<- Share_good_on_change_CS_lowest_long[which((Share_good_on_change_CS_lowest_long$variable=="Rice")), ]
#write.csv(share_cs_rice, file= "share_rice_CS_lowest.csv")
share_cs_veg<- Share_good_on_change_CS_lowest_long[which((Share_good_on_change_CS_lowest_long$variable=="Fresh.or.Chilled.Vegetables.Other.than.Potatoes")), ]
#write.csv(share_cs_veg, file= "share_veg_CS_lowest.csv")

share_cs2<- ddply(Share_good_on_change_CS_higher_long,.(variable), summarise, sum_sector =sum(value, na.rm=T))
share_cs2$total<- sum(share_cs2$sum_sector)
share_cs2 <- share_cs2 %>% 
  arrange(desc(variable)) %>%
  mutate(prop = sum_sector / sum(share_cs2$sum_sector) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

plot_cs<-ggplot(share_cs2, aes( x="",  y=prop, fill=variable)) + geom_bar(stat="identity", width=1)+  xlab("")+ylab("share of CS change")+coord_polar("y",start=0)+  theme_void() +  theme(legend.position="none") +  geom_text(aes(y=ypos, label = round(prop,2)), color = "white", size=4) 
plot_cs+theme_minimal()+theme(legend.position = "bottom")+ scale_fill_manual(labels = c("Fruit", "Potatoes", "Vegetables", "Cereals and Flour", "Oils & Fats", "Rice", "Sugar"), values = c("#FF33CC", "#996633", "#339900", "#FFCC00", "#000000", "#0099FF","#FF6633"))+labs(colour = "Sector")+theme(legend.position = "bottom")


share_cs2_rice<- Share_good_on_change_CS_higher_long[which((Share_good_on_change_CS_higher_long$variable=="Rice")), ]
write.csv(share_cs2_rice, file= "share_rice_CS_higher.csv")
share_cs2_veg<- Share_good_on_change_CS_higher_long[which((Share_good_on_change_CS_higher_long$variable=="Fresh.or.Chilled.Vegetables.Other.than.Potatoes")), ]
write.csv(share_cs2_veg, file= "share_veg_CS_higher.csv")

Share_good_on_change_CS_lowest$iso3c<-countrycode(Share_good_on_change_CS_lowest$Country, origin = 'country.name', destination = 'iso3c')
sectors_share<-left_join(Share_good_on_change_CS_lowest, cons_elasticity, by="iso3c")
names(sectors_share)[names(sectors_share) == "Country.x"] <- "Country"
names(sectors_share)[names(sectors_share) == "Country.y"] <- "Country2"

share_cs$Consumption_Segment<-"Lowest"
share_cs2$Consumption_Segment<-"Higher"
shares_cs_both<-bind_rows(share_cs,share_cs2)
shares_cs_both$Sector<- shares_cs_both$variable
plot_cs<-ggplot(shares_cs_both, aes( x=Consumption_Segment,  y=prop, fill=Sector)) + geom_bar(stat="identity", width=0.5)+  xlab("")+ylab("share of CS change")+   theme(legend.position="none") +  geom_text(aes(y=ypos, label = round(prop,2)), color = "white", size=4) 
plot_cs+theme_minimal()+theme(legend.position = "bottom")+ scale_fill_manual(labels = c("Fruit", "Potatoes", "Vegetables", "Cereals and Flour", "Oils & Fats", "Rice", "Sugar"), values = c("#FF33CC", "#996633", "#339900", "#FFCC00", "#000000", "#0099FF","#FF6633"))+labs(colour = "Sector")+theme(legend.position = "bottom")
