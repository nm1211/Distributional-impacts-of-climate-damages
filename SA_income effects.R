## Income effects
library(dplyr)
library(gplot2)
library(countrycode)
library(wbstats)
library(tidyr)
library(scales)

##Read in all necessary data
gcd_mapper <- read.csv(file="Appendix_Mapping_GCD_GTAP.csv", sep=";")
country_code_GTAP <- read.csv(file = "Codes_Countries.csv", sep=";")
gcd_raw=read.csv(file="HH_consumption_GCD.csv", sep=";")
GCD <- merge(gcd_raw,gcd_mapper, by = "Product.or.Service")
GCD_GTAP <- merge(GCD, country_code_GTAP, by = "Country")
GDP_country <- read.csv(file = "GDP_per_capita_PPP.csv", sep = ";")
GDP_country$X2019 <- as.numeric(sub(",", ".", GDP_country$X2019))
GDP_country$iso3c<-countrycode(GDP_country$Country, origin = 'country.name', destination = 'iso3c')

countries<-GDP_country$iso3c
deflator<-wb_data(country = "USA",
                  indicator = "NY.GDP.DEFL.ZS",
                  start_date = 2010, end_date = 2014)
deflator<-deflator[,c(2,3,4,5)]
years<-c(2010, 2014)

deflator <- deflator %>%
  filter(date %in% years) %>%
  dplyr::rename(
    deflator = NY.GDP.DEFL.ZS,
  ) %>% 
  pivot_wider(
    names_from = date,
    values_from = deflator,
    names_glue = "deflator_{date}"  )%>%
  mutate(
    # Deflation factor to convert from 2014 PPP to 2010 PPP
    deflation_factor = deflator_2014/ deflator_2010 )


PPP<-wb_data(country = countries,
             indicator = c("PA.NUS.PPP", "PA.NUS.ATLS", "PA.NUS.FCRF"),
             start_date = 2014, end_date = 2014)
PPP<-PPP[,c(2,3,5,6,7)]

#rename and create conversion factor
PPP <- PPP %>%
  select(iso3c, country, PA.NUS.PPP,PA.NUS.FCRF,PA.NUS.ATLS ) %>%
  dplyr::rename(
    ppp_conversion = PA.NUS.PPP,
    exchange_rate= PA.NUS.FCRF,
    DEC= PA.NUS.ATLS
  ) %>% 
  mutate(
    ppp_adj_2014 =  exchange_rate/ppp_conversion,
    ppp_dec_2014= DEC/ppp_conversion)

PPP$deflation_factor<-deflator$deflation_factor
nr_products <- c("Fresh or Chilled Fruit", "Fresh or Chilled Potatoes" , "Fresh or Chilled Vegetables Other than Potatoes" ,"Frozen, Preserved or Processed Fruit and Fruit-based Product" , "Frozen, Preserved or Processed Vegetables and Vegetable-based Product", 
                 "Beef and Veal", "Lamb, Mutton and Goat" , "Other Meats and Meat Preparations", "Pork",  "Poultry",
                 "Fresh, Chilled or Frozen Fish and Seafood", "Preserved or Processed Fish and Seafood",
                 "Cheese","Preserved Milk and Other Milk Products","Eggs and Egg-Based Products" ,"Fresh Milk",
                 "Other Cereals, Flour and Other Products", "Rice",
                 "Butter and Margarine" , "Other Edible Oil and Fats",
                 "Beer" ,   "Bread" , "Coffee, Tea and Cocoa", "Mineral Waters, Soft Drinks, Fruit and Vegetable Juices", "Other Bakery Products"  ,"Spirits"    , "Sugar" ,"Tobacco" , "Wine")


total_value_capture_per_sector_change_in_Mio_USD <- read_excel("total_value_capture_per_sector_change_in_Mio_USD.xlsx")
total_value_capture_per_sector_change_in_Mio_USD = t(total_value_capture_per_sector_change_in_Mio_USD)
colnames(total_value_capture_per_sector_change_in_Mio_USD) <- total_value_capture_per_sector_change_in_Mio_USD[1,]
total_value_capture_per_sector_change_in_Mio_USD<-total_value_capture_per_sector_change_in_Mio_USD[-1,]
total_value_capture_per_sector_change_in_Mio_USD<- as.data.frame(total_value_capture_per_sector_change_in_Mio_USD)
total_value_capture_per_sector_change_in_Mio_USD$i..GTAPsector<- row.names(total_value_capture_per_sector_change_in_Mio_USD)
names(total_value_capture_per_sector_change_in_Mio_USD)[names(total_value_capture_per_sector_change_in_Mio_USD) == "i..GTAPsector"] <- "GTAPsector"

##Merge the datasets together

result_prod2 <- merge(GCD_GTAP, total_value_capture_per_sector_change_in_Mio_USD, by = "GTAPsector")
result_prod2$totalvalue=result_prod2[cbind(seq_len(nrow(result_prod2)),match(result_prod2$Country_Code, colnames(result_prod2)))]
final_prod2 <- data.frame(result_prod2$Country, result_prod2$GTAPsector, result_prod2$Consumption.Segment,result_prod2$Product.or.Service, result_prod2$X.PPP, result_prod2$totalvalue)
colnames(final_prod2)<- c("Country","GTAP_Sector","Consumption_Segment","Product_or_Service","PPP","Total_Value") 
final_prod2 <- final_prod2[order(final_prod2$Product_or_Service),]

final_prod2$PPP = as.numeric(sub(",", ".", final_prod2$PPP, fixed= TRUE))
final_prod2$Total_Value = as.numeric(sub(",", ".", final_prod2$Total_Value, fixed = TRUE))
final_prod2<- filter(final_prod2, Product_or_Service %in% nr_products)
final_prod2$iso3c<-countrycode(final_prod2$Country, origin = 'country.name', destination = 'iso3c')

final_prod2<-left_join(final_prod2, PPP, by="iso3c")
CP_CSfinal_good <- read.csv("CP_CSfinal_good.csv")

##Compute the net effect
###multiply by  ppp factor (exchange (LCU/$current)/ppp conversion (LCU(int.$))) to get ppp ($int.) and then deflate by dividing T.V. with deflation factor(2014/2010) to get 2010 values 
data_merged_ext2<- left_join(final_prod2, CP_CSfinal_good, by=c("Country", "Consumption_Segment", "Product_or_Service"))
income_effects_all <- data_merged_ext2%>%
  mutate(
    TV_ppp2010= Total_Value*ppp_adj_2014,
    TV_ppp2010dec= Total_Value*ppp_dec_2014,
    TV_defl= TV_ppp2010/deflation_factor,
    TV_defldec= TV_ppp2010dec/deflation_factor,
    net_diffPPP2010= TV_defl +CScontribution,
    net_diffPPP2010dec= TV_defldec +CScontribution,
  )
income_effects_allN<-filter(income_effects_all, Consumption_Segment== "All")

write.csv(income_effects_all, "income_effects_all.csv")
write.csv(income_effects_all, "income_effects_allN.csv")

income_effects <- income_effects_all %>%
  group_by(Country, Consumption_Segment) %>%
  dplyr::summarise(net_diff_sum = sum(net_diffPPP2010, na.rm = T),
                   net_diff_sumdec = sum(net_diffPPP2010dec, na.rm = T),
                   CS_agg = sum(CScontribution, na.rm = T),
                   TV_agg = sum(Total_Value, na.rm = T),
                   TV_pppagg = sum(TV_ppp2010, na.rm = T),
                   TV_pppaggdec = sum(TV_ppp2010dec, na.rm = T),
                   PPP= sum(PPP, na.rm=T)
                   
  ) %>% 
  ungroup()
write.csv(income_effects, "income_effects_agg.csv")

income_effectsN<-filter(income_effects, Consumption_Segment=="All")
write.csv(income_effectsN, "income_effects_aggN.csv")

##For plots

CP_CSfinal2 <- read.csv("CP_CSfinal2.csv")

IE<- left_join(income_effectsN, CP_CSfinal2, by=c("Country", "Consumption_Segment"))
IE$CS_facto_agg<-abs(IE$CS_facto_agg)
IE$category<-ifelse(IE$net_diff_sumdec>0, "Net Gain", "Net Loss")
IE$DV_gain<-ifelse(IE$net_diff_sumdec>0, 1, 0)

IE$iso3c<-countrycode(IE$Country, origin = 'country.name', destination = 'iso3c')
write.csv(IE, "IE.csv")

summary(IE$net_diff_sumdec)
quantile(IE$net_diff_sumdec,prob = seq(0, 1, length = 101))

plot<-ggplot(IE, aes(y=net_diff_sumdec, x=CS_facto_agg, color=category)) +
  geom_point() +  geom_text(label=IE$iso3c, size=5, hjust=0, vjust=0)+ylab("Net diff. between total value captured and CS loss (in Mio PPP USD)")+xlab("Consumer surplus loss (%)")
plot+theme_minimal(base_size=13)+labs(colour = "")+theme(legend.position = "bottom")+scale_colour_grey(end=0.6)+   
  scale_y_continuous(breaks = pretty(IE$net_diff_sumdec[IE$net_diff_sumdec > -15000  ], n = 6)) +
  coord_cartesian(ylim = c(-15000, 5000))
count <- sum(IE$DV_gain > 0)

###Offsets for lowest income segement
##first keep only net gain countries

IE_gain<-filter(IE, DV_gain==1)
countries_gain<-unique(IE_gain$iso3c)
IE_lowest<-filter(income_effects_all, Consumption_Segment=="Lowest")

IE_lowest <- IE_lowest %>%
  group_by(Country) %>%
  dplyr::summarise(net_diff_sumdec = sum(net_diffPPP2010dec, na.rm = T),
                   CS_agg = sum(CScontribution, na.rm = T),
                   TV_pppagg = sum(TV_ppp2010dec, na.rm = T)
                   
  ) %>% 
  ungroup()


IE_lowest$Consumption_Segment<- "Lowest"
IE_lowest<- left_join(IE_lowest, CP_CSfinal2, by=c("Country", "Consumption_Segment"))

IE_lowest$iso3c<-countrycode(IE_lowest$Country, origin = 'country.name', destination = 'iso3c')
IE_lowest<-filter(IE_lowest, iso3c %in%countries_gain)
IE_lowest$offset<-IE_lowest$CS_agg/IE_lowest$TV_pppagg
plot<-ggplot(IE_lowest, aes(y= abs(offset), x=abs(CS_facto_agg)) )+
  geom_point() +  geom_text(label=IE_lowest$iso3c, size=5, hjust=0, vjust=0)+ylab("CS offsets (%)")+xlab("Consumer surplus loss (%)")

plot+theme_minimal(base_size=13)+labs(colour = "")+theme(legend.position = "bottom")+scale_colour_grey(end=0.6)

IEL<-filter(income_effects, Consumption_Segment==c("Lowest"))
IEA<-filter(income_effects, Consumption_Segment==c("All"))
IE_CSl<-rbind(IEL, IEA)
IE_CSl<-IE_CSl[, c(1,2,5)]
IE_CSl=IE_CSl %>% 
  pivot_wider(names_from = Consumption_Segment, values_from = c(CS_agg))
IE_CSl$CS_expL<- IE_CSl$Lowest/IE_CSl$All

IE_lowest<-left_join(IE_lowest, IE_CSl, by="Country")

plot<-ggplot(IE_lowest, aes(y= abs(offset), x=abs(CS_expL)) )+
  geom_point() +  geom_text(label=IE_lowest$iso3c, size=5, hjust=0, vjust=0)+ylab("CS offsets (%)")+xlab("Consumer surplus loss for lowest income segment (% of total)")

plot+theme_minimal(base_size=13)+labs(colour = "")+theme(legend.position = "bottom")+scale_colour_grey(end=0.6)

