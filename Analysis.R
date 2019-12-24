# Analysis
# Software Engineering for Economists
# The Effect of Asset Purchases on Asset Prices and Inequality
# Thomas Schiller 


## Prepare working space
rm(list = ls())
gc()

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load packages
toload <- c("ggplot2", "ggfortify","forecast","tidyverse","stargazer", "lodown", "readxl", "dplyr", "astsa", "vars", "urca", "lubridate", "rlist", "zoo", "xts")

lapply(toload,require, character.only = T)

## Load preprepared data
load("AssetPurchaseData.RData")

## Plot and examine data visually

# GDP
ggplot(GDP)+ 
  stat_smooth(aes(x=(index(GDP)), y=(GDP)), method = lm, formula = y ~ poly(x, 25), se = FALSE, color = "#09557f") +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
  theme(axis.title.x = element_blank())+theme(panel.grid.minor = element_blank())+
  scale_y_continuous(labels=comma, name="Billions of Chained U.S. Dollars", breaks = scales::pretty_breaks(n = 10))+
  theme(axis.title=element_text(size=8), axis.text=element_text(size=8))+
  ggtitle("Figure 2: Real Gross Domestic Product")+theme(plot.title = element_text(size=10))+
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.3, color="grey" ))+
  theme(axis.ticks.y = element_blank())+
  labs(caption="Source: FRED")

# Inflation expectations
ggplot(InflationExpectations)+ 
  geom_line(aes(x=(index(InflationExpectations)), y=(InflationExpectations)), color = "#09557f") +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
  theme(axis.title.x = element_blank())+theme(panel.grid.minor = element_blank())+
  scale_y_continuous(labels=comma, name="Percent", breaks = scales::pretty_breaks(n = 10))+
  theme(axis.title=element_text(size=8), axis.text=element_text(size=8))+
  ggtitle("Figure 3: US Inflation Expectations")+theme(plot.title = element_text(size=10))+
  theme(panel.grid.major.x = element_blank(),    panel.grid.major.y = element_line( size=.3, color="grey" ))+
  theme(axis.ticks.y = element_blank())+
  labs(caption="Source: FRED")


# TIPS Yields
ggplot(LongTermAverageTIPSYield)+ 
  geom_line(aes(x=(index(LongTermAverageTIPSYield)), y=(LongTermAverageTIPSYield)), color = "#09557f") +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
  theme(axis.title.x = element_blank())+theme(panel.grid.minor = element_blank())+
  scale_y_continuous(labels=comma, name="Percent", breaks = scales::pretty_breaks(n = 10))+
  theme(axis.title=element_text(size=8), axis.text=element_text(size=8))+
  ggtitle("Figure 4: Treasury Inflation-Indexed Long-Term Average Yield")+theme(plot.title = element_text(size=10))+
  theme(panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.3, color="grey" ))+
  theme(axis.ticks.y = element_blank())+
  labs(caption="Source: FRED")


# Median Sales Prices
ggplot(MedianSalesPriceHouses)+ 
  stat_smooth(aes(x=(index(MedianSalesPriceHouses)), y=(MedianSalesPriceHouses)), method = lm, formula = y ~ poly(x, 27), se = FALSE, color = "#09557f") +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
  theme(axis.title.x = element_blank())+theme(panel.grid.minor = element_blank())+
  scale_y_continuous(labels=comma, name="U.S. Dollars", breaks = scales::pretty_breaks(n = 10))+
  theme(axis.title=element_text(size=8), axis.text=element_text(size=8))+
  ggtitle("Figure 5: Median Sales Price of Houses Sold for the United States")+theme(plot.title = element_text(size=10))+
  theme(
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.3, color="grey" ))+
  theme(axis.ticks.y = element_blank())+
  labs(caption="Source: FRED")


# Ten Year Treasury


## Test for stationarity (both trend and difference stationarity)

# KPSS test for 
