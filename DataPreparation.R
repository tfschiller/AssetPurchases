# Data Preparation
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


## Read-in data (GDP, Fed Total Assets, Long Term Average TIPS Yield, Wilshire 5000, Median Sales Price)

# Function to read in variables (GDP, Fed Total Assets, Long Term Average TIPS Yield, Wilshire 5000, Median Sales Price)
data_reader<-function(column){
  
  column_position<-as.numeric(deparse(substitute(column)))
  data<-lapply(paste0("Data/",list.files("Data")), function(x) read_csv(x)[column_position])
  data <- lapply(data, as.ts)
}

data <- data_reader(2)
names<-as.list(tools::file_path_sans_ext(list.files("Data")))
names(data)<-tools::file_path_sans_ext(list.files("Data"))

# Read in corresponding dates and change name of list items to names of variables
time <- lapply(paste0("Data/",list.files("Data")), function(x) read_csv(x)[1])
names(time)<-names


# For loop to read-in data to Global Environment, variable by variable
for (i in 1:length(time)) {
  
  assign(unlist(names[i]), do.call(rbind, Map(data.frame, A=time[i], B=data[i])), env =.GlobalEnv)
  
}


# Convert to xts Extensible Time-Series Object
convert_to_xts<-function(variable){
  bb<-deparse(substitute(variable))
  assign(bb, xts(variable[,-1], order.by = as.Date(variable$DATE)), env =.GlobalEnv)
}


convert_to_xts(FedTotalAssets)
convert_to_xts(GDP)
convert_to_xts(LongTermAverageTIPSYield)
convert_to_xts(MedianSalesPriceHouses)
convert_to_xts(TenYearTreasuryConstantMaturity)
convert_to_xts(Wilshire5000)


# Log and Difference GDP, Median Sales Price, Wilshire 5000 to make Stationary (based on KPSS test) 
LogGDPDiff<-diff(log(GDP))
LogMedianSalesPriceHousesDiff<-diff(log(MedianSalesPriceHouses))
LogWilshire5000Diff<-diff(log(Wilshire5000))





## Equalise lengths of lists (eg so that there is a GDP entry for each )

# Create daily index that spans January 2004 - January 2019
daily<-xts(,seq(start(GDP),end(GDP),"days"))


# Function that converts datasets of different intervals into daily, filling with most recent value using daily index
convert_to_daily<-function(dataset){
  aa <-deparse(substitute(dataset))
  dd <-merge(dataset, daily) 
  assign(aa, na.locf(dd), env =.GlobalEnv)
}

convert_to_daily(FedTotalAssets)
convert_to_daily(GDP)
convert_to_daily(LongTermAverageTIPSYield)
convert_to_daily(MedianSalesPriceHouses)
convert_to_daily(TenYearTreasuryConstantMaturity)
convert_to_daily(Wilshire5000)


## Calucate Inflation Expectations (Inflation Expectation = Treasury Yield - TIPs Yield)
InflationExpectations<-TenYearTreasuryConstantMaturity - LongTermAverageTIPSYield



## Save data frames so they can then be loaded into a clean R for analysis
save(FedTotalAssets, GDP, InflationExpectations, MedianSalesPriceHouses, TenYearTreasuryConstantMaturity, Wilshire5000, file="AssetPurchaseData.RData")


##########################################################################################################



