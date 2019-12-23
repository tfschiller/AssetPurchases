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


## Equalise lengths of lists (eg so that there is a GDP entry for each )

# Create daily index that spans January 2004 - January 2019
daily<-xts(,seq(start(GDP),end(GDP),"days"))

GDP2<-


## Construct Household Balance Sheet for First and Fifth Quintiles 

financialAsset<-read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 6 16", skip=88)
financialAssets<-financialAsset[-c(1:6,13:84),]
colnames(financialAssets)[1]<-"Percentile of income"

stockHolding<-read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 7")
stockHoldings<-stockHolding[-c(1:7,14:26),-c(2:20,22:31)]
colnames(stockHoldings)[1]<-"Percentile of income"
colnames(stockHoldings)[2]<-"Stock Holdings"

nonfinancialAsset<-read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 9 16", skip=2)
nonfinancialAssets<-nonfinancialAsset[-c(1:92, 99:170),]
colnames(nonfinancialAssets)[1]<-"Percentile of income"

headers <- read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 13 16 Alt", range="A90:K91", col_names = FALSE)
headersDebt <- sapply(headers,paste,collapse="_")
debt<-read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 13 16 Alt", range="A98:K103", col_names = FALSE)
names(debt)<-headersDebt
colnames(debt)[1]<-"Percentile of income"

householdPortfolio<-Reduce(function(x, y) left_join(x, y, by="Percentile of income"), list(financialAssets,stockHoldings,nonfinancialAssets, debt))
colnames(householdPortfolio)[c(11,19,23,24,25,26,28,29,30,31)]<-c("Other financial assets", "Other nonfinancial assets", 
                                                                  "HELOC - Secured by primary residence", "Other residential debt", 
                                                                  "Credit card balances", "Lines of credit not secured by residential property",
                                                                  "Vehicle loans", "Other installment loans", "Other debt", "Any debt")
householdPortfolio[1:3,5]<-0
householdPortfolio[,2:31]<-as.numeric(unlist(householdPortfolio[,2:31]))


# Calculate relative weights of each asset class for the different income quintiles (as percentage of total asset holdings)

pcts <- as.data.frame(lapply(householdPortfolio[,-1], function(x) {
  x / apply(householdPortfolio[,-1],1,sum)
}))*100


# Keep only the asset class a la Domanski et al 2016






