# Software Engineering for Economists
# The Effect of Asset Purchases on Asset Prices and Inequality
# Thomas Schiller 


## Prepare working space
rm(list = ls())
gc()

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load packages
toload <- c("ggplot2", "ggfortify","forecast","tidyverse","stargazer", "lodown", "readxl", "dplyr", "astsa", "vars", "urca", "lubridate")

lapply(toload,require, character.only = T)


## Read-in data (GDP, Fed Total Assets, Long Term Average TIPS Yield, Wilshire 5000, Median Sales Price)

data <- lapply(paste0("Data/",list.files("Data")), function(x) read_csv(x)[column_position])
data <- lapply(data, as.ts)

data_reader<-function(column){
  
  column_position<-as.numeric(deparse(substitute(column)))
  data<-lapply(paste0("Data/",list.files("Data")), function(x) read_csv(x)[column_position])
  data <- lapply(data, as.ts)
}

data_framer<-function(position, name){
    variable<-deparse(substitute(name))
    number<-as.numeric(deparse(substitute(position)))
    assign(variable, as.data.frame(data[[number]]), env =.GlobalEnv)
} 

data_framer(1, TenYearTreasury)
data_framer(2, FedTotalAssets)
data_framer(3, GDP)
data_framer(4, TIPSYield)
data_framer(5, HousePrices)
data_framer(6, Wilshire5000)







ts_organise_data<-function(column, position, name){
  
  data_reader<-function(column){
    
    column_position<-as.numeric(deparse(substitute(column)))
    data<-lapply(paste0("Data/",list.files("Data")), function(x) read_csv(x)[column_position])
    data <- lapply(data, as.ts)
  }
  
  gg<-data_reader(column_position)
  
  data_framer<-function(position, name){
    variable<-deparse(substitute(name))
    number<-as.numeric(deparse(substitute(position)))
    assign(variable, as.data.frame(gg[[number]]), env =.GlobalEnv)
  }
  
}



ts_organise_data(2, 1, TenYearTreasury)




# mapply(data_framer, 1:6, as.list(list.files("Data")))? 


# Equalise lengths of lists (eg so that there is a GDP entry for each )


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






