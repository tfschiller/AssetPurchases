# Inequality 
# Software Engineering for Economists
# The Effect of Asset Purchases on Asset Prices and Inequality
# Thomas Schiller 

## Prepare working space
rm(list = ls())
gc()

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load packages
toload<-c("ggplot2", "ggfortify","forecast","tidyverse","stargazer", "lodown", "readxl", "dplyr", "astsa", "vars", "urca", "lubridate", "rlist", "zoo", "xts")

lapply(toload,require, character.only = T)


## Construct Household Balance Sheet for First and Fifth Quintiles 

# Financial Assets
financialAsset<-read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 6 16 means")
financialAssets<-financialAsset[10:15,]
colnames(financialAssets)[1]<-"Percentile of income"
colnames(financialAssets)[2:12]<-read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 6 16 means", range = "B3:L3", col_names = FALSE)

# Non Financial Assets 
nonfinancialAsset<-read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 9 16 means")
nonfinancialAssets<-nonfinancialAsset[10:15,]
colnames(nonfinancialAssets)[1]<-"Percentile of income"
colnames(nonfinancialAssets)[2:9]<-read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 9 16 means", range = "B3:I3", col_names = FALSE)

# Total Assets
householdPortfolio<-Reduce(function(x, y) left_join(x, y, by="Percentile of income"), list(financialAssets,nonfinancialAssets))
colnames(householdPortfolio)[c(11,18)]<-c("Other financial assets", "Other nonfinancial assets")
householdPortfolio[,2:20]<-as.numeric(unlist(householdPortfolio[,2:20]))

# Combine the fifth and sixth rows as these represent the eighth and ninth deciles 
householdPortfolio[5,2:20]<-householdPortfolio[5,2:20]+householdPortfolio[6,2:20]
householdPortfolio<-householdPortfolio[-6,]
householdPortfolio[c(1,5),1]<-c("0–19.9", "80–100")


householdPortfolio[is.na(householdPortfolio)]<-0



## Calculate percent of household portfolio for each asset type by income distrbution
householdassets<-as.data.frame(lapply(householdPortfolio[,-1], function(x) {
  x / apply(householdPortfolio[,-1],1,sum)}))*100

colnames(householdassets)<-colnames(householdPortfolio[2:20])  
householdassets<-cbind(householdPortfolio[,-c(2:length(householdPortfolio))], householdassets)


## Debt 
debt<-read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 13 16 Alt means", range="A12:K17", col_names = FALSE)
colnames(debt)[1:length(debt)]<-c("Percentile of income", "Mortgages - Secured by primary residence", "HELOC - Secured by primary residence", "Other residential debt", 
                                                                  "Credit card balances", "Lines of credit not secured by residential property", "Education loans",
                                                                  "Vehicle loans", "Other installment loans", "Other debt", "Any debt")


# Combine the fifth and sixth rows as these represent the eighth and ninth deciles 
debt[5,2:11]<-debt[5,2:11]+debt[6,2:11]
debt<-debt[-6,]
debt[c(1,5),1]<-c("0–19.9", "80–100")


# Calculate libaility type as percentage of total debt
householddebt<-as.data.frame(lapply(debt[,-1], function(x) {
  x / apply(debt[,-1],1,sum)}))*100

householddebt<-cbind(debt[,-c(2:length(debt))], householddebt)
colnames(householddebt)<-c("Percentile of income", colnames(debt[2:11]))


## Calculate balance sheet with mean values (ie do not convert to percentages of total balance sheet)
means<-cbind(householdPortfolio, debt[,-1])


## Keep only the relevant asset classes a la Domanski et al 2016
Assets<-cbind("Percentile of income"=householdassets$`Percentile of income`, Deposits=c(householdassets$`Transaction accounts`+householdassets$`Certificates of deposit`),
              Stocks=householdassets$Stocks,
              Bonds=householdassets$`Savings bonds`+householdassets$Bonds,
              "Mutual funds"=rowSums(householdassets[c(7,8,10)]),
              "Real estate"=householdassets$`Primary residence`+householdassets$`Other residential property`+householdassets$`Equity in nonresidential property`)


Debts<-cbind("Mortgage debt"=rowSums(householddebt[2:4]),
             "Other debt"=rowSums(householddebt[5:11]))


## Balance sheet with the mean value of different asset classes held by each income quintile
meansBalanceSheet<-as_data_frame(cbind("Percentile of income"=means$`Percentile of income`, Deposits=c(means$`Transaction accounts`+means$`Certificates of deposit`),
                         Stocks=means$Stocks,
                         Bonds=means$`Savings bonds`+means$Bonds,
                         "Mutual funds"=rowSums(means[c(7,8,10)]),
                         "Real estate"=means$`Primary residence`+means$`Other residential property`+means$`Equity in nonresidential property`,
                         "Mortgage debt"=rowSums(means[21:23]),
                         "Other debt"=rowSums(means[24:30])))


# Combined balance sheet (a la Domanski et al 2016 Table 2)
balanceSheet<-as_data_frame(cbind(Assets, Debts))

# Set number of decimal places
balanceSheet[,2:8]<-matrix(data=as.numeric(unlist(balanceSheet[,2:8])),nrow = 5, ncol = 7)
balanceSheet<- balanceSheet %>% mutate_if(is.numeric, round, digits = 2)

save(debt, balanceSheet, meansBalanceSheet, file = "InequalityTables.RData")




