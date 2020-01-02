# Analysis
# Software Engineering for Economists
# The Effect of Asset Purchases on Asset Prices and Inequality
# Thomas Schiller 


## Prepare working space
rm(list = ls())
gc()

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load packages
toload <- c("ggplot2", "ggfortify","forecast","tidyverse","stargazer", "lodown", "readxl", "dplyr", "astsa", "vars", "urca", "lubridate", 
            "rlist", "zoo", "xts", "scales", "tseries", "VARsignR")
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
ggplot(na.omit(InflationExpectations))+ 
  geom_line(aes(x=(index(na.omit(InflationExpectations))), y=(na.omit(InflationExpectations))), color = "#09557f") +
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

# Ten Year Treasury Yield
ggplot(TenYearTreasuryConstantMaturity)+ 
  geom_line(aes(x=(index(TenYearTreasuryConstantMaturity)), y=(TenYearTreasuryConstantMaturity)), color = "#09557f") +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
  theme(axis.title.x = element_blank())+theme(panel.grid.minor = element_blank())+
  scale_y_continuous(labels=comma, name="Percent", breaks = scales::pretty_breaks(n = 10))+
  theme(axis.title=element_text(size=8), axis.text=element_text(size=8))+
  ggtitle("Figure 6: 10-Year Treasury Constant Maturity Rate")+theme(plot.title = element_text(size=10))+
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line( size=.3, color="grey" ))+
  theme(axis.ticks.y = element_blank())+
  labs(caption="Source: FRED")

# Wilshire5000
ggplot(Wilshire5000)+ 
  geom_line(aes(x=(index(Wilshire5000)), y=(Wilshire5000)), color = "#09557f") +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
  theme(axis.title.x = element_blank())+theme(panel.grid.minor = element_blank())+
  scale_y_continuous(labels=comma, name="Index", breaks = scales::pretty_breaks(n = 10))+
  theme(axis.title=element_text(size=8), axis.text=element_text(size=8))+
  ggtitle("Figure 7: Wilshire 5000 Full Cap Price Index")+theme(plot.title = element_text(size=10))+
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line( size=.3, color="grey" ))+
  theme(axis.ticks.y = element_blank())+
  labs(caption="Source: FRED")


## Test for stationarity (both trend and difference stationarity)

# KPSS test for trend stationarity (against alternative of unit root)
kpss.test(FedTotalAssets, null = "Trend", lshort = TRUE) # Unit root at 1% 
kpss.test(GDP, null = "Trend", lshort = TRUE) # Unit root at 1% 
kpss.test(InflationExpectations, null = "Trend", lshort = TRUE) # Unit root at 1% 
kpss.test(MedianSalesPriceHouses, null = "Trend", lshort = TRUE) # Unit root at 1% 

# Check Partial Autocorrelation and for the presence of a unit root (difference stationarity)
ggPacf(LogGDPDiff)
adf.test(na.omit(LogGDPDiff), k=1)

ggPacf(DiffInflationExpectations)
adf.test(na.omit(DiffInflationExpectations), k=1)

ggPacf(LogMedianSalesPriceHousesDiff)
adf.test(na.omit(LogMedianSalesPriceHousesDiff), k=1)

ggPacf(DiffTenYearTreasury)
adf.test(na.omit(DiffTenYearTreasury), k=1)

ggPacf(LogWilshire5000Diff)
adf.test(na.omit(LogWilshire5000Diff), k=1)

summary(ur.df(na.omit(LogGDPDiff), type = "trend", selectlags = 'AIC')) # Suggests lag of one 

# Combined relevant variables into a single matrix
x<-cbind(LogFedTotalAssetsDiff, LogGDPDiff, DiffInflationExpectations, LogMedianSalesPriceHousesDiff, DiffTenYearTreasury, LogWilshire5000Diff)
colnames(x)<-c('FedTotalAssets', 'LogGDPDiff', 'DiffInflationExpectations', 'LogMedianSalesPriceHousesDiff', 'DiffTenYearTreasury', 'LogWilshire5000Diff')


## VAR

# Select appropriate lag based on AIC
VARselect(na.omit(x), lag.max = 4)

# Compute VAR and plot IRFs
var<-VAR(na.omit(x), p=3, type = "const")
par(mar=c(1,1,1,1))
plot(irf(var, impulse = 'FedTotalAssets', response = 'LogGDPDiff',  n.ahead = 100, boot = TRUE, runs = 100, ci = 0.95))

plot(irf(var, impulse = 'FedTotalAssets', response = 'DiffInflationExpectations',  n.ahead = 100, boot = TRUE, runs = 100, ci = 0.95))

plot(irf(var, impulse = 'FedTotalAssets', response = 'LogMedianSalesPriceHousesDiff',  n.ahead = 100, boot = TRUE, runs = 100, ci = 0.95))

plot(irf(var, impulse = 'FedTotalAssets', response = 'DiffTenYearTreasury',  n.ahead = 100, boot = TRUE, runs = 100, ci = 0.95))

plot(irf(var, impulse = 'FedTotalAssets', response = 'LogWilshire5000Diff',  n.ahead = 100, boot = TRUE, runs = 100, ci = 0.95))


## Estimating VARs using sign restrictions(following Uhlig 2005, Danne 2015)

# Sign restrictions (ie positive Fed Balance Sheet Shock does not decrease LogGDP or Inflation Expectations for x periods. No sign restrictions are imposed
# on Median Sales Price of Houses, Ten Year Treasury Yield or Wilshire 5000)
constr<-c(+1, +2, +3)

y<-cbind(log(FedTotalAssets)*100, log(GDP)*100, log(InflationExpectations)*100, log(MedianSalesPriceHouses)*100, log(TenYearTreasuryConstantMaturity)*100, log(Wilshire5000)*100)

model1<- uhlig.reject(ts(na.omit(y)), nlags=12, draws=200, subdraws=200, nkeep=1000, KMIN=1, KMAX=6, constrained=constr, constant=FALSE, steps=60)
irfs1<-model1$IRFS
vl <- c("Federal Reserve Total Assets","GDP","Inflation Expectations","Median Sales Price Houses", "Ten Year Treasury Yield", "Wilshire 5000")


## Saves a copy of the impulse responses to working directory to be used in markdown
irfplot(irfdraws=irfs1, type="median", labels=vl, save=TRUE, bands=c(0.16, 0.84), grid=TRUE, bw=TRUE)

fevd1 <- model1$FEVDS
fevdplot(fevd1, label=vl, save=FALSE, bands=c(0.16, 0.84), grid=TRUE,
         bw=FALSE, table=FALSE, periods=NULL)

save(var, model1, irfs1, file = "VARmodel.RData")


