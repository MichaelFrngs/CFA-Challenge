#Remember inputs in the individual revenues models must have matching iterations & years.
setwd("D:/Users/MihalyTower/Documents/Actual Documents/R Programing studies/Projects/CFA NYSE-LEN Monte Carlo")
library(ggplot2)
library(readr)
library(dplyr)
library(scales)

#Brings in CalAtlantic Revenue Model
source("LEN.HomeBuildingRevenueModel.R")
source("CalAtlanticRevenueModel.R")
source("Lennar.Other.Revenue.Model.R")
source("LennarFinServicesRevenueModel.R")
source("NCC Model.R")
source("NWC Model.R")

###COGS####
LEN.HomeBuildingCOGS.Matrix = LEN.HomeBuildRevData
names(LEN.HomeBuildingCOGS.Matrix) = c("Cogs","Trial","year","Iteration","Segment")
LEN.HomeBuildingCOGS.Matrix[1] = data.frame(LEN.HomeBuildRevData[1] * 0.8134) #81% Assumption: past 4 year average cogs as a % of sales
LEN.HomeBuildingCOGS.Matrix[2:5] = data.frame(LEN.HomeBuildRevData[2:5])
 
Financing.COGS.Matrix = Cumulative.Predicted.FinancialRevenue.Data
names(Financing.COGS.Matrix) = c("Cogs","Trial","year","Iteration","Segment")
Financing.COGS.Matrix[1] = data.frame(Cumulative.Predicted.FinancialRevenue.Data[1] * 0.7953) #79% Assumption:past 4 year average cogs as a % of  sales
Financing.COGS.Matrix[2:5] = data.frame(Cumulative.Predicted.FinancialRevenue.Data[2:5])


CalAtlantic.COGS.Matrix = Cumulative.Predicted.CalAtlanticRevenue.Data
names(CalAtlantic.COGS.Matrix) = c("Cogs","Trial","year","Iteration","Segment")
CalAtlantic.COGS.Matrix[1] = data.frame(Cumulative.Predicted.CalAtlanticRevenue.Data[1] * 0.78) #79% Assumption:past 4 year average cogs as a % of  sales
CalAtlantic.COGS.Matrix[2:5] = data.frame(Cumulative.Predicted.CalAtlanticRevenue.Data[2:5])


Multifamily.COGS.Matrix = Cumulative.Predicted.OtherRevenue.Data
names(Multifamily.COGS.Matrix) =c("Cogs","Trial","year","Iteration","Segment")
Multifamily.COGS.Matrix[1] = data.frame(Cumulative.Predicted.OtherRevenue.Data[1] * 1.02) #102% Assumption: approximate of 2 year historic average of cogs as a % of sales
Multifamily.COGS.Matrix[2:5] = data.frame(Cumulative.Predicted.OtherRevenue.Data[2:5])


#Gross Margin####
#Creates the matrix with the right shape & names. Will be overwritten.
Gross.Margin.Matrix = LEN.HomeBuildRevData
names(Gross.Margin.Matrix) = c("GrossMargin","Trial","year","Iteration","Segment")

#Equals Revenues - All COGS from all rev. models
Gross.Margin.Matrix[1] = data.frame(LEN.HomeBuildRevData[1]+Cumulative.Predicted.FinancialRevenue.Data[1])+
  Cumulative.Predicted.OtherRevenue.Data[1] + Cumulative.Predicted.CalAtlanticRevenue.Data[1]  - LEN.HomeBuildingCOGS.Matrix[1]-Financing.COGS.Matrix[1]- 
  CalAtlantic.COGS.Matrix[1] - Multifamily.COGS.Matrix[1]
 
#Imports static columns+
Gross.Margin.Matrix[2:4] = data.frame(LEN.HomeBuildRevData[2:4])
Gross.Margin.Matrix[5] = data.frame("LennarAggregate")

#Percentage of sales method
Operating.Expense.Matrix = LEN.HomeBuildRevData
names(Operating.Expense.Matrix) = c("Operating.Expns","Trial","year","Iteration","Segment")
Operating.Expense.Matrix[1] = data.frame(LEN.HomeBuildRevData[1] * 0.1201) #past 4 year average % of home building sales
Operating.Expense.Matrix[2] = data.frame(LEN.HomeBuildRevData[2])
Operating.Expense.Matrix[3] = data.frame(LEN.HomeBuildRevData[3])

CalAtlantic.Operating.Expense.Matrix = Cumulative.Predicted.CalAtlanticRevenue.Data
names(Operating.Expense.Matrix) = c("Operating.Expns","Trial","year","Iteration","Segment")
CalAtlantic.Operating.Expense.Matrix[1] = data.frame(CalAtlantic.Operating.Expense.Matrix[1] * 0.1154) #past 4 year average % of home building sales
CalAtlantic.Operating.Expense.Matrix[2] = data.frame(CalAtlantic.Operating.Expense.Matrix[2])
CalAtlantic.Operating.Expense.Matrix[3] = data.frame(CalAtlantic.Operating.Expense.Matrix[3])

#Multifamily Operating expense missing
#Finance Operating expense missing





#EBIT#####

#Shapes the ebit matrix the same way as homebuilding matrix, but we will overwrite column one for EBIT
EBIT.Matrix = LEN.HomeBuildRevData
names(EBIT.Matrix) = c("EBIT","Trial","Year","Iteration","Segment")
EBIT.Matrix[1] = data.frame(Gross.Margin.Matrix[1] - Operating.Expense.Matrix[1]-CalAtlantic.Operating.Expense.Matrix[1]) #past 4 year average % of home building sales
EBIT.Matrix[5] = data.frame("LennarAggregate")

ggplot(data= rbind(EBIT.Matrix), aes(x=Year, y=EBIT, color = Trial,alpha=0, varwidth = TRUE))+ylab("Consolidated EBIT (Millions)")  +
  geom_boxplot(show.legend = FALSE,outlier.stroke = 0, outlier.size = 0, outlier.shape = NULL, notch = TRUE) + 
  stat_summary(fun.y=mean,colour="black",linetype="dashed", geom="line", size=0.8, show.legend = FALSE)+ scale_x_continuous(breaks = seq(from=2013,to=endyear,by=1))   +
  scale_y_continuous(breaks = seq(from=0,to=2900,by=100),limits = c(0, 2900)) + stat_summary(fun.y=mean,colour="darkred", color = "black", geom="point", shape = 18, size=4, show.legend = FALSE)
ggsave(filename="EBIT.pdf",device = pdf, height = 6, width = 10) 

#EBIT SUMMARY STATISTICS####
#beginning forecast year
OutputYear = 2018
while (OutputYear < endyear) {
print("###########################################################################################")
print(paste(OutputYear, "            Mean"))
#aggregate(x, by, FUNCTION)
print(aggregate(EBIT.Matrix %>%select(EBIT,Trial,Year,Segment)%>%filter(Year == OutputYear)%>%select(EBIT), EBIT.Matrix %>%select(EBIT,Trial,Year,Segment)%>%filter(Year == OutputYear)%>%select(Segment), mean)
)
Sys.sleep(.25)
print("------------------------------------------------------------------------")
#Standard Deviation
print(paste(OutputYear, "Standard Deviation"))
print(aggregate(EBIT.Matrix %>%select(EBIT,Trial,Year,Segment)%>%filter(Year == OutputYear)%>%select(EBIT),EBIT.Matrix %>%select(EBIT,Trial,Year,Segment)%>%filter(Year == OutputYear)%>%select(Segment), sd))
Sys.sleep(.25)
print("------------------------------------------------------------------------")

#Summary
print(paste(OutputYear, "Statistics Summary"))
print(aggregate(EBIT.Matrix %>%select(EBIT,Trial,Year,Segment)%>%filter(Year == OutputYear)%>%select(EBIT),EBIT.Matrix %>%select(EBIT,Trial,Year,Segment)%>%filter(Year == OutputYear)%>%select(Segment), summary))
Sys.sleep(.25)
print("###########################################################################################")
  OutputYear=OutputYear+1
}






Historical.CF.Statement = data.frame(read.csv("LEN CF 14-17.csv"))



#This section Aggregates all data###############

#Creates the right size of matrix
TotalRevenue = data.frame(matrix(nrow = nrow(Cumulative.Predicted.FinancialRevenue.Data)*4, ncol = 5))

#Aggregate Revenues
names(TotalRevenue) = c("Revenue","Trial","Year","Iteration","Segment")
TotalRevenue[1] = data.frame(rbind(LEN.HomeBuildRevData[1], Cumulative.Predicted.FinancialRevenue.Data[1],Cumulative.Predicted.OtherRevenue.Data[1], Cumulative.Predicted.CalAtlanticRevenue.Data[1]))
TotalRevenue[2] = data.frame(rbind(LEN.HomeBuildRevData[2], Cumulative.Predicted.FinancialRevenue.Data[2],Cumulative.Predicted.OtherRevenue.Data[2], Cumulative.Predicted.CalAtlanticRevenue.Data[2]))
TotalRevenue[3] = data.frame(rbind(LEN.HomeBuildRevData[3], Cumulative.Predicted.FinancialRevenue.Data[3],Cumulative.Predicted.OtherRevenue.Data[3], Cumulative.Predicted.CalAtlanticRevenue.Data[3]))
TotalRevenue[4] = data.frame(rbind(LEN.HomeBuildRevData[4], Cumulative.Predicted.FinancialRevenue.Data[4],Cumulative.Predicted.OtherRevenue.Data[4], Cumulative.Predicted.CalAtlanticRevenue.Data[4]))
TotalRevenue[5] = data.frame(rbind(LEN.HomeBuildRevData[5], Cumulative.Predicted.FinancialRevenue.Data[5],Cumulative.Predicted.OtherRevenue.Data[5], Cumulative.Predicted.CalAtlanticRevenue.Data[5]))


#SEGMENT Revenue SUMMARY DATA###########
#Year
Yr= 2018                         

#mean
print(paste(Yr, "mean by segment"))
#aggregate(x, by, FUNCTION)
aggregate(TotalRevenue %>%select(Revenue,Trial,Year,Segment)%>%filter(Year == Yr)%>%select(Revenue), TotalRevenue %>%select(Revenue,Trial,Year,Segment)%>%filter(Year == Yr)%>%select(Segment), mean)

#Standard Deviation
print(paste(Yr, "Standard Deviation by segment"))
aggregate(TotalRevenue %>%select(Revenue,Trial,Year,Segment)%>%filter(Year == Yr)%>%select(Revenue),TotalRevenue %>%select(Revenue,Trial,Year,Segment)%>%filter(Year == Yr)%>%select(Segment), sd)

#Summary
print(paste(Yr, "Statistics Summary by segment"))
aggregate(TotalRevenue %>%select(Revenue,Trial,Year,Segment)%>%filter(Year == Yr)%>%select(Revenue),TotalRevenue %>%select(Revenue,Trial,Year,Segment)%>%filter(Year == Yr)%>%select(Segment), summary)

SummaryData = TotalRevenue %>%
select(Revenue,Trial,Year,Segment) %>%
filter(Year == 2018)
#Above formulas broken down
#TotalRevenue %>%select(Revenue,Trial,Year,Segment)%>%filter(Year == Yr)%>%select(Revenue)
#TotalRevenue %>%select(Revenue,Trial,Year,Segment)%>%filter(Year == Yr)%>%select(Segment)
summary(SummaryData)






#FCF########

Taxes = .21
#Free cash flow formula 
FCFF.Matrix = EBIT.Matrix[1] * (1-Taxes) + Cumulative.PredictedNWC.Data[1] + Cumulative.PredictedNCC.Data[1]
#Imports columns 2 through five from Ebit Matrix
FCFF.Matrix[2:5] = EBIT.Matrix[2:5]  


ggplot(data= rbind(FCFF.Matrix), aes(x=Year, y=FCFF, color = Trial,alpha=0, varwidth = TRUE))+ylab("Consolidated FCFF (Millions)")  +
  geom_boxplot(show.legend = FALSE,outlier.stroke = 0, outlier.size = 0, outlier.shape = NULL, notch = TRUE) + 
  stat_summary(fun.y=mean,colour="black",linetype="dashed", geom="line", size=0.8, show.legend = FALSE)+ scale_x_continuous(breaks = seq(from=2013,to=endyear,by=1))   +
  scale_y_continuous(breaks = seq(from=0,to=2600,by=100),limits = c(0, 2600)) + stat_summary(fun.y=mean,colour="darkred", color = "black", geom="point", shape = 18, size=4, show.legend = FALSE) +
ggsave(filename="FCFF.pdf",device = pdf, height = 6, width = 10)

names(FCFF.Matrix) = c("FCFF","Trial","Year","Iteration","Segment")


#####
OutputYear = 2018
while (OutputYear < endyear) {
  print("###########################################################################################")
  print(paste(OutputYear, "            Mean"))
  #aggregate(x, by, FUNCTION)
  print(aggregate(FCFF.Matrix %>%select(FCFF,Trial,Year,Segment)%>%filter(Year == OutputYear)%>%select(FCFF), FCFF.Matrix %>%select(FCFF,Trial,Year,Segment)%>%filter(Year == OutputYear)%>%select(Segment), mean)
  )
  Sys.sleep(.25)
  print("------------------------------------------------------------------------")
  #Standard Deviation
  print(paste(OutputYear, "Standard Deviation"))
  print(aggregate(FCFF.Matrix %>%select(FCFF,Trial,Year,Segment)%>%filter(Year == OutputYear)%>%select(FCFF),FCFF.Matrix %>%select(FCFF,Trial,Year,Segment)%>%filter(Year == OutputYear)%>%select(Segment), sd))
  Sys.sleep(.25)
  print("------------------------------------------------------------------------")
  
  #Summary
  print(paste(OutputYear, "Statistics Summary"))
  print(aggregate(FCFF.Matrix %>%select(FCFF,Trial,Year,Segment)%>%filter(Year == OutputYear)%>%select(FCFF),FCFF.Matrix %>%select(FCFF,Trial,Year,Segment)%>%filter(Year == OutputYear)%>%select(Segment), summary))
  Sys.sleep(.25)
  print("###########################################################################################")
  OutputYear=OutputYear+1
}
  



#Discounted Cash Flow Model #########  
# of Forecast Years before terminal value is used. Must equal Previous inputs
Forecast.years = 4


Future.FCFF.Matrix = FCFF.Matrix %>% 
  select(FCFF,Trial,Year,Iteration,Segment)%>%
  filter(Year > 2017) 

discount.rate = 0.10

DiscountedFuture.FCFF.Matrix = Future.FCFF.Matrix
DiscountedFuture.FCFF.Matrix[1] = Future.FCFF.Matrix[1] / ((1+discount.rate)**(Future.FCFF.Matrix[,3]-2017))


#This section assigns each datapoint to its corresponding iteration datapoint by year. This will help do NPV.
L = 1
Temp = 0
CumulativeTempVar =0
while(L <= k) {                  #Number of repetitions here should be the number of years. Currently, it is set to 4
Temp = data.frame(DiscountedFuture.FCFF.Matrix[L,1],DiscountedFuture.FCFF.Matrix[L+k,1],DiscountedFuture.FCFF.Matrix[L+k*2,1],
                    DiscountedFuture.FCFF.Matrix[L+k*3,1])
#Loop binds all the iterations to create data
CumulativeTempVar = rbind(CumulativeTempVar,Temp)
L = L+1
}
names(CumulativeTempVar) = c(2021,2020,2019,2018)
CumulativeTempVar = CumulativeTempVar[2:k,]

#Future Free Cashflows only.
NPVs.Discounted.FFCFF = data.frame(rowSums(CumulativeTempVar))
names(NPVs.Discounted.FFCFF) = "DiscountedCFs"
#Histogram
hist(NPVs.Discounted.FFCFF[,],breaks = 150, col=blues9, xlim = c(2000,7000), xlab = "NPV of Discounted CFS", main = "Histogram of Possible FFCFF NPVs")

summary(NPVs.Discounted.FFCFF)


WACC = .0931
Terminal.Growth.Rate = 0.02
#Q3BalanceSheetCash + Rialto Sale
Cash = 1048.416 + 340
MarketableSecurities = 0
InterestBearingDebt = 9407.987 + 1651  #SeniorNotes + Other Liabilities (10-k Figures)
MinorityInterests = 113
#NPVofMultifamily.Equity.In.Earnings.From.Unconsolidated.Entities = 73.432 * (Terminal.Growth.Rate) / (WACC-Terminal.Growth.Rate)

#2022 FCF * (1+g) / (WACC - g)
TerminalValue = (Future.FCFF.Matrix[n:k,1] * (1+Terminal.Growth.Rate)) / (WACC-Terminal.Growth.Rate)
#Discounted by 4 Years
DiscountedTerminal.Value =  TerminalValue / (1+WACC)**Forecast.years 


EnterpriseValue = NPVs.Discounted.FFCFF + DiscountedTerminal.Value #+ NPVofMultifamily.Equity.In.Earnings.From.Unconsolidated.Entities
FirmValue = EnterpriseValue + Cash + MarketableSecurities
EquityValue = FirmValue - InterestBearingDebt - MinorityInterests

SharesOutstanding = 292.540824
Intrinsic.Value.Per.Share = EquityValue/SharesOutstanding
names(Intrinsic.Value.Per.Share) = "Instrinsic Value Per Share"
hist(Intrinsic.Value.Per.Share[,],breaks = 150, xaxp=c(-30,90,10), col=blues9, xlim = range(-30,90), xlab = "Intrinsic Value Per Share", main = "Histogram of Possible Intrinsic Values", freq = )

summary(Intrinsic.Value.Per.Share)
paste("Standard Deviation =", sd(unlist(Intrinsic.Value.Per.Share[])))
