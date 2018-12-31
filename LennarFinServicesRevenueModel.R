setwd("D:/Users/MihalyTower/Documents/Actual Documents/R Programing studies/Projects/CFA NYSE-LEN Monte Carlo")
library(ggplot2)
library(readr)
library(dplyr)
library(scales)

#Lennar Financial Services Revenue Model
Cumulative.Predicted.FinancialRevenue.Data = data.frame()


#INPUTS
Sales.Growth.SD.Assumption = 0.1365
years = 4 #Number of years to forecast
n = 1 #Number of Trials per year
k = 3000 #Number of data points per trial)


j = 0 #Leave this alone. It's a counter
i = 0 #Leave this alone. It's a counter

HistoricalRev.Array = array(c(427.3, 454.4, 620.5, 687.3, 770.1)) #Place historical segment revenues here
HistoricalFrame = data.frame()

rev.growth.rate.array = array(c(0.6667,0.0487,0.051,.0657)) 



#Historical Dataframe to display when we plot
while (j < 5) {
  #Begin iterations for each year
  
  while (i < n) {
    #K = number of points to create
    revenue.iterations = data.frame(rep(HistoricalRev.Array[j+1],k))
    revenue.iterations[,2] = data.frame(Trial.Group = paste(j,"Hist"))
    revenue.iterations[,3] = data.frame(Year = 2013+j)
    revenue.iterations[,4] = data.frame(Iteration = seq(from = 1, to =k))
    
    HistoricalFrame = data.frame(rbind(revenue.iterations,HistoricalFrame))
    
    i = i+1
  }
  #Reset loop
  i = 0
  j = j+1
}
names(HistoricalFrame) = c("Revenue", "Trial", "Year", "Iteration")



#Reset Counter
j = 0 #Leave this alone. It's a counter
i = 0 #Leave this alone. It's a counter
z = 0 #Leave this alone. It's a counter
g.cumulative = 1 #Initiate variable
#Creates growth rate matrix
while (j < years) {
  #Begin iterations for each year

  while (i < n) {
      
    #Create growth rates                                        #K = number of points to create
    while (z<=j)
    {g.temp =  1+rnorm(k, mean= rev.growth.rate.array[(j+1)], sd= Sales.Growth.SD.Assumption)
    g.cumulative = g.temp * g.cumulative
    z=z+1
    }
    
    revenue.iterations2 = data.frame((g.cumulative)*770.1)#*HistoricalRev.Array)
    revenue.iterations2[,2] = data.frame(Trial.Group = paste("Trial ",i+1,2018+j))
    revenue.iterations2[,3] = data.frame(Year = 2018+j)
    revenue.iterations2[,4] = data.frame(Iteration = paste(seq(1:k)))
    
    Cumulative.Predicted.FinancialRevenue.Data = data.frame(rbind(revenue.iterations2,Cumulative.Predicted.FinancialRevenue.Data))
    
    i = i+1
  }
  #Reset loop
  i = 0
  j = j+1
  z=0
  
  #Changes sales growth rate to the array. (ADD THIS IF YOU WANT TO VARY)
  #Sales.Growth.Assumption = rev.growth.rate.array[(j)]
}

endyear = 2018+years

names(Cumulative.Predicted.FinancialRevenue.Data) = c("Revenue", "Trial", "Year", "Iteration")


#rbind here merges historical & forecast data for plotting
#ggplot(data= rbind(Cumulative.Predicted.FinancialRevenue.Data,HistoricalFrame), aes(x=Year, y=Revenue, color = Trial ,alpha=0, varwidth = TRUE))  +
#  geom_boxplot(show.legend = FALSE) +
#  stat_summary(fun.y=mean,colour="darkred",geom="point", shape=18, size=3, show.legend = FALSE)+ scale_x_continuous(breaks = seq(from=2013,to=endyear,by=1))  + ggsave(filename="FinancialRevenue.pdf",device = pdf, height = 10, width = 20) +ylab("Financial Services Revenue")

ggplot(data= rbind(Cumulative.Predicted.FinancialRevenue.Data), aes(x=Year, y=Revenue, color = Trial,alpha=0, varwidth = TRUE))+ylab("Financial Service Revenue (Millions)")  +
  geom_boxplot(show.legend = FALSE,outlier.stroke = 0, outlier.size = 0, outlier.shape = NULL, notch = TRUE) + 
  stat_summary(fun.y=mean,colour="black",linetype="dashed", geom="line", size=0.8, show.legend = FALSE)+ scale_x_continuous(breaks = seq(from=2013,to=endyear,by=1))   +
  scale_y_continuous(breaks = seq(from=0,to=2000,by=100),limits = c(0, 2000)) + stat_summary(fun.y=mean,colour="darkred", color = "black", geom="point", shape = 18, size=4, show.legend = FALSE)
ggsave(filename="FinancialServices.pdf",device = pdf, height = 6, width = 10) 


#Print statistical summaries for every forecast year
OutputYear = 2018
while (OutputYear < endyear) {
  LEN.FinSvc.SummaryData = Cumulative.Predicted.FinancialRevenue.Data %>%
    select(Revenue,Trial,Year) %>%
    filter(Year == OutputYear)
  print(paste("Standard Deviation of Financial Service Revenue for", OutputYear, " = $" , sd(as.numeric(unlist((LEN.FinSvc.SummaryData[1])))),"million"))
  print(summary(LEN.FinSvc.SummaryData))
  OutputYear=OutputYear+1
}




Cumulative.Predicted.FinancialRevenue.Data[5] = data.frame(Segment = "FinancialRevenue")
