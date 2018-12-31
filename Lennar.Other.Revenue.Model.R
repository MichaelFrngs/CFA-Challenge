setwd("D:/Users/MihalyTower/Documents/Actual Documents/R Programing studies/Projects/CFA NYSE-LEN Monte Carlo")
library(ggplot2)
library(readr)
library(dplyr)
library(scales)

#THIS SECTION INCLUDES ONLY LENAR MULTIFAMILY (RIALTO WILL BE SOLD)
#Other.Revenue Model
#Lennar Other Services Revenue Model

#Initiate data.frame to prevent error later.
Cumulative.Predicted.OtherRevenue.Data = data.frame()

#INPUTS
Sales.Growth.SD.Assumption = 0.20
rev.growth.rate.array = array(c(0.0538,0.18,0.18,0.18)) #The first number here is the SECOND growth rate
years = 4 #Number of years to forecast
n = 1 #Number of trials per year
k = 3000 #Number of iterations (# of data points)


#Place historical segment revenues here. This is just for viewing, it is not used.
HistoricalRev.Array = array(c(14.746, 69.78, 164.613, 287.441, 394.741)) 

#Initiate dataframe to prevent error during loop
HistoricalFrame = data.frame()
j = 0 #Leave this variable alone. It's a counter
i = 0 #Leave this variable alone. It's a counter
#Historical Dataframe to display historical points when we plot
while (j < 5) {
  #Begin iterations for each year
  while (i < n) {
    #K = number of points to create
    revenue.iterations = data.frame(rep(HistoricalRev.Array[j+1],k))
    revenue.iterations[,2] = data.frame(Trial.Group = paste(j,"Hist"))
    revenue.iterations[,3] = data.frame(Year = 2013+j)
    revenue.iterations[,4] = data.frame(Iteration = seq(from = 1, to=k))
    
    HistoricalFrame = data.frame(rbind(revenue.iterations,HistoricalFrame))
    
    i = i+1
  }
  #Reset loop
  i = 0
  j = j+1
}

names(HistoricalFrame) = c("Revenue", "Trial", "Year", "Iteration")



j = 0 #Leave this alone. It's a counter for years.
i = 0 #Leave this alone. It's a counter for iterations
z = 0 #Leave this alone. It's a counter
g.cumulative = 1 #Initiate variable
while (j < years) {
  #Begin iterations for each year
  while (i < n) {
    
    #Creates growth rates
    while (z<=j)
    {g.temp =  1+rnorm(k, mean= rev.growth.rate.array[(j+1)], sd= Sales.Growth.SD.Assumption)
    g.cumulative = g.temp * g.cumulative
    z=z+1
    }
    
    revenue.iterations2 = data.frame(g.cumulative*394.741) #394.741 = 2017 revenue
    revenue.iterations2[,2] = data.frame(Trial.Group = paste("Trial ",i+1,2018+j))
    revenue.iterations2[,3] = data.frame(Year = 2018+j)
    revenue.iterations2[,4] = data.frame(Iteration = seq(from = 1, to=k))
    
    Cumulative.Predicted.OtherRevenue.Data = data.frame(rbind(revenue.iterations2,Cumulative.Predicted.OtherRevenue.Data))
    
    i = i+1
  }
  #Reset loop
  i = 0
  j = j+1

  #Changes sales growth rate to the array. (ADD THIS IF YOU WANT TO VARY)
  Sales.Growth.Assumption = rev.growth.rate.array[(j)]
  
}

#Evaluates the last year forecasted
endyear = 2018+years
names(Cumulative.Predicted.OtherRevenue.Data) = c("Revenue", "Trial", "Year","Iteration")



              #rbind here merges historical & forecast data for plotting
#ggplot(data= rbind(Cumulative.Predicted.OtherRevenue.Data,HistoricalFrame), aes(x=Year, y=Revenue, color = Trial,alpha=0, varwidth = TRUE))  +
#  geom_boxplot(show.legend = FALSE) +
#  stat_summary(fun.y=mean,colour="darkred",geom="point", shape=18, size=3, show.legend = FALSE)+ scale_x_continuous(breaks = seq(from=2013,to=endyear,by=1))  + ggsave(filename="Rialto&MultifamilyRevenue.pdf",device = pdf, height = 10, width = 20) +ylab("Rialto&MultifamilyRevenue.pdf")

ggplot(data= rbind(Cumulative.Predicted.OtherRevenue.Data), aes(x=Year, y=Revenue, color = Trial,alpha=0, varwidth = TRUE))+ylab("Multifamily Revenue (Millions)")  +
  geom_boxplot(show.legend = FALSE,outlier.stroke = 0, outlier.size = 0, outlier.shape = NULL, notch = TRUE) + 
  stat_summary(fun.y=mean,colour="black",linetype="dashed", geom="line", size=0.8, show.legend = FALSE)+ scale_x_continuous(breaks = seq(from=2013,to=endyear,by=1))   +
  scale_y_continuous(breaks = seq(from=0,to=1200,by=100),limits = c(0, 1200)) + stat_summary(fun.y=mean,colour="darkred", color = "black", geom="point", shape = 18, size=4, show.legend = FALSE)
ggsave(filename="MultifamilyRevenue.pdf",device = pdf, height = 6, width = 10) 


#Print statistical summaries
LEN.MultiFamilyAndRialto.SummaryData = Cumulative.Predicted.OtherRevenue.Data %>%
  select(Revenue,Trial,Year) %>%
  filter(Year == 2018)
#print(LEN.MultiFamilyAndRialto.SummaryData) 
summary(LEN.MultiFamilyAndRialto.SummaryData)
print(paste("Standard Deviation of LEN.Fin.Svc Revenue $" , sd(as.numeric(unlist((LEN.MultiFamilyAndRialto.SummaryData[1]))))))


#Print statistical summaries for every forecast year
OutputYear = 2018
while (OutputYear < endyear) {
  LEN.MultiFamilyAndRialto.SummaryData = Cumulative.Predicted.OtherRevenue.Data %>%
    select(Revenue,Trial,Year) %>%
    filter(Year == OutputYear)
  print(paste("Standard Deviation of Multifamily Revenue for", OutputYear, " = $" , sd(as.numeric(unlist((LEN.MultiFamilyAndRialto.SummaryData[1])))),"million"))
  print(summary(LEN.MultiFamilyAndRialto.SummaryData))
  OutputYear=OutputYear+1
}





Cumulative.Predicted.OtherRevenue.Data[5] = data.frame(Segment = "Rialto&MultiFamily")