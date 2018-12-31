setwd("D:/Users/MihalyTower/Documents/Actual Documents/R Programing studies/Projects/CFA NYSE-LEN Monte Carlo")
library(ggplot2)
library(readr)
library(dplyr)
library(scales)

#Home building Revenue section
HistoricalRevenue = data.frame(read.csv("LEN.HomeBuildingHistoricalRevenue.csv"))
LEN.HomeBuildRevData = data.frame()

#INPUTS

Sales.Growth.SD.Assumption = 0.0763
years = 4 #Number of years to forecast
n = 1 #Number of trials (Vertical lines in output graph per year)
k = 3000 #Number of iterations (data points) 
rev.growth.rate.array = array(c(.033,.051,.051,.0657)) #The first number here is the SECOND growth rate


#Place historical segment revenues here
HistoricalRev.Array = array(c(5354.9, 7025.1, 8466.9, 9741.3, 11200.2)) 

#Initiate frame to prevent error during loop
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


  

j = 0 #Leave this alone. It's a counter
i = 0 #Leave this alone. It's a counter
z = 0 #Leave this alone. It's a counter
g.cumulative = 1 #Initiate variable
while (j < years) {
  #Begin iterations for each year
  while (i < n) {
    #Create growth rates                                        #K = number of points to create
    while (z<=j)
    {g.temp =  1+rnorm(k, mean= rev.growth.rate.array[(j+1)], sd= Sales.Growth.SD.Assumption)
    g.cumulative = g.temp * g.cumulative
    z=z+1
    }
    revenue.iterations2 = data.frame(g.cumulative*(11200.2)) #11200.2 = 2017 revenue
    revenue.iterations2[,2] = data.frame(Trial.Group = paste("Trial ",i+1,2018+j))
    revenue.iterations2[,3] = data.frame(Year = 2018+j)
    revenue.iterations2[,4] = data.frame(Iteration = seq(from = 1, to=k))
    
    LEN.HomeBuildRevData = data.frame(rbind(revenue.iterations2,LEN.HomeBuildRevData))
    
 i = i+1
  }
  #Reset loop
  i = 0
  j = j+1

  #Changes sales growth rate to the array. (ADD THIS IF YOU WANT TO VARY)
  Sales.Growth.Assumption = rev.growth.rate.array[(j)]
    
}

endyear = 2018+years

names(LEN.HomeBuildRevData) = c("Revenue", "Trial", "Year", "Iteration")

            #rbind here merges historical & forecast data for plotting
#ggplot(data=rbind(LEN.HomeBuildRevData,HistoricalFrame), aes(x=Year, y=Revenue, color = Trial,alpha=0, varwidth = TRUE))  +
#  geom_boxplot(show.legend = FALSE) +
#  stat_summary(fun.y=mean,colour="darkred",geom="point", shape=18, size=3, show.legend = FALSE)+ scale_x_continuous(breaks = seq(from=2013,to=endyear,by=1))  + ggsave(filename="HomeBuildingRevenue.pdf",device = pdf, height = 10, width = 20) +ylab("HomeBuildingRevenue")

ggplot(data= rbind(LEN.HomeBuildRevData), aes(x=Year, y=Revenue, color = Trial,alpha=0, varwidth = TRUE))+ylab("Homebuilding Revenue (Millions)")  +
  geom_boxplot(show.legend = FALSE,outlier.stroke = 0, outlier.size = 0, outlier.shape = NULL, notch = TRUE) + 
  stat_summary(fun.y=mean,colour="black",linetype="dashed", geom="line", size=0.8, show.legend = FALSE)+ scale_x_continuous(breaks = seq(from=2013,to=endyear,by=1))   +
  scale_y_continuous(breaks = seq(from=10000,to=18000,by=1000),limits = c(10000, 18000)) + stat_summary(fun.y=mean,colour="darkred", color = "black", geom="point", shape = 18, size=4, show.legend = FALSE)+
ggsave(filename="HomebuildingRevenue.pdf",device = pdf, height = 6, width = 10) 


#Print statistical summaries for every forecast year
OutputYear = 2018
while (OutputYear < endyear) {
  LEN.HomeBuildRev.SummaryData = LEN.HomeBuildRevData %>%
    select(Revenue,Trial,Year) %>%
    filter(Year == OutputYear)
  print(paste("Standard Deviation of Homebuilding Revenue for", OutputYear, " = $" , sd(as.numeric(unlist((LEN.HomeBuildRev.SummaryData[1])))),"million"))
  print(summary(LEN.HomeBuildRev.SummaryData))
  OutputYear=OutputYear+1
}


LEN.HomeBuildRevData[5] = data.frame(Segment = "HomeBuilding")
