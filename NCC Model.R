setwd("D:/Users/MihalyTower/Documents/Actual Documents/R Programing studies/Projects/CFA NYSE-LEN Monte Carlo")
library(ggplot2)
library(readr)
library(dplyr)
library(scales)



#Lennar CalAtlantic Services Revenue Model
Cumulative.PredictedNCC.Data = data.frame()



#INPUTS

Sales.Growth.Assumption = 0.1
Sales.Growth.SD.Assumption = 0.1
years = 4 #Number of years to forecast
n = 1 #Number of Trials per year
k = 3000 #Number of iterations per trial




#Place historical segment revenues here
HistoricalRev.Array = array(c(155.492, 165.431, 74.720, 279.776, 412.469)) 
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
names(HistoricalFrame) = c("NccChange", "Trial", "Year", "Iteration")


j = 0 #Leave this alone. It's a counter
i = 0 #Leave this alone. It's a counter
while (j < years) {
  #Begin iterations for each year
  while (i < n) {
    #K iterations per trial, can be increased.
    revenue.iterations = data.frame(((1+rnorm(k, mean= Sales.Growth.Assumption, sd= Sales.Growth.SD.Assumption))**(j+1))*(412.469)) #412.469 = 2017 NCC
    revenue.iterations[,2] = data.frame(Trial.Group = paste("Trial ",i+1,2018+j))
    revenue.iterations[,3] = data.frame(Year = 2018+j)
    revenue.iterations[,4] = data.frame(Iteration = seq(from = 1, to=k))
    
    Cumulative.PredictedNCC.Data = data.frame(rbind(revenue.iterations,Cumulative.PredictedNCC.Data))
    
    i = i+1
  }
  
  #Reset loop
  i = 0
  j = j+1
}


endyear = 2018+years
names(Cumulative.PredictedNCC.Data) = c("NccChange", "Trial", "Year","Iteration")


#rbind here merges historical & forecast data for plotting
#ggplot(data= rbind(Cumulative.PredictedNCC.Data,HistoricalFrame), aes(x=Year, y=NccChange, color = Trial,alpha=0, varwidth = TRUE))  +
#  geom_boxplot(show.legend = FALSE) +
#  stat_summary(fun.y=mean,colour="darkred",geom="point", shape=18, size=3, show.legend = FALSE)+
#  scale_x_continuous(breaks = seq(from=2013,to=endyear,by=1))  +ylab("Change in NCC") + ggsave(filename="NCC.pdf",device = pdf, height = 10, width = 20) +ylab("NCC.Pdf")

ggplot(data= rbind(Cumulative.PredictedNCC.Data), aes(x=Year, y=NccChange, color = Trial,alpha=0, varwidth = TRUE))+ylab("Change in NCC (Millions)")  +
  geom_boxplot(show.legend = FALSE,outlier.stroke = 0, outlier.size = 0, outlier.shape = NULL, notch = TRUE) + 
  stat_summary(fun.y=mean,colour="black",linetype="dashed", geom="line", size=0.8, show.legend = FALSE)+ scale_x_continuous(breaks = seq(from=2013,to=endyear,by=1))   +
  scale_y_continuous(breaks = seq(from=0,to=1000,by=100),limits = c(0, 1000)) + stat_summary(fun.y=mean,colour="darkred", color = "black", geom="point", shape = 18, size=4, show.legend = FALSE)
ggsave(filename="Change in NCC.pdf",device = pdf, height = 6, width = 10) 


#Print statistical summaries for every forecast year
OutputYear = 2018
while (OutputYear < endyear) {
  NCC.SummaryData = Cumulative.PredictedNCC.Data %>%
    select(NccChange,Trial,Year) %>%
    filter(Year == OutputYear)
  print(paste("Standard Deviation of NCC for", OutputYear, " = $" , sd(as.numeric(unlist((NCC.SummaryData[1])))),"million"))
  print(summary(NCC.SummaryData))
  OutputYear=OutputYear+1
}




Cumulative.PredictedNCC.Data[5] = data.frame(Segment = "LennarAggregate")
