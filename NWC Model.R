setwd("D:/Users/MihalyTower/Documents/Actual Documents/R Programing studies/Projects/CFA NYSE-LEN Monte Carlo")
library(ggplot2)
library(readr)
library(dplyr)
library(scales)



#Lennar CalAtlantic Services Revenue Model
HistoricalNWC = data.frame(read.csv("NWC Matrix.csv"))
Cumulative.PredictedNWC.Data = data.frame()



#INPUTS
NWC.Growth.Assumption = 0.55
NWC.Growth.SD.Assumption = 0.1
years = 4 #Number of years to forecast
n = 1 #Number of Trials per year
k = 3000 #Number of iterations per trial


#Place historical segment revenues here
HistoricalRev.Array = array(c(-1468.132, -1582.612, -1313.566, -685.063, -187.359)) 




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
names(HistoricalFrame) = c("NwcChange", "Trial", "Year", "Iteration")




j = 0 #Leave this alone. It's a counter
i = 0 #Leave this alone. It's a counter
while (j < years) {
  #Begin iterations for each year
  while (i < n) {
    #K iterations per trial, can be increased.
    revenue.iterations2 = data.frame(((1+rnorm(k, mean= NWC.Growth.Assumption, sd= NWC.Growth.SD.Assumption))**(j+1))*(-187.359)) #-187.359 = 2017 NWC Change
    revenue.iterations2[,2] = data.frame(Trial.Group = paste("Trial ",i+1,2018+j))
    revenue.iterations2[,3] = data.frame(Year = 2018+j)
    revenue.iterations2[,4] = data.frame(Iteration = seq(from = 1, to=k))
    
    Cumulative.PredictedNWC.Data = data.frame(rbind(revenue.iterations2,Cumulative.PredictedNWC.Data))
    
    i = i+1
  }
  
  #Reset loop
  i = 0
  j = j+1
}



endyear = 2018+years



names(Cumulative.PredictedNWC.Data) = c("NwcChange", "Trial", "Year", "Iteration")


          #rbind here merges historical & forecast data for plotting
#ggplot(data= rbind(Cumulative.PredictedNWC.Data,HistoricalFrame), aes(x=Year, y=NwcChange, color = Trial,alpha=0, varwidth = TRUE))  +
#  geom_boxplot(show.legend = FALSE) +
#  stat_summary(fun.y=mean,colour="darkred",geom="point", shape=18, size=3, show.legend = FALSE)+
#  scale_x_continuous(breaks = seq(from=2013,to=endyear,by=1))  +ylab("Change in NWC") + ggsave(filename="NWC.pdf",device = pdf, height = 10, width = 20) +ylab("NWC.Pdf")

ggplot(data= rbind(Cumulative.PredictedNWC.Data), aes(x=Year, y=NwcChange, color = Trial,alpha=0, varwidth = TRUE))+ylab("Change in NWC (Millions)")  +
  geom_boxplot(show.legend = FALSE,outlier.stroke = 0, outlier.size = 0, outlier.shape = NULL, notch = TRUE) + 
  stat_summary(fun.y=mean,colour="black",linetype="dashed", geom="line", size=0.8, show.legend = FALSE)+ scale_x_continuous(breaks = seq(from=2013,to=endyear,by=1))   +
  scale_y_continuous(breaks = seq(from=-1000,to=100,by=100),limits = c(-1000, 100)) + stat_summary(fun.y=mean,colour="darkred", color = "black", geom="point", shape = 18, size=4, show.legend = FALSE)
ggsave(filename="Change in NWC.pdf",device = pdf, height = 6, width = 10) 


#Print statistical summaries for every forecast year
OutputYear = 2018

while (OutputYear < endyear) {
NWC.SummaryData = Cumulative.PredictedNWC.Data %>%
  select(NwcChange,Trial,Year) %>%
  filter(Year == OutputYear)

print(paste("Standard Deviation of Change in NWC for", OutputYear, " = $" , sd(as.numeric(unlist((NWC.SummaryData[1])))),"million"))

print(summary(NWC.SummaryData))

OutputYear=OutputYear+1
}








Cumulative.PredictedNWC.Data[5] = data.frame(Segment = "LennarAggregate")
