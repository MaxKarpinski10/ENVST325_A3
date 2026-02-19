#Homework 3
#Max Karpinski 
#ENVST 325: Environmental Data Science, Hamilton College

#ggplot
#set up for longform data, easy/nice 

#base R plotting
#default makes ugly, simple graphs, poor formatting, adds a lot of features 

#InClass----
#install.packages(c("ggplot2","dplyr", "lubridate"))
library(ggplot2)
library(dplyr)
library(lubridate)

datCO2 = read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")

colnames(datCO2)[4] = "CO2"

datCO2$Entity = as.factor(datCO2$Entity)

US = datCO2 %>%
  filter(Entity == "United States")
  plot(US$Year, US$CO2, type='b', xlab = "Year", ylab = "Fossil fuel emissions (billions of tons of CO2)", pch=19, yaxt = "n")
axis(2, seq(0, 6000000000, by=2000000000), 
     seq(0,6, by=2), las=2)

#Shifting to ggplot
ggplot(US, aes(x=Year, y=CO2))+
  geom_point()+
  geom_line()+
  labs(x="Year", y="US fossil fuel C02 emissions (tons CO2)")+
  theme_classic()

NorthA = datCO2 %>%
  filter(Entity == "United States" | Entity == "Mexico" | Entity == "Canada")

ggplot(NorthA, aes(x=Year, y=CO2, color=Entity))+
  geom_point()+
  geom_line()+
  scale_color_manual(values=c("red","royalblue", "darkgoldenrod3"))

#In_class Prompts----
#Prompt 1:
#Make a plot of air temperature anomalies in the Northern and Southern Hemisphere in base R and in ggplot2. 
tempAnom = read.csv("/cloud/project/activity03/climate-change.csv")
tempAnom$date = ymd(tempAnom$Day)

NorthHem = datC02 %>%
  filter(Entity == ""
plot(tempAnom$date, tempAnom$temperature_anomaly)
ggplot(tempAnom, aes(date, temperature_anomaly))+
  geom_point()

#Prompt 2:
#Plot the total all time emissions for the United States, Mexico, and Canada.


#Homework----

#Question 1:
#Make a graph that communicates about emissions from any countries of your choice. 
#Explain how you considered principles of visualization in making your graph.

#Want to look at the G7 (economic alliance countries): Canada, France, Germany, Italy, Japan, the United Kingdom, and the United States
G7 = datCO2 %>%
  filter(Entity == "United States" | Entity == "France" | Entity == "Canada" | Entity == "Germany" | Entity == "Italy" | Entity == "Japan" | Entity == "United Kingdom")

ggplot(G7, aes(x=Year, y=CO2, color=Entity))+
  geom_point()+
  geom_line()+
  scale_color_manual(values=c("red","royalblue", "darkgoldenrod3", "green", "purple", "yellow", "orange"))


