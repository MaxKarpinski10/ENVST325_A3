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
#Prompt 1: ----
#Make a plot of air temperature anomalies in the Northern and Southern Hemisphere in base R and in ggplot2. 

#Setting up
tempAnom <- read.csv("/cloud/project/activity03/climate-change.csv")
tempAnom$date <- ymd(tempAnom$Day)
NorthHem <- tempAnom %>% filter(Entity == "Northern Hemisphere")
SouthHem <- tempAnom %>% filter(Entity == "Southern Hemisphere")

#Base R
plot(NorthHem$date, NorthHem$temperature_anomaly, 
     type = "l", col = "blue", 
     main = "Temp Anomalies", xlab = "Year", ylab = "Anomaly")
lines(SouthHem$date, SouthHem$temperature_anomaly, col = "red")

#ggplot
both_hemispheres <- tempAnom %>% 
  filter(Entity %in% c("Northern Hemisphere", "Southern Hemisphere"))
ggplot(both_hemispheres, aes(x = date, y = temperature_anomaly, color = Entity)) +
  geom_line() +
  labs(title = "Hemisphere Temperature Anomalies (ggplot2)",
       x = "Year", 
       y = "Temperature Anomaly") 

#Prompt 2: ----
#Plot the total all time emissions for the United States, Mexico, and Canada.

# --- Prompt 2 ---

emissionsData <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")
colnames(emissionsData)[4] <- "Annual_CO2"
north_america_totals <- emissionsData %>%
  filter(Entity %in% c("United States", "Mexico", "Canada")) %>%
  group_by(Entity) %>%
  summarize(Total_All_Time = sum(Annual_CO2, na.rm = TRUE))
#Making Barchart
ggplot(north_america_totals, aes(x = Entity, y = Total_All_Time, fill = Entity)) +
  geom_col() +
  labs(title = "Total All-Time CO2 Emissions (USA, Mexico, Canada)",
       x = "Country",
       y = "Total CO2 emissions")


#Homework----

#Question 1: ----
#Make a graph that communicates about emissions from any countries of your choice. 
#Explain how you considered principles of visualization in making your graph.

#Want to look at the G7 (economic alliance countries): 
#Canada, France, Germany, Italy, Japan, the United Kingdom, and the United States

datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")
colnames(datCO2)[4] <- "CO2"
G7_list <- c("United States", "France", "Canada", "Germany", "Italy", "Japan", "United Kingdom")
G7 <- datCO2 %>%
  filter(Entity %in% G7_list)
ggplot(G7, aes(x = Year, y = CO2, color = Entity)) +
  geom_line(linewidth = 1) +  # Lines are better for showing trends over time
  scale_color_manual(values = c("red", "royalblue", "darkgoldenrod3", "green", "purple", "orange", "black")) +
  labs(title = "Annual CO2 Emissions of G7 Nations",
       subtitle = "Trends in economic alliance emissions over time",
       x = "Year",
       y = "CO2 emissions")

#Principles of Visualization that I considered include:
#Selection (Data Thinning): Instead of plotting all countries, I filtered for the G7 nations. 
#This narrows the focus to a specific economic group, making the trends easier to compare and interpret.
#Color Distinction: I used scale_color_manual to assign distinct colors to each country which ensures 
#that the reader can quickly track different countries trends
#Visual Encoding: I choose to plot with a line without data points to reduce clunkiness and see a clear trendline
#Clarity & Labeling: I included a clear title, subscript and axis labels


#Question 2: ----
#You are tasked with communicating the change in world air temperatures and CO emissions to a broad audience 
#in visually appealing graphs. Make two graphs to present in your word document side by side. 
#Plot world CO emissions on one graph and world air temperature anomalies on the other graph

#Making CO2 emissions graph
world_emissions <- annual_co_emissions_by_region %>%
  filter(Entity == "World") %>%
  rename(CO2 = 4)
ggplot(world_emissions, aes(x = Year, y = CO2)) +
  geom_line(color = "red", linewidth = 1) +
  labs(title = "Global Annual CO2 Emissions",
       x = "Year",
       y = "CO2 Emissions")
#Making world air temperatures graphs 
world_temp <- tempAnom %>%
  filter(Entity == "World") %>%
  mutate(date = ymd(Day))
ggplot(world_temp, aes(x = date, y = temperature_anomaly)) +
  geom_line(color = "Blue", linewidth = 1) +
  labs(title = "Global Temperature Anomalies",
       x = "Year",
       y = "Anomaly (°C)")

#Interpretation: The graphs reveal a clear upward trend in both global CO2 emissions 
#and temperature anomalies, particularly accelerating after around 1950.


#Question 3: ---- 
#Look up any type of environmental data of your interest in our world in data 
#(link in tutorial). Download the csv and upload it to RStudio Cloud. 
#Remake the graph. You may make the graph exactly as it is or alter it to present
#the data in a different format. Explain how you considered principles of visualization 
#in making your graph. Explain the main conclusion of the graph.

#I am looking at the per capita energy use of countries over time, the whole world, and the USA. 
energyData <- read.csv("/cloud/project/activity03/per-capita-energy-use.csv")
colnames(energyData)[4] <- "Energy_Per_Capita"
targets <- c("Africa", "Asia", "Europe", "North America", 
             "South America", "Oceania", "World", "United States")
energy_subset <- energyData %>%
  filter(Entity %in% targets)
ggplot(energy_subset, aes(x = Year, y = Energy_Per_Capita, color = Entity)) +
  geom_line(linewidth = 1) + scale_y_continuous(breaks = seq(0, 100000, by = 10000)) +
  labs(title = "Per capita energy consumption, 1980 to 2023",
       subtitle = "Energy use is measured in kilowatt-hours per person.",
       x = "Year",
       y = "kWh per person",
       caption = "Source: Our World in Data")


#Question 4 ----
#Copy the URL to your R script here.
https://github.com/MaxKarpinski10/ENVST325_A3.git