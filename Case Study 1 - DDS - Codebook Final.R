#
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(maps)
library(ggthemes)
library(plotly)
library(reshape)
library(githubinstall)
library(envirofacts)
library(eia)
library(stringr)
library(mapproj)
library(countrycode)
library(WDI)
library(WHO)
library(stringr)
library(jsonlite)
library(plyr)
library(Rmisc)
library(class)
library(caret)
library(e1071)
library(scales)
library(RCurl) #getURL
library(rvest) #html_table, html_node
library(maps)
library(fiftystater)
library(mapproj)
#
###################################################################
############# Import Data #############
###################################################################
beer.file.path <- "C:\\Users\\dloveday\\Dropbox\\Family\\School\\SMU\\Courses\\Spring 2021\\DS 6306 - Doing Data Science\\Lecture Notes\\Unit 8 and 9 Case Study 1\\Beers.csv"
brewery.file.path <- "C:\\Users\\dloveday\\Dropbox\\Family\\School\\SMU\\Courses\\Spring 2021\\DS 6306 - Doing Data Science\\Lecture Notes\\Unit 8 and 9 Case Study 1\\Breweries.csv"
beer.per.capita.file.path <- "C:\\Users\\dloveday\\Dropbox\\Family\\School\\SMU\\Courses\\Spring 2021\\DS 6306 - Doing Data Science\\Lecture Notes\\Unit 8 and 9 Case Study 1\\Supplementary\\Beer - Gal per Capita 2021.csv"  # SOURCE: https://worldpopulationreview.com/state-rankings/alcohol-consumption-by-state
beer.total.consumption.file.path <- "C:\\Users\\dloveday\\Dropbox\\Family\\School\\SMU\\Courses\\Spring 2021\\DS 6306 - Doing Data Science\\Lecture Notes\\Unit 8 and 9 Case Study 1\\Supplementary\\Beer - Total Gal per State.csv"
#
beer.dB <- read.csv(beer.file.path)
brewery.dB <- read.csv(brewery.file.path)
beer.per.capita <- read.csv(beer.per.capita.file.path)
beer.total.consumption <- read.csv(beer.total.consumption.file.path)
#

dev.new()
ggplot(data = beer.dB, aes(x = ABV, y = IBU))+
  geom_point()

master.beer.dB <- merge( x = beer.dB, y = brewery.dB, by.x = c("Brewery_id"), by.y = c("Brew_ID") )
###################################################################
#### Cleanse/Consolidate Data ####
###################################################################
master.beer.dB$Label <- master.beer.dB$Name.x
master.beer.dB$Brewer <- master.beer.dB$Name.y
master.beer.dB <- master.beer.dB[ , !names(master.beer.dB) %in% c("Name.x", "Name.y")]
master.beer.dB$State.Long <- state.name[match(master.beer.dB$State, state.abb)]
master.beer.dB$State.Long.Lower <- tolower(master.beer.dB$State.Long)

beer.per.capita$State <- state.abb[match(beer.per.capita$ï..State, state.name)]
beer.per.capita$Per.capita.gal <- beer.per.capita$alcoholConsumptionGallons
beer.per.capita$Per.capita.mm.gal <- beer.per.capita$Per.capita.gal / 1000000
beer.per.capita <- beer.per.capita %>% arrange(desc(beer.per.capita$Per.capita.gal))
beer.per.capita$Per.capita.rank <- seq(1, nrow(beer.per.capita), by = 1)
beer.per.capita <- beer.per.capita[, !names(beer.per.capita) %in% c("alcoholConsumptionGallons", "ï..State")]


beer.total.consumption$State.Long <- beer.total.consumption$State
beer.total.consumption$State <- state.abb[match(beer.total.consumption$State.Long, state.name)]
beer.total.consumption$Gross.mm.gal <- beer.total.consumption$Gallons.Overall..Millions.
beer.total.consumption$Gross.gal <- beer.total.consumption$Gross.mm.gal * 1000000
beer.total.consumption$Gross.rank <- beer.total.consumption$Rank 
beer.total.consumption <- beer.total.consumption[, !names(beer.total.consumption) %in% c("Gallons.Overall..Millions.", "Rank")]

master.state.dB <- merge(beer.total.consumption, beer.per.capita, by = "State")
master.state.dB$Population <- master.state.dB$Gross.gal / master.state.dB$Per.capita.gal
master.state.dB$State.Long.Lower <- tolower(master.state.dB$State.Long)

# Calculate stats for ABV & IBU for each beer style #
master.beer.style.dB <- data.frame(Style = unique(master.beer.dB$Style))
for (i in 1:nrow(master.beer.style.dB)) {
  master.beer.style.dB.temp <- filter(master.beer.dB, master.beer.dB$Style == master.beer.style.dB$Style[i])
  master.beer.style.dB$Count[i] <- nrow(master.beer.style.dB.temp)
  master.beer.style.dB$Percent.Count[i] <- round(master.beer.style.dB$Count[i] / nrow(master.beer.dB) * 100,0)
  master.beer.style.dB$ABV.median[i] <- median(master.beer.style.dB.temp$ABV, na.rm = TRUE)
  master.beer.style.dB$ABV.mean[i] <- mean(master.beer.style.dB.temp$ABV, na.rm = TRUE)
  master.beer.style.dB$ABV.sd[i] <- sd(master.beer.style.dB.temp$ABV, na.rm = TRUE)
  master.beer.style.dB$IBU.median[i] <- median(master.beer.style.dB.temp$IBU, na.rm = TRUE)
  master.beer.style.dB$IBU.mean[i] <- mean(master.beer.style.dB.temp$IBU, na.rm = TRUE)
  master.beer.style.dB$IBU.sd[i] <- sd(master.beer.style.dB.temp$IBU, na.rm = TRUE)
}

# Fill in NULL's with median or means based on style of the beer
for (i in 1:nrow(master.beer.dB)) {
  if (is.na(master.beer.dB$ABV[i])) {master.beer.dB$ABV[i] <- master.beer.style.dB$ABV.median[master.beer.style.dB$Style == master.beer.dB$Style[i]]  }
  if (is.na(master.beer.dB$ABV[i])) {master.beer.dB$ABV[i] <- master.beer.style.dB$ABV.mean[master.beer.style.dB$Style == master.beer.dB$Style[i]]  }
  
  if (is.na(master.beer.dB$IBU[i])) {master.beer.dB$IBU[i] <- master.beer.style.dB$IBU.median[master.beer.style.dB$Style == master.beer.dB$Style[i]]  }
  if (is.na(master.beer.dB$IBU[i])) {master.beer.dB$IBU[i] <- master.beer.style.dB$IBU.mean[master.beer.style.dB$Style == master.beer.dB$Style[i]]  }
}
master.beer.dB <- filter(master.beer.dB, !is.na(master.beer.dB$IBU))

###################################################################
#### EDA Analysis ####
###################################################################
#
############### (1) State metrics - Consumption & Breweries (Gross & Per Capita)
state.vector <- unique(master.state.dB$State)
for (i  in 1:length(state.vector)) {
  master.state.dB$Gross.brewery[ master.state.dB$State == state.vector[i] ] <- sum(str_count(brewery.dB$State, state.vector[i]))
  #
  master.state.dB.temp <- filter(master.beer.dB, master.beer.dB$State == state.vector[i])
  master.state.dB$ABV.median[ master.state.dB$State == state.vector[i] ] <- median(master.state.dB.temp$ABV, na.rm = TRUE)
  master.state.dB$IBU.median[ master.state.dB$State == state.vector[i] ] <- median(master.state.dB.temp$IBU, na.rm = TRUE)
  master.state.dB$ABV.max[ master.state.dB$State == state.vector[i] ] <- max(master.state.dB.temp$ABV, na.rm = TRUE)
  master.state.dB$IBU.max[ master.state.dB$State == state.vector[i] ] <- max(master.state.dB.temp$IBU, na.rm = TRUE)
}
master.state.dB$Per.capita.brewery <- master.state.dB$Gross.brewery / master.state.dB$Population
#
############### (1) Plots
#
## Master Plotting Parms
map.data.state <- merge(map_data("state"),master.state.dB, by.x = "region", by.y = "State.Long.Lower")
map.data.state <- map.data.state[order(map.data.state$order), ]
##
#
### 1.A US Heatmap of Gross Consumption
#
dev.new()
master.state.dB %>% ggplot( aes(map_id = State.Long.Lower) )+
  theme_bw()+
  theme(legend.title = element_text(size=7))+
  theme(legend.text = element_text(size=5))+
  geom_map(aes(fill = Gross.mm.gal), map = fifty_states, colour = "black" )+
  expand_limits(x = fifty_states$long, y = fifty_states$lat)+
  scale_fill_viridis_c(option = "magma", name = "Total Annual Gallons (MM)") +
  ggtitle("Total 2020 Consumed Volume by State Heatmap")+
  coord_map()

dev.new()
master.state.dB %>% ggplot(aes(x = Population/10000000, y = Gross.mm.gal))+
  theme_bw()+
  geom_point()+
  ylab("Total Gallons (MM) Consumed")+
  xlab("Population (millions of residents)")

plot.data <- master.state.dB[,c("State", "Gross.mm.gal")]
plot.data <- arrange(plot.data, desc(plot.data$Gross.mm.gal))
plot.data <- transform(plot.data, State = reorder(State, -Gross.mm.gal))

dev.new()
plot.data %>% 
  ggplot( aes(x = State, y = Gross.mm.gal) )+
  theme_bw()+
  theme(axis.title = element_text(size = 8))+
  theme(axis.text = element_text(size = 6))+
  geom_bar(stat = "identity" , position = "dodge")+
  ggtitle("Total Consumption by State")+
  ylab("2020 Statewide Consumption (MM gal)")
#
### 1.B US Heatmap of Per Capita Consumption
#
dev.new()
master.state.dB %>% ggplot( aes(map_id = State.Long.Lower) )+
  theme_bw()+
  theme(legend.title = element_text(size=7))+
  theme(legend.text = element_text(size=5))+
  geom_map(aes(fill = Per.capita.gal), map = fifty_states, colour = "black" )+
  expand_limits(x = fifty_states$long, y = fifty_states$lat)+
  scale_fill_viridis_c(option = "magma", name = "Annual Gallons per Capita") +
  ggtitle("2020 Consumed Volumes per Capita by State Heatmap")+
  coord_map()
#
### 1.C US Heatmap of Gross Brewery Count
#
dev.new()
master.state.dB %>% ggplot( aes(map_id = State.Long.Lower) )+
  theme_bw()+
  theme(legend.title = element_text(size=7))+
  theme(legend.text = element_text(size=5))+
  geom_map(aes(fill = Gross.brewery), map = fifty_states, colour = "black" )+
  expand_limits(x = fifty_states$long, y = fifty_states$lat)+
  scale_fill_viridis_c(option = "magma", name = "Total Brewery Count") +
  ggtitle("Total Brewery Count by State Heatmap")+
  coord_map()
#
### 1.D US Heatmap of Breweries per Capita (million)
#
dev.new()
master.state.dB %>% ggplot( aes(map_id = State.Long.Lower) )+
  theme_bw()+
  theme(legend.title = element_text(size=7))+
  theme(legend.text = element_text(size=5))+
  geom_map(aes(fill = Per.capita.brewery*1000000), map = fifty_states, colour = "black" )+
  expand_limits(x = fifty_states$long, y = fifty_states$lat)+
  scale_fill_viridis_c(option = "magma", name = "Breweries per Capita (million residents)") +
  ggtitle("Brewery Count per Capita by State Heatmap")+
  coord_map()
#
### 1.DD US Heatmap of median ABV
#
dev.new()
master.state.dB %>% ggplot( aes(map_id = State.Long.Lower) )+
  theme_bw()+
  theme(legend.title = element_text(size=7))+
  theme(legend.text = element_text(size=5))+
  geom_map(aes(fill = ABV.median), map = fifty_states, colour = "black" )+
  expand_limits(x = fifty_states$long, y = fifty_states$lat)+
  scale_fill_viridis_c(option = "magma", name = "Median ABV for all Labels") +
  ggtitle("Median ABV For All Labels Produced in State")+
  coord_map()
#

#
### 1.DDD US Heatmap of median IBU
#
dev.new()
master.state.dB %>% ggplot( aes(map_id = State.Long.Lower) )+
  theme_bw()+
  theme(legend.title = element_text(size=7))+
  theme(legend.text = element_text(size=5))+
  geom_map(aes(fill = IBU.median), map = fifty_states, colour = "black" )+
  expand_limits(x = fifty_states$long, y = fifty_states$lat)+
  scale_fill_viridis_c(option = "magma", name = "Median IBU for all Labels") +
  ggtitle("Median IBU For All Labels Produced in State")+
  coord_map()
#
### 1.E Histogram of Gross Breweries by State
#
plot.data <- master.state.dB[,c("State", "Gross.brewery")]
plot.data <- arrange(plot.data, desc(plot.data$Gross.brewery))
plot.data <- transform(plot.data, State = reorder(State, -Gross.brewery))
plot.data <- head(plot.data, 10)

dev.new()
plot.data %>% 
  ggplot( aes(x = State, y = Gross.brewery) )+
  theme_bw()+
  theme(axis.title = element_text(size = 8))+
  theme(axis.text = element_text(size = 6))+
  geom_bar(stat = "identity" , position = "dodge")+
  ggtitle("Top 10: Total Brewery Count by State")+
  ylab("Statewide Count of Breweries")
#
### 1.F Histogram of Per Capita Breweries by State
#
plot.data <- master.state.dB[,c("State", "Per.capita.brewery")]
plot.data <- arrange(plot.data, desc(plot.data$Per.capita.brewery))
plot.data <- transform(plot.data, State = reorder(State, -Per.capita.brewery))
plot.data <- head(plot.data, 10)

dev.new()
plot.data %>% 
  ggplot( aes(x = State, y = Per.capita.brewery*1000000) )+
  theme_bw()+
  theme(axis.title = element_text(size = 8))+
  theme(axis.text = element_text(size = 6))+
  geom_bar(stat = "identity" , position = "dodge")+
  ylab("Breweries per Capita (million residents)")+
  ggtitle("Top 10: Breweries per Capita by State")

#
### 1.G Top 10 Histogram of Per Capita Consumption by State
#
plot.data <- master.state.dB[,c("State", "Per.capita.gal")]
plot.data <- arrange(plot.data, desc(plot.data$Per.capita.gal))
plot.data <- transform(plot.data, State = reorder(State, -Per.capita.gal))
plot.data <- head(plot.data, 10)

dev.new()
plot.data %>% 
  ggplot( aes(x = State, y = Per.capita.gal) )+
  theme_bw()+
  theme(axis.title = element_text(size = 8))+
  theme(axis.text = element_text(size = 6))+
  geom_bar(stat = "identity" , position = "dodge")+
  ylab("2020 Consumption Per Capita by State")+
  ggtitle("Top 10: Consumption per Capita by State")
#
#
### 1.G2 Histogram of Per Capita Consumption by State
#
plot.data <- master.state.dB[,c("State", "Per.capita.gal")]
plot.data <- arrange(plot.data, desc(plot.data$Per.capita.gal))
plot.data <- transform(plot.data, State = reorder(State, -Per.capita.gal))

dev.new()
plot.data %>% 
  ggplot( aes(x = State, y = Per.capita.gal) )+
  theme_bw()+
  theme(axis.title = element_text(size = 8))+
  theme(axis.text = element_text(size = 6))+
  geom_bar(stat = "identity" , position = "dodge")+
  ylab("2020 Consumption Per Capita by State")+
  ggtitle("Consumption per Capita by State")


#
############### (4) IBU & Alcohol Content Analysis by State (Histograms)
#
#### Median ABV by State
plot.data <- master.state.dB[,c("State", "ABV.median")]
plot.data <- transform(plot.data, State = reorder(State, -ABV.median))

dev.new()
plot.data %>% melt(id.vars = c("State")) %>%
  ggplot( aes(x = State, y = value, fill = variable) )+
  theme_bw()+
  theme(axis.title = element_text(size = 7))+
  theme(axis.text = element_text(size = 5))+
  geom_bar(stat = "identity" , position = "dodge", color = "black", show.legend = FALSE, width = 0.65)+
  ylab("Median ABV")+
  ggtitle("Median ABV by State")
#
#
#### Max ABV by State Historgram
plot.data <- master.state.dB[,c("State", "ABV.max")]
plot.data <- transform(plot.data, State = reorder(State, -ABV.max))

dev.new()
plot.data %>% melt(id.vars = c("State")) %>%
  ggplot( aes(x = State, y = value, fill = variable) )+
  theme_bw()+
  theme(axis.title = element_text(size = 7))+
  theme(axis.text = element_text(size = 5))+
  geom_bar(stat = "identity" , position = "dodge", color = "black", show.legend = FALSE, width = 0.65)+
  ylab("Max ABV")+
  ggtitle("Max ABV by State")
#

#
#### Top 10: Max ABV by State Historgram
plot.data <- master.state.dB[,c("State", "ABV.max")]
plot.data <- arrange(plot.data, desc(plot.data$ABV.max))
plot.data <- transform(plot.data, State = reorder(State, -ABV.max))
plot.data <- head(plot.data, 10)

dev.new()
plot.data %>% melt(id.vars = c("State")) %>%
  ggplot( aes(x = State, y = value, fill = variable) )+
  theme_bw()+
  theme(axis.title = element_text(size = 10))+
  theme(axis.text = element_text(size = 8))+
  geom_bar(stat = "identity" , position = "dodge", color = "black", show.legend = FALSE, width = 0.65)+
  ylab("Max ABV")+
  ggtitle("Top 10: Max ABV by State")

#
### US Heatmap of max ABV
#
dev.new()
master.state.dB %>% ggplot( aes(map_id = State.Long.Lower) )+
  theme_bw()+
  theme(legend.title = element_text(size=7))+
  theme(legend.text = element_text(size=5))+
  geom_map(aes(fill = ABV.max), map = fifty_states, colour = "black" )+
  expand_limits(x = fifty_states$long, y = fifty_states$lat)+
  scale_fill_viridis_c(option = "magma", name = "Max ABV for all Labels") +
  ggtitle("Max ABV For All Labels Produced in State")+
  coord_map()

#
#### Top 10: Median ABV by State
plot.data <- master.state.dB[,c("State", "ABV.median")]
plot.data <- arrange(plot.data, desc(plot.data$ABV.median))
plot.data <- transform(plot.data, State = reorder(State, -ABV.median))
plot.data <- head(plot.data, 10)

dev.new()
plot.data %>% melt(id.vars = c("State")) %>%
  ggplot( aes(x = State, y = value, fill = variable) )+
  theme_bw()+
  theme(axis.title = element_text(size = 10))+
  theme(axis.text = element_text(size = 8))+
  geom_bar(stat = "identity" , position = "dodge", color = "black", show.legend = FALSE, width = 0.65)+
  ylab("Median ABV")+
  ggtitle("Top 10: Median ABV by State")

#
#### Median IBU by State
plot.data <- master.state.dB[,c("State", "IBU.median")]
plot.data <- transform(plot.data, State = reorder(State, -IBU.median))

dev.new()
plot.data %>% melt(id.vars = c("State")) %>%
  ggplot( aes(x = State, y = value) )+
  theme_bw()+
  theme(axis.title = element_text(size = 7))+
  theme(axis.text = element_text(size = 5))+
  geom_bar(stat = "identity" , position = "dodge", fill = "grey80", color = "navy", show.legend = FALSE, width = 0.65)+
  ylab("Median IBU")+
  ggtitle("Median IBU by State")

#### Top 10: Median IBU by State
plot.data <- master.state.dB[,c("State", "IBU.median")]
plot.data <- arrange(plot.data, plot.data$IBU.median)
plot.data <- transform(plot.data, State = reorder(State, -IBU.median))
plot.data <- head(plot.data,10)

dev.new()
plot.data %>% melt(id.vars = c("State")) %>%
  ggplot( aes(x = State, y = value) )+
  theme_bw()+
  theme(axis.title = element_text(size = 10))+
  theme(axis.text = element_text(size = 8))+
  geom_bar(stat = "identity" , position = "dodge", fill = "grey80", color = "navy", show.legend = FALSE, width = 0.65)+
  ylab("Median IBU")+
  ggtitle("Top 10: Median IBU by State")

#### Max IBU by State Historgram
plot.data <- master.state.dB[,c("State", "IBU.max")]
plot.data <- arrange(plot.data, desc(plot.data$IBU.max))
plot.data <- transform(plot.data, State = reorder(State, -IBU.max))

dev.new()
plot.data %>% melt(id.vars = c("State")) %>%
  ggplot( aes(x = State, y = value) )+
  theme_bw()+
  theme(axis.title = element_text(size = 7))+
  theme(axis.text = element_text(size = 5))+
  geom_bar(stat = "identity" , position = "dodge", fill = "grey80", color = "navy", show.legend = FALSE, width = 0.65)+
  ylab("Max IBU")+
  ggtitle("Max IBU by State")

#### Top 10: Max IBU by State Historgram
plot.data <- master.state.dB[,c("State", "IBU.max")]
plot.data <- arrange(plot.data, desc(plot.data$IBU.max))
plot.data <- transform(plot.data, State = reorder(State, -IBU.max))
plot.data <- head(plot.data, 10)

dev.new()
plot.data %>% melt(id.vars = c("State")) %>%
  ggplot( aes(x = State, y = value) )+
  theme_bw()+
  theme(axis.title = element_text(size = 10))+
  theme(axis.text = element_text(size = 8))+
  geom_bar(stat = "identity" , position = "dodge", fill = "grey80", color = "navy", show.legend = FALSE, width = 0.65)+
  ylab("Max IBU")+
  ggtitle("Top 10: Max IBU by State")



#
#
### US Heatmap of max IBU
#
dev.new()
master.state.dB %>% ggplot( aes(map_id = State.Long.Lower) )+
  theme_bw()+
  theme(legend.title = element_text(size=7))+
  theme(legend.text = element_text(size=5))+
  geom_map(aes(fill = IBU.max), map = fifty_states, colour = "black" )+
  expand_limits(x = fifty_states$long, y = fifty_states$lat)+
  scale_fill_viridis_c(option = "magma", name = "Max IBU for all Labels") +
  ggtitle("Max IBU For All Labels Produced in State")+
  coord_map()
#

#
############### (4) ABV Content Analysis
#
#### ABV Histograms for top 10 most popular styles
plot.data.ref <- arrange(master.beer.style.dB, desc(master.beer.style.dB$Count))[1:10,c("Style", "ABV.median", "ABV.mean", "ABV.sd")]
plot.data <- master.beer.dB[master.beer.dB$Style %in% plot.data.ref$Style, c("Style", "ABV")]

dev.new()
plot.data %>% melt(id.vars = c("Style")) %>%
  ggplot( aes(x = value, fill = Style) )+
  theme_bw()+
  theme(axis.title = element_text(size = 7))+
  theme(axis.text = element_text(size = 5))+
  theme(legend.title = element_text(size = 10))+
  theme(legend.text = element_text(size = 8))+
  geom_density(alpha = 0.25)+
  ylab("Alcohol by Volume (ABV)")+
  ggtitle("Top 10 Most Populuar Beer Styles: ABV Histograms")


dev.new()
plot.data %>% melt(id.vars = c("Style")) %>%
  ggplot( aes(x = value, fill = Style) )+
  theme_bw()+
  theme(axis.title = element_text(size = 7))+
  theme(axis.text = element_text(size = 5))+
  theme(legend.title = element_text(size = 10))+
  theme(legend.text = element_text(size = 8))+
  geom_density(alpha = 0.25)+
  facet_wrap(~ Style)+
  ylab("Alcohol by Volume (ABV)")+
  ggtitle("Top 10 Most Populuar Beer Styles: ABV Histograms")+
  theme(strip.text = element_text(size=5))


#
#### ABV Histograms for American IPA by Top Producing States
plot.data.ref <- arrange(master.beer.style.dB, desc(master.beer.style.dB$Count))[1,c("Style", "ABV.median", "ABV.mean", "ABV.sd")]
plot.data <- master.beer.dB[master.beer.dB$Style %in% plot.data.ref$Style, c("Style", "ABV", "State")] %>% filter(State %in% c("CO", "CA", "OR", "WA", "TX"))

dev.new()
plot.data %>%
  ggplot( aes(x = ABV, fill = State) )+
  theme_bw()+
  geom_density(alpha = 0.25)+
  xlab("Alcohol by Volume (ABV)")+
  ggtitle("American IPA (#1 Most Popular): ABV Histograms by Top 5 Producing States")

dev.new()
plot.data %>%
  ggplot( aes(x = ABV, fill = State) )+
  theme_bw()+
  facet_wrap(~ State)+
  geom_density(alpha = 0.25)+
  xlab("Alcohol by Volume (ABV)")+
  ggtitle("American IPA (#1 Most Popular): ABV Histograms by Top 5 Producing States")


#
############### (7) ABV vs IBU Relationships
#
#
#### ABV vs IBU Relationships - All Styles, All Labels Scatter
dev.new()
master.beer.dB %>% ggplot(aes( x = ABV, y = IBU, color = Style))+
  theme_bw()+
  geom_point(position = "jitter" , show.legend = FALSE)
#
#### ABV vs IBU Relationships - Top 10 Styles, All Labels Scatter
plot.data.ref <- arrange(master.beer.style.dB, desc(master.beer.style.dB$Count))[1:10,c("Style", "ABV.median", "ABV.mean", "ABV.sd")]
plot.data <- master.beer.dB[master.beer.dB$Style %in% plot.data.ref$Style, c("Style", "ABV", "IBU")]

dev.new()
plot.data %>% ggplot(aes(x = ABV, y = IBU, color = Style))+
  theme_bw()+
  geom_point(position = "jitter")+
  geom_smooth(method = 'lm', se = FALSE)

#
#### ABV vs IBU Relationships - Top 10 Styles, All Labels minus IBU = 69 - Scatter
plot.data.ref <- arrange(master.beer.style.dB, desc(master.beer.style.dB$Count))[1:10,c("Style", "ABV.median", "ABV.mean", "ABV.sd")]
plot.data <- master.beer.dB[master.beer.dB$Style %in% plot.data.ref$Style, c("Style", "ABV", "IBU")] %>% filter(IBU != 69)

dev.new()
plot.data %>% ggplot(aes(x = ABV, y = IBU, color = Style))+
  theme_bw()+
  geom_point(position = "jitter")+
  geom_smooth(method = 'lm', se = FALSE, linestyle = "dashed")
#
#### Breweries per Total Gallons Consumed by State
master.state.dB$Breweries.per.mm.gal.total <- master.state.dB$Gross.brewery / master.state.dB$Gross.mm.gal

dev.new()
ggplot()+
  theme_bw()+
  geom_point( data = filter(master.state.dB, master.state.dB$Gross.brewery >= 28 | master.state.dB$Gross.mm.gal >= 200), aes(y = Gross.brewery, x = Gross.mm.gal), color = "orange", size = 3.5)+
  geom_point( data = master.state.dB,aes(y = Gross.brewery, x = Gross.mm.gal), size = 1.75)+
  geom_text( data = filter(master.state.dB, master.state.dB$Gross.brewery >= 28 | master.state.dB$Gross.mm.gal >= 200), 
              aes(y = Gross.brewery, x = Gross.mm.gal, label = State),
              nudge_x = 0.2,
              nudge_y = 1.25)+
  ylab("Brewery Count")+
  xlab("Total Gallons (MM) Consumed")

#
#### Breweries per Capita Gallons Consumed by State
master.state.dB$Breweries.per.gal.capita <- master.state.dB$Gross.brewery / master.state.dB$Per.capita.gal

dev.new()
ggplot()+
  theme_bw()+
  geom_point(data = filter(master.state.dB, master.state.dB$Gross.brewery >= 20 | master.state.dB$Per.capita.gal >= 3), aes(y = Gross.brewery, x = Per.capita.gal), color = "orange", size = 3.5)+
  geom_point(data = master.state.dB,aes(y = Gross.brewery, x = Per.capita.gal), size = 1.75)+
  geom_text( data = filter(master.state.dB, master.state.dB$Gross.brewery >= 20 | master.state.dB$Per.capita.gal >= 3), 
             aes(y = Gross.brewery, x = Per.capita.gal, label = State),
             nudge_x = -0.1,
             nudge_y = 0.9)+
  ylab("Brewery Count")+
  xlab("Per Capita Gallons Consumed")


#
############### (8) k-NN analysis
#
#
#### Subset for IPA's & other Ale's (any beer with "Ale" but not IPA)
knn.data.all.Ale <- filter(master.beer.dB, grepl('India|IPA|Ale', Style) | grepl('India|IPA|Ale', Label))
knn.data.IPA     <- filter(knn.data.all.Ale, grepl('India|IPA', Style) | grepl('India|IPA', Label))
knn.data.other.Ale <- knn.data.all.Ale %>% anti_join(knn.data.IPA)

for (i in 1:nrow(knn.data.all.Ale)) {
  if (grepl('India|IPA', knn.data.all.Ale$Style[i]) |  grepl('India|IPA|Ale', knn.data.all.Ale$Label) ) { knn.data.all.Ale$Ale.Type[i] <- "IPA" }
  else { knn.data.all.Ale$Ale.Type[i] <- "Other" }
}

knn.data.all.Ale$ABV.scale <- scale(knn.data.all.Ale$ABV)
ABV.scale.mean <-  mean(knn.data.all.Ale$ABV)
ABV.scale.sd   <-  sd(knn.data.all.Ale$ABV)

knn.data.all.Ale$IBU.scale <- scale(knn.data.all.Ale$IBU)
IBU.scale.mean <- mean(knn.data.all.Ale$IBU)
IBU.scale.sd   <- sd(knn.data.all.Ale$IBU)

ABV.synth.seq <- seq(min(knn.data.all.Ale$ABV), max(knn.data.all.Ale$ABV), (max(knn.data.all.Ale$ABV) - min(knn.data.all.Ale$ABV))/50)
IBU.synth.seq <- seq(min(knn.data.all.Ale$IBU), max(knn.data.all.Ale$IBU), (max(knn.data.all.Ale$IBU) - min(knn.data.all.Ale$IBU))/50)

# Create synthetic grid of ABV & IBU points
ABV.IBU.synth.dB <- data.frame()
ABV.IBU.synth.dB <- expand.grid(ABV = ABV.synth.seq, IBU = IBU.synth.seq)
ABV.IBU.synth.dB$ABV.scale <- (ABV.IBU.synth.dB$ABV - ABV.scale.mean) / ABV.scale.sd
ABV.IBU.synth.dB$IBU.scale <- (ABV.IBU.synth.dB$IBU - IBU.scale.mean) / IBU.scale.sd

# Run k.nn for test/train at range of k's to identify sweet spot
knn.test.train.list <- list()

for (j in 1:50) {
  set.seed(j)
  splitPerc <- 0.70 #Use a 70/30 train to test ratio
  
  trainIndicies <- sample( 1:dim(knn.data.all.Ale)[1], round(splitPerc * dim(knn.data.all.Ale)[1]))
  train <- knn.data.all.Ale[trainIndicies, ]
  test <- knn.data.all.Ale[-trainIndicies, ]
  
  k.max <- 300
  knn.Ale.test.train <- data.frame(matrix(nrow = k.max, ncol = 4))
  colnames(knn.Ale.test.train) <- c("k", "Accuracy", "Sensitivity", "Specificity")
  
  for (i in 1:k.max) {
    classifications = knn(train[,c(14,15)],test[,c(14,15)],train$Ale.Type, prob = TRUE, k = i)
    confusion.matrix <- confusionMatrix(table(classifications,test$Ale.Type))
    
    knn.Ale.test.train$k[i] <- i
    knn.Ale.test.train$Accuracy[i] <-  confusion.matrix$overall[1]
    knn.Ale.test.train$Sensitivity[i] <- confusion.matrix$byClass[1]
    knn.Ale.test.train$Specificity[i] <- confusion.matrix$byClass[2]
  }
  knn.test.train.list[[j]] <- knn.Ale.test.train
}
#############################################################################################################################
# Calculate mean Accuracy/Sensitivity/Specificity for 50 random 30/70 test/train splits
knn.Ale.unlist <- do.call(rbind, knn.test.train.list)
knn.Ale.stats <- data.frame(matrix(nrow = k.max))
for (i in 1:k.max) {
  temp.data <- filter(knn.Ale.unlist, knn.Ale.unlist$k == i)
  
  knn.Ale.stats$k[i] <- i
  knn.Ale.stats$Accuracy.mean[i] <- mean(temp.data$Accuracy)
  knn.Ale.stats$Sensitivity.mean[i] <- mean(temp.data$Sensitivity)
  knn.Ale.stats$Specificity.mean[i] <- mean(temp.data$Specificity)
}

dev.new()
knn.Ale.stats %>% 
  ggplot(aes(x = k, y = Accuracy))+
  theme_bw()+
  geom_line(aes(x = k, y = Accuracy.mean), color = "black", size = 0.9)+
  geom_line(aes(x = k, y = Sensitivity.mean), color = "blue", linetype = "dotted", size = 0.9)+
  geom_line(aes(x = k, y = Specificity.mean), color = "red", linetype = "dotted", size = 0.9)+
  geom_vline(aes(xintercept = sqrt(nrow(train))))+
  ylab("Rate")

#############################################################################################################################  
# Run synthetic grid through k.NN model at k = 34

synth.class <- knn(knn.data.all.Ale[,c(14,15)], ABV.IBU.synth.dB[,c(3,4)], knn.data.all.Ale$Ale.Type, prob = TRUE, k = round(sqrt(nrow(train))))
ABV.IBU.synth.dB$prediction <- synth.class
ABV.IBU.synth.dB$probability <- attributes(synth.class)$prob

for (i in 1:nrow(ABV.IBU.synth.dB)) {
  if (ABV.IBU.synth.dB$prediction[i] == "IPA") { ABV.IBU.synth.dB$IPA.prob[i] <- ABV.IBU.synth.dB$probability[i]*100 }
  if (ABV.IBU.synth.dB$prediction[i] != "IPA") { ABV.IBU.synth.dB$IPA.prob[i] <- (1 - ABV.IBU.synth.dB$probability[i])*100 }
  ABV.IBU.synth.dB$Other.prob[i] <- 100 - ABV.IBU.synth.dB$IPA.prob[i]
}

# Probability heatmap of IPA
dev.new()
ABV.IBU.synth.dB %>%
  ggplot()+
  theme_bw()+
  theme(legend.title = element_text(size=7))+
  theme(legend.text = element_text(size=5))+
  geom_raster(data = ABV.IBU.synth.dB, aes(x = ABV, y = IBU, fill = IPA.prob), interpolate = TRUE)+
  geom_point(data = ABV.IBU.synth.dB, aes(x = ABV, y = IBU), size = 0.25, color = "white", alpha = 0.2)+
  geom_point(data = knn.data.IPA, aes(x = ABV, y = IBU), color = "red")+
  geom_point(data = knn.data.other.Ale, aes(x = ABV, y = IBU), color = "blue")+
  scale_fill_viridis_c(option = "magma", name = "IPA Probability")

# Probability heatmap of Other
dev.new()
ABV.IBU.synth.dB %>%
  ggplot()+
  theme_bw()+
  theme(legend.title = element_text(size=7))+
  theme(legend.text = element_text(size=5))+
  geom_raster(data = ABV.IBU.synth.dB, aes(x = ABV, y = IBU, fill = Other.prob), interpolate = TRUE)+
  geom_point(data = ABV.IBU.synth.dB, aes(x = ABV, y = IBU), size = 0.25, color = "white", alpha = 0.2)+
  geom_point(data = knn.data.IPA, aes(x = ABV, y = IBU), color = "red")+
  geom_point(data = knn.data.other.Ale, aes(x = ABV, y = IBU), color = "blue")+
  scale_fill_viridis_c(option = "magma", name = "'Other Ale' Probability")

  

#class.titanic.test.clean <- knn(titanic.train.clean[,c(13,14)] ,titanic.test.clean[c(12,13)], titanic.train.clean$Survived, k = 4 )

#
#### (8a) IPA / Other Ale's - Simple Scatter Plot
dev.new()
ggplot()+
  theme_bw()+
  geom_point(data = knn.data.IPA, aes(x = ABV, y = IBU), color = "red")+
  geom_point(data = knn.data.other.Ale, aes(x = ABV, y = IBU), color = "blue")

#
#### (8a) IPA / Other Ale's w/ grid points - Simple Scatter Plot
dev.new()
ggplot()+
  theme_bw()+
  geom_point(data = knn.data.IPA, aes(x = ABV, y = IBU), color = "red")+
  geom_point(data = knn.data.other.Ale, aes(x = ABV, y = IBU), color = "blue")+
  geom_point(data = ABV.IBU.synth.dB, aes(x = ABV, y = IBU), color = "black", size = 0.25, alpha = 0.25)

