---
title: "Data Visualization and Analysis (CSE-6242)"
author: "egriffin33   Eric Griffin"
date: "June 2, 2018"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
##Activity2



###Gun violence Incidents in continential US  (2013 ~ 2018)
The goal in this activity is to explore a geolocated dataset), where items are associated with a position on a planet's surface (typically specified using latitude and longitude). 

 **This version is my original rmd that acts as my notebook to show my full analysis (see ac2.pdf for my submitted version which meets  grading specifications** 
The first dataset I chose was listed under the SocialSciences section the following public-datasets, https://github.com/awesomedata/awesome-public-datasets

Additonally  a second dataset was used to apply US 2013 Census poulation data into the analysis, this data was created from
http://www.enchantedlearning.com/usa/states/population.shtml

I have performed exploratory analysis of the data to compare how states normally attributed to having high concentrations of gun violence that are directly associated to large metropolitan areas is altered when the events are adjusted (per capita) for the total population of the state. 

several maps are used to visualize and illustrate  total gun death, total injuries and per capita gun deaths.




## load map and plotting libraries 
```{r, warning=FALSE, include=TRUE}
library(ggplot2)
library(dplyr)
library(maps)
library(mapdata)

```

```{r }
# Gun violence dataset for years (2013 ~ 2018)
# dataset obtained from https://github.com/jamesqo/gun-violence-data
# A comprehensive, accessible database that contains records of over
#  260k US gun violence incidents from January 2013 to March 2018.
```
```{r, cache=TRUE}
gv_data       <- read.csv("stage3.csv", header = TRUE)

#clean dataset, align map state names to gun violence by making all states lowercase
gv_data$state <- tolower(gv_data$state)

#load plot of United States  
states_map <- map_data("state")

# group gun violence data by state summarizing  number killed and number injured
gv_states2    <- gv_data  %>% group_by(state ) %>% 
                          summarise(Killed = sum(n_killed), Injured = sum(n_injured))

#create map of actual people killed in gun violence grouped  by state
gv_map        <- merge(states_map, gv_states2, by.x = "region", by.y = "state")
```

```{r, include=TRUE}
print(gv_states2)

```

```{r include=TRUE}

sortTotKilled <- gv_states2[order(-gv_states2$Killed), ]
sortTotKilled <- cbind(sortTotKilled, TotKilledRank = seq(from=1, to=51))

sortTotInjured <- gv_states2[order(-gv_states2$Injured), ]
sortTotInjured <- cbind(sortTotInjured, TotInjuredRank = seq(from=1, to=51))
```

## Geospatial map plots of gun violence in US (2013 ~ 2018)

This first mapping shows which states have the overall higher gun deaths.
using a gradient color fill provides a heat map of gun violence intensity

```{r , include=TRUE}
library(ggplot2)

g <- ggplot ( gv_map, aes(x=long, y=lat, group=group, fill = Killed )) 
g <- g + geom_polygon(colour = "black") + coord_map("polyconic") 
g <- g + scale_fill_gradient2(low  = "#559999", mid ="#FFFDEE", 
                              high = "#BB650B", midpoint = median(gv_map$Killed))
g <- g + ggtitle("Total number of people killed in gun violence (2013 ~ 2018)")


print(g)

```

### Top 5 states with highest total gun deaths
```{r, include=TRUE}
head(sortTotKilled,5)
```

Second mapping shows then number of people injured in gun VIolence by state
the same gratient color fill from white = low to dark amber  = High
```{r ,include=TRUE}


g <- ggplot ( gv_map, aes(x=long, y=lat, group=group, fill = Injured)) 
g <- g + geom_polygon(colour = "black") + coord_map("polyconic") 
g <- g + scale_fill_gradient2(low  = "#559999", mid ="#FFFDEE",  
                              high = "#BB650B", midpoint = median(gv_map$Injured))
g <- g + ggtitle("Total number of gun injury incidents (2013 ~ 2018)")

print(g)
```

### Top 5 states with highest gun injuries
```{r, include=TRUE}
head(sortTotInjured,5)
```

### Load census 2013 population data 
By dividing the reported number of incidents per state with the 2013 US census data, the rate of gun violence per capitia is normalized in the data.

```{r , include=TRUE}
# 
#census population data extracted manually from
#http://www.enchantedlearning.com/usa/states/population.shtml
# saved locally into a csv table

census <-read.csv("census.csv", header = TRUE, sep = ",")
census$State <- tolower(census$State)

head(census,5)
```


```{r , include=TRUE}
## just an intermediate working table to merge in census data columns into 
gv_map_norm <- merge(gv_map, census,by.x = "region" ,by.y = "State" )

#create new dataset with normalized violence based on state population
gvmn2 <- cbind(gv_map_norm,(gv_map_norm$Killed/gv_map_norm$Population))
names(gvmn2) <- c("region", "long", "lat", "group", "order", "subregion",
                  "Killed", "Injured", "Ranking","Population", "normalized")



sortedgv <-  gvmn2 %>% group_by(region) %>% summarise(Killed = mean(Killed), Injured = mean(Injured), PerCapita = mean(normalized))
sortedgv <- sortedgv[order(-sortedgv$PerCapita), ]
```

### Map plot of gun violence normalized by population of each state.
This map highlights the states with the highest per capitia gun violence.
The same color gratient is applied to show the shift in gun violence intensity based on population


```{r, include=TRUE}
g <- ggplot(gvmn2, aes(x=long, y=lat, group=group, fill = normalized)) 
g <- g + geom_polygon(colour = "black") + coord_map("polyconic") 
g <- g + scale_fill_gradient2(low  = "#559999", mid ="#FFFDEE", 
                              high = "#BB650B", midpoint = median(gvmn2$normalized ))
g <- g + ggtitle("Rate a Gun Violence Per Capita.(2013 ~ 2018")

```
```{r}
print(g)

```
```{r, include=TRUE}

sorted2 <- cbind(sortedgv, PerCapitaRank = seq(from=1, to=49))

```

### Highest gun violence states when adjusted per capita 
```{r,include=TRUE}
head(sorted2, 5)

```
### Lowest gun violence states when adjusted per capita
```{r, include=TRUE}

tail(sorted2,5)
```


