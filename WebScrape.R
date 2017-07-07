#Visualizing obesity across India and Web scraping using R
#Loading the packages

require(rvest)
#rvest is the package to scrape Web pages in R
require(ggplot2)
require(dplyr)
require(scales)
require(maps)
require(mapproj)



#Loading the Data--------------

obesity<-read_html("https://en.m.wikipedia.org/wiki/Obesity_in_India")

#html_nodes() to select a particular HTML element from the above page
#table because we want to select a "table"

#Converting to a R dataframe
obesity = obesity %>%
  html_nodes("table") %>% 
  .[[1]] %>%
  html_table()


head(obesity)

#Cleaning the Data 
str(obesity)

#removing the % and making the data numeric

for(i in 2:4){
  obesity[,i] = gsub("%", "", obesity[,i])
  obesity[,i] = as.numeric(obesity[,i])
}

str(obesity)

#Loading the Map data
india<-map(database = "world",regions = "india", exact = F , boundary = T)

india<-map_data(india ,region="india", exact= F)

str(india)

#Merging both datasets




