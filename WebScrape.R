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

obesity<-read_html("https://en.wikipedia.org/wiki/Obesity_in_the_United_States")

#html_nodes() to select a particular HTML element from the above page
#table because we want to select a "table"

#Converting to a R dataframe
obesity = obesity %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>% 
  .[[1]] %>%
  html_table(fill=T)


head(obesity)

#Cleaning the Data 
str(obesity)

#removing the % and making the data numeric

for(i in 2:4){
  obesity[,i] = gsub("%", "", obesity[,i])
  obesity[,i] = as.numeric(obesity[,i])
}

str(obesity)

#Fixing the names to remove spaces

names(obesity)
names(obesity) = make.names(names(obesity))
names(obesity)


#Loading the map-----------------

states = map_data("state")

# create a new variable name for state
obesity$region = tolower(obesity$State.and.District.of.Columbia)

#merging the datasets
states = merge(states, obesity, by="region", all.x=T)
str(states)






