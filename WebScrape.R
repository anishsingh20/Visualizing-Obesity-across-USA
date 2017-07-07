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
str(USdata)


#for adding Names to the states in the Map

statenames = states %>% 
  group_by(region) %>%
  summarise(
    long = mean(range(long)), 
    lat = mean(range(lat)), 
    group = mean(group), 
    Obese.adults = mean(Obese.adults), 
    Obese.children.and.adolescents = mean(Obese.children.and.adolescents)
  )






#Plotting the data------------------------


#For adults

ggplot(states, aes(x = long, y = lat, group = group, fill = Obese.adults)) + 
  geom_polygon(color = "white",show.legend = T) +
  scale_fill_gradient(name = "Percent", low = "#FAB8D2", high = "#F91C74", guide = "colorbar", na.value="black", breaks = pretty_breaks(n = 5)) +
  labs(title="Obesity in Adults for USA",x = "Longitude",y = "Latitude") +
  coord_map() +
  #adding States names to the map
  geom_text(data=statenames, aes(x = long, y = lat, label = region), size=3)


ggplot(aes(x = State.and.District.of.Columbia, y =Obese.adults),data = obesity) + 
  geom_col(width=1,color="black",fill="#7975B9",alpha=0.9) +
  coord_flip() +
  labs(x = "Percentage of Obese Adults",y="States")
#Highest adult percentage which are Obese is in Mississipi




#For 

