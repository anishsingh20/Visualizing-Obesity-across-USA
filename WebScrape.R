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


#Loading the map data-----------------

states = map_data("state")

# create a new variable region for state
obesity$region = tolower(obesity$State.and.District.of.Columbia)

#merging the datasets
states = merge(states, obesity, by="region", all.x=T)
str(USdata)


#for adding Names to the states in the Map- making a new data frame

statenames = states %>% 
  group_by(region) %>%
  summarise(
    long = mean(range(long)), 
    lat = mean(range(lat)), 
    group = mean(group), 
    Obese.adults = mean(Obese.adults), 
    Obese.children.and.adolescents = mean(Obese.children.and.adolescents)
  ) 


#Data frame consisting of top 10 Most Obese Adults States 
topstate = states %>% 
  group_by(region) %>%
  summarise(
    
    Obese.adults = mean(Obese.adults), 
    Obese.children.and.adolescents = mean(Obese.children.and.adolescents)
    
  ) %>%
  arrange(desc(Obese.adults)) %>%
  top_n(10)

#Plotting the top 10 states 

ggplot(aes(x = reorder(region,Obese.adults), y = Obese.adults),data = topstate) + 
  geom_col(color="black",fill="#1EDBC2",alpha=0.6) +
  labs(y = "Percentage of Obese Adults",x="Top 10 States") +
  coord_flip()







#Plotting the data------------------------


#For adults

ggplot(states, aes(x = long, y = lat, group = group, fill = Obese.adults)) + 
  geom_polygon(color = "white",show.legend = T) +
  scale_fill_gradient(name = "Percent", low = "#FAB8D2", high = "#F91C74", guide = "colorbar", na.value="black", breaks = pretty_breaks(n = 5)) +
  labs(title="Obesity in Adults for USA",x = "Longitude",y = "Latitude") +
  coord_map() +
  #adding States names to the states on the map
  geom_text(data=statenames, aes(x = long, y = lat, label = region), size=3)


#Barplot
ggplot(aes(x = reorder(region,Obese.adults), y =Obese.adults),data = statenames) + 
  geom_col(width=1,color="black",fill="#7975B9",alpha=0.6) +
  coord_flip() +
  labs(x = "Percentage of Obese Adults",y="States")
#Highest adult percentage which are Obese is in Mississipi

which.min(x = statenames$Obese.adults)
#Connecticut is the state with least Obese Adults


#------------------------------------------------------------


#Now Analyzing the Obese Children and Teens

#Finding top 15 States with Most Obese Children and Teens
topChild = states %>%
      group_by(region) %>%
      summarise(Obese.Child.and.Teens = mean(Obese.children.and.adolescents)) %>%
      top_n(15)
      
#Barplot

ggplot(data = topChild, aes(x = reorder(region,Obese.Child.and.Teens), y = Obese.Child.and.Teens))+
        geom_col(color="black",fill="#6EE543",alpha=0.8) +
        coord_flip()
  

#Map for Obesity in Children 

ggplot(states, aes(x = long, y = lat, group = group, fill = Obese.children.and.adolescents)) + 
  geom_polygon(color = "white") +
  scale_fill_gradient(name = "Percent Obese", low = "#B8D5EC", high = "#0A4B7D", guide = "colorbar", na.value="black", breaks = pretty_breaks(n = 5)) +
  labs(title="Obesity in Children and Teens", x = "Longitude",y = "latitude") +
  coord_map() + 
  #adding States names to the states on the map
  geom_text(data=statenames, aes(x = long, y = lat, label = region), size=3)
  
#Most Obese Children and Teens for Delaware State




#Barplot

ggplot(aes(x = reorder(region,Obese.children.and.adolescents),y = Obese.children.and.adolescents),
           data = statenames) + 
  geom_col(color="black",fill="#F43E3E",width=1)   +
  coord_flip() + 
  labs(x = "States", y ="Percentage of Obese Children and Teens",title="Barplot of Obese Children and Teens")
