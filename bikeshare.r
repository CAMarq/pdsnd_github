
#load libraries
library(tidyverse)
library(ggplot2)
library(readr)
library(lubridate)

new_york_city = read.csv('new_york_city.csv')
washington = read.csv('washington.csv')
chicago = read.csv('chicago.csv')

#create combined data without the extra columns in chicago and NY city, create diferentiatior column "City"

new_york_city_8col <- new_york_city %>%
  select(-c(8,
            9)) %>%
  cbind(City="New York")

chicago_8col <- chicago %>%
  select(-c(8,
            9)) %>%
  cbind(City="Chicago")

washington_8col <- washington %>%
  cbind(City="Washington")

all_cities <- rbind(new_york_city_8col,
                    chicago_8col,
                    washington_8col)

names(all_cities)[1]="ID"
names(all_cities)[2]="Start_Time"
names(all_cities)[3]="End_Time"
names(all_cities)[4]="Trip_Duration"
names(all_cities)[5]="Start_Station"
names(all_cities)[6]="End_Station"
names(all_cities)[7]="User_Type"


head(new_york_city_8col)

head(chicago_8col)

#To Generate "Date" type column and assign name to Month

Month <- all_cities %>%
  mutate(Clean.Start.Date = as.Date(Start_Time),
         Clean.End.Time = as.Date(End_Time)) %>%
  mutate(Month = month(Clean.Start.Date, label = TRUE, abbr = FALSE))


#To change column Month to be a factor

Month$Month <- factor(Month$Month, levels=month.name)

#Graph
ggplot(data=Month, aes(x=Month)) +
  geom_bar(color="blue", fill="blue")+
  scale_x_discrete() +
  ylab('Count of Users') +
  ggtitle("Usage per Month")

#Summary Data
table(Month$Month)

#Summary Data

Start_Stations <- all_cities %>%
  group_by(Start_Station) %>%
  summarise(Incidences=n()) %>%
  top_n(3) %>%
  arrange(desc(Incidences))


Start_Stations

#Graph
all_cities %>%
  group_by(Start_Station) %>%
  summarise(Incidences=n()) %>%
  top_n(3) %>%
  ggplot(., aes(x=Start_Station, y=Incidences))+
  geom_bar(stat="Identity", color="orange", fill="orange") +
  xlab('Start Station') +
  ylab('Incidences') +
  ggtitle("Top 3 Start Stations")

#Summary Data
End_Stations <-all_cities %>%
  group_by(End_Station) %>%
  summarise(Incidences=n()) %>%
  top_n(3) %>%
  arrange(desc(Incidences))

End_Stations

#Graph
all_cities %>%
  group_by(End_Station) %>%
  summarise(Incidences=n()) %>%
  top_n(3) %>%
  ggplot(., aes(x=End_Station, y=Incidences))+
  geom_bar(stat="Identity", color="purple", fill="purple") +
  xlab('End Station') +
  ylab('Incidences') +
  ggtitle("Top 3 End Stations")

system('python -m nbconvert Explore_bikeshare_data.ipynb')
