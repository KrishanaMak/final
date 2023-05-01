library(tidyverse)
library(tidycensus)
library(sf)
library(ggplot2)
      
#load data in
shooting = read_csv("school-shootings-data.csv")
      
#data with columns that I will be using
filtered_data = shooting[, c(-4,-7,-18,-19,-20,-21,-22,-23,-24,-25,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-39,-40,-43,-44,-45,-46)]
view(filtered_data)     
# add population data to filtered_data
pop_data <- get_acs(geography = "state", variables = "B01003_001", year = 2019, survey = "acs1", output = "wide")

#remove columns that are not important for the purposes of my research and renaming other columns    
pop_data = pop_data[, c(-1,-4)]
colnames(pop_data) = c('state','population')

#merging pop_data to filtered data and lower casing the items in the state column to make it easier to make maps later on.
complete_data <- merge(filtered_data, pop_data, by.x = "state", by.y = "state") %>%
  mutate(state = tolower(state))

#lowercasing pop_data just in case I end up using it again
pop_data <- pop_data %>%
  mutate(state = tolower(state))

#create a function classifying states as regions. First I created, columns titled
# northeast, south, midwest, and west with the corresponding states for each region.
#After this I created if else statements to classify each state as the region that it
#belongs too. 
assign_region <- function(data) {
  # classifying which states are in which region
  northeast <- c("district of columbia", "maine", "new hampshire", "vermont", "massachusetts", "rhode island", "connecticut", "new york", "pennsylvania", "new jersey")
  south <- c("delaware", "maryland", "virginia", "west virginia", "north carolina", "south carolina", "georgia", "florida", "kentucky", "tennessee", "alabama", "mississippi", "arkansas", "louisiana", "texas", "oklahoma")
  midwest <- c("ohio", "michigan", "indiana", "illinois", "wisconsin", "minnesota", "iowa", "missouri", "north dakota", "south dakota", "nebraska", "kansas")
  west <- c("montana", "idaho", "wyoming", "colorado", "new mexico", "arizona", "utah", "nevada", "california", "oregon", "washington", "alaska", "hawaii")
  
  # Use the state's location to assign it to a region
  data$region <- ifelse(data$state %in% northeast, "Northeast",
                        ifelse(data$state %in% south, "South",
                               ifelse(data$state %in% midwest, "Midwest",
                                      ifelse(data$state %in% west, "West", NA))))
  
  return(data)
}

# steps to apply the function. Create a separate dataframe with a list of states
# from the data frame complete_data. Then apply the function assign_region to the
# data frame states. Finally, attach the region column to complete_data. 
states = data.frame(states = complete_data[,1])

df_with_region <- assign_region(states)

complete_data$region <- df_with_region[,2]

#create a dataframe with the number of shootings for each state
shootings_per_state <- complete_data %>% 
  group_by(state) %>% 
  summarize(num_shootings = n()) %>%
  arrange(desc(num_shootings))
#California had the highest number of shootings (40)

#create a dataframe with the total number of casualties for each state
total_casualties_by_state <- complete_data %>%
  group_by(state) %>%
  summarize(total_casualties = sum(casualties, na.rm = TRUE)) %>%
  arrange(desc(total_casualties))
#California had the highest number of casualties (82)

#create a dataframe with the count of the number of shootings for each region
shootings_per_region <- complete_data %>% 
  group_by(region) %>% 
  summarize(num_shootings = n()) %>%
  arrange(desc(num_shootings))
# the South had the most number of shootings (165)

#create a dataframe with the total number of casualties for each region
total_casualties_by_region <- complete_data %>%
  group_by(region) %>%
  summarize(total_casualties = sum(casualties, na.rm = TRUE)) %>%
  arrange(desc(total_casualties))
# the South had the most number of casualties (248)

#create a dataframe with the number of casualties by state based on population
cas_state_pop <- total_casualties_by_state %>% 
  group_by(state) %>% 
  left_join(pop_data, by = "state") %>% 
  mutate(casualties_per_capita = total_casualties / population) %>%
  arrange(desc(casualties_per_capita))
#DC has the highest casualties per capita at (9.918540e-06)

#create a dataframe with the number of shootings by state based on population
shooting_state_pop <- shootings_per_state %>% 
  group_by(state) %>% 
  left_join(pop_data, by = "state") %>% 
  mutate(shootings_per_capita = num_shootings/ population) %>%
  arrange(desc(shootings_per_capita))
#DC has the highest shootings per capita at (7.084672e-06)


#create a dataframe of the number of casualties by region based on population
cas_region_pop <- cas_state_pop
states_2 = cas_region_pop[,1]
regions <- assign_region(states_2)
cas_region_pop$region <- regions[,2]
cas_region_pop <- cas_region_pop %>%
  group_by(region$region) %>%
  summarise(total_casualties = sum(total_casualties),
            population = sum(population)) %>%
  mutate(casualties_per_capita = total_casualties / population) %>%
  arrange(desc(casualties_per_capita))
colnames(cas_region_pop) = c('region','total_casualties','population', 'casualties_per_capita')
#The West has the highest casualties per capita (2.378855e-06)

#create a dataframe with the number of shootings by region based on population
shooting_region_pop <- shooting_state_pop
states_3 = shooting_region_pop[,1]
regions_3 <- assign_region(states_3)
shooting_region_pop$region <- regions_3[,2]
shooting_region_pop <- shooting_region_pop %>%
  group_by(region$region) %>%
  summarise(num_shootings = sum(num_shootings),
            population = sum(population)) %>%
  mutate(shootings_per_capita = num_shootings / population) %>%
  arrange(desc(shootings_per_capita))
colnames(shooting_region_pop) = c('region','num_shootings','population', 'shootings_per_capita')
#The South has the highest shootings per capita (1.340564e-06)

#create a dataframe with the shootings per year
shootings_per_year <- complete_data %>%
  group_by(year) %>%
  summarize(num_shootings = n()) %>%
  arrange(desc(num_shootings))
#2022 had the highest number of school shootings (46)

#create a dataframe with the casualties per year
casualties_per_year <- complete_data %>%
  group_by(year) %>%
  summarize(casualties = sum(casualties)) %>%
  arrange(desc(casualties))
#2018 had the highest number of casualties (95)


#####GRAPHS

###line graphs

#graph for number of shootings by year
shootings_per_year_line = ggplot(shootings_per_year, aes(x = year, y = num_shootings)) +
  geom_line(color = 'black') +
  geom_text(aes(label = num_shootings, color = 'red'), vjust = -1) +
  labs(title = "Number of Shootings per Year", x = "Year", y = "Number of Shootings")
shootings_per_year_line

#graph for number of casualties by year
casualties_per_year_line = ggplot(casualties_per_year, aes(x = year, y = casualties)) +
  geom_line(color = 'black') +
  geom_text(aes(label = casualties, color = 'red'), vjust = -1) +
  labs(title = "Number of Casualties per Year", x = "Year", y = "Number of Casualties")
casualties_per_year_line

# maps created using the help of this video: https://www.youtube.com/watch?v=AgWgPSZ7Gp0
#create map_data dataframe at the state level. and rename the columns so that it 
#is easier to merge with other data Sadly, does not include hawaii or alaska :(
map_data <- map_data('state')
colnames(map_data) = c('long','lat','group', 'origin', 'state', 'subregion')

#create a dataframe with regional data by applying assign_region function to map_data
regional_map_data <- map_data
state_4 = data.frame(state = regional_map_data[,5])
regions_4 <- assign_region(state_4)
regional_map_data$region <- regions_4[,2]

#map for casualties per capita by state
map_data_casualties_per_capita_by_state <- left_join(map_data, cas_state_pop, by ='state')

map_casualties_per_capita_by_state<- ggplot(map_data_casualties_per_capita_by_state, aes(x = long, y = lat, group=group))+
  geom_polygon(aes(fill = casualties_per_capita),color = 'black') +
  scale_fill_fermenter(palette="Set2", n.breaks = 7) +
  labs(title = 'Map of Casualties Per Capita By State')
map_casualties_per_capita_by_state

#map for casualties per capita by region
map_data_casualties_per_capita_by_region <- left_join(regional_map_data, cas_region_pop, by ='region')

map_casualties_per_capita_by_region <- ggplot(map_data_casualties_per_capita_by_region, aes(x = long, y = lat, group=group))+
  geom_polygon(aes(fill = casualties_per_capita),color = 'black') +
  scale_fill_fermenter(palette="Set2", n.breaks = 7) +
  labs(title = 'Map of Casualties Per Capita By Region')
map_casualties_per_capita_by_region

#map for shootings per capita by state
map_data_shootings_per_capita_by_state <- left_join(map_data, shooting_state_pop, by ='state')

map_shootings_per_capita_by_state<- ggplot(map_data_shootings_per_capita_by_state, aes(x = long, y = lat, group=group))+
  geom_polygon(aes(fill = shootings_per_capita),color = 'black') +  
  scale_fill_fermenter(palette="Set2", n.breaks = 5) +
  labs(title = 'Map of Shooting Per Capita By State')
map_shootings_per_capita_by_state

#map for shootings per capita by region
map_data_shootings_per_capita_by_region <- left_join(regional_map_data, shooting_region_pop, by ='region')

map_shootings_per_capita_by_region <- ggplot(map_data_shootings_per_capita_by_region, aes(x = long, y = lat, group=group))+
  geom_polygon(aes(fill = shootings_per_capita),color = 'black') +
  scale_fill_fermenter(palette="Set2", n.breaks = 6) +
  labs(title = 'Map of Shootings Per Capita By State')
map_shootings_per_capita_by_region

#map of number of shootings per state
map_data_shootings_per_state <- left_join(map_data, shooting_state_pop, by ='state')
map_shootings_per_state<- ggplot(map_data_shootings_per_state, aes(x = long, y = lat, group=group))+
  geom_polygon(aes(fill = num_shootings),color = 'black') +
  labs(title = 'Map of Number of Shootings Per State') +
  scale_fill_fermenter(palette="Set2",  n.breaks = 6)
map_shootings_per_state

#map of number of shootings per region
map_data_shootings_per_region <- left_join(regional_map_data, shooting_region_pop, by ='region')
map_shootings_per_region<- ggplot(map_data_shootings_per_region, aes(x = long, y = lat, group=group))+
  geom_polygon(aes(fill = num_shootings),color = 'black') + 
  labs(title = 'Map of Number of Shootings Per Region') +
  scale_fill_fermenter(palette="Set2",  n.breaks = 6)
map_shootings_per_region

#map of number of casualties per state
map_data_casualties_per_state <- left_join(map_data, cas_state_pop, by ='state')
map_casualties_per_state<- ggplot(map_data_casualties_per_state, aes(x = long, y = lat, group=group))+
  geom_polygon(aes(fill = total_casualties),color = 'black') +
  labs(title = 'Map of Number of Casualties Per State') +
  scale_fill_fermenter(palette="Set2",  n.breaks = 6)
map_casualties_per_state

#map of number of casualties per region
map_data_casualties_per_region <- left_join(regional_map_data, cas_region_pop, by ='region')
map_casualties_per_region<- ggplot(map_data_casualties_per_region, aes(x = long, y = lat, group=group))+
  geom_polygon(aes(fill = total_casualties),color = 'black') + 
  labs(title = 'Map of Number of Casualties Per Region') +
  scale_fill_fermenter(palette="Set2",  n.breaks = 7)
map_casualties_per_region

# map that shows the different regions and the states associated with them
map_state_region <- ggplot(regional_map_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = region), color = 'black') + 
  labs(title = 'Regions of the United States') +
  scale_fill_manual(values = c("aquamarine", "seagreen2", "lightpink2", "plum3", "purple"))

map_state_region


