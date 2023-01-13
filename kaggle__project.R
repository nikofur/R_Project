library(tidyverse)

titanic_data <- read_csv('train.csv')
View(titanic_data)
t <- ggplot(data = titanic_data)

# Get a count of how many passengers from each home planet
passenger_count <- titanic_data %>%
  group_by(HomePlanet) %>%
  summarize(count = n())
passenger_count

write.table(passenger_count, file = "csvs/passenger_count.csv", sep = ",", col.names = NA,
            qmethod = "double")

# Pie graph showing number of passengers from each Home Planet
t + geom_bar(mapping = aes(x = HomePlanet, fill = HomePlanet)) + 
  labs(x = "Home Planet", y = "Number of Passengers",
       title = "Number of Passengers per Home Planet") + 
  coord_polar()

ggsave("passengers_per_home_planet.png", width = 5, height = 5,
       path = "plots")

# Creating a column in the data showing the total amount spent on all amenities
titanic_data <- mutate(titanic_data,
                       TotalAmenities = RoomService + FoodCourt + ShoppingMall + Spa + VRDeck)

# 
amenities_per_hp <- titanic_data %>%
  group_by(HomePlanet) %>%
  summarize(Average = mean(TotalAmenities, na.rm = TRUE),
            Total = sum(TotalAmenities, na.rm = TRUE)) %>%
  arrange(desc(Total))
amenities_per_hp

write.table(amenities_per_hp, file = "csvs/amenities_per_hp.csv", sep = ",", col.names = NA,
            qmethod = "double")

# Where do Earthlings Spend Their Money?
earth_spending <- titanic_data %>%
  filter(HomePlanet == 'Earth') %>%
  summarize(Room_Service = mean(RoomService, na.rm = TRUE),
            Food_Court = mean(FoodCourt, na.rm = TRUE),
            Shopping_Mall = mean(ShoppingMall, na.rm = TRUE),
            Spa_ = mean(Spa, na.rm = TRUE),
            VR_Deck = mean(VRDeck, na.rm = TRUE))
earth_spending

earth_spending_long <- pivot_longer(earth_spending, cols = Room_Service:VR_Deck, names_to = 'Amenity', 'value')
earth_spending_long
earth_spending_bar <- ggplot(data = earth_spending_long)
earth_spending_bar + geom_col(mapping = aes(x = Amenity, y = value,
                                            fill = Amenity)) +
  labs(x = "Amenities", y = "Average Amount Spent", 
       title = "Earthlings") +
  guides(x = guide_axis(angle = 45))

ggsave("earthling_spending_bar.png", width = 5, height = 5,
       path = "plots")

# Where do Europians spend their money?
europa_spending <- titanic_data %>%
  filter(HomePlanet == 'Europa') %>%
  summarize(Room_Service = mean(RoomService, na.rm = TRUE),
            Food_Court = mean(FoodCourt, na.rm = TRUE),
            Shopping_Mall = mean(ShoppingMall, na.rm = TRUE),
            Spa_ = mean(Spa, na.rm = TRUE),
            VR_Deck = mean(VRDeck, na.rm = TRUE))
europa_spending

europa_spending_long <- pivot_longer(europa_spending, cols = Room_Service:VR_Deck, names_to = "Amenity", 'value')
europa_spending_long
europa_spending_bar <- ggplot(data = europa_spending_long)
europa_spending_bar + geom_col(mapping = aes(x = Amenity, y = value,
                                             fill = Amenity)) +
  labs(x = "Amenities", y = "Average Amount Spent",
       title = "Europanites") + 
  guides(x = guide_axis(angle = 45))

ggsave("europanites_spending_bar.png", width = 5, height = 5,
       path = "plots")

# Where do Martians spend their money?
mars_spending <- titanic_data %>%
  filter(HomePlanet == 'Mars') %>%
  summarize(Room_Service = mean(RoomService, na.rm = TRUE),
            Food_Court = mean(FoodCourt, na.rm = TRUE),
            Shopping_Mall = mean(ShoppingMall, na.rm = TRUE),
            Spa_ = mean(Spa, na.rm = TRUE),
            VR_Deck = mean(VRDeck, na.rm = TRUE))
mars_spending

mars_spending_long <- pivot_longer(mars_spending, cols = Room_Service:VR_Deck, names_to = "Amenity", 'value')
mars_spending_long
mars_spending_bar <- ggplot(data = mars_spending_long)
mars_spending_bar + geom_col(mapping = aes(x = Amenity, y = value,
                                           fill = Amenity)) +
  labs(x = "Amenities", y = "Average Amount Spent",
       title = "Martians") + 
  guides(x = guide_axis(angle = 45))

ggsave("martians_spending_bar.png", width = 5, height = 5,
       path = "plots")

# Does age affect spending?
age_spending <- titanic_data %>%
  group_by(Age) %>%
  summarize(avg_spending = mean(TotalAmenities, na.rm = TRUE))
age_spending

age_spending_col <- ggplot(data = age_spending)
age_spending_col + geom_col(mapping = aes(x = Age, y = avg_spending, 
                                          fill = Age)) +
  labs(y = 'Amount', title = 'Avg Spent on Amenities by Age')

ggsave("avg_spending_by_age.png", width = 5, height = 5,
       path = "plots")

age_by_hp <- titanic_data %>%
  filter(HomePlanet != 'NA')

age_point <- ggplot(data = age_by_hp)
age_point + geom_point(mapping = aes(x = Age, y = TotalAmenities, color = HomePlanet)) +
  facet_wrap(~ HomePlanet, nrow = 1) + 
  labs(y = "Amount Spent", 
       title = "Money Spent on Ammenities Per Age")

ggsave('age_spending_point.png', width = 5, height = 5,
       path = "plots")


# Displaying the average age of passengers from each Home Planet
avg_age <- group_by(titanic_data, HomePlanet)
summarize(avg_age, age = mean(Age, na.rm = TRUE))

group_by(HomePlanet) %>%
  summarize(count = n(),
            avgRoomService = mean(RoomService, na.rm = TRUE),
            totalRoomService = sum(RoomService, na.rm = TRUE),
            avgFoodCourt = mean(FoodCourt, na.rm = TRUE),
            totalFoodCourt = sum(FoodCourt, na.rm = TRUE),
            avgShoppingMall = mean(ShoppingMall, na.rm = TRUE),
            totalShoppingMall = sum(ShoppingMall, na.rm = TRUE),
            avgSpa = mean(Spa, na.rm = TRUE),
            totalSpa = sum(Spa, na.rm = TRUE),
            avgVRDeck = mean(VRDeck, na.rm = TRUE),
            totalVRDeck = sum(VRDeck, na.rm = TRUE),
  )
View(amount_spent)
write.table(amount_spent, file = "amount_spent.csv", sep = ",", col.names = NA,
            qmethod = "double")

rs <- ggplot(data = amount_spent)
# Column graphs for average and total spent on Room Service
room_service <- amount_spent %>%
  select(HomePlanet, avgRoomService, totalRoomService)
room_service

rs + geom_col(mapping = aes(x = HomePlanet, y = avgRoomService,
                            fill = HomePlanet)) +
  labs(x = "Home Planet", y = "Amount Spent",
       title = "Average Money Spent on Room Service")
ggsave("avg_room_service.png", width = 4, height = 4,
       path = "plots")

rs + geom_col(mapping = aes(x = HomePlanet, y = totalRoomService,
                            fill = HomePlanet)) +
  labs(x = "Home Planet", y = "Total Spent",
       title = "Total Money Spent on Room Service")
ggsave("total_room_service.png", width = 4, height = 4,
       path = "plots")

# Column graphs showing average and total spent at Food Court
food_court <- amount_spent %>%
  select(HomePlanet, avgFoodCourt, totalFoodCourt)
food_court

rs + geom_col(mapping = aes(x = HomePlanet, y = avgFoodCourt,
                            fill = HomePlanet)) +
  labs(x = "Home Planet", y = "Amount Spent",
       title = "Average Money Spent at Food Court")
ggsave("avg_food_court.png", width = 4, height = 4,
       path = "plots")

rs + geom_col(mapping = aes(x = HomePlanet, y = totalFoodCourt,
                            fill = HomePlanet)) +
  labs(x = "Home Planet", y = "Total Spent",
       title = "Total Money spent on Food Court")
ggsave("total_food_court.png", width = 4, height = 4,
       path = "plots")

# Columns graphs showing average and total spent at Shopping Mall
shopping_mall <- amount_spent %>%
  select(HomePlanet, avgShoppingMall, totalShoppingMall)
shopping_mall

rs + geom_col(mapping = aes(x = HomePlanet, y = avgShoppingMall,
                            fill = HomePlanet)) +
  labs(x = "Home Planet", y = "Amount Spent",
       title = "Average Money Spent at Shopping Mall")
ggsave("avg_shopping_mall.png", width = 4, height = 4,
       path = "plots")

rs + geom_col(mapping = aes(x = HomePlanet, y = totalShoppingMall,
                            fill = HomePlanet)) +
  labs(x = "Home Planet", y = "Total Spent",
       title = "Total Money Spent at Shopping Mall")
ggsave("total_shopping_mall.png", width = 4, height = 4,
       path = "plots")

# Column graphs showing average and total spent at Spa
spa <- amount_spent %>%
  select(HomePlanet, avgSpa, totalSpa)
spa

rs + geom_col(mapping = aes(x = HomePlanet, y = avgSpa,
                            fill = HomePlanet)) +
  labs(x = "Home Planet", y = "Amount Spent",
       title = "Average Money Spent at Spa")
ggsave("avg_spa.png", width = 4, height = 4,
       path = "plots")

rs + geom_col(mapping = aes(x = HomePlanet, y = totalSpa,
                            fill = HomePlanet)) +
  labs(x = "Home Planet", y = "Total Spent",
       title = "Total Money Spent at Spa")
ggsave("total_spa.png", width = 4, height = 4,
       path = "plots")

# Column graphs showing average and total spent at VRDeck
vrdeck <- amount_spent %>%
  select(HomePlanet, avgVRDeck, totalVRDeck)
vrdeck

rs + geom_col(mapping = aes(x = HomePlanet, y = avgVRDeck,
                            fill = HomePlanet)) +
  labs(x = "Home Planet", y = "Amount Spent",
       title = "Average Money Spent at VR Deck")
ggsave("avg_vrdeck.png", width = 4, height = 4,
       path = "plots")

rs + geom_col(mapping = aes(x = HomePlanet, y = totalVRDeck,
                            fill = HomePlanet)) +
  labs(x = "Home Planet", y = "Total Spent",
       title = "Total Money Spent at VR Deck")
ggsave("total_vrdeck.png", width = 4, height = 4,
       path = "plots")

# Boxplot showing disparity in spending
earthlings <- titanic_data %>%
  filter(HomePlanet == 'Earth') %>%
  group_by(Age) %>%
  summarize(avg_spent = mean(TotalAmenities, na.rm = TRUE))
View(earthlings)

europanites <- titanic_data %>%
  filter(HomePlanet == 'Europa') %>%
  group_by(Age) %>%
  summarize(avg_spent = mean(TotalAmenities, na.rm = TRUE))
View(europanites)

martians <- titanic_data %>%
  filter(HomePlanet == "Mars") %>%
  group_by(Age) %>%
  summarize(avg_spent = mean(TotalAmenities, na.rm = TRUE))
View(martians)
##########
spending_box <- titanic_data %>%
  drop_na() %>%
  filter(Age > 12) %>%
  group_by(HomePlanet, Age) %>%
  summarize(avg_spending = mean(TotalAmenities, na.rm = TRUE))
View(spending_box)

p <- ggplot(data = spending_box, aes(x = HomePlanet, y = avg_spending,
                                     color = HomePlanet)) +
  geom_boxplot(outlier.color="red") +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  labs(x = 'Home Planet', y = 'Average Spending',
       title = 'Disparity of Spending at differ')
p
