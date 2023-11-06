setwd("~/Desktop/carto")

library(sf)
library(tmap)
library(grid)

housing <- read.csv('housing.csv')

housing_sf <- st_as_sf(housing, coords = c("longitude", "latitude"), crs = 4326)

# Load the map of the United States from the "maps" package
library(maps)
us_map <- map_data("usa")
states <- map_data("state")
state_boundaries <- map("state", fill = FALSE, plot = FALSE)

states_plot <- ggplot() + 
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "black", color = "white", size = 0.1) +
  coord_fixed() +
  theme(panel.background = element_rect(fill = "black"),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())


# Add your data to the map using the sf object you created earlier
states_plot + 
  geom_sf(data = housing_sf, aes(color = median_house_value), size=0.1) +
#  lims(x = c(-130, -114), y = c(32, 43)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Housing Prices in California")

states_plot + 
  geom_sf(data = housing_sf, aes(color = median_house_value), size=0.1) +
  lims(x = c(-130, -114), y = c(32, 43)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Housing Prices in California") +
  theme(panel.background = element_rect(fill = "black"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"))


#works, making it look better

ggplot() + 
  geom_sf(data = housing_sf, color = 'blue', size=0.1) +
  geom_sf(data=us_map, aes(fill=region), color='black', alpha=0.3) +
  scale_fill_manual(values = c("black", "gray30", "gray60")) +
  coord_sf() +
  theme_void() +
  theme(
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

pit_state <- read.csv('pit-state.csv')
beds <- read.csv('2007-2022-HIC-Counts-by-State.csv')


houseincome <- read.csv('houseincome.csv')
houseincome <- subset(houseincome, select = -c(5,6,7))
houseincome <- houseincome[-c(52, 53), ]

library(ggplot2)
ggplot(houseincome, aes(x = median.house.prices, y = median.income, size = population)) +
  geom_point() +
  scale_size_continuous(range = c(5, 15)) +
  labs(x = "Median House Price", y = "Median Income", title = "Scatter Plot with Bubbles") +
  theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())

library(viridis)
ggplot(houseincome, aes(x = median.house.prices, y = median.income, size = population, color = population)) +
  geom_point() +
  geom_text(aes(label = State), size = 1.5, fontface = "bold", color='white') +
  scale_size_continuous(range = c(5, 15)) +
  scale_color_viridis() +
  labs(x = "Median House Price", y = "Median Income", title = "Scatter Plot with Bubbles") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        text = element_text(color = "white"))

price <- ggplot(houseincome, aes(x = median.house.prices, y = median.income, size = population, color = population)) +
  geom_point() +
  geom_text(aes(label = State), size = 1.5, fontface = "bold", color='white') +
  scale_size_continuous(range = c(10, 20)) +
  scale_color_viridis() +
  labs(x = "Median House Price", y = "Median Income", title = "Scatter Plot with Bubbles") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        text = element_text(color = "white"),
        axis.line = element_line(color = "white"),
        axis.text = element_text(color = "white")) 

ggsave('houseprice.png', price, width = 8, height = 6, dpi = 300)



