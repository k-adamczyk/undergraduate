#CoC - continuum of care, more detailed maps, do that for california
setwd("~/Desktop/carto")
pit_state <- read.csv('pit-state.csv')

pop_state <- read.csv('population.csv')


library(sf)
library(tmap)
library(grid)

# load shapefile
us_states <- st_read("cb_2018_us_state_20m.shp")

# project to Albers equal area projection
us_states <- st_transform(us_states, 2163)

us_states_map <- tm_shape(us_states) +
  tm_polygons() + 
  tm_layout(frame = FALSE)

# add state names
us_states_map_names <- us_states_map + 
  tm_text("NAME", size = 0.7)

us_states_sf <- merge(us_states, pit_state, by.x = "STUSPS", by.y = "State")
us_states_sf2 <- merge(us_states_sf, pop_state, by.x = "NAME", by.y = "state")

library(readr)
us_states_sf2$Overall.Homeless..2022 <- parse_number(us_states_sf2$Overall.Homeless..2022)
us_states_sf2$number <- parse_number(us_states_sf2$number)

#create ratio
#us_states_sf2$Overall.Homeless..2022 <- as.numeric(us_states_sf2$Overall.Homeless..2022)
us_states_sf2$homeless_ratio <- us_states_sf2$Overall.Homeless..2022/us_states_sf2$number

library(tmaptools)
tm_shape(us_states_sf2) +
  tm_fill("homeless_ratio", n = 5, style = "quantile", palette = "Blues") +
  tm_borders(lwd = 0.5, col = "white") +
  tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6, legend.text.color = "white", bg.color = "black")  +
  tm_text("NAME", size = 0.7)

tm_shape(us_states_sf) +
  tm_fill("Overall.Homeless..2022", n = 5, style = "quantile", palette = "Blues") +
  tm_borders(lwd = 0.5, col = "white") +
  tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6, legend.text.color = "white", bg.color = "black")  +
  tm_scale_bar2(breaks = c(0, 50, 100), width = 0.5, position = c("left", "bottom"), text.size = 0.5) + 
  tm_text("STUSPS", size = 0.7)

library(ggplot2)

ggplot() +
  geom_sf(data = us_states_sf2, aes(fill = homeless_ratio)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey90") +
  labs(fill = "% of homeless") +
  theme_void() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.caption = element_text(size = 10),
        legend.position = "bottom",
        legend.title.align = 0.5,
        legend.text = element_text(size = 8, color="white"),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.title = element_text(size = 10, color="white"),
        legend.background = element_rect(fill="black", color= NA),
        panel.background = element_rect(fill = "black", color = NA)) +
  coord_sf(crs = st_crs(2163), xlim = c(-3000000, 2500000), ylim = c(-2000000, 2500000))

ggplot() +
  geom_sf(data = us_states_sf2, aes(fill = homeless_ratio)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey90") +
  labs(fill = "% of homeless") +
  theme_void() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.caption = element_text(size = 10),
        legend.position = "bottom",
        legend.title.align = 0.5,
        legend.text = element_text(size = 8, color="white"),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.title = element_text(size = 10, color="white"),
        legend.background = element_rect(fill="black", color= NA),
        panel.background = element_rect(fill = "black", color = NA)) +
  coord_sf(crs = st_crs(2163), xlim = c(-6000000, 3500000), ylim = c(-3000000, 4000000)) +
  geom_sf_label(data = us_states_sf2, aes(label = STUSPS), size = 3)


ggplot() +
  geom_sf(data = us_states_sf2, aes(fill = homeless_ratio)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey90") +
  labs(fill = "% of homeless") +
  theme_void() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.caption = element_text(size = 10),
        legend.position = "bottom",
        legend.title.align = 0.5,
        legend.text = element_text(size = 8, color = "white"),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.title = element_text(size = 10, color = "white"),
        legend.background = element_rect(fill = "black", color = NA),
        panel.background = element_rect(fill = "black", color = NA)) +
  coord_sf(crs = st_crs(2163), xlim = c(-6000000, 3500000), ylim = c(-3000000, 4000000)) +
  geom_sf_text(data = us_states_sf2, aes(label = NAME), size = 3, color = "black", bg.color = "transparent")+
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"))

cities <- read.csv('cities.csv')
cities$Overall.Homeless..2022 <- parse_number(cities$Overall.Homeless..2022)
#top 9 of COC -> major cities
#los angeles, new york, seattle, san jose, oakland, sacramento city, phoenix, san diego, san fracisco
#la 12,488,000
#ny 18,867,000
#seattle 3,489,000
#san jose 1809,000
#oakland 439,483
#sacramento 2,186,000
#phoenix 4,652,000
#san diego 3,295,000
#san fran 3,318,000

major <- subset(cities, CoC.Number %in% c("CA-600", "NY-600", "WA-500", "CA-500", "CA-502", 
                                          "CA-503", "AZ-502", "CA-601", "CA-501"))

library(dplyr)
major <- major %>% arrange(desc(Overall.Homeless..2022))

major$pop <- c("12488000", "18867000", "3489000", "1809000", "439483","2186000",
                  "4652000","3295000", "3318000")

major$pop <- as.numeric(major$pop)

major$ratio <- major$Overall.Homeless..2022/major$pop

major$names <- c('Los Angeles', 'New York', 'Seattle', 'San Jose', 'Oakland', 
                 'Sacramento', 'Phoenix', 'San Diego', 'San Francisco')

#ratio
ggplot(major, aes(x = names, y = ratio)) +
  geom_col(position = "dodge", fill = "grey80", color = "black") +
  labs(title = "Homeless Ratio by Cities", x = "City", y = "Ratio") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none")

ggplot(major, aes(x = names, y = Overall.Homeless..2022)) +
  geom_col(position = "dodge", fill = "grey80", color = "black") +
  labs(title = "Number of Homeless by Cities", x = "City", y = "Number") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none")

ggplot(major, aes(x = names, y = 1)) +
  geom_point(aes(size = Overall.Homeless..2022, color = Overall.Homeless..2022), 
             position = position_dodge(width = 0.75)) +
  labs(title = "Number of Homeless by Cities", x = "City", y = "Number") +
  theme_bw() +
  scale_color_gradient(low = "lavender", high = "darkorchid4") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none")

ggplot(major, aes(x = names, y = 1, size = Overall.Homeless..2022, fill = Overall.Homeless..2022)) +
  geom_point(position = position_dodge(width = 0.9), shape = 21, stroke = 0.5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  scale_size(range = c(15, 30)) +
  labs(title = "Number of Homeless by Cities", x = "City", y = "Number") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none") +
  geom_text(aes(label = Overall.Homeless..2022), size = 3, color = "white", fontface = "bold", position = position_dodge(width = 1)) 

#now need to save the map and the graph as svg, make the map better in inkscape
#and add this dots onto a map with cities

time <- read.csv('homelesstime.csv')

#create proportion variable
time$homeless <- parse_number(time$homeless)
time$homeless <- as.numeric(time$homeless)

time$population <- as.numeric(time$population)

time$percent <- time$homeless/time$population

time$por <- c('0.166%', '0.170%', '0.173%', '0.113%', '0.172%')

library(ggplot2)
ggplot(time, aes(x = year, y = 1, size = percent, fill = percent)) +
  geom_point(alpha = 0.7, shape = 21) +
  geom_text(aes(label = year), size = 3, fontface = "bold", color='black') +
  scale_size_continuous(range = c(2, 12)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Homelessness in the US",
       y = "",
       x = "",
       size = "Homelessness (%)",
       fill = "Homelessness (%)") +
  theme_minimal() +
  theme(plot.margin = unit(c(0,0,0,0), "cm"),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18, margin = margin(20, 0, 20, 0)),
        legend.position = c(0.65, 0.5),
        text = element_text(color = "black")) +
  coord_flip() +
  guides(size = guide_legend(override.aes = list(size = c(2, 4, 6, 8, 10, 12))),
         fill = guide_legend(override.aes = list(size = 4, alpha = 0.7))) +
  theme(legend.position = c(0.65, 0.5),
        legend.direction = "vertical",
        legend.box = "horizontal")

ggplot(time, aes(x = 1, y = year, size = percent, fill = percent)) +
  geom_point(alpha = 0.7, shape = 21, stroke= 1, color='white') +
  geom_text(aes(label = por), size = 3, fontface = "bold", color='white') +
  scale_size_continuous(range = c(10, 22)) +
  scale_fill_viridis_c(option = "viridis", na.value = "grey90", direction=-1) +
  labs(title = "Homelessness in the US",
       y = "",
       x = "",
       size = "Homelessness (%)",
       fill = "Homelessness (%)") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18),
     #   legend.position = c(0.5, 0.5),
        text = element_text(color = "white")) +
  coord_flip() 
#  guides(size = guide_legend(override.aes = list(size = c(2, 4, 6, 8, 10, 12))),
 #        fill = guide_legend(override.aes = list(size = 4, alpha = 0.7))) +
 # theme(legend.position = c(0.65, 0.5),
  #      legend.direction = "vertical",
   #     legend.box = "horizontal")
ggsave("hom_years.png", width = 10, height = 8, dpi = 300)
