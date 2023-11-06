setwd("~/Desktop/uni")

pay <- read.csv('payratios.csv')

library(dplyr)
pay2 <- select(pay, Pay.Ratio, State, Sector_Desc)
summary(pay2$Pay.Ratio)
pay2$Pay.Ratio2 <- as.numeric(pay2$Pay.Ratio)
summary(pay2$Pay.Ratio2)

pay2clean <-  pay2[complete.cases(pay2$Pay.Ratio2), ]

#create a new one w means for each state
mean_by_state <- pay2clean %>%
  group_by(State) %>%
  mutate(mean_value = mean(Pay.Ratio2))


# Plot the mean pay ratio for each state using a bar chart
library(ggplot2)
ggplot(mean_by_state, aes(x = State, y = mean_value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Mean Pay Ratio by State") +
  xlab("State") +
  ylab("Mean Pay Ratio")

#by industry
mean_by_industry <- pay2clean %>%
  group_by(Sector_Desc) %>%
  mutate(mean_value = mean(Pay.Ratio2))

ggplot(mean_by_industry, aes(x = Sector_Desc, y = mean_value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Mean Pay Ratio by Sector") +
  xlab("Sector") +
  ylab("Mean Pay Ratio")

means = subset(mean_by_state, select = -c(Pay.Ratio,Pay.Ratio2, Sector_Desc))
means2 <- means[!duplicated(means), ]

ggplot(means2, aes(x = State, y = mean_value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Mean Pay Ratio by State") +
  xlab("State") +
  ylab("Mean Pay Ratio")

means2 <- means2[-3,]

library(writexl)
write_xlsx(means2, "~/Desktop/uni/meansbystates.xlsx")

setwd("~/carto")

payratio<- read.csv('pay.csv')

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

us_states_sf <- merge(us_states, payratio, by.x = "NAME", by.y = "State")

library(ggplot2)
library(viridis)


library(tmaptools)
tm_shape(us_states_sf) +
  tm_fill("mean_value", n = 5, style = "quantile", palette = "Purples", title='Mean Pay Ratio') +
  tm_borders(lwd = 0.5, col = "black") +
  tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6, legend.text.color = "black", bg.color = "white")  +
  tm_text("STUSPS", size = 0.7)
tmap_save(filename = "payratio_map.svg", width = 10, height = 8, units = "in", dpi = 300)


