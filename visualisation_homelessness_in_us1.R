#do 2 maps - one proportion of beds for total pop and the other for number of homeless

setwd("~/Desktop/carto")

beds <- read.csv('2007-2022-HIC-Counts-by-State.csv')

beds <- beds[, c(1, 2, 10, 11, 12, 13, 14)]

names(beds)[c(1, 2, 3, 4, 5, 6, 7)] <- c('state', 'total_beds', 'with_children',
                                                      'with_no_children', 'with_only_children',
                                                      'veterans', 'youth')
beds <- beds[-1, ]
beds <- beds[-57, ]
beds2 <- beds[-56, ]

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

pit_state <- read.csv('pit-state.csv')
per_hom <- merge(pit_state, beds2, by.x = "State", by.y = "state")
per_hom$total_beds <- parse_number(per_hom$total_beds)
per_hom$Overall.Homeless..2022 <- parse_number(per_hom$Overall.Homeless..2022)
per_hom$beds_per_hom <- per_hom$total_beds/per_hom$Overall.Homeless..2022
per_hom <- per_hom[, c(1, 582)]
library(openxlsx)
write.xlsx(per_hom, "per_hom.xlsx", rowNames = FALSE)


us_states_sf <- merge(us_states, beds2, by.x = "STUSPS", by.y = "state")
us_states_sf2 <- merge(us_states_sf, pop_state, by.x = "NAME", by.y = "state")
us_states_sf3 <- merge(us_states_sf2, pit_state, by.x = "STUSPS", by.y = "State")

library(readr)
us_states_sf3$total_beds <- parse_number(us_states_sf3$total_beds)
us_states_sf3$number <- parse_number(us_states_sf3$number)
us_states_sf3$Overall.Homeless..2022 <- parse_number(us_states_sf3$Overall.Homeless..2022)

#create 2 ratios
us_states_sf3$beds_per_pop <- us_states_sf3$total_beds/us_states_sf3$number
us_states_sf3$beds_per_hom <- us_states_sf3$total_beds/us_states_sf3$Overall.Homeless..2022

#one with just STUSPS, 
shelters <- us_states_sf3[, c(1:9, 591:593)]
write.csv(shelters, "shelters.csv", row.names = FALSE)

write.csv(my_data, "my_data.csv", row.names = FALSE)

library(tmaptools)
tm_shape(us_states_sf3) +
  tm_fill("beds_per_pop", n = 5, style = "quantile", palette = "Oranges") +
  tm_borders(lwd = 0.5, col = "white") +
  tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6, legend.text.color = "white", bg.color = "black")  +
  tm_text("STUSPS", size = 0.7)

hex_sp <- hexbin(as(us_states_sf3, "Spatial"), xbnds = NULL, ybnds = NULL, IDs = TRUE)

# Create a tmap object from the hex_sp object
hex_map <- tm_shape(hex_sp) +
  tm_fill("beds_per_pop", n = 5, style = "quantile", palette = "Oranges") +
  tm_borders(lwd = 0.5, col = "white") +
  tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6, legend.text.color = "white", bg.color = "black")

# Display the map
hex_map

hex_sp <- hexagons(st_coordinates(us_states_sf3))

# convert hex_sp to an sf object
hex_sf <- st_as_sf(hex_sp)

# join bed-to-population ratio data to hex_sf
hex_sf$beds_per_pop <- us_states_sf3$beds_per_pop[hex_sp$cell]

# plot hexagons with tmap
tm_shape(hex_sf) +
  tm_fill("beds_per_pop", n = 5, style = "quantile", palette = "Oranges") +
  tm_borders(lwd = 0.5, col = "white") +
  tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6, legend.text.color = "white", bg.color = "black") +
  tm_text("STUSPS", size = 0.7)

library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)

spdf <- geojson_read("DATA/us_states_hexgrid.geojson.json",  what = "sp")
library(broom)
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

# Calculate the centroid of each hexagon to add the label:
library(rgeos)
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

# Now I can plot this shape easily as described before:
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="skyblue", color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_void() +
  coord_map()

coords <- lapply(st_geometry(us_states_sf3), st_coordinates)
coords_mat <- do.call(rbind, coords)

# Create a hexagonal grid from the US states data
hex_df <- hexbin(coords_mat, xbins = 50)

# Convert the hexagons to a data.frame and then to a spatial object
hex_df_df <- data.frame(x = hex_df@xcm, y = hex_df@ycm, count = hex_df@count)
hex_sp <- st_as_sf(hex_df_df, coords = c("x", "y"), crs = st_crs(us_states_sf3))

tm_shape(hex_sp) +
tm_polygons(style = "hexagonal", border.col = "white", border.lwd = 0.5, hex.size = 0.5) 

tm_shape(hex_sp) +
  tm_symbols(col = "count", palette = "Oranges", n = 5, style = "quantile", size = 0.7) +
  tm_borders(lwd = 0.5, col = "white") +
  tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6, legend.text.color = "white", bg.color = "black") +
  tm_text("STUSPS", size = 0.7)

# Plot the hexagonal map
tm_shape(hex_sp) +
  tm_fill(col = "count", palette = "Oranges", n = 5, style = "quantile") +
  tm_borders(col = "white", lwd = 0.5) +
  tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6, legend.text.color = "white", bg.color = "black") +
  tm_text("STUSPS", size = 0.7)

# Plot the hexagonal map
tm_shape(hex_sp) +
  tm_fill(col = "count", palette = "Oranges", n = 5, style = "quantile") +
  tm_borders(col = "white", lwd = 0.5) +
  tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6, legend.text.color = "white", bg.color = "black")  +
  tm_text("STUSPS", size = 0.7)
hexgrid <- tm_shape(us_states_sf3) +
  tm_fill("beds_per_pop", n = 5, style = "quantile", palette = "Oranges") +
  tm_borders(lwd = 0.5, col = "white") +
  tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6, legend.text.color = "white", bg.color = "black")  +
  tm_text("STUSPS", size = 0.7) +
  tm_shape(tmaptools::hex_grid(c(us_states_sf3$geometry), dx = 0.2, dy = 0.2)) +
  tm_borders(lwd = 0.1, col = "white", alpha = 0.5) +
  tm_layout(frame = FALSE)



# Plot the hexagonal map
tmap_mode("plot")
plot(hexgrid)

tm_shape(us_states_sf3) +
  tm_fill("beds_per_pop", n = 5, style = "quantile", palette = "Oranges") +
  tm_borders(lwd = 0.5, col = "white") +
  tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6, legend.text.color = "white", bg.color = "black")  +
  tm_text("STUSPS", size = 0.7)


tm_shape(us_states_sf3) +
  tm_fill("beds_per_hom", n = 5, style = "quantile", palette = "Purples") +
  tm_borders(lwd = 0.5, col = "white") +
  tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6, legend.text.color = "white", bg.color = "black")  +
  tm_text("STUSPS", size = 0.7)




