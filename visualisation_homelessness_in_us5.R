

setwd("~/Desktop/carto")
pit_state <- read.csv('pit-state.csv')

race_distribution <- pit_state[, c(1, 2, 29, 30, 31, 32, 33, 34, 35, 36, 46, 47, 48, 49, 50, 51,
                                   52, 53, 63, 64, 65, 66, 67, 68, 69, 70, 80, 81, 82, 83, 84, 85, 
                                   86, 87, 97, 98, 99, 100, 101, 102, 103, 104)]


names(race_distribution)[c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
                             18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 
                             31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42)] <- c('ES_non_hispanic', 'ES_hispanic',
                                                               'ES_white', 'ES_african_american',
                                                               'ES_asian', 'ES_american_indian',
                                                               'ES_pacific_islander', 'ES_multiple_race',
                                                               'TH_non_hispanic', 'TH_hispanic',
                                                               'TH_white', 'TH_african_american',
                                                               'TH_asian', 'TH_american_indian',
                                                               'TH_pacific_islander', 'TH_multiple_race',
                                                               'SH_non_hispanic', 'SH_hispanic',
                                                               'SH_white', 'SH_african_american',
                                                               'SH_asian', 'SH_american_indian',
                                                               'SH_pacific_islander', 'SH_multiple_race',
                                                               'TS_non_hispanic', 'TS_hispanic',
                                                               'TS_white', 'TS_african_american',
                                                               'TS_asian', 'TS_american_indian',
                                                               'TS_pacific_islander', 'TS_multiple_race',
                                                               'TUS_non_hispanic', 'TUS_hispanic',
                                                               'TUS_white', 'TUS_african_american',
                                                               'TUS_asian', 'TUS_american_indian',
                                                               'TUS_pacific_islander', 'TUS_multiple_race')

#this data is separated by state, need to add a total row

total_race_dist <- subset(race_distribution, State == "Total")

#create a radar chart comparing sheltered and unsheltered based on race
library(fmsb)

# select the columns you need
radar_data <- total_race_dist[, c('TS_non_hispanic', 'TS_hispanic',
                                    'TS_white', 'TS_african_american',
                                    'TS_asian', 'TS_american_indian',
                                    'TS_pacific_islander', 'TS_multiple_race',
                                    'TUS_non_hispanic', 'TUS_hispanic',
                                    'TUS_white', 'TUS_african_american',
                                    'TUS_asian', 'TUS_american_indian',
                                    'TUS_pacific_islander', 'TUS_multiple_race')]

radar_data <- race_distribution[, c('TS_non_hispanic', 'TS_hispanic',                                    'TS_white', 'TS_african_american',                                    'TS_asian', 'TS_american_indian',                                    'TS_pacific_islander', 'TS_multiple_race',                                    'TUS_non_hispanic', 'TUS_hispanic',                                    'TUS_white', 'TUS_african_american',                                    'TUS_asian', 'TUS_american_indian',                                    'TUS_pacific_islander', 'TUS_multiple_race')]
column_names <- substr(colnames(radar_data), 4, nchar(colnames(radar_data)))

# plot radar chart
fms_radar <- plot_ly(radar_data, type = 'scatterpolar', mode = 'lines') %>%
  add_trace(name = 'Total Sheltered') %>%
  add_trace(name = 'Total Unsheltered') %>%
  for(i in 1:length(column_names)) {
    add_trace(r = radar_data[,i], theta = column_names[i], name = column_names[i],
              subplot = paste('polar', i, sep = '')) %>%
      layout(polar = list(radialaxis = list(visible = TRUE)),
             xaxis = list(domain = c(((i-1)/length(column_names))), i/length(column_names)),
             yaxis = list(domain = c(0,1)),
             showlegend = FALSE)
  }

# update layout to show legend
fms_radar <- layout(fms_radar, showlegend = TRUE)


library(fmsb)

#drop the non-hispanic cuz it's some bs
radar_data <- total_race_dist[, c('TS_hispanic',
                                    'TS_white', 'TS_african_american',
                                    'TS_asian', 'TS_american_indian',
                                    'TS_pacific_islander', 'TS_multiple_race',
                                    'TUS_hispanic',
                                    'TUS_white', 'TUS_african_american',
                                    'TUS_asian', 'TUS_american_indian',
                                    'TUS_pacific_islander', 'TUS_multiple_race')]

radar_data$TS_hispanic <- parse_number(radar_data$TS_hispanic)
radar_data$TS_white <- parse_number(radar_data$TS_white)
radar_data$TS_african_american <- parse_number(radar_data$TS_african_american)
radar_data$TS_asian <- parse_number(radar_data$TS_asian)
radar_data$TS_american_indian <- parse_number(radar_data$TS_american_indian)
radar_data$TS_pacific_islander <- parse_number(radar_data$TS_pacific_islander)
radar_data$TS_multiple_race <- parse_number(radar_data$TS_multiple_race)
radar_data$TUS_hispanic <- parse_number(radar_data$TUS_hispanic)
radar_data$TUS_white <- parse_number(radar_data$TUS_white)
radar_data$TUS_african_american <- parse_number(radar_data$TUS_african_american)
radar_data$TUS_asian <- parse_number(radar_data$TUS_asian)
radar_data$TUS_american_indian <- parse_number(radar_data$TUS_american_indian)
radar_data$TUS_pacific_islander <- parse_number(radar_data$TUS_pacific_islander)
radar_data$TUS_multiple_race <- parse_number(radar_data$TUS_multiple_race)


# Create data

radar_data_2 <- data.frame(
  Category = c('hispanic', 'white', 'african_american',
               'asian', 'american_indian', 'pacific_islander', 'multiple_race'),
  TS = c(radar_data$TS_hispanic, radar_data$TS_white,
         radar_data$TS_african_american, radar_data$TS_asian, radar_data$TS_american_indian,
         radar_data$TS_pacific_islander, radar_data$TS_multiple_race),
  TUS = c(radar_data$TUS_hispanic, radar_data$TUS_white,
          radar_data$TUS_african_american, radar_data$TUS_asian, radar_data$TUS_american_indian,
          radar_data$TUS_pacific_islander, radar_data$TUS_multiple_race)
)

library(plotly)

# Set the bar width
bar_width <- 0.35

# Plot the bars
plot_ly(radar_data_2, x = ~Category) %>%
  add_trace(y = ~TS, name = "TS", type = "bar",
            offset = -bar_width/2,
            width = bar_width,
            marker = list(color = "blue")) %>%
  add_trace(y = ~TUS, name = "TUS", type = "bar",
            offset = bar_width/2,
            width = bar_width,
            marker = list(color = "red")) %>%
  layout(barmode = "group",
         xaxis = list(tickangle = -45, tickfont = list(size = 12)))

library(openxlsx)
write.xlsx(radar_data_2, "race_dist.xlsx", rowNames = FALSE)
library(tidyr)

radar_data_2_long <- pivot_longer(radar_data_2, cols = c(TS, TUS), names_to = "Metric", values_to = "Value")

library(plotly)

plot_ly(radar_data_2_long, type = "scatterpolar",
        mode = "lines+markers",
        line = list(shape = "spline"),
        marker = list(symbol = "circle", size = 8),
        theta = ~Category,
        r = ~Value,
        color = ~Metric,
        colors = c("blue", "red")) %>%
  layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, max(radar_data_2_long$Value)))),showlegend = TRUE)

plot_ly(radar_data_2_long, type = "scatterpolar",
        mode = "lines",
        line = list(shape = "spline", width = 3),
        theta = ~Category,
        r = ~Value,
        color = ~Metric,
        colors = c("blue", "red")) %>%
  add_trace(type = "scatterpolar",
            mode = "markers",
            marker = list(symbol = "circle", size = 8),
            theta = ~Category,
            r = ~Value,
            color = ~Metric,
            colors = c("blue", "red")) %>%
  layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, max(radar_data_2_long$Value)))),showlegend = TRUE)

# Create a polar chart
# Create a polar chart
plot_ly(radar_data_2_long, type = "scatterpolar",
        mode = "lines+markers",
        line = list(shape = "spline"),
        marker = list(symbol = "circle", size = 8),
        theta = ~Category,
        r = ~Value,
        color = ~Metric,
        colors = c("blue", "red")) %>%
  layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, max(radar_data_2_long$Value)))),showlegend = TRUE)




# Calculate the maximum value to use for the radial axis range
max_val <- max(radar_data_2$TS, radar_data_2$TUS)

# Create a radial bar chart
plot_ly(radar_data_2, type = "scatterpolar", mode = "markers",
        marker = list(symbol = "square"),
        theta = ~Category,
        r = ~TS,
        name = "TS",
        hoverinfo = "text",
        text = ~paste0("TS: ", TS)) %>%
  add_trace(r = ~TUS,
            name = "TUS",
            hoverinfo = "text",
            text = ~paste0("TUS: ", TUS)) %>%
  layout(polar = list(radialaxis = list(range = c(0, max_val))),
         showlegend = TRUE)

plot_ly(radar_data_2, theta = ~Category, r = ~TS, type = 'scatterpolar', 
        fill = 'toself', line = list(color = 'blue', width = 3), name = 'TS') %>%
  add_trace(theta = ~Category, r = ~TUS, type = 'scatterpolar', 
            fill = 'toself', line = list(color = 'red', width = 3), name = 'TUS')

# Create sample data
radar_data_2 <- data.frame(Category = c("A", "B", "C", "D", "E"),
                           TS = c(2, 5, 1, 6, 3),
                           TUS = c(3, 2, 4, 1, 5))

# Set up parameters for the polar plot
theta <- seq(0, 2*pi, length.out = nrow(radar_data_2)+1)[-1]
r <- max(radar_data_2$TS, radar_data_2$TUS)

# Set up parameters for the triangular bars
max_width <- 0.5
bar_widths <- seq(max_width/2, max_width, length.out = nrow(radar_data_2))

# Create the plot
plot_ly() %>%
  add_trace(type = "scatterpolar",
            mode = "lines",
            line = list(color = "black"),
            showlegend = FALSE) %>%
  add_trace(data = radar_data_2,
            type = "barpolar",
            width = bar_widths,
            offset = -(max_width/2),
            marker = list(color = "blue")) %>%
  layout(polar = list(radialaxis = list(range = c(0, r))),
         showlegend = FALSE)

print(radar_data_2$r)
print(radar_data_2$theta)

max_val <- max(radar_data_2$TS, radar_data_2$TUS)

# Define number of bars to use
num_bars <- 10

# Define the radius values for each bar
radius_values <- seq(0, max_val, length.out = num_bars)

# Define the thickness of each bar
thickness_values <- seq(0.1, 1, length.out = num_bars)

# Create a list of bar colors
bar_colors <- c("blue", "red")

# Create a list to store the bar traces
bar_traces <- list()

# Loop through each category and create a bar trace for each
for (i in seq_along(radar_data_2$Category)) {
  cat <- radar_data_2$Category[i]
  ts_val <- radar_data_2$TS[i]
  tus_val <- radar_data_2$TUS[i]
  
  # Calculate the width of the bar at each radius
  bar_widths <- thickness_values * max_val / num_bars
  
  # Calculate the position of the bar at each radius
  bar_positions <- radius_values - (thickness_values/2) * max_val / num_bars
  
  # Create a bar trace for the TS values
  ts_trace <- list(
    type = "barpolar",
    r = c(rep(0, num_bars - length(ts_val)), ts_val),
    theta = seq(0, 360, length.out = num_bars),
    width = bar_widths,
    base = bar_positions,
    marker = list(color = bar_colors[1])
  )
  
  # Create a bar trace for the TUS values
  tus_trace <- list(
    type = "barpolar",
    r = c(rep(0, num_bars - length(tus_val)), tus_val),
    theta = seq(0, 360, length.out = num_bars),
    width = bar_widths,
    base = bar_positions,
    marker = list(color = bar_colors[2])
  )
  
  # Add the bar traces to the list
  bar_traces[[i * 2 - 1]] <- ts_trace
  bar_traces[[i * 2]] <- tus_trace
}

library(plotly)
# Plot the bar chart
plot_ly() %>%
  add_trace(bar_traces) %>%
  layout(
    polar = list(radialaxis = list(range = c(0, max_val))),
    showlegend = FALSE
  )


# Define bar width
# Set the bar width
# Define the bar width and base size
# Create example data
radar_data_2 <- data.frame(Category = c("Cat1", "Cat2", "Cat3", "Cat4", "Cat5"),
                           TS = c(12, 8, 10, 15, 5),
                           TUS = c(8, 10, 12, 5, 15))

# Define the number of categories and angles for the star chart
num_categories <- nrow(radar_data_2)
angles <- seq(0, 360, length.out = num_categories + 1)

# Define the bar width and height
bar_width <- 0.2
bar_height <- 1

# Define the number of categories
n <- length(unique(radar_data_2$Category))

# Define the angles for each category
angles <- seq(0, 2*pi - 2*pi/n, by = 2*pi/n)

# Define the bar width
bar_width <- 0.2

# Plot the bars
plot_ly(radar_data_2, theta = angles, r = ~TS, name = "TS", type = "bar",
        width = bar_width,
        marker = list(color = "blue")) %>%
  add_trace(r = ~TUS, name = "TUS", type = "bar",
            width = bar_width,
            marker = list(color = "red")) %>%
  layout(polar = list(radialaxis = list(range = c(0, max(radar_data_2$TS, radar_data_2$TUS))),
                      angularaxis = list(direction = "clockwise", tickvals = angles, ticktext = unique(radar_data_2$Category))),
         showlegend = TRUE)







radarchart(radar_data_2)

race_dist <- as.data.frame(matrix(78666,
                                  157637,
                                  154557,
                                  3909,
                                  8843,
                                  4692,
                                  18992,
                                  61564,
                                  133758,
                                  62809,
                                  4352,
                                  10775,
                                  5769,
                                  16391, ncol=14)))
colnames(race_dist) <- c('TS_hispanic', 'TS_white', 'TS_african_american',
                    'TS_asian', 'TS_american_indian', 'TS_pacific_islander', 'TS_multiple_race',
                    'TUS_hispanic', 'TUS_white', 'TUS_african_american',
                    'TUS_asian', 'TUS_american_indian', 'TUS_pacific_islander', 'TUS_multiple_race')

is.numeric(radar_data$TS_hispanic)

library(plotly)

# Set up data for the radar chart
radar_data_2$Category <- factor(radar_data_2$Category, levels = c('hispanic', 'white', 'african_american',
                                                                  'asian', 'american_indian', 'pacific_islander', 'multiple_race'))
radar_data_2$TS_scaled <- radar_data_2$TS/max(radar_data_2$TS)
radar_data_2$TUS_scaled <- radar_data_2$TUS/max(radar_data_2$TUS)

# Create the radar chart
plot_ly(radar_data_2, type = 'scatterpolar', mode = 'lines',
        line = list(width = 1.5, shape = 'spline')) %>%
  add_trace(r = radar_data_2$TS_scaled, theta = radar_data_2$Category,
            name = 'Total Sheltered', fill = 'toself', subplot = 'polar1') %>%
  add_trace(r = radar_data_2$TUS_scaled, theta = radar_data_2$Category,
            name = 'Total Unsheltered', fill = 'toself', subplot = 'polar2') %>%
  layout(
    polar = list(subplot = list(list(polar = list(radialaxis = list(visible = TRUE),
                                                  angularaxis = list(visible = FALSE))),
                                list(polar = list(radialaxis = list(visible = TRUE),
                                                  angularaxis = list(visible = FALSE))))),
    showlegend = TRUE,
    margin = list(l = 50, r = 50, t = 50, b = 50),
    title = list(text = 'Radar Chart of Race Distribution by Shelter Status')
  )


# Set maximum value for the axes
max_value <- 160000

# Create a list of colors for the lines
colors <- c('#FF0000', '#00FF00')

# Create a list of legends
legends <- list('TS' = colors[1], 'TUS' = colors[2])

# Create the radar chart
radarchart(
  data = radar_data_2[, -1],
  axistype = 1,
  maxmin = TRUE,
  pcol = colors,
  pfcol = c('#FFCCCC', '#CCFFCC'),
  plwd = 4,
  centerzero = TRUE,
  title = 'Race Distribution by Shelter Status',
  vlabels = radar_data$Category,
  seg = 8,
  cglcol = 'grey',
  cglty = 1,
  cglwd = 0.8,
  axislabcol = 'black',
  axislabcol.x = 'black',
  axislabcol.y = 'black',
  caxislabels = seq(0, max_value, 2),
  calcex = 1.1,
  legend = legends
)



