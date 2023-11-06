

shush <- read.csv('shush.csv')

library(reshape2)
shush_long <- reshape2::melt(shush, id.vars = "X")

# create a radar chart
plot_ly(shush_long, type = "scatterpolar",
        mode = "markers+lines",
        line = list(shape = "spline"),
        marker = list(symbol = "square", size = 10),
        theta = ~variable,
        r = ~value,
        color = ~X,
        colors = c("blue", "red"),
        hoverinfo = "text",
        text = ~paste0(X, ": ", value)) %>%
  layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, max(shush_long$value) + 100))),
         showlegend = TRUE)

# create a new data frame with the required format
radar_data <- data.frame(
  Year = c(2018, 2019, 2020, 2021, 2022),
  Sheltered = c(358363, 356422, 354386, 326126, 348630),
  Unsheltered = c(194467, 211293, 226080, 54504, 233832)
)

# calculate the maximum value to use for the radial axis range
max_val <- max(radar_data$Sheltered, radar_data$Unsheltered)

# create a radar chart with bars for sheltered and unsheltered values
plot_ly(radar_data, type = "scatterpolar", mode = "markers+text",
        theta = ~Year,
        text = ~Year,
        hoverinfo = "text",
        marker = list(symbol = "square", size = 10),
        textfont = list(size = 18),
        textposition = "bottom center") %>%
  add_trace(r = ~Sheltered, name = "Sheltered",
            hoverinfo = "text",
            text = ~paste0("Sheltered: ", Sheltered)) %>%
  add_trace(r = ~Unsheltered, name = "Unsheltered",
            hoverinfo = "text",
            text = ~paste0("Unsheltered: ", Unsheltered)) %>%
  layout(polar = list(radialaxis = list(range = c(0, max_val))),
         showlegend = TRUE)

library(plotly)

library(tidyverse)
library(plotly)

# Creating the dataframe
df <- data.frame(
  X = c("Sheltered", "Unsheltered"),
  `2018` = c(358363, 194467),
  `2019` = c(356422, 211293),
  `2020` = c(354386, 226080),
  `2021` = c(326126, 54504),
  `2022` = c(348630, 233832)
)

# Reshape the data
df_long <- gather(df, key = "Year", value = "Value", -X)

# Create a radar chart
plot_ly(df_long, type = "scatterpolar", mode = "none") %>%
  add_trace(
    data = subset(df_long, X == "Sheltered"),
    r = ~Value,
    theta = ~Year,
    name = "Sheltered",
    hoverinfo = "text",
    text = ~paste("Year: ", Year, "<br>", "Value: ", Value),
    fill = "tozeroy",
    line = list(color = "blue"),
    showlegend = TRUE
  ) %>%
  add_trace(
    data = subset(df_long, X == "Unsheltered"),
    r = ~Value,
    theta = ~Year,
    name = "Unsheltered",
    hoverinfo = "text",
    text = ~paste("Year: ", Year, "<br>", "Value: ", Value),
    fill = "tozeroy",
    line = list(color = "red"),
    showlegend = TRUE
  ) %>%
  layout(
    polar = list(radialaxis = list(visible = TRUE, range = c(0, max(df_long$Value)))),
    showlegend = TRUE,
    xaxis = list(tickangle = -45, tickfont = list(size = 12))
  )
df_long <- tidyr::pivot_longer(df, cols = -X, names_to = "Year", values_to = "Value")

# Create a radar chart with bars
plot_ly(df_long, type = "scatterpolar", mode = "none") %>%
  add_bars(
    data = subset(df_long, X == "Sheltered"),
    x = ~Year,
    y = ~Value,
    name = "Sheltered",
    hoverinfo = "text",
    text = ~paste("Year: ", Year, "<br>", "Value: ", Value),
    marker = list(color = "blue"),
    showlegend = TRUE
  ) %>%
  add_bars(
    data = subset(df_long, X == "Unsheltered"),
    x = ~Year,
    y = ~Value,
    name = "Unsheltered",
    hoverinfo = "text",
    text = ~paste("Year: ", Year, "<br>", "Value: ", Value),
    marker = list(color = "red"),
    showlegend = TRUE
  ) %>%
  layout(
    polar = list(radialaxis = list(visible = TRUE, range = c(0, max(df_long$Value)))),
    showlegend = TRUE,
    xaxis = list(tickangle = -45, tickfont = list(size = 12))
  )

plot_ly(df_long, type = "barpolar", theta = ~Year, r = ~Value, color = ~X) %>%
  layout(
    polar = list(radialaxis = list(visible = TRUE, range = c(0, max(df_long$Value)))),
    showlegend = TRUE,
    xaxis = list(tickangle = -45, tickfont = list(size = 12))
  )

# Create a radar chart
plot_ly(df_long, type = "scatterpolar", mode = "none") %>%
  add_bars(
    data = subset(df_long, X == "Sheltered"),
    r = ~Value,
    theta = ~Year,
    name = "Sheltered",
    hoverinfo = "text",
    text = ~paste("Year: ", Year, "<br>", "Value: ", Value),
    marker = list(color = "blue"),
    showlegend = TRUE,
    opacity = 0.8,
    width = 0.2,
    offset = -0.1,
    position = "dodge"
  ) %>%
  add_bars(
    data = subset(df_long, X == "Unsheltered"),
    r = ~Value,
    theta = ~Year,
    name = "Unsheltered",
    hoverinfo = "text",
    text = ~paste("Year: ", Year, "<br>", "Value: ", Value),
    marker = list(color = "red"),
    showlegend = TRUE,
    opacity = 0.8,
    width = 0.2,
    offset = 0.1,
    position = "dodge"
  ) %>%
  layout(
    polar = list(radialaxis = list(visible = TRUE, range = c(0, max(df_long$Value)))),
    showlegend = TRUE,
    xaxis = list(tickangle = -45, tickfont = list(size = 12))
  )

# Create a radar chart with bars
plot_ly(df_long, type = "scatterpolar", mode = "none") %>%
  add_trace(
    data = subset(df_long, X == "Sheltered"),
    r = ~Value,
    theta = ~Year,
    name = "Sheltered",
    hoverinfo = "text",
    text = ~paste("Year: ", Year, "<br>", "Value: ", Value),
    fill = "tozeroy",
    line = list(color = "blue"),
    showlegend = TRUE
  ) %>%
  add_trace(
    data = subset(df_long, X == "Unsheltered"),
    r = ~Value,
    theta = ~Year,
    name = "Unsheltered",
    hoverinfo = "text",
    text = ~paste("Year: ", Year, "<br>", "Value: ", Value),
    fill = "tozeroy",
    line = list(color = "red"),
    showlegend = TRUE
  ) %>%
  add_bars(
    data = subset(df_long, X %in% c("Sheltered", "Unsheltered")),
    x = ~Year,
    y = ~Value,
    name = ~X,
    hoverinfo = "text",
    text = ~paste("Year: ", Year, "<br>", "Value: ", Value),
    marker = list(color = c("blue", "red")),
    offset = 0.1,
    width = 0.2,
    showlegend = FALSE
  ) %>%
  layout(
    polar = list(radialaxis = list(visible = TRUE, range = c(0, max(df_long$Value)))),
    showlegend = TRUE,
    xaxis = list(tickangle = -45, tickfont = list(size = 12))
  )

plot_ly(df_long, type = "barpolar", theta = ~Year, r = ~Value, color = ~X, 
        width = 800, height = 500, barmode = "overlay") %>%
  layout(
    polar = list(radialaxis = list(visible = TRUE, range = c(0, max(df_long$Value)))),
    showlegend = TRUE,
    xaxis = list(tickangle = -45, tickfont = list(size = 12))
  )

plot_ly(df_long, type = "barpolar", theta = ~Year, r = ~Value, color = ~X, width = 800, height = 500) %>%
  layout(
    polar = list(radialaxis = list(visible = TRUE, range = c(0, max(df_long$Value)))),
    showlegend = TRUE,
    xaxis = list(tickangle = -45, tickfont = list(size = 12)),
    bargap = 0.2
  )


df_modified <- data.frame(
  X = c("Kinds"),
  `2018S` = c(358363),
  `2018U` = c(194467),
  `2019S` = c(356422),
  `2019U` = c(211293),
  `2020S` = c(354386),
  `2020U` = c(226080),
  `2021S` = c(326126),
  `2021U` = c(54504),
  `2022S` = c(348630),
  `2022U` = c(233832)
)

df_long_mod <- tidyr::pivot_longer(df_modified, cols = -X, names_to = "Year", values_to = "Value")

plot_ly(df_long_mod, type = "barpolar", theta = ~Year, r = ~Value, color = ~X, 
        width = 800, height = 500, barmode = "overlay") %>%
  layout(
    polar = list(radialaxis = list(visible = TRUE, range = c(0, max(df_long_mod$Value)))),
    showlegend = TRUE,
    xaxis = list(tickangle = -45, tickfont = list(size = 12))
  )

plot_ly(df_long_mod, type = "barpolar", theta = ~Year, r = ~Value, color = ~X,
        width = 800, height = 500, barmode = "overlay",
        marker = list(color = ifelse(df_long_mod$Value < 300000, "purple", "blue"))) %>%
  layout(
    polar = list(radialaxis = list(visible = TRUE, range = c(0, max(df_long_mod$Value)))),
    showlegend = TRUE,
    xaxis = list(tickangle = -45, tickfont = list(size = 12))
  )

plot_ly(df_long_mod, type = "barpolar", theta = ~Year, r = ~Value, color = ~X, 
        width = 800, height = 500, barmode = "overlay",
        marker = list(color = ifelse(df_long_mod$Value < 300000, "purple", "blue"))) %>%
  layout(
    polar = list(radialaxis = list(visible = TRUE, range = c(0, max(df_long_mod$Value)))),
    showlegend = FALSE,
    xaxis = list(tickangle = -45, tickfont = list(size = 12)),
    paper_bgcolor = "black",
    plot_bgcolor = "black",
    font = list(color = "white"),
    polar = list(
      angularaxis = list(
        linecolor = "white",
        tickcolor = "white",
        showline = TRUE,
        ticks = ""
      ),
      radialaxis = list(
        linecolor = "white",
        gridcolor = "white",
        tickfont = list(color = "white"),
        tickmode = "array",
        tickvals = seq(0, max(df_long_mod$Value), by = 200000),
        ticks = "outside"
      )
    ),
    barmode = "overlay",
    colorway = c("purple", "blue"),
    colors = ifelse(df_long_mod$Value < 300000, "purple", "blue")
  )

plot_ly(df_long_mod, type = "barpolar", theta = ~Year, r = ~Value, color = ~X, 
        width = 800, height = 500, barmode = "overlay",
        marker = list(color = ifelse(df_long_mod$Value < 300000, "purple", "#FA4616"))) %>%
  layout(
    polar = list(
      bgcolor = "black",
      angularaxis = list(
        linecolor = "white",
        tickcolor = "white",
        showline = TRUE,
        ticks = ""
      ),
      radialaxis = list(
        visible = TRUE,
        range = c(0, max(df_long_mod$Value)),
        gridcolor = "white",
        linecolor = "transparent",
        tickfont = list(color = "transparent"),
        tickmode = "array",
        tickvals = seq(0, max(df_long_mod$Value), by = 233832),
        ticks = "outside"
      )
    ),
    showlegend = FALSE,
    xaxis = list(tickangle = -45, tickfont = list(size = 12)),
    paper_bgcolor = "black",
    plot_bgcolor = "black",
    font = list(color = "transparent"),
    barmode = "overlay",
    colorway = c("purple", "#FA4616"),
    colors = ifelse(df_long_mod$Value < 300000, "purple", "#FA4616")
  )




    
    