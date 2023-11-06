homeless <- data.frame(
  Year = c(2018, 2019, 2020, 2021, 2022),
  Families = c(180413, 171670, 171575, 137509, 161070),
  Chronically = c(88640, 96141, 110528, 56101, 13861)
)


library(viridis)

ggplot(homeless, aes(x = Year, y = 1, size = Families, fill = Families)) +
  geom_point(alpha = 0.7, shape = 22) +
  geom_text(aes(label = Year), size = 3, fontface = "bold", color='white') +
  scale_size_continuous(range = c(10, 22), breaks = c(2, 4, 6, 8, 10)) +
  scale_fill_viridis(option = "D", direction = -1) + # Use Viridis color scale
  labs(title = "Number of homeless people in families throughout years",
       y = "",
       x = "",
       size = "Scale",
       fill = "Scale") +
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
        legend.position = c(0.5, 0.5),
        text = element_text(color = "white")) +
  coord_flip() +
  guides(size = guide_legend(override.aes = list(size = c(2, 4, 6, 8, 10))),
         fill = guide_legend(override.aes = list(size = 4, alpha = 0.7))) +
  theme(legend.position = c(0.65, 0.5),
        legend.direction = "vertical",
        legend.box = "horizontal")


ggplot(homeless, aes(x = Year, y = 1, size = Families, fill = Families)) +
  geom_point(alpha = 0.7, shape = 22) +
  geom_rect(aes(xmin = Year - 0.45, xmax = Year + 0.45, ymin = -Inf, ymax = Inf, fill = "white"), 
            data = homeless %>% select(Year, Chronically) %>% mutate(width = Families/10), 
            inherit.aes = FALSE, alpha = 0.7) +
  geom_text(aes(label = Year), size = 3, fontface = "bold", color='white') +
  scale_size_continuous(range = c(10, 22), breaks = c(2, 4, 6, 8, 10)) +
  scale_fill_viridis(option = "D", direction = -1) + # Use Viridis color scale
  labs(title = "Number of homeless people in families throughout years",
       y = "",
       x = "",
       size = "Scale",
       fill = "Scale") +
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
        legend.position = c(0.5, 0.5),
        text = element_text(color = "white")) +
  coord_flip() +
  guides(size = guide_legend(override.aes = list(size = c(2, 4, 6, 8, 10))),
         fill = guide_legend(override.aes = list(size = 4, alpha = 0.7))) +
  theme(legend.position = c(0.65, 0.5),
        legend.direction = "vertical",
        legend.box = "horizontal")

ggplot(homeless, aes(x = Year, y = 1)) +
  geom_tile(aes(width = Families/10, height = 1, fill = Families), alpha = 0.8) +
  geom_rect(data = homeless, aes(xmin = Year - 0.45, xmax = Year + 0.45, ymin = 0.5, ymax = 1.5, fill = "white", alpha = Chronically), color = "black") +
  geom_text(data = homeless, aes(x = Year, y = 0.5, label = Families), size = 4, fontface = "bold") +
  scale_fill_gradient(low = "white", high = "red") +
  scale_alpha_continuous(range = c(0, 1), guide = FALSE) +
  theme_minimal() +
  labs(title = "Number of homeless people in families throughout years",
       y = "",
       x = "",
       fill = "Scale",
       subtitle = "White rectangles represent chronically homeless",
       caption = "Data source: Department of Housing and Urban Development") +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(color = "black"))



ggplot(homeless, aes(x = Year, y = 1, size = Families, fill = Families)) +
  geom_point(alpha = 0.7, shape = 22) +
  geom_tile(aes(width = Families/10, height = 1, fill = as.factor(Families)), alpha = 0.8) +
  geom_tile(data = homeless %>% mutate(Chronically = -Chronically),
            aes(width = Chronically/200, height = 1, fill = "white"), alpha = 0.8) 
  scale_size_continuous(range = c(10, 22), breaks = c(2, 4, 6, 8, 10)) +
  scale_fill_viridis(option = "D", direction = -1) + # Use Viridis color scale
  labs(title = "Number of homeless people in families throughout years",
       y = "",
       x = "",
       size = "Scale",
       fill = "Scale") +
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
        legend.position = c(0.5, 0.5),
        text = element_text(color = "white")) +
  coord_flip() +
  guides(size = guide_legend(override.aes = list(size = c(2, 4, 6, 8, 10))),
         fill = guide_legend(override.aes = list(size = 4, alpha = 0.7))) +
  theme(legend.position = c(0.65, 0.5),
        legend.direction = "vertical",
        legend.box = "horizontal")


ggplot(homeless, aes(x = Year, y = 1)) +
  geom_tile(aes(width = Families/10, height = 1, fill = as.factor(Families)), alpha = 0.8) +
  geom_tile(data = homeless %>% mutate(Chronically = -Chronically),
            aes(width = Chronically/200, height = 1, fill = "white"), alpha = 0.8) +
  geom_rect(data = homeless, aes(xmin = Year-0.5, xmax = Year+0.5, ymin = -0.5, ymax = 0.5, fill = as.factor(Chronically)), alpha = 0.8) +
  geom_text(data = homeless, aes(x = Year, y = 0.5, label = Families), size = 4, fontface = "bold") +
  geom_text(data = homeless, aes(x = Year, y = 0.5, label = Chronically), size = 4, fontface = "bold", color = "black") +
  scale_fill_manual(values = c("white", viridis(5))) +
  labs(title = "Number of homeless people in families throughout years",
       y = "",
       x = "",
       fill = "Scale",
       subtitle = "White tiles represent chronically homeless",
       caption = "Data source: Department of Housing and Urban Development") +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(color = "black"))

ggplot(homeless, aes(x = Year, y = 1, size = Families, fill = Families)) +
  geom_point(alpha = 0.7, shape = 22) +
  geom_rect(data = homeless, aes(xmin = Year - 0.45, xmax = Year + 0.45, ymin = 0.6, ymax = 0.8, fill = "white"),
            show.legend = TRUE, alpha = 0.8) +
  geom_rect(data = homeless, aes(xmin = Year - 0.45, xmax = Year + 0.45, ymin = 0.6, ymax = 0.8, fill = Chronically),
            show.legend = TRUE, alpha = 0.8) +
  scale_size_continuous(range = c(10, 22), breaks = c(2, 4, 6, 8, 10)) +
  scale_fill_viridis(option = "D", direction = -1) + # Use Viridis color scale
  labs(title = "Number of homeless people in families throughout years",
       y = "",
       x = "",
       size = "Scale",
       fill = "Scale") +
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
        legend.position = c(0.5, 0.5),
        text = element_text(color = "white")) +
  coord_flip() +
  guides(size = guide_legend(override.aes = list(size = c(2, 4, 6, 8, 10))),
         fill = guide_legend(override.aes = list(size = 4, alpha = 0.7))) +
  theme(legend.position = c(0.65, 0.5),
        legend.direction = "vertical",
        legend.box = "horizontal")





