setwd("~/Desktop/carto")
pit_state <- read.csv('pit-state.csv')

kind_distribution <- pit_state[, c(1, 295, 296, 300, 301, 367, 381, 451, 465, 533, 547, 574, 575)]

names(kind_distribution)[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)] <- 
  c('State', 'SH_families', 'U_families', 'SH_chronically', 
                                      'U_chronically', 'SH_Veterans', 'U_Veterans', 
                                      'SH_unacompannied_youth', 'U_unacompannied_youth',
                                      'SH_parenting_youth','U_parenting_youth', 'SH_youth_kids', 'U_youth_kids')

kind_distribution$SH_families <- parse_number(kind_distribution$SH_families)
kind_distribution$U_families <- parse_number(kind_distribution$U_families)
kind_distribution$SH_chronically <- parse_number(kind_distribution$SH_chronically)
kind_distribution$U_chronically <- parse_number(kind_distribution$U_chronically)
kind_distribution$SH_Veterans <- parse_number(kind_distribution$SH_Veterans)
kind_distribution$U_Veterans <- parse_number(kind_distribution$U_Veterans)
kind_distribution$SH_unacompannied_youth <- parse_number(kind_distribution$SH_unacompannied_youth)
kind_distribution$U_unacompannied_youth <- parse_number(kind_distribution$U_unacompannied_youth)
kind_distribution$SH_parenting_youth <- parse_number(kind_distribution$SH_parenting_youth)
kind_distribution$U_parenting_youth <- parse_number(kind_distribution$U_parenting_youth)
kind_distribution$SH_youth_kids <- parse_number(kind_distribution$SH_youth_kids)
kind_distribution$U_youth_kids <- parse_number(kind_distribution$U_youth_kids)

total_kind_dist <- subset(kind_distribution, State == "Total")

total_kind_dist$SH_youth_with_kids <- total_kind_dist$SH_parenting_youth + total_kind_dist$SH_youth_kids
total_kind_dist$U_youth_with_kids <- total_kind_dist$U_parenting_youth + total_kind_dist$U_youth_kids


sheltered <- total_kind_dist[, c(2, 4, 6, 8, 14)]
unsheltered <- total_kind_dist[, c(3, 5, 7, 9, 15)]

sheltered$total <- sheltered$SH_families + sheltered$SH_chronically + sheltered$SH_Veterans + sheltered$SH_unacompannied_youth + sheltered$SH_youth_with_kids
sheltered$families <- sheltered$SH_families/sheltered$total
sheltered$chronically <- sheltered$SH_chronically/sheltered$total
sheltered$veterans <- sheltered$SH_Veterans/sheltered$total
sheltered$unacompannied_youth <- sheltered$SH_unacompannied_youth/sheltered$total
sheltered$youth_with_children <- sheltered$SH_youth_with_kids/sheltered$total

shel <- sheltered[, c(7:11)]

library(ggplot2)
library(ggforce)

# Create a data frame with your data
my_df <- data.frame(category = c("Families", "Chronically", "Veterans", "Unaccompanied youth", "Youth with children"),
                    ratio = c(0.2986963, 0.3725371, 0.1283717, 0.1122243, 0.08817065))

# Calculate the end and start angles for each category
my_df$end_angle <- cumsum(my_df$ratio) * 2*pi
my_df$start_angle <- c(0, head(my_df$end_angle, n = -1))

# Create the arc chart
ggplot(my_df, aes(x = 0, y = 0, fill = category)) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1, start = start_angle, end = end_angle), color = "white") +
  coord_polar(theta = "y") +
  scale_fill_viridis_d(direction = -1) +
  theme_void() +
  theme(legend.position = "bottom")

library(ggparliament)
library(tidyverse)

# Create the data frame to be used
my_df <- data.frame(category = c("Families", "Chronically", "Veterans", "Unaccompanied youth", "Youth with children"),
                    ratio = c(0.2986963, 0.3725371, 0.1283717, 0.1122243, 0.08817065))
ru_semicircle <- parliament_data(election_data = ru,
                                 type = "semicircle", # Parliament type
                                 parl_rows = 10,      # Number of rows of the parliament
                                 party_seats = ru$seats) # Seats per party

ggplot(my_df, aes(x = x, y = y, colour = party_short)) +
  geom_parliament_seats() + 
  theme_ggparliament() +
  labs(title = "Russia, 2016") +
  scale_colour_manual(values = ru_semicircle$colour, 
                      limits = ru_semicircle$party_short) 

library(ggalt)
# Create a pie chart
p <- ggplot(my_df, aes(x = "", y = ratio, fill = category)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar(theta = "y") +
  theme_void() + 
  theme(legend.position = "bottom")

# Customize the chart to look like a parliament chart
p + scale_fill_manual(values = c("#008080", "#FF7F50", "#6495ED", "#DC143C", "#FFD700"), 
                      name = NULL) + 
  annotate("text", x = 0.9, y = 0, label = "Democratic", angle = 0, size = 5, fontface = "bold") + 
  annotate("text", x = -0.9, y = 0, label = "Republican", angle = 0, size = 5, fontface = "bold") + 
  annotate("text", x = 0.35, y = 0, label = "House", angle = 0, size = 4, fontface = "bold") + 
  annotate("text", x = -0.35, y = 0, label = "Senate", angle = 0, size = 4, fontface = "bold")

library(ggforce)
ggplot(my_df, aes(fill = category, y = ratio, x = 1)) +
  geom_bar(aes(y = ratio, x = ifelse(category %in% c("Families", "Chronically"), -1, 1) * abs((row_number() - 3) / 2)^(1/2), width = abs((row_number() - 3) / 2)^(1/2)), stat = "identity", color = "black") +
  geom_hline(yintercept = 0, size = 0.5, color = "black") +
  scale_fill_viridis_d(direction = -1) +
  coord_flip() +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = NULL, title = "Hemicycle Parliament Chart")

parl_df <- data.frame(category = c("Families", "Chronically", "Veterans", "Unaccompanied youth", "Youth with children"),
                    seats = c(0.2986963, 0.3725371, 0.1283717, 0.1122243, 0.08817065))


house_of_reps <- data.frame(
  party = c("Families", "Chronically", "Veterans", "Unaccompanied youth", "Youth with children"),
  seats = c(130, 162, 56, 49, 38),
  color = c("yellow", "red", "green", "lightblue", 'blue')
) 
house_of_reps <- house_of_reps %>% 
  mutate(party = as.character(party), color = as.character(color)) %>%
  parliament_data(election_data = .,
                  parl_rows = 8,
                  party_seats = .$seats,
                  type = 'semicircle')
ggplot(data = house_of_reps) +
  geom_parliament_seats(aes(x = x,  y = y, color = party)) +
  theme_ggparliament() +
  scale_color_manual(values = house_of_reps$color, 
                     limits = house_of_reps$party)

library(viridis)

house_of_reps <- data.frame(
  party = c("Families", "Chronically", "Veterans", "Unaccompanied youth", "Youth with children"),
  seats = c(130, 162, 56, 49, 38),
  color = magma(5, begin = 0.3, end = 1)
) 

house_of_reps <- house_of_reps %>% 
  mutate(party = as.character(party), color = as.character(color)) %>%
  parliament_data(election_data = .,
                  parl_rows = 8,
                  party_seats = .$seats,
                  type = 'semicircle')

sheltered <- ggplot(data = house_of_reps) +
  geom_parliament_seats(aes(x = x,  y = y, color = party)) +
  scale_color_manual(values = house_of_reps$color, 
                     limits = house_of_reps$party) +
  theme_ggparliament() +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    axis.text = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "transparent"),
    axis.text.x = element_blank(), 
    axis.text.y = element_blank()
  )

ggsave("sheltered.png", plot = sheltered, device = "png", dpi = 300)




unsheltered$total <- unsheltered$U_families + unsheltered$U_chronically + unsheltered$U_Veterans + unsheltered$U_unacompannied_youth + unsheltered$U_youth_with_kids
unsheltered$families <- unsheltered$U_families/unsheltered$total
unsheltered$chronically <- unsheltered$U_chronically/unsheltered$total
unsheltered$veterans <- unsheltered$U_Veterans/unsheltered$total
unsheltered$unacompannied_youth <- unsheltered$U_unacompannied_youth/unsheltered$total
unsheltered$youth_with_children <- unsheltered$U_youth_with_kids/unsheltered$total

unshel <- unsheltered[, c(7:11)]
table(unshel)

house_of_reps_uns <- data.frame(
  party = c("Families", "Chronically Homeless", "Veterans", "Unaccompanied youth", "Youth with children"),
  seats = c(19, 311, 52, 49, 4),
  color = magma(5, begin = 0.3, end = 1)
) 

house_of_reps_uns <- house_of_reps_uns %>% 
  mutate(party = as.character(party), color = as.character(color)) %>%
  parliament_data(election_data = .,
                  parl_rows = 8,
                  party_seats = .$seats,
                  type = 'semicircle')

unsheltered <- ggplot(data = house_of_reps_uns) +
  geom_parliament_seats(aes(x = x,  y = y, color = party)) +
  scale_color_manual(values = house_of_reps_uns$color, 
                     limits = house_of_reps_uns$party) +
  theme_ggparliament() +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    axis.text = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "transparent"),
    axis.text.x = element_blank(), 
    axis.text.y = element_blank()
  )

ggsave("unsheltered.png", plot = unsheltered, device = "png", dpi = 300)

