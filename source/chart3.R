# Load packages
library(ggplot2)
library(dplyr)

# Load file
incarceration_trends <- read.csv(
  "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

# Create Map 
total_black_population <- incarceration_trends %>% 
  group_by(state) %>%  
  summarize(
    total_prison_pop = sum(total_prison_pop, na.rm = TRUE),
    black_prison_pop = sum(black_prison_pop, na.rm = TRUE)
  ) %>% 
  mutate(ratio = round((black_prison_pop / total_prison_pop) * 100, 2)) %>% 
  replace(is.na(.), 0) %>% 
  select(state, ratio) %>% 
  filter(state != "DC") 

state_name <- state.name[match(total_black_population$state, state.abb)]

total_black_population <- total_black_population %>% 
  mutate(region = tolower(state_name)) %>% 
  select(region, ratio)

state_shape <- map_data("state") 
ratio_map <- left_join(state_shape, total_black_population, by = "region")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

us_map <- ggplot(ratio_map) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = ratio),
    color = "white",
    size = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(fill = "Ratio in %") +
  ggtitle("Black Prison Population to Total Prison Population Ratio in the US") +
  blank_theme

print(us_map)




