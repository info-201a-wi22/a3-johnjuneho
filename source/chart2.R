# Load packages
library(ggplot2)
library(dplyr)

# Load file
incarceration_trends <- read.csv(
  "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

# Manipulate dataframe 
state_pop <- incarceration_trends %>% 
  filter(state == "GA") %>%
  select(year, total_prison_pop, black_prison_pop) %>% 
  group_by(year) %>% 
  summarize(
    total_prison_pop = sum(total_prison_pop, na.rm = TRUE),
    black_prison_pop = sum(black_prison_pop, na.rm = TRUE)
  ) %>% 
  filter(year < 2017) %>% 
  mutate(ratio = round((black_prison_pop / total_prison_pop), 2))


# Create chart 
chart2 <- ggplot(data = state_pop, aes(x = year, y = ratio)) +
  geom_line(color = "red") +
  xlab("Year") +
  ylab("Ratio") +
  scale_y_continuous(labels = scales::label_percent()) +
  ggtitle("Black Prison Population to Total Prison Population in Georgia")

print(chart2)
