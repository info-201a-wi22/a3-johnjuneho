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
black_total_pop <- incarceration_trends %>% 
  filter(year >= 1970 & year <= 2016) %>% 
  group_by(year) %>% 
  summarize(total = sum(black_prison_pop, na.rm = TRUE)) %>% 
  mutate(race = "Black")

white_total_pop <- incarceration_trends %>% 
  filter(year >= 1970 & year <= 2016) %>% 
  group_by(year) %>% 
  summarize(total = sum(white_prison_pop, na.rm = TRUE)) %>% 
  mutate(race = "White")

aapi_total_pop <- incarceration_trends %>%
  filter(year >= 1970 & year <= 2016) %>% 
  group_by(year) %>% 
  summarize(total = sum(aapi_prison_pop, na.rm = TRUE)) %>% 
  mutate(race = "AAPI")

latinx_total_pop <- incarceration_trends %>% 
  filter(year >= 1970 & year <= 2016) %>% 
  group_by(year) %>% 
  summarize(total = sum(latinx_prison_pop, na.rm = TRUE)) %>% 
  mutate(race = "Latinx") 

native_total_pop <- incarceration_trends %>% 
  filter(year >= 1970 & year <= 2016) %>% 
  group_by(year) %>% 
  summarize(total = sum(native_prison_pop, na.rm = TRUE)) %>% 
  mutate(race = "Native")

other_race_total_pop <- incarceration_trends %>% 
  filter(year >= 1970 & year <= 2016) %>% 
  group_by(year) %>% 
  summarize(total = sum(other_race_prison_pop, na.rm = TRUE)) %>% 
  mutate(race = "Others")

# Create chart 
chart1 <- ggplot(data = NULL, aes(x = year, y = total, color = factor(race))) +
  geom_line(data = black_total_pop) +
  geom_line(data = white_total_pop) +
  geom_line(data = aapi_total_pop) +
  geom_line(data = latinx_total_pop) + 
  geom_line(data = native_total_pop) +
  geom_line(data = other_race_total_pop) +
  scale_x_continuous(breaks = seq(1970, 2016, 5)) +
  ggtitle("Total Prison Population by Race in the US") +
  xlab("Year") +
  ylab("Total Prison Population") +
  scale_color_discrete(name = "Race")

print(chart1)



  