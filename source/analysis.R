## 1. Summary Analysis 

# Load packages 
library(dplyr)
library(tidyverse)

# Load file  
incarceration_trends <- read.csv(
  "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

# Which county in what state had the greatest difference between black jail population and white jail population 
# in the most recent year (End of June)? 

greatest_difference_year <- incarceration_trends %>% 
  filter(year == max(incarceration_trends$year, na.rm = TRUE)) %>% 
  select(year, white_jail_pop, black_jail_pop, state, county_name) %>% 
  mutate(county_state = paste0(county_name, ", ", state)) %>% 
  na.omit(white_jail_pop, black_jail_pop) %>% 
  mutate(difference = black_jail_pop - white_jail_pop) %>% 
  arrange(-difference) %>% 
  filter(row_number() == 1) %>% 
  pull(county_state)

# Cook County, IL 

# What was the greatest average prison population between 1988 and 2016 for each race? (December, 31)?

greatest_prison_race <- incarceration_trends %>%
  group_by(year) %>% 
  summarize(
    black_prison_pop = sum(black_prison_pop, na.rm = TRUE),
    white_prison_pop = sum(white_prison_pop, na.rm = TRUE),
    aapi_prison_pop = sum(aapi_prison_pop, na.rm = TRUE),
    latinx_prison_pop = sum(latinx_prison_pop, na.rm = TRUE),
    native_prison_pop = sum(native_prison_pop, na.rm = TRUE),
    other_race_prison_pop = sum(other_race_prison_pop, na.rm = TRUE)
  ) %>% 
  filter(year >= 1988 & year <= 2016) %>% 
  summarize(
    black_prison_pop = round(mean(black_prison_pop), 2),
    white_prison_pop = round(mean(white_prison_pop), 2),
    aapi_prison_pop = round(mean(aapi_prison_pop), 2),
    latinx_prison_pop = round(mean(latinx_prison_pop), 2),
    native_prison_pop = round(mean(native_prison_pop), 2),
    other_race_prison_pop = round(mean(other_race_prison_pop), 2)
  )

black_greatest_prison_average <- pull(greatest_prison_race, black_prison_pop) # 391925.03
white_greatest_prison_average <- pull(greatest_prison_race, white_prison_pop) # 319819.79
aapi_greatest_prison_average <- pull(greatest_prison_race, aapi_prison_pop) # 3794.34
latinx_greatest_prison_average <- pull(greatest_prison_race, latinx_prison_pop) # 133105
native_greatest_prison_average <- pull(greatest_prison_race, native_prison_pop) # 7534.21
other_race_greatest_prison_average <- pull(greatest_prison_race, other_race_prison_pop) # 9709.83

# Which year (End of June) between 1985 and 2018 had the greatest increase in daily average black jail population?

greatest_increase_black_prison <- incarceration_trends %>%
  group_by(year) %>% 
  summarize(black_jail_pop = sum(black_jail_pop, na.rm = TRUE)) %>% 
  filter(year >= 1985)

greatest_increase_black_prison <- greatest_increase_black_prison %>% 
  mutate(shift = lag(greatest_increase_black_prison$black_jail_pop, default = NA)) %>% 
  mutate(increase_black_jail_pop = black_jail_pop - shift) %>% 
  select(-shift) %>% 
  arrange(-increase_black_jail_pop) %>% 
  filter(row_number() == 1) %>% 
  pull(year)

# 1988 

# What was the greatest increase in prison population from 1987 to 1988 (War on Drugs) between races?

greatest_prison_increase <- incarceration_trends %>% 
  filter(year >= 1987 & year <= 1988) %>% 
  group_by(year) %>% 
  summarize(
    black_prison_pop = sum(black_prison_pop, na.rm = TRUE),
    white_prison_pop = sum(white_prison_pop, na.rm = TRUE),
    aapi_prison_pop = sum(aapi_prison_pop, na.rm = TRUE),
    latinx_prison_pop = sum(latinx_prison_pop, na.rm = TRUE),
    native_prison_pop = sum(native_prison_pop, na.rm = TRUE),
    other_race_prison_pop = sum(other_race_prison_pop, na.rm = TRUE)
  ) 

greatest_prison_increase <- greatest_prison_increase %>% 
  mutate(
    shift_black = lag(greatest_prison_increase$black_prison_pop),
    shift_white = lag(greatest_prison_increase$white_prison_pop),
    shift_aapi = lag(greatest_prison_increase$aapi_prison_pop),
    shift_latinx = lag(greatest_prison_increase$latinx_prison_pop),
    shift_native = lag(greatest_prison_increase$native_prison_pop),
    shift_other = lag(greatest_prison_increase$other_race_prison_pop),
  ) %>% 
  mutate(
    increase_black = black_prison_pop - shift_black,
    increase_white = white_prison_pop - shift_white,
    increase_aapi = aapi_prison_pop - shift_aapi,
    increase_latinx = latinx_prison_pop - shift_latinx,
    increase_native = native_prison_pop - shift_native,
    increase_other = other_race_prison_pop - shift_other
  ) %>% 
  select(
    -shift_black,
    -shift_white,
    -shift_aapi,
    -shift_latinx,
    -shift_native,
    -shift_other
  )

greatest_prison_increase <- greatest_prison_increase %>% 
  filter(year == 1988) %>% 
  pull(increase_black)

# 82090 (Black)

# In the United States, what was the ratio between black prison population to total prison population in 1988?

black_prison_total_ratio <- incarceration_trends %>% 
  filter(year == 1988) %>% 
  group_by(year) %>% 
  summarize(
    total_prison_pop = sum(total_prison_pop, na.rm = TRUE), 
    black_prison_pop = sum(black_prison_pop, na.rm = TRUE)
  ) %>% 
  mutate(ratio = (black_prison_pop / total_prison_pop) * 100) %>% 
  mutate(ratio = round(ratio, 2)) %>% 
  pull(ratio)

# 43.21% 

## 2. Trend over time chart 

# Load packages
library(ggplot2)

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

## 3. Variable Comparison Chart 

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

## 4. Map 

# Manipulate dataframe 
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

# Create Map 
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









