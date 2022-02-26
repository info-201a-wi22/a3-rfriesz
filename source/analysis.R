data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
library("tidyr")
library("ggplot2")
library("dplyr")
library("plotly")
library("tidyverse")
library("maps")
install.packages("reshape2")
library("reshape2")
install.packages("usmap")
library("usmap")
View(data)

# State with the most incarcerated black people in jail
highest_state_black_incarcerated <- data %>%
  select(state, black_jail_pop) %>%
  na.omit() %>%
  filter(black_jail_pop == max(black_jail_pop)) %>%
  pull(state)

# Percent of total Latino population in Jail in 2018
total_percent_latino_population_jail <- data %>%
  select(year, latinx_jail_pop, latinx_pop_15to64) %>%
  na.omit() %>%
  filter (year == 2018)
total_jail_latinx <- sum(total_percent_latino_population_jail$latinx_jail_pop)
total_pop_latinx <- sum(total_percent_latino_population_jail$latinx_pop_15to64)
total_percent_latino_population_jail <- total_jail_latinx / total_pop_latinx

# Percent of total Black population in Jail in 2018
total_percent_black_population_jail <- data %>%
  select(year, black_jail_pop, black_pop_15to64) %>%
  na.omit() %>%
  filter (year == 2018)
total_jail_black <- sum(total_percent_black_population_jail$black_jail_pop)
total_pop_black <- sum(total_percent_black_population_jail$black_pop_15to64)
total_percent_black_population_jail <- total_jail_black / total_pop_black

# Percent of total White population in Jail in 2018
total_percent_white_population_jail <- data %>%
  select(year, white_jail_pop, white_pop_15to64) %>%
  na.omit() %>%
  filter (year == 2018)
total_jail_white <- sum(total_percent_white_population_jail$white_jail_pop)
total_pop_white <- sum(total_percent_white_population_jail$white_pop_15to64)
total_percent_white_population_jail <- total_jail_white / total_pop_white

# The urbanicity with the highest total jail population
urbanicity_highest_incarceration <- data %>%
  select(urbanicity, total_jail_pop) %>%
  na.omit() %>%
  filter(total_jail_pop == max(total_jail_pop)) %>%
  pull(urbanicity)

# Trend over time chart: the white population in jail from 2001 to 2010
white_population_jail_time <- data %>%
  select(year, white_jail_pop) %>%
  filter(year >= 2001, year <= 2010) %>%
  na.omit()%>%
  group_by(year) %>%
  summarize(total_white_jail_pop = sum(white_jail_pop)) 
chart_1 <- ggplot(white_population_jail_time, aes(x=year, y = total_white_jail_pop, fill="national total" )) +
    geom_col(width = 0.5, position = position_dodge(0.7)) +
    labs(title = "National number of white people in jail (2001-2010)") +
    labs(y = "Number of people in jail")
    

# The amount of white vs black people in jail from 2000 to 2010
white_population <- data %>%
  select(year, white_jail_pop) %>%
  filter(year >= 2000, year <= 2010) %>%
  na.omit() %>%
  group_by(year) %>%
  summarize(total_white_jail_pop = sum(white_jail_pop))
black_population <- data %>%
  select(year, black_jail_pop) %>%
  filter(year >= 2000, year <= 2010) %>%
  na.omit() %>%
  group_by(year) %>%
  summarize(total_black_jail_pop = sum(black_jail_pop))
black_white_jail_population <- full_join(white_population, black_population, by="year")
black_white_jail_population_long <- melt(black_white_jail_population, id="year")
chart_2 <- ggplot(data=black_white_jail_population_long, aes(x=year, y=value, colour=variable)) +
  geom_line() +
  labs(y = "Number of people in jail") +
  labs(title = "National number of white and black people in jail (2000-2010") +
  labs(colour = "Different Race")

#The difference between the white and black incarceration rate for each state in 2012
percentage_black_incarcerated <- data %>%
  filter(year==2012) %>%
  select(state, year, black_pop_15to64, black_prison_pop) %>%
  na.omit() %>%
  group_by(state) %>%
  summarize(black_incarcerated_proportion = sum(black_prison_pop)/sum(black_pop_15to64))

percentage_white_incarcerated <- data %>%
  filter(year==2012) %>%
  select(state, year, white_pop_15to64, white_prison_pop) %>%
  na.omit() %>%
  group_by(state) %>%
  summarize(white_incarcerated_proportion = sum(white_prison_pop)/sum(white_pop_15to64))
comparison_incarceration_rate <- full_join(percentage_black_incarcerated, percentage_white_incarcerated, by="state") %>%
  mutate(difference = black_incarcerated_proportion / white_incarcerated_proportion)
map_1 <- plot_usmap(data = comparison_incarceration_rate, values = "difference", color = "grey") +
  scale_fill_continuous(
    low = "lightblue", high = "navy", name="How many times larger proportion of black people in jail (2012)", label = scales::comma) +
  theme(legend.position = "right") +
  labs(title = "White vs Black people difference in proportion of population in jail (2012)")
    
                          





  
  
  
  



  
  

  