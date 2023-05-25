library(tidyverse)

# Read in data
gapminder_data <- read_csv("data/gapminder_data.csv")

# What is the mean life expectancy?
# summarize()

summarize(gapminder_data, averageLifeExp = mean(lifeExp))

gapminder_data %>% 
  summarize(averageLifeExp = mean(lifeExp))

gap_minder_data_summarized <- gapminder_data %>% 
  summarize(averageLifeExp = mean(lifeExp))

# What is the mean population in the gapminder dataset?

gapminder_data_pop <- gapminder_data %>% 
  summarize(averagepop = mean(pop))

# What is the mean population AND the mean lifeExp?

gapminder_data %>% 
  summarize(averageLifeExp = mean(lifeExp),
            averagepop = mean(pop))

# What is the mean life expectancy for the most recent year?
# filter()
# max()

gapminder_data %>% 
  summarize(maxYear = max(year))

gapminder_data %>% 
  filter(year == 2007) %>% 
  summarize(meanLifeExp = mean(lifeExp))

gapminder_data %>% 
  filter(year == max(year)) %>% 
  summarize(meanLifeExp = mean(lifeExp))

# What is the mean GDP per capita for the first/earliest year?

gapminder_data %>% 
  filter(year == min(year)) %>% 
  summarize(meangdpPercap = mean(gdpPercap))

# > < !=

# What is the mean life expectancy for EACH year?
# group_by()

gapminder_data %>% 
  group_by(year) %>% 
  summarize(meanLifeExp = mean(lifeExp))

# What is the mean life expectancy for each continent?

gapminder_data %>% 
  group_by(continent) %>% 
  summarize(meanLifeExp = mean(lifeExp))

# What is the mean life expectancy AND mean GDP per capita for each continent in a single result tibble?

gapminder_data %>% 
  group_by(continent) %>% 
  summarize(meanLifeExp = mean(lifeExp),
            meangdpPercap = mean(gdpPercap))

# What is the GDP (not per capita)?
# mutate()

gapminder_data %>% 
  mutate(gdp = gdpPercap * pop)

# Make a new column for population in millions

gapminder_data_popmil <- gapminder_data %>% 
  mutate(popInMill = pop / 1000000,
         gdp = gdpPercap * pop)

# select(): chooses a subset of columns from a dataset

gapminder_data %>%
  select(year, pop)

# Select every column except for continent

gapminder_data %>% 
  select(-continent)

# Create a tibble with only country, continent, year, lifeExp

gapminder_data %>% 
  select(-pop, -gdpPercap)

gapminder_data %>% 
  select(country, continent, year, lifeExp) # may be useful if writing code that others will read

# Select helper function: starts_with(), ends_with(), contains()

gapminder_data %>%
  select(year, starts_with("c"))

gapminder_data %>% 
  select(contains("e"))

# pivot_wider()

gapminder_data %>% 
  select(country, continent, year, lifeExp) %>% 
  pivot_wider(
    names_from = year,
    values_from = lifeExp
  )

# Pivot wider, but populate values with GDP per capita

gapminder_data %>% 
  select(country, continent, year, gdpPercap) %>% 
  pivot_wider(
    names_from = year,
    values_from = gdpPercap
  )

# pivot_longer() -> condensing 3 columns into 1 column

gapminder_data %>% 
  pivot_longer(cols = c(pop, lifeExp, gdpPercap), 
               names_to = "measurement_type",
               values_to = "measureent")

# Is there a relationship between GDP and CO2 emissions?
# Prep Step

gapminder_data_2007 <- gapminder_data %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent)

# Read in CO2 data, use skip to get headings from second line, use col_names() to overwrite original column names

co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip = 2,
         col_names = c("region", "country", "year", "series", "value", "footnotes", "source"))

# recode()

co2_emissions <- co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(
    names_from = "series",
    values_from = "value") %>% 
  filter(year == 2005) %>% 
  select(-year)


# inner_join: join two data sets, removes any countries that are not in both data sets

inner_join(gapminder_data_2007, co2_emissions, by = "country")


# Vectors: list of values that are all the same type (character, numbers, etc)
# c()

my_vec <- c("panda", "dog", "cat", "horse")
num_vec <- c(1, 2, 3, 4)

proof <- gapminder_data %>% 
  pull(year)

# example - filter for identifiers in data set
your_data %>% 
  filter(id == "id_of_interst1" & id == "id_of_interest2")

your_data %>% 
  filter(id %in% c("id1", "id2", "id3"))

