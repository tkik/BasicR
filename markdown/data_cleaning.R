## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(tidyverse)
library(nycflights13)
library(magrittr)

## -----------------------------------------------------------------------------




## ---- echo = FALSE, out.width = "70%", eval=TRUE------------------------------
knitr::include_graphics("https://hbctraining.github.io/Intro-to-R/img/tidyverse_website.png?raw=true", dpi = 100)

## -----------------------------------------------------------------------------

## A single command
sqrt(83)

## Base R method of running more than one command
round(sqrt(83), digit = 2)

sqrt(83) %>% round(digit = 2)


## -----------------------------------------------------------------------------

flights %>%
  select(year, month, day) %>%
  head()


flights %>%
  select(year:dep_time, ends_with("time")) %>%
  head()



flights %>%
  select(year:dep_time, ends_with("time")) %>%
  arrange(desc(air_time)) %>% 
  head()




## -----------------------------------------------------------------------------

flights_sml <- flights %>% 
  head(n=100)

flights_sml %>% 
  filter(month==3 & year==2013)

flights_sml %>% 
  select(year:dep_time) %>% 
  mutate(date=paste(year, month, day))

try(flights_sml %>% 
  select(year:dep_time) %>% 
  mutate(speed=distance/air_time *60))


flights_sml %>% 
  select(distance, air_time) %>% 
  mutate(speed=distance/air_time *60)



## -----------------------------------------------------------------------------

flights %>% 
  select(year:arr_delay) %>%
  group_by(month) %>% 
  summarise(mean_delay=mean(arr_delay, na.rm=T), sd=sd(arr_delay, na.rm=T))

flights %>% 
  select(year:dest) %>%
  group_by(month, dest) %>% 
  summarise(mean_delay=mean(arr_delay, na.rm=T), sd=sd(arr_delay, na.rm=T), count=n())




## ---- echo = FALSE, out.width = "70%", eval=TRUE------------------------------
knitr::include_graphics("https://swcarpentry.github.io/r-novice-gapminder/fig/14-tidyr-fig1.png?raw=true", dpi = 100)
knitr::include_graphics("https://swcarpentry.github.io/r-novice-gapminder/fig/14-tidyr-fig3.png?raw=true", dpi = 100)


## -----------------------------------------------------------------------------
library(gapminder)



## -----------------------------------------------------------------------------
gap_wide <- gapminder %>% 
  pivot_wider(names_from = year, values_from = c(pop, gdpPercap,lifeExp ))

gap_long <- gap_wide %>%
  pivot_longer(
    cols = c(-continent, -country),
    names_to = "obstype_year", values_to = "obs_values"
  ) %>%
  separate(obstype_year, into = c('obs_type', 'year'), sep = "_")
  

## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------

library(readr)



