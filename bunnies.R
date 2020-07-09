
library(tidyverse)
library(lubridate)
library(ggthemes)


### Copied and modified from EDI:

# Package ID: knb-lter-jrn.210086006.83 Cataloging System:https://pasta.edirepository.org.
# Data set title: Rabbit survey data on creosotebush and grassland routes from the long-term Small Mammal Exclusion Study at Jornada Basin LTER, 1996-ongoing.
# Data set creator:  Brandon Bestelmeyer - USDA-ARS Jornada Experimental Range 
# Data set creator:  David Lightfoot - University of New Mexico 
# Data set creator:  Robert Schooley - University of Illinois 
# Contact:  Robert Schooley -  University of Illinois  - schooley@illinois.edu
# Contact:  Data Manager -  Jornada Basin LTER  - datamanager.jrn.lter@gmail.com
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@Virginia.edu 

infile1 <- trimws("http://pasta.lternet.edu/package/data/eml/knb-lter-jrn/210086006/83/95929035c0259ea562f4aab7dcc4c10f") 
bunnies <- read_csv(infile1,
                 col_types=list( 
                   col_datetime("%Y-%m-%d %H:%M:%S"), 
                   col_character(),  
                   col_character(), 
                   col_number(), 
                   col_number(),  
                   col_character(),  
                   col_character(),  
                   col_character(),  
                   col_character()), 
                 na=c(" ",".","NA")  ) %>%
  mutate(date = as.Date(date_time),
         year = year(date),
         month = month(date),
         quarter = factor(quarter(date),
                          levels = 1:4,
                          labels = paste0("Q",1:4))) %>%
  filter(!(is.na(date) & !(species %in% c("END","GATE","START"))))  ## needs to be verified by John A. 

## Find mean END mileage to fill missing ones
mean_end <- bunnies %>%
  filter(species == "END" & !is.na(mileage) & mileage > 1) %>%
  group_by(route) %>%
  summarise(mean_end = mean(mileage))

# Double-check if multiple dates per month?
bunnies %>%
  mutate(year = year(date),
         month = month(date)) %>%
  group_by(year, month, route) %>%
  distinct(date) %>%
  filter(n() > 1)
# No, so can use dates as unique sampling event identifier

## Find sampling event route ends
ends <- bunnies %>%
  filter(species == "END" & !is.na(mileage))  %>%
  group_by(date,route) %>%
  summarise(route_end = mileage) 

## Calculate the seasonal density
dens_bunnies <- bunnies %>%
  filter(species %in% c("LECA","SYAU")) %>% # only count species rows
  left_join(ends) %>%
  left_join(mean_end) %>%
  mutate(route_end = coalesce(route_end,mean_end)) %>% # fill missing ends
  group_by(date,year,quarter, route,species) %>%
  summarise(count = n(),  # Number of rabbits
            density = count/unique(route_end)) %>% # density of rabbits on route
  group_by(year, quarter, route, species) %>%
  summarise(mean_density = mean(density)) %>% # some cases of multiple samplings per quarter
  ungroup %>%
  complete(year, quarter, species, route, 
           fill = list(mean_density = 0)) %>% # zero observed rabbits
  filter(year %in% 1997:2018) %>% # only include complete years
  mutate(route = factor(route, 
                        levels = c(1,3), 
                        labels = c("Creosote", "Grassland")))
  
dens_bunnies %>%
  mutate(species_pretty = factor(species,
                          levels = c("LECA","SYAU"),
                          labels = c("Jackrabbit \n(Lepus californicus)",
                                     "Cottontail \n(Sylvilagus audubonii)"))) %>%
  ggplot(aes(year,mean_density, color = quarter)) +
  geom_line() +
  geom_point() +
  theme_few() +
  facet_grid(route ~ species_pretty) +
  ylab("Rabbit density (# rabbits/mile)") +
  xlab("Year") +
  theme(legend.position = c(0.1,0.85)) +
  scale_color_discrete(name= NULL)



