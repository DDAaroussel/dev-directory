library(nycflights13)
library(tidyverse)


# 1. Filter exercises -----------------------------------------------------
#1
filter(flights, arr_delay > 120 | arr_delay < 120)
#2
filter(flights, dest == 'IAH' | dest == 'HOU')
#3
# UA, AA, DL
filter(flights, carrier %in% c('UA', 'AA', 'DL'))
#4
filter(flights, month %in% c(7, 8, 9))
#5
filter(flights, arr_delay > 120 & dep_delay <= 0)
#6
filter(flights, dep_delay > 60 | arr_delay < 30)
#7
filter(flights, dep_time > 1 & dep_time < 600)
#8 
flights %>% filter(between(dep_time, 1, 600))
#9
flights %>% filter(is.na(flights$dep_time))


# 2. Arrange exercises ----------------------------------------------------
#1
flights %>% arrange(!is.na(dep_time), dep_time)
#2
flights %>% arrange(dep_delay)
#3
flights %>% arrange(air_time, desc(distance))
#4
flights %>% arrange(distance)
flights %>% arrange(desc(distance))


# Select exercises --------------------------------------------------------
#1 
vars <- c("dep_time", "dep_delay", "arr_time", "arr_delay")
select(flights, one_of(vars))
#2
select(flights, dep_time, dep_time)
#3
#selects a match against any of the strings within the string vector
#4
select(flights, contains("TIME"))


# Mutate exercises --------------------------------------------------------
#1
mutate(flights, 
       dep_time_int = as.numeric(dep_time),
       sched_dep_time_int = as.numeric(sched_dep_time))
#2
flights_v2 <- mutate(flights,
                     diff_air_time = arr_time - dep_time,
                     diff_delay = air_time - diff_air_time)
#3
top_10 <- flights %>% 
  mutate(ranked = min_rank((dep_delay))) %>%
  arrange(desc(ranked)) %>%
  head(10)
#4
#the shorter array is repeatedly adding itself along the longer array


# Summarise exercises -----------------------------------------------------
#1
#A flight is 15 minutes early 50% of the time, and 15 minutes late 50% of the time
flights %>%
  group_by(flight) %>%
  summarise(n = n(),
            fifteen_late = mean(arr_delay == 15, na.rm = TRUE),
            fifteen_early = mean(arr_delay == -15, na.rm = TRUE)) %>%
  filter(near(fifteen_late, 0.5) & near(fifteen_early, 0.5))
  
