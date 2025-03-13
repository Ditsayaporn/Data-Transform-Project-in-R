install.packages("nycflights23")
library(nycflights23)

data("flights")
data("airlines")

# view table
View(flights) 
View(airlines)

# view data frame structure
glimpse(flights)
glimpse(airlines)

# view column name
names(flights)
names(airlines)

# result column name
> names(flights)
 [1] "year"           "month"          "day"            "dep_time"      
 [5] "sched_dep_time" "dep_delay"      "arr_time"       "sched_arr_time"
 [9] "arr_delay"      "carrier"        "flight"         "tailnum"       
[13] "origin"         "dest"           "air_time"       "distance"      
[17] "hour"           "minute"         "time_hour"     
> names(airlines)
[1] "carrier" "name"

# 1. top five carriers in April 2023
top_5_airline_apr23 <- flights %>%
  filter(month == 4) %>%
  count(carrier) %>%
  arrange(-n) %>%
  head(5) %>%
  left_join(airlines,by = "carrier") %>%
  select(name, everything())

# 2. Top 5 destination in Y2023
top_5_destination <- flights %>%
  count(dest) %>%
  arrange(-n) %>%
  head(5)

# 3. Top 5 arrive delay airline 
top_5_arr_delay_airline <- flights %>% 
  mutate(arr_delay_status = case_when(
    arr_delay > 0 ~ "Delay",
    arr_delay == 0 ~ "On Time",
    arr_delay < 0 ~ "Early",
    is.na(arr_delay) ~ "Unknown"
  )) %>%
  filter(arr_delay_status == "Delay") %>%
  group_by(carrier) %>%
  summarise(delay_count = n()) %>%
  arrange(desc(delay_count)) %>%
  head(5) %>%
  left_join(airlines,by = "carrier") %>%
  select(name, everything())

# 4. Top 5 airlines with the most airplane
total_airplanes_airline <- flights %>%
  filter(!is.na(tailnum)) %>%
  group_by(carrier) %>%
  summarise(unique_tailnum_count = n_distinct(tailnum), 
            .groups = "drop") %>%
  left_join(airlines, by = "carrier") %>%
  arrange(desc(unique_tailnum_count)) %>%
  head(5) %>%
  select(name, unique_tailnum_count)

# 5. Long distance by airline name by destination
long_dist_airline <- flights %>% 
  group_by(dest) %>%
  summarise(long_dist = max(distance)) %>%
  arrange(desc(long_dist)) %>%
  select(dest, long_dist) %>%
  left_join(flights, by = c("dest", "long_dist" = "distance")) %>%
  group_by(dest) %>%
  filter(row_number() == 1) %>%
  left_join(airlines, by = "carrier") %>%
  select(airline_name = name, dest, long_dist)
