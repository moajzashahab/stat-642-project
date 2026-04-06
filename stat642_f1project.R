# Stat 642
# Project Idea: F1 Gap to Q1 vs works/customer

## dataset: https://github.com/theOehrly/Fast-F1

setwd("~/OneDrive/Documents/Student/TAMU Grad School/2026 Spring/Stat 642/Project/f1_data/03_fastf1")

library(tidyverse)
library(lubridate)
library(ggplot2)

####
# Get Data In
####

q1 <- read.csv("f1_teams_table.csv")
# table that has mostly complete data
# lacks weather, circuit data

modeling <- read.csv("modeling_table.csv")
# table that has incomplete works & engine data
# but adds weather, throttle % data

circuits <- read.csv("all_f1_circuits.csv")
# table that has data on all circuits
# source: https://www.kaggle.com/datasets/kishan305/formula-1-circuits-1950-present

####
# EDA and clean on q1
####

# quick EDA
summary(q1)
head(q1)

# check of which races on q1 data
q1_event_summary <- q1 %>%
  count(Year, EventName)

# all the events
q1_event_summary %>% count(Year)

# per event
print(q1_event_summary)
#2024 has 20+ events?

q1 %>% filter(Year == 2024 & EventName == "Abu Dhabi Grand Prix")
# double Alpines

# remove duplicates
q1_unique <- q1 %>% 
  distinct(Year, EventName, DriverId, .keep_all = TRUE)

# check
q1_unique %>% filter(Year == 2024 & EventName == "Abu Dhabi Grand Prix")

# recheck of which races on q1 data
q1_event_summary <- q1_unique %>%
  count(Year, EventName)

# all the events
q1_event_summary %>% count(Year)

# per event
print(q1_event_summary)
# 2014 & 2016 are only years with more than 20 entries
# this tracks

## NAs

# looking for where there is NA in q1 data
q1_NA <- q1_unique %>% filter(is.na(Q1time))

q1_NA_event_summary <- q1_NA %>%
  count(Year, EventName)

print(q1_NA_event_summary) # 2020 sakhir grand prix
# note that this event did happen, so some error in data here

####
# EDA and clean on modeling
####

# quick EDA
summary(modeling)
head(modeling)

# check of which races on q1 data
modeling_event_summary <- q1 %>%
  count(Year, EventName)

print(modeling_event_summary)

modeling_event_summary %>% count(Year)

# looking for where there is NA in modeling data
modeling_NA <- modeling %>% filter(is.na(Q1_time))
# I forgot to look at summary data. No NAs present in Q1_time

# look for 2020 sakhir data
modeling_2020_sakhir <- modeling %>% filter(Year == 2020 & EventName == "Sakhir Grand Prix")
head(modeling_2020_sakhir)
# esta aqui!
# need plan to add later

# rainfall
table(modeling$Rainfall) # 4 blanks, 3396 total

modeling %>% filter(Rainfall == "")
# indicates rainfall by driver/session, not just session
# 2019 austraian vettel
# 2019 russian kvyat
# 2020 styrian raikkonen
# 2021 italian leclerc

modeling_rain <- modeling %>%
  filter(!is.na(Rainfall))

summary(modeling_rain) # 2018 onwards has rain (matches fast f1 years)

# Create a summary table of sessions with their conditions
session_conditions <- modeling %>%
  group_by(Year, EventName) %>%
  summarize(
    # Identify unique non-NA rainfall values in this session
    unique_rain = list(unique(na.omit(Rainfall))),
    
    # Apply logic to determine session condition
    SessionCondition = case_when(
      length(unique_rain[[1]]) == 0 ~ "no data",
      length(unique_rain[[1]]) > 1  ~ "mixed",
      unique_rain[[1]] == "True"    ~ "wet",
      unique_rain[[1]] == "False"   ~ "dry",
      TRUE                          ~ "error"
    ),
    .groups = 'drop'
  ) %>%
  select(-unique_rain) # Drop the helper list column

# Join this label back into your modeling or your final q1 table
modeling_SC <- modeling %>%
  left_join(session_conditions, by = c("Year", "EventName"))

# View the breakdown of conditions
table(session_conditions$SessionCondition)

summary(session_conditions)
head(session_conditions)

summary(modeling_SC)
table(modeling_SC$SessionCondition)
head(modeling_SC)

modeling_SC %>% filter(Rainfall == "")
# shows mixed conditions on all



####
# Clean q1 data (delete this as moved earlier)
####

# remove duplicates
q1_unique <- q1 %>% 
  distinct(Year, EventName, DriverId, .keep_all = TRUE)

# check
q1_unique %>% filter(Year == 2024 & EventName == "Abu Dhabi Grand Prix")

# drop rows with NA (only on q1 time)
# clean_q1 <- q1 %>% drop_na()
# this loses the 2020 sakhir gp data

# gap to fastest q1 time
# q1_updated <- clean_q1 %>%
#  group_by(Year, EventName) %>%
#  mutate(
#    FastestQ1 = min(Q1time, na.rm = TRUE),
#    GapToFastest = Q1time - FastestQ1
#  ) %>%
#  ungroup()
# doesn't work without dropping rows
# so do after join

####
# Clean modeling data (delete as moved earlier)
####
table(modeling$Rainfall) # 4 blanks, 3396 total

modeling %>% filter(Rainfall == "")
# indicates rainfall by driver/session, not just session
# 2019 austraian vettel
# 2019 russian kvyat
# 2020 styrian raikkonen
# 2021 italian leclerc

modeling_rain <- modeling %>%
  filter(!is.na(Rainfall))

summary(modeling_rain) # 2018 onwards has rain (matches fast f1 years)

# Create a summary table of sessions with their conditions
session_conditions <- modeling %>%
  group_by(Year, EventName) %>%
  summarize(
    # Identify unique non-NA rainfall values in this session
    unique_rain = list(unique(na.omit(Rainfall))),
    
    # Apply logic to determine session condition
    SessionCondition = case_when(
      length(unique_rain[[1]]) == 0 ~ "no data",
      length(unique_rain[[1]]) > 1  ~ "mixed",
      unique_rain[[1]] == "True"    ~ "wet",
      unique_rain[[1]] == "False"   ~ "dry",
      TRUE                          ~ "error"
    ),
    .groups = 'drop'
  ) %>%
  select(-unique_rain) # Drop the helper list column

# Join this label back into your modeling or your final q1 table
modeling_SC <- modeling %>%
  left_join(session_conditions, by = c("Year", "EventName"))

# View the breakdown of conditions
table(session_conditions$SessionCondition)

summary(session_conditions)
head(session_conditions)

summary(modeling_SC)
table(modeling_SC$SessionCondition)
head(modeling_SC)

modeling_SC %>% filter(Rainfall == "")




####
# Join modeling table to q1
####

# join
q1_w_modeling <- q1_unique %>%
  left_join(
    modeling_SC %>% select(Year, EventName, DriverId, Q1_time, Rainfall, FullThrottlePct, SessionCondition),
    by = c("Year", "EventName", "DriverId")
  ) %>%
  # Fill in the missing Sakhir 2020 times
  mutate(Q1time = coalesce(Q1time, Q1_time)) %>%
  select(-Q1_time)

# add gap to fastest q1 time
q1_final <- q1_w_modeling %>%
  filter(!is.na(Q1time)) %>%
  group_by(Year, EventName) %>%
  mutate(
    FastestQ1 = min(Q1time, na.rm = TRUE),
    GapToFastest = Q1time - FastestQ1
  ) %>%
  ungroup()

summary(q1_final)
head(q1_final)

q1_final_dup <- q1_final[duplicated(q1_final), ]
summary(q1_final_dup)
table(q1_final_dup$Year)

q1_final_u <- q1_final %>% 
  distinct(Year, EventName, DriverId, .keep_all = TRUE)

q1_fast <- q1_final_u %>% filter(Year > 2017)
table(q1_fast$SessionCondition)
q1_fast %>% filter(Rainfall == "")
head(q1_fast)

q1_fast[duplicated(q1_fast), ]
# no duplicates... because I wanted to be sure

# look at rows that have NA on FullThrottlePct
na_throttle_df <- q1_final_u %>% 
  filter(is.na(FullThrottlePct))

# See which years/events are missing this data
na_throttle_df %>%
  count(Year, EventName) %>%
  print(n = 79)

summary(na_throttle_df) # we have no throttle data between 2014 - 2017

summary(q1_final_u %>% filter(!is.na(FullThrottlePct)))
# we have throttle data between 2018-2025
# again, matches fast f1

summary(q1_final_u %>% filter(!is.na(SessionCondition)))

# so final data set:
summary(q1_final_u)

#but only 2018+ has rain/throttle



####
# EDA on circuits
####

summary(circuits)
head(circuits)
# note that Best.Lap.Timing and Best.Lap.Time are the same data
# the former is a string, the latter is a number

# convert first and last year running to a number
circuits <- circuits %>%
  mutate(first.year = as.numeric(str_sub(First.Grand.Prix, 1, 4)))
circuits <- circuits %>%
  mutate(last.year = as.numeric(str_sub(Last.Grand.Prix, 1, 4)))

table(circuits$Circuit.Type)

# I wonder if laps times vs track length has a correlation
cor_value <- cor(circuits$Best.Lap.Time, circuits$Track.Length..km., use = "complete.obs")
print(cor_value)
# 0.6907, not great
# possibly due to MANY years in data

# plot it
ggplot(circuits, aes(x = Best.Lap.Time, y = Track.Length..km.)) +
  geom_point() +
  geom_smooth(method = "lm",
              color = "blue",
              se = TRUE) +
  theme_minimal() +
  labs(title = paste("Correlation:", round(cor_value, 2)))
# well... that explains that.
# 2 circuits ran in the 50s:
# nurburing (slow, lots of corners, lap time of over 10 minutes)
# pescara (25.5 kms, lap time of 9:44)

circuits_2014 <- circuits %>% filter(Best.Lap.Year > 2013) 
summary(circuits_2014) # really is 2017 and up, 22 circuits
cor_value <- cor(circuits_2014$Best.Lap.Time, circuits_2014$Track.Length..km., use = "complete.obs")
print(cor_value) # 0.8290, much better

# plot 2014+
ggplot(circuits_2014, aes(x = Best.Lap.Time, y = Track.Length..km.)) +
  geom_point() +
  geom_smooth(method = "lm",
              color = "blue",
              se = TRUE) +
  theme_minimal() +
  labs(title = paste("Correlation:", round(cor_value, 2)))

# nice. so IMO we can use track distance as an estimate of lap time/quickness

# double check to see if any track has a best lap year before 2014 and was raced on after
circuits_2013 <- circuits %>% filter(Best.Lap.Year <= 2013) 

circuits_2013 <- circuits_2013 %>%
  mutate(last.year = as.numeric(str_sub(Last.Grand.Prix, 1, 4)))

summary(circuits_2013)

circuits_2013 %>% filter(last.year > 2013)

# short version: there are 10, but several have had layout changes that affects this

# still want to compare:
# * % max throttle vs track distance
# * q1 fastest time vs track distance
# but may not need for planned goals

# another way: limit to circuits that occured during v6 era data set (2014+)
circuits_v6 <- circuits %>% filter(last.year > 2013) 
summary(circuits_v6)

####
# Join with Circuit Data
####

# Step 1: get list of circuits from both data sets (names don't match)
print(circuits_v6$Circuit)
# for manual creation of join table
# write.csv(circuits_v6$Circuit, "circuit_names.csv", row.names = FALSE)

print(q1_final_u %>% distinct(EventName, .keep_all = FALSE), n=38)
# for manual creation of join table
# q1_circuit_names <- q1_final_u %>% distinct(EventName, .keep_all = FALSE)
# write.csv(q1_circuit_names, "circuit_names_q1.csv", row.names = FALSE)

# Step 2: figure out why circuits give 32 results, while q1 gives 38
# 70th anniversary GP is clue: 6 tracks are mapped twice

# Step 3: merge

# 3a. Create the Calendar Bridge (Year-Event to Physical Track)
calendar_bridge <- q1_final %>%
  distinct(Year, EventName) %>%
  mutate(Circuit_Match = case_when(
    # Handle the "Same Track, Different Name" cases
    EventName %in% c("Styrian Grand Prix", "Austrian Grand Prix") ~ "Red Bull Ring",
    EventName %in% c("70th Anniversary Grand Prix", "British Grand Prix") ~ "Silverstone Circuit",
    EventName %in% c("Sakhir Grand Prix", "Bahrain Grand Prix") ~ "Bahrain International Circuit",
    EventName %in% c("Mexico City Grand Prix", "Mexican Grand Prix") ~ "Autódromo Hermanos Rodríguez",
    EventName %in% c("São Paulo Grand Prix", "Brazilian Grand Prix") ~ "Autódromo José Carlos Pace",
    EventName %in% c("European Grand Prix", "Azerbaijan Grand Prix") ~ "Baku City Circuit",
    
    # Handle the "German GP" alternating tracks
    EventName == "German Grand Prix" & Year %in% c(2014, 2016, 2018, 2019) ~ "Hockenheimring",
    EventName == "German Grand Prix" & Year %in% c(2013) ~ "Nürburgring",
    EventName == "Eifel Grand Prix" ~ "Nürburgring",
    
    # Catch-all for simple matches (Monaco GP -> Circuit de Monaco, etc.) (numbers are for manual referncing)
    EventName == "Australian Grand Prix" ~ "Melbourne Grand Prix Circuit", #1
    EventName == "Malaysian Grand Prix"  ~ "Sepang International Circuit", #2
    EventName == "Chinese Grand Prix"    ~ "Shanghai International Circuit", #3
    EventName == "Monaco Grand Prix"     ~ "Circuit de Monaco", #4
    EventName == "Canadian Grand Prix"   ~ "Circuit Gilles Villeneuve", #5
    EventName == "Spanish Grand Prix"    ~ "Circuit de Barcelona-Catalunya", #6
    EventName == "Hungarian Grand Prix"  ~ "Hungaroring", #7
    EventName == "Belgian Grand Prix"    ~ "Circuit de Spa-Francorchamps", #8
    EventName == "Italian Grand Prix"    ~ "Autodromo Nazionale Monza", #9
    EventName == "Singapore Grand Prix"  ~ "Marina Bay Street Circuit", #10
    EventName == "Japanese Grand Prix"   ~ "Suzuka Circuit", #11
    EventName == "Russian Grand Prix"    ~ "Sochi Autodrom", #12
    EventName == "United States Grand Prix" ~ "Circuit of the Americas", #13
    EventName == "Abu Dhabi Grand Prix"  ~ "Yas Marina Circuit", #14
    EventName == "French Grand Prix"     ~ "Circuit Paul Ricard", #15
    EventName == "Portuguese Grand Prix" ~ "Algarve International Circuit", #16
    EventName == "Tuscan Grand Prix"     ~ "Autodromo Internazionale del Mugello", #17
    EventName == "Emilia Romagna Grand Prix" ~ "Autodromo Internazionale Enzo e Dino Ferrari", #18
    EventName == "Dutch Grand Prix"      ~ "Circuit Park Zandvoort", #19
    EventName == "Las Vegas Grand Prix" ~ "Las Vegas Street Circuit", #20
    EventName == "Miami Grand Prix"      ~ "Miami International Autodrome", #21
    EventName == "Qatar Grand Prix"      ~ "Lusail International Circuit", #22
    EventName == "Saudi Arabian Grand Prix" ~ "Jeddah Corniche Circuit", #23
    EventName == "Turkish Grand Prix"    ~ "Istanbul Park", #24
    TRUE ~ EventName 
  ))

# 3b. Join the bridge to main
q1_with_bridge <- q1_final_u %>%
  left_join(calendar_bridge, by = c("Year", "EventName"))

# 3c. Join the circuit metadata (Track Length, etc.)
final_data <- q1_with_bridge %>%
  left_join(circuits_v6, by = c("Circuit_Match" = "Circuit"))

summary(final_data)

summary(final_data %>% filter(Year > 2017) )
head(final_data %>% filter(Year > 2017))

dup_check_final <- final_data %>% distinct(Year, EventName, DriverId, .keep_all = TRUE)
# matches number of observations
# I may be a bit paranoid.

write.csv(final_data, "not_final_data.csv", row.names = TRUE)
