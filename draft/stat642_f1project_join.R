# Stat 642
# Project Idea: F1 Gap to Q1 vs works/customer
# EDA, Clean, and Join

## dataset: https://github.com/theOehrly/Fast-F1

setwd("~/OneDrive/Documents/Student/TAMU Grad School/2026 Spring/Stat 642/Project/f1_data/03_fastf1")

library(tidyverse)
library(lubridate)
library(ggplot2)
library(car)

####
# Get Data In
####

f1_teams <- read.csv("f1_teams_table.csv")
# table that has mostly complete data
# lacks weather, circuit data

modeling <- read.csv("modeling_table.csv")
# table that has incomplete works & engine data
# but adds weather, throttle % data

circuits <- read.csv("all_f1_circuits.csv")
# table that has data on all circuits
# source: https://www.kaggle.com/datasets/kishan305/formula-1-circuits-1950-present

####
# EDA and clean on f1_teams
####

# quick EDA
summary(f1_teams)
head(f1_teams)

# check of which races on f1_teams data
f1_teams_event_summary <- f1_teams %>%
  count(Year, EventName)

# all the events
f1_teams_event_summary %>% count(Year)

# per event
print(f1_teams_event_summary)
#2024 has over 20 entries

f1_teams %>% filter(Year == 2024 & EventName == "Abu Dhabi Grand Prix")
# double Alpines

# remove duplicates
f1_teams_unique <- f1_teams %>% 
  distinct(Year, EventName, DriverId, .keep_all = TRUE)

# check
f1_teams_unique %>% filter(Year == 2024 & EventName == "Abu Dhabi Grand Prix")

# recheck of which races on f1_teams data
f1_teams_event_summary <- f1_teams_unique %>%
  count(Year, EventName)

# all the events
f1_teams_event_summary %>% count(Year)

# per event
print(f1_teams_event_summary)
# 2014 & 2016 are only years with more than 20 entries
# this tracks

## NAs

# looking for where there is NA in q1 data
f1_teams_NA <- f1_teams_unique %>% filter(is.na(Q1time))

f1_teams_NA_event_summary <- f1_teams_NA %>% count(Year, EventName)

print(f1_teams_NA_event_summary) # 2020 sakhir grand prix
# note that this event did happen, so some error in data here

# remove extra tables
rm(f1_teams_event_summary, f1_teams_NA, f1_teams_NA_event_summary)
# maybe unique?

####
# EDA and clean on modeling
####

# quick EDA
summary(modeling)
head(modeling)

# check of which races on f1_teams data
modeling_event_summary <- f1_teams %>% count(Year, EventName)

print(modeling_event_summary)
modeling_event_summary %>% count(Year) # 2022 should have 22, 2024 should have 24
# missing:
# 2022 Sao Paulo GP -> this is in the original modeling data?
# 2024 Monaco GP

# looking for where there is NA for Q1)time in modeling data
modeling_NA <- modeling %>% filter(is.na(Q1_time))
modeling_NA # nowhere

# look for 2020 sakhir data
modeling_2020_sakhir <- modeling %>% filter(Year == 2020 & EventName == "Sakhir Grand Prix")
head(modeling_2020_sakhir)
# it is here

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
rm(modeling_rain)

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

# Join this label back into your modeling or your final f1_teams table
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

# clean up by deleting all but modeling_SC and session conditions
rm(modeling_rain, modeling_2020_sakhir, modeling_event_summary, modeling_NA)

####
# Join modeling table to f1_teams
####

# join
f1_teams_w_modeling <- f1_teams_unique %>%
  left_join(
    modeling_SC %>% select(Year, EventName, DriverId, Q1_time, Rainfall, FullThrottlePct, SessionCondition),
    by = c("Year", "EventName", "DriverId")
  ) %>%
  # Fill in the missing Sakhir 2020 times
  mutate(Q1time = coalesce(Q1time, Q1_time)) %>%
  select(-Q1_time)

# add gap to fastest q1 time
f1_teams_final <- f1_teams_w_modeling %>%
  filter(!is.na(Q1time)) %>%
  group_by(Year, EventName) %>%
  mutate(
    FastestQ1 = min(Q1time, na.rm = TRUE),
    GapToFastest = Q1time - FastestQ1,
    PerToFastest = GapToFastest / FastestQ1
  ) %>%
  ungroup()

summary(f1_teams_final)
head(f1_teams_final)

# cars that exceed 107% lap time
# f1_teams_slow <- f1_teams_final %>% filter(PerToFastest > 0.07)
f1_teams_final <- f1_teams_final %>% mutate(is_beyond_107 = ifelse(PerToFastest > 0.07, 1, 0))
table(f1_teams_final$is_beyond_107)
#table(f1_teams_slow$SessionCondition) # most are in mixed conditions

f1_teams_final[duplicated(f1_teams_final), ] # we have duplicates
summary(f1_teams_final[duplicated(f1_teams_final), ])

# remove duplicates
f1_teams_final_u <- f1_teams_final %>% 
  distinct(Year, EventName, DriverId, .keep_all = TRUE)

# filter for fastf1 data (2018 and newer)
f1_teams_fastf1 <- f1_teams_final_u %>% filter(Year > 2017)
table(f1_teams_fastf1$SessionCondition)
f1_teams_fastf1 %>% filter(Rainfall == "")
head(f1_teams_fastf1)

f1_teams_fastf1[duplicated(f1_teams_fastf1), ]
# no duplicates... because I wanted to be sure

#### rewrite
# check on FullThrottlePct
sum(is.na(f1_teams_final_u$FullThrottlePct)) # 1619 rows
sum(is.na(f1_teams_fastf1$FullThrottlePct)) # 0 rows
summary(f1_teams_final_u %>% 
  filter(is.na(FullThrottlePct))) # missing from 2014-2017

sum(is.na(f1_teams_final_u$SessionCondition)) # 1619 rows
sum(is.na(f1_teams_fastf1$SessionCondition)) # 0 rows
summary(f1_teams_final_u %>% 
          filter(is.na(SessionCondition))) # missing from 2014-2017

# rename f1_teams_final_u as f1_teams_all
f1_teams_all <- f1_teams_final_u

# remove unused datasets
rm(f1_teams_final, f1_teams_final_dup, f1_teams_final_u,
   f1_teams_unique, f1_teams_w_modeling, f1_teams_fastf1)

# so f1_teams_all includes 2014+ but only rainfall and throttle for 2018+

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

# remove unused datasets
rm(circuits_2013, circuits_2014)

####
# Join with Circuit Data
####

# Step 1: get list of circuits from both data sets (names don't match)
# print(circuits_v6$Circuit)
# for manual creation of join table
# write.csv(circuits_v6$Circuit, "circuit_names.csv", row.names = FALSE)

print(f1_teams_all %>% distinct(EventName, .keep_all = FALSE), n=38)
# for manual creation of join table
# f1_teams_circuit_names <- f1_teams_final_u %>% distinct(EventName, .keep_all = FALSE)
# write.csv(f1_teams_circuit_names, "circuit_names_f1_teams.csv", row.names = FALSE)

# Step 2: figure out why circuits give 32 results, while f1_teams gives 38
# 70th anniversary GP is clue: 6 tracks are mapped twice

# Step 3: merge

# 3a. Create the Calendar Bridge (Year-Event to Physical Track)
calendar_bridge <- f1_teams_all %>%
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
f1_teams_w_bridge <- f1_teams_all %>%
  left_join(calendar_bridge, by = c("Year", "EventName"))

# 3c. Join the circuit metadata (Track Length, etc.)
f1_teams_w_circuit <- f1_teams_w_bridge %>%
  left_join(circuits_v6, by = c("Circuit_Match" = "Circuit"))

summary(f1_teams_w_circuit)

summary(f1_teams_w_circuit %>% filter(Year > 2017) )
head(f1_teams_w_circuit %>% filter(Year > 2017))

dup_check <- f1_teams_w_circuit %>% distinct(Year, EventName, DriverId, .keep_all = TRUE)
# matches number of observations
# I may be a bit paranoid.

# remove unused data sets
rm(dup_check, f1_teams_w_bridge)

write.csv(f1_teams_w_circuit, "f1_teams_joined.csv", row.names = TRUE)

###############
# Data Analysis Start
###############

# create section of data that is just fastf1
final_data_fastf1 <- final_data %>% filter(Year > 2017) 
summary(final_data_fastf1) #note full throttle has no NAs

# correlation between track length and full throttle pct
# note, should do comparison between dry only and all data
final_data_fastf1_dry <- final_data_fastf1 %>% filter(SessionCondition == "dry") # make dry data
cor_value <- cor(final_data_fastf1_dry$FullThrottlePct, final_data_fastf1_dry$Track.Length..km., use = "complete.obs")
print(cor_value)

# plot it
ggplot(final_data_fastf1_dry, aes(x = FullThrottlePct, y = Track.Length..km.)) +
  geom_point() +
  geom_smooth(method = "lm",
              color = "blue",
              se = TRUE) +
  theme_minimal() +
  labs(title = paste("Correlation:", round(cor_value, 2)))

# starting models (no interaction)
model_v1 <- lm(GapToFastest ~ works + engine + Year, data = final_data)
summary(model_v1)
vif(model_v1)

model_v1p <- lm(PerToFastest ~ works + engine + Year, data = final_data)
summary(model_v1p)

model_v2 <- lm(GapToFastest ~ works + engine + Year + Track.Length..km., data = final_data)
summary(model_v2)
vif(model_v2)

model_v3 <- lm(GapToFastest ~ works + Year + Track.Length..km., data = final_data)
summary(model_v3)

model_v4 <- lm(GapToFastest ~ works + engine + Year + FullThrottlePct, data = final_data_fastf1)
summary(model_v4)

model_v5 <- lm(GapToFastest ~ works + Year + engine + FullThrottlePct + SessionCondition, data = final_data_fastf1)
summary(model_v5)

# interactions included

model_v6 <- lm(GapToFastest ~ works * engine * Year, data = final_data)
summary(model_v6)

model_v7 <- lm(GapToFastest ~ works * engine * Year * Track.Length..km., data = final_data)
summary(model_v7)

model_v8 <- lm(GapToFastest ~ works * Year * Track.Length..km., data = final_data)
summary(model_v8)

model_v9 <- lm(GapToFastest ~ works * engine * Year * FullThrottlePct, data = final_data_fastf1)
summary(model_v9)

model_v10 <- lm(GapToFastest ~ works * Year * engine * FullThrottlePct * SessionCondition, data = final_data_fastf1)
summary(model_v10)

model_v10p <- lm(PerToFastest ~ works * Year * engine * FullThrottlePct * SessionCondition, data = final_data_fastf1)
summary(model_v10p)

### need to filter by dry sessions + not outliers + 2018+
### stupid way of doing this

final_final_data <- final_data %>% filter(is_beyond_107 == 0 & SessionCondition == "dry")

model_v10 <- lm(GapToFastest ~ works * Year * engine * FullThrottlePct, data = final_final_data)
summary(model_v10)

model_v10p <- lm(PerToFastest ~ works * Year * engine * FullThrottlePct, data = final_final_data)
summary(model_v10p)

model_v11p <- lm(PerToFastest ~ Year + works * engine, data = final_final_data)
summary(model_v11p)

model_v12p <- lm(PerToFastest ~ Year * works * engine, data = final_final_data)
summary(model_v12p)

# check vif > 5 for models

# check stepwise
reduced_model_v10 <- step(model_v10, direction = "both")
summary(reduced_model_v10)

reduced_model_v12p <- step(model_v12p, direction = "both")
summary(reduced_model_v12p)

# OUTLIERS (107% rule or 3xIQR)

# check contrasts

# check normality (?)