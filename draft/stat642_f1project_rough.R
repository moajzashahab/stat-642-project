# Stat 642
# Project Idea: F1

## dataset: https://www.kaggle.com/datasets/jtrotman/formula-1-race-data

setwd("~/OneDrive/Documents/Student/TAMU Grad School/2026 Spring/Stat 642/Project/f1_data")

library(tidyverse)
library(lubridate)

####
# Get Data In
####


# Load the core files needed for qualifying analysis
results    <- read_csv("results.csv")
qualifying <- read_csv("qualifying.csv")
constructors <- read_csv("constructors.csv")
races      <- read_csv("races.csv")

# Filter for the V6 Hybrid Era (2014–Present)
hybrid_races <- races %>% 
  filter(year >= 2014) %>% 
  select(raceId, year, round, name)

# Function to convert "M:S.ms" to total seconds
f1_time_to_seconds <- function(time_str) {
  # Handle cases where the time might be NA or missing
  if (is.na(time_str) || time_str == "" || time_str == "\\N") {
    return(NA)
  }
  
  # Use lubridate to parse the period
  # Adding "0:" if the time is just "S.ms" without a minute
  if (!grepl(":", time_str)) {
    time_str <- paste0("0:", time_str)
  }
  
  parsed_time <- ms(time_str)
  return(as.numeric(period_to_seconds(parsed_time)))
}

#### use just q1 time (Gemini)

# 1. Apply the function within your mutate block
df_analysis <- qualifying %>%
  left_join(constructors, by = "constructorId") %>%
  left_join(races, by = "raceId") %>%
  filter(year >= 2014) %>%
  mutate(
    # FIX: Use name.x for team name as you discovered
    team_name = str_trim(name.x),
    
    # NEW: Convert the q1 strings to numeric seconds
    # we use rowwise() or sapply() because ms() expects a specific format
    q1_seconds = sapply(q1, f1_time_to_seconds)
  ) %>%
  # Filter out rows where q1 was NA or \N so the model doesn't crash
  filter(!is.na(q1_seconds)) %>%
  mutate(
    # Define Team Status (Predictor 1)
    team_status = as.factor(case_when(
      team_name %in% c("Mercedes", "Ferrari", "Renault", "Alpine F1 Team") ~ "Works",
      team_name == "Red Bull" & year >= 2019 ~ "Works",
      TRUE ~ "Customer"
    )),
    # Define Engine (Predictor 2)
    engine = as.factor(case_when(
      team_name %in% c("Mercedes", "Williams", "Force India", "Racing Point", "Aston Martin", "McLaren") ~ "Mercedes",
      team_name %in% c("Ferrari", "Sauber", "Haas", "Alfa Romeo") ~ "Ferrari",
      team_name %in% c("Renault", "Lotus F1", "Alpine F1 Team", "Red Bull") & year <= 2018 ~ "Renault",
      team_name %in% c("Red Bull", "Toro Rosso", "AlphaTauri", "RB") & year >= 2019 ~ "Honda",
      TRUE ~ "Other"
    ))
  )

# 2. Run the model using the new numeric column
model_v1 <- lm(q1_seconds ~ team_status + engine + year, data = df_analysis)

summary(model_v1)

###### using gap to top of q1 as response variable (Gemini)

df_analysis <- qualifying %>%
  left_join(constructors, by = "constructorId") %>%
  left_join(races, by = "raceId") %>%
  filter(year >= 2014) %>%
  mutate(
    # Use name.x for team name
    team_name = str_trim(name.x),
    # Convert q1 strings to numeric seconds
    q1_seconds = sapply(q1, f1_time_to_seconds)
  ) %>%
  # Filter out missing times before calculating gaps
  filter(!is.na(q1_seconds)) %>%
  # --- NEW STEP: Calculate Gap to Best ---
  group_by(raceId) %>% 
  mutate(
    q1_best_time = min(q1_seconds, na.rm = TRUE),
    q1_gap = q1_seconds - q1_best_time
  ) %>%
  ungroup() %>%
  # ---------------------------------------
mutate(
  team_status = as.factor(case_when(
    team_name %in% c("Mercedes", "Ferrari", "Renault", "Alpine F1 Team") ~ "Works",
    team_name == "Red Bull" & year >= 2019 ~ "Works",
    TRUE ~ "Customer"
  )),
  engine = as.factor(case_when(
    team_name %in% c("Mercedes", "Williams", "Force India", "Racing Point", "Aston Martin", "McLaren") ~ "Mercedes",
    team_name %in% c("Ferrari", "Sauber", "Haas", "Alfa Romeo") ~ "Ferrari",
    team_name %in% c("Renault", "Lotus F1", "Alpine F1 Team", "Red Bull") & year <= 2018 ~ "Renault",
    team_name %in% c("Red Bull", "Toro Rosso", "AlphaTauri", "RB") & year >= 2019 ~ "Honda",
    TRUE ~ "Other"
  ))
)

# Now run the model using q1_gap
model_gap <- lm(q1_gap ~ team_status + engine + year, data = df_analysis)

summary(model_gap)

######################
# No AI Below

bb_df_analysis <- qualifying %>%
  left_join(constructors, by = "constructorId") %>%
  left_join(races, by = "raceId") %>%
  filter(year >= 2014)

team_names_2014 <- read.csv("team_names_2014.csv")

bb_df_analysis_2 <- bb_df_analysis %>%
  left_join(team_names_2014, by = c("constructorId" = "Team_Id", "year" = "Year"))

head(bb_df_analysis_2)
bb_df_analysis$year

team_names_2014$Year
