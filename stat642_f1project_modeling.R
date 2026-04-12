# Stat 642
# Project Idea: F1 Gap to Q1 vs works/customer
# 

## dataset: https://github.com/theOehrly/Fast-F1

setwd("~/OneDrive/Documents/Student/TAMU Grad School/2026 Spring/Stat 642/Project/f1_data/03_fastf1")

library(tidyverse)
library(lubridate)
library(ggplot2)
library(car)

####
# Get Data In
####

f1_all <- read.csv("f1_teams_joined.csv")


###############
# Data Analysis Start
###############

# create section of data that is just fastf1
f1_fast <- f1_all %>% filter(Year > 2017) 
summary(f1_fast) #note full throttle has no NAs

# correlation between track length and full throttle pct
# note, should do comparison between dry only and all data
f1_fast_dry <- f1_fast %>% filter(SessionCondition == "dry") # make dry data
cor_value <- cor(f1_fast_dry$FullThrottlePct, f1_fast_dry$Track.Length..km., use = "complete.obs")
print(cor_value)

# plot it
ggplot(f1_fast_dry, aes(x = FullThrottlePct, y = Track.Length..km.)) +
  geom_point() +
  geom_smooth(method = "lm",
              color = "blue",
              se = TRUE) +
  theme_minimal() +
  labs(title = paste("Correlation:", round(cor_value, 2)))

# starting models (no interaction)
model_v1 <- lm(GapToFastest ~ works + engine + Year, data = f1_all)
summary(model_v1)
vif(model_v1)

model_v1p <- lm(PerToFastest ~ works + engine + Year, data = f1_all)
summary(model_v1p)

model_v2 <- lm(GapToFastest ~ works + engine + Year + Track.Length..km., data = f1_all)
summary(model_v2)
vif(model_v2)

model_v3 <- lm(GapToFastest ~ works + Year + Track.Length..km., data = f1_all)
summary(model_v3)

model_v4 <- lm(GapToFastest ~ works + engine + Year + FullThrottlePct, data = f1_fast)
summary(model_v4)

model_v5 <- lm(GapToFastest ~ works + Year + engine + FullThrottlePct + SessionCondition, data = f1_fast)
summary(model_v5)

# interactions included

model_v6 <- lm(GapToFastest ~ works * engine * Year, data = f1_all)
summary(model_v6)

model_v7 <- lm(GapToFastest ~ works * engine * Year * Track.Length..km., data = f1_all)
summary(model_v7)

model_v8 <- lm(GapToFastest ~ works * Year * Track.Length..km., data = f1_all)
summary(model_v8)

model_v9 <- lm(GapToFastest ~ works * engine * Year * FullThrottlePct, data = f1_fast)
summary(model_v9)

model_v10 <- lm(GapToFastest ~ works * Year * engine * FullThrottlePct * SessionCondition, data = f1_fast)
summary(model_v10)

model_v10p <- lm(PerToFastest ~ works * Year * engine * FullThrottlePct * SessionCondition, data = f1_fast)
summary(model_v10p)

### need to filter by dry sessions + not outliers + 2018+
### stupid way of doing this

f1_fast_dry_107 <- f1_fast %>% filter(is_beyond_107 == 0 & SessionCondition == "dry")

model_v10 <- lm(GapToFastest ~ works * Year * engine * FullThrottlePct, data = f1_fast_dry_107)
summary(model_v10)

model_v10p <- lm(PerToFastest ~ works * Year * engine * FullThrottlePct, data = f1_fast_dry_107)
summary(model_v10p)

model_v11p <- lm(PerToFastest ~ Year + works * engine, data = f1_fast_dry_107)
summary(model_v11p)

model_v12p <- lm(PerToFastest ~ Year * works * engine, data = f1_fast_dry_107)
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