# Stat 642
# Project Idea: F1 Gap to Q1 vs works/customer
# 

## dataset: https://github.com/theOehrly/Fast-F1

setwd("~/OneDrive/Documents/Student/TAMU Grad School/2026 Spring/Stat 642/Project/f1_data/03_fastf1")

library(tidyverse)
library(lubridate)
library(ggplot2)
library(car)
library(lme4)
library(glmnet)
library(emmeans)

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

#### make Year as a Block
f1_fast$YearBlock <- as.factor(f1_fast$Year)

#### make fullthrottlePct centered #new 4/2026
f1_fast$Throttle_Centered <- f1_fast$FullThrottlePct - mean(f1_fast$FullThrottlePct)

# correlation between track length and full throttle pct
# note, should do comparison between dry only and all data
f1_fast_dry <- f1_fast %>% filter(SessionCondition == "dry") # make dry data
#
f1_fast_dry_107 <- f1_fast %>% filter(is_beyond_107 == 0 & SessionCondition == "dry") # dry w/o outliers

f1_fast_wet_107 <- f1_fast %>% filter(is_beyond_107 == 0 & SessionCondition == "wet") # wet w/o outliers

f1_fast_mixed_107 <- f1_fast %>% filter(is_beyond_107 == 0 & SessionCondition == "mixed") # mixed w/o outliers

f1_fast_107 <- f1_fast %>% filter(is_beyond_107 == 0) # all w/out outliers

f1_slow <- f1_fast %>% filter(is_beyond_107 == 1) # outliers only


### testing Full Throttle % vs Track Length
cor_value <- cor(f1_fast_dry_107$FullThrottlePct, f1_fast_dry_107$Track.Length..km., use = "complete.obs")
print(cor_value)

# plot it
ggplot(f1_fast_dry_107, aes(x = FullThrottlePct, y = Track.Length..km.)) +
  geom_point() +
  geom_smooth(method = "lm",
              color = "yellow",
              se = TRUE) +
  theme_dark() +
  labs(title = paste("Correlation:", round(cor_value, 2)))

# Full Throttle vs Per to Fastest (included)
cor_value <- cor(f1_fast_dry_107$FullThrottlePct, f1_fast_dry_107$PerToFastest, use = "complete.obs")
print(cor_value)

# plot it
ggplot(f1_fast_dry_107, aes(x = FullThrottlePct, y = PerToFastest)) +
  geom_point() +
  geom_smooth(method = "lm",
              color = "yellow",
              se = TRUE) +
  theme_dark() +
  labs(title = paste("Correlation:", round(cor_value, 2)))

# plot it in better colors
ggplot(f1_fast_dry_107, aes(x = FullThrottlePct, y = PerToFastest)) +
  geom_point() +
  geom_smooth(method = "lm",
              color = "yellow",
              se = TRUE) +
  theme_dark() +
  theme(
    # Invert the panel and plot background to pitch black
    panel.background = element_rect(fill = "grey69"),
    plot.background = element_rect(fill = "grey20", color = "black"),
    
    # Invert the grid lines to a subtle dark grey
    panel.grid.major = element_line(color = "grey20"),
    panel.grid.minor = element_line(color = "grey10"),
    
    # Make axes and labels white for high contrast
    axis.title = element_text(color = "grey90", size = 14, face = "bold"),
    axis.text = element_text(color = "grey90", size = 12),
    axis.ticks = element_line(color = "grey90"),
    
    # Remove the grey box around the legend if applicable
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white")
  )

# split based on works
ggplot(f1_fast_dry_107, aes(x = FullThrottlePct, y = PerToFastest)) +
  geom_point() +
  geom_smooth(method = "lm",
              color = "blue",
              se = TRUE) +
  facet_wrap(~ works) +
  theme_minimal() +
  labs(title = paste("Correlation:", round(cor_value, 2)))

ggplot(f1_fast_dry_107, aes(x = FullThrottlePct, y = PerToFastest)) +
  geom_bin2d() + 
  facet_wrap(~ works) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal()

# this is probably the graph we want
ggplot(f1_fast_dry_107, aes(x = FullThrottlePct, y = PerToFastest)) +
  geom_point() +
  geom_smooth(method = "lm",
              color = "blue",
              se = TRUE) +
  facet_grid(engine ~ works) +
  theme_minimal()

ggplot(f1_fast_dry_107, aes(x = FullThrottlePct, y = PerToFastest)) +
  geom_point() +
  geom_smooth(method = "lm",
              color = "yellow",
              se = TRUE) +
  facet_grid(engine ~ works) +
  theme_dark() +
  theme(
    # Invert the panel and plot background to pitch black
    panel.background = element_rect(fill = "grey69"),
    plot.background = element_rect(fill = "grey20", color = "black"),
    
    # Invert the grid lines to a subtle dark grey
    panel.grid.major = element_line(color = "grey20"),
    panel.grid.minor = element_line(color = "grey10"),
    
    # Make axes and labels white for high contrast
    axis.title = element_text(color = "grey90", size = 14, face = "bold"),
    axis.text = element_text(color = "grey90", size = 12),
    axis.ticks = element_line(color = "grey90"),
    
    # Remove the grey box around the legend if applicable
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white")
  )


ggplot(f1_fast_dry_107, aes(x = FullThrottlePct, y = PerToFastest)) +
  geom_point() +
  geom_smooth(method = "lm",
              color = "blue",
              se = TRUE) +
  theme_minimal() +
  labs(title = paste("Correlation:", round(cor_value, 2)))

ggplot(f1_fast_dry_107, aes(x = FullThrottlePct, y = PerToFastest)) +
  geom_point() +
  geom_smooth(method = "lm",
              color = "yellow",
              se = TRUE) +
  theme_dark() +
  theme(
    # Invert the panel and plot background to pitch black
    panel.background = element_rect(fill = "grey69"),
    plot.background = element_rect(fill = "grey20", color = "black"),
    
    # Invert the grid lines to a subtle dark grey
    panel.grid.major = element_line(color = "grey20"),
    panel.grid.minor = element_line(color = "grey10"),
    
    # Make axes and labels white for high contrast
    axis.title = element_text(color = "grey90", size = 14, face = "bold"),
    axis.text = element_text(color = "grey90", size = 12),
    axis.ticks = element_line(color = "grey90"),
    
    # Remove the grey box around the legend if applicable
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white")
  )

ggplot(f1_fast_dry_107, aes(x = FullThrottlePct, y = GapToFastest)) +
  geom_point() +
  geom_smooth(method = "lm",
              color = "blue",
              se = TRUE) +
  facet_grid(YearBlock ~ works) +
  theme_minimal()


########
# starting models (no interaction)
model_v1 <- lm(GapToFastest ~ works + engine + YearBlock, data = f1_fast_107)
summary(model_v1)
vif(model_v1)

model_v1p <- lm(PerToFastest ~ works + engine + Year, data = f1_all)
summary(model_v1p)

model_v2 <- lm(PerToFastest ~ works + engine + Year + Track.Length..km., data = f1_all)
summary(model_v2)
vif(model_v2)

model_v3 <- lm(GapToFastest ~ works + Year + Track.Length..km., data = f1_all)
summary(model_v3)

model_v4 <- lm(PerToFastest ~ works + engine + Year + FullThrottlePct, data = f1_fast)
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

model_v9p <- lm(PerToFastest ~ works * engine * Year * FullThrottlePct, data = f1_fast)
summary(model_v9p)

model_v9p_b <- lm(PerToFastest ~ SessionCondition + Year + works * engine * FullThrottlePct, data = f1_fast)
summary(model_v9p_b)

model_v10 <- lm(GapToFastest ~ works * Year * engine * FullThrottlePct * SessionCondition, data = f1_fast)
summary(model_v10)

model_v10p <- lm(PerToFastest ~ works * Year * engine * FullThrottlePct * SessionCondition, data = f1_fast)
summary(model_v10p)

model_v10b <- lm(PerToFastest ~ YearBlock * works * engine * FullThrottlePct, data = f1_fast_dry_107)
summary(model_v10)

model_v10p <- lm(PerToFastest ~ YearBlock * works * engine + FullThrottlePct, data = f1_fast_dry_107)
summary(model_v10p)

model_v11p <- lm(PerToFastest ~ Year * works * engine, data = f1_fast_dry_107)
summary(model_v11p)

#### better models

model_a <- lm(PerToFastest ~ YearBlock * works * engine, data = f1_fast_dry_107) ### this one!
summary(model_a)

#new 4/20/26
model_b <- lm(PerToFastest ~ YearBlock * works * engine * SessionCondition, data = f1_fast_107)
summary(model_b)
anova(model_b)

model_b_log <- lm(log(PerToFastest+0.001) ~ YearBlock * works * engine * SessionCondition, data = f1_fast_107)
summary(model_b_log)
anova(model_b_log)

model_c <- lm(PerToFastest ~ works * engine * SessionCondition, data = f1_fast_107) # used for 3-way contrast
summary(model_c)

model_d <- lm(PerToFastest ~ YearBlock * works * engine, data = f1_fast_107) # used for 3-way contrast
summary(model_d)
anova(model_d)

summary(aov(model_b))
summary(aov(model_d))

###
res_a <- resid(model_a)
res_b <- resid(model_b)
res_b_log <- resid(model_b_log)
res_c <- resid(model_c)

###
boxplot(res_a ~ YearBlock, data = f1_fast_dry_107)
boxplot(res_a ~ works, data = f1_fast_dry_107)
boxplot(res_a ~ engine, data = f1_fast_dry_107)
#
boxplot(res_b ~ YearBlock, data = f1_fast_107)
boxplot(res_b ~ works, data = f1_fast_107)
boxplot(res_b ~ engine, data = f1_fast_107)
boxplot(res_b ~ SessionCondition, data = f1_fast_107)

#
boxplot(res_c ~ YearBlock, data = f1_fast_107)
boxplot(res_c ~ works, data = f1_fast_107)
boxplot(res_c ~ engine, data = f1_fast_107)
###

plot(model_a, which = 1)
plot(model_b, which = 1)
plot(model_b_log, which = 1)
plot(model_c, which = 1)

###

plot(model_a, which = 2)
plot(model_b, which = 2)
plot(model_b_log, which = 2)
plot(model_c, which = 2)

# check vif > 5 for models

# check stepwise
reduced_model_v10p <- step(model_v10p, direction = "both")
summary(reduced_model_v10p)

reduced_model_v12p <- step(model_v12p, direction = "both")
summary(reduced_model_v12p)

reduced_model_b <- step(model_b, direction = "both")
summary(reduced_model_b)

#### end new 4/20/26

######
f1_2018 <- f1_fast_dry_107 %>% filter(Year == 2018)
f1_2019 <- f1_fast_dry_107 %>% filter(Year == 2019)
f1_2020 <- f1_fast_dry_107 %>% filter(Year == 2020)
f1_2021 <- f1_fast_dry_107 %>% filter(Year == 2021)
f1_2022 <- f1_fast_dry_107 %>% filter(Year == 2022)
f1_2023 <- f1_fast_dry_107 %>% filter(Year == 2023)
f1_2024 <- f1_fast_dry_107 %>% filter(Year == 2024)
f1_2025 <- f1_fast_dry_107 %>% filter(Year == 2025)

boxplot(f1_2018$PerToFastest)
boxplot(f1_2019$PerToFastest)
boxplot(f1_2020$PerToFastest)
boxplot(f1_2021$PerToFastest)
boxplot(f1_2022$PerToFastest)
boxplot(f1_2023$PerToFastest)
boxplot(f1_2024$PerToFastest)
boxplot(f1_2025$PerToFastest)

boxplot(f1_fast_dry_107$PerToFastest)

# OUTLIERS (107% rule or 3xIQR)



# check normality (?)


model_mixed <- lmer(PerToFastest ~ works * engine + (1 | SessionCondition), data = f1_fast)
summary(model_mixed)


### LASSO
# create model matrix (handles factors automatically)
x <- model.matrix(PerToFastest ~ Year + works + engine, data = f1_fast_dry_107)[,-1]
y_vec <- f1_fast_dry_107$PerToFastest

# fit lasso
fit <- cv.glmnet(x, y_vec, alpha = 1)

# coefficients
coef(fit, s = "lambda.min")


##### #new 4/20/26
# check contrasts - 4 way
# Calculate the means for every combination
m_grid <- emmeans(model_b, ~ works | YearBlock * engine * SessionCondition)

# Contrast Works vs Customer for 2018 Ferrari Dry
contrast(m_grid, "pairwise", at = list(YearBlock="2018", engine="Ferrari", SessionCondition="dry"), adjust = "bonferroni")

##

# check contrasts - session condition
m_grid <- emmeans(model_c, ~ SessionCondition | works * engine)
# Contrast Works vs Customer for 2018 Ferrari Dry
contrast(m_grid, "pairwise", at = list(SessionCondition="dry", engine="Ferrari"), adjust = "bonferroni")

m_grid <- emmeans(model_c, ~ works | SessionCondition * engine)
contrast(m_grid, "pairwise", at = list(SessionCondition="dry", engine="Ferrari"), adjust = "bonferroni")

##

# check contrasts - works | engine * year
m_grid <- emmeans(model_d, ~ works | YearBlock * engine)
# Contrast Works vs Customer for 2018 Ferrari Dry
contrast(m_grid, "pairwise", at = list(SessionCondition="dry", engine="Ferrari"), adjust = "bonferroni")
#####



### t-test of session conditions vs % throttle
t.test(f1_fast_dry_107$FullThrottlePct, f1_fast_wet_107$FullThrottlePct)
# p-value < 0.0001 for dry v wet
t.test(f1_fast_dry_107$FullThrottlePct, f1_fast_mixed_107$FullThrottlePct)
# p-value < 0.0002 for dry v mixed
t.test(f1_fast_wet_107$FullThrottlePct, f1_fast_mixed_107$FullThrottlePct)
# p-value < 0.004 for wet v mixed

### t-test of session conditions vs % PerToFastest
t.test(f1_fast_dry_107$FullThrottlePct, f1_fast_wet_107$PerToFastest)
# p-value < 0.0001 for dry v wet
t.test(f1_fast_dry_107$FullThrottlePct, f1_fast_mixed_107$PerToFastest)
# p-value < 0.0001 for dry v mixed
t.test(f1_fast_wet_107$FullThrottlePct, f1_fast_mixed_107$PerToFastest)
# p-value < 0.0001 for wet v mixed

ggplot(f1_fast_107, aes(x = SessionCondition, y = FullThrottlePct)) +
  geom_violin(trim = FALSE, fill = "yellow") +
  geom_boxplot(width = 0.3) +
  geom_jitter(width = 0.01, alpha = 0.5) +
  theme_dark()

ggplot(f1_fast_107, aes(x = SessionCondition, y = PerToFastest)) +
  geom_violin(trim = FALSE, fill = "lightblue") +
  geom_boxplot(width = 0.3) +
  geom_jitter(width = 0.01, alpha = 0.5) +
  theme_minimal()

#same plots, only cooler
ggplot(f1_fast_107, aes(x = SessionCondition, y = FullThrottlePct)) +
  geom_violin(trim = FALSE, fill = "#FFFF00", color = "black", linewidth = 0.7) + 
  geom_boxplot(width = 0.4, fill = "white", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.05, alpha = 0.6, color = "grey15") +
  theme_dark() +
  theme(
    # Invert the panel and plot background to pitch black
    panel.background = element_rect(fill = "grey69"),
    plot.background = element_rect(fill = "grey20", color = "black"),
    
    # Invert the grid lines to a subtle dark grey
    panel.grid.major = element_line(color = "grey20"),
    panel.grid.minor = element_line(color = "grey10"),
    
    # Make axes and labels white for high contrast
    axis.title = element_text(color = "grey80", size = 14, face = "bold"),
    axis.text = element_text(color = "grey80", size = 12),
    axis.ticks = element_line(color = "grey80"),
    
    # Remove the grey box around the legend if applicable
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white")
  )

ggplot(f1_fast_107, aes(x = SessionCondition, y = PerToFastest)) +
  geom_violin(trim = FALSE, fill = "#FFFF00", color = "black", linewidth = 0.7) + 
  geom_boxplot(width = 0.4, fill = "white", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.05, alpha = 0.6, color = "grey15") +
  theme_dark() +
  theme(
    # Invert the panel and plot background to pitch black
    panel.background = element_rect(fill = "grey69"),
    plot.background = element_rect(fill = "grey20", color = "black"),
    
    # Invert the grid lines to a subtle dark grey
    panel.grid.major = element_line(color = "grey20"),
    panel.grid.minor = element_line(color = "grey10"),
    
    # Make axes and labels white for high contrast
    axis.title = element_text(color = "grey80", size = 14, face = "bold"),
    axis.text = element_text(color = "grey80", size = 12),
    axis.ticks = element_line(color = "grey80"),
    
    # Remove the grey box around the legend if applicable
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white")
  )
