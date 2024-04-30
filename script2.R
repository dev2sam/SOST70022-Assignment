# set working directory
setwd("C:/Users/ShaheerAhmed/Desktop/Manchester/Data Science/Material/Longitudinal Data Analysis/Coursework/Analysis")

# load packages
library("tidyverse")
library("lme4")
library("plm")
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lattice)

# import data
load("./data/us_clean.RData")

# see first few cases and variables
head(usl)


# ----------------------
# Descriptive statistics
# ----------------------

# Average hours worked at job each week by wave
usl %>%
  group_by(wave) %>%
  summarise(mean_wphjob = mean(wphjob, na.rm = T))

# Average hours worked at job each week by sex
usl %>%
  group_by(disex) %>%
  summarise(mean_wphjob = mean(wphjob, na.rm = T))

# Average hours worked at job each week by sex in each wave
usl %>%
  group_by(disex, wave) %>%
  summarise(mean_wphjob = mean(wphjob, na.rm = T))

# unconditional change graph for average hours worked
usl %>%
  ggplot(aes(wave, wphjob)) + geom_point(alpha = .005) +
  stat_summary(fun = mean, group = 1, geom = "line", lwd = 1.5) +
  theme_bw() +
  labs(x = "Wave", y = "Hours worked per week")

# conditional change graph by sex for average hours worked
usl %>%
  ggplot(aes(wave, wphjob)) + geom_point(alpha = .005) +
  stat_summary(aes(color = disex, group = disex),
               fun = mean, geom = "line", lwd = 1.5) +
  theme_bw() +
  labs(x = "Wave", y = "Hours worked per week", color = "Sex")

# conditional change graph by sex for average hours worked
usl %>%
  ggplot(aes(wave, wphjob)) + geom_point(alpha = .005) +
  stat_summary(aes(color = disex, group = disex),
               fun = mean, geom = "line", lwd = 1.5) +
  theme_bw() +
  labs(x = "Wave", y = "Hours worked per week", color = "Sex")



# -----------------------------------------------
# How does productivity change over time
# -----------------------------------------------

m0 <- lmer(data = usl, wphjob ~ 1 + (1 | idauniq))
summary(m0)

qqmath(ranef(m0, condVar = TRUE))

usl$wave <- as.numeric(usl$wave)
usl <- mutate(usl, wave0 = wave - 1)
select(usl, idauniq, wave, wave0)
m1 <- lmer(data = usl, wphjob ~ 1 + wave0 + (1 | idauniq))
summary(m1)

m2 <- lmer(data = usl, wphjob ~ 1 + wave0 + (1 + wave0 | idauniq))
summary(m2)

qqmath(ranef(m2, condVar = TRUE))



model1 <- lmer(wphjob ~ 1 + wave + (1 | idauniq), data = usl)
summary(model1)

# Calculate average working hours per wave
avg_wphjob_per_wave <- usl %>%
  group_by(wave) %>%
  summarize(avg_wphjob = mean(wphjob, na.rm = TRUE))

# Plotting
ggplot(avg_wphjob_per_wave, aes(x = wave, y = avg_wphjob)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Average Working Hours per Week Over Time",
       x = "Wave",
       y = "Average Working Hours per Week") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

