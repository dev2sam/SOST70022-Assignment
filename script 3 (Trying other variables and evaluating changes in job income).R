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
library(lmerTest)


# make object with the variable names we want
vars5 <- c("idauniq", "dhwork", "scworki", "iasinc", "disex")
vars6 <- c("idauniq", "DhWork", "scworki", "IaSInc", "DiSex")
vars7 <- c("idauniq", "DhWork", "scworki", "IaSInc", "DiSex")
vars8 <- c("idauniq", "dhwork", "scworki", "iasinc", "indsex")
vars9 <- c("idauniq", "dhwork", "scworki", "iasinc", "indsex")

# import data using relative link and select some variables
us5 <- read_dta("./data/wave_5.dta", col_select = vars5)
us6 <- read_dta("./data/wave_6.dta", col_select = vars6)
us7 <- read_dta("./data/wave_7.dta", col_select = vars7)
us8 <- read_dta("./data/wave_8.dta", col_select = vars8)
us9 <- read_dta("./data/wave_9.dta", col_select = vars9)


# rename variables so all datatsets follow same naming conventions
us6 <- us6 %>%
  rename(
    iasinc = IaSInc,
    dhwork = DhWork,
    disex = DiSex
  )
us7 <- us7 %>%
  rename(
    iasinc = IaSInc,
    dhwork = DhWork,
    disex = DiSex
  )
us8 <- us8 %>%
  rename(
    disex = indsex
  )
us9 <- us9 %>%
  rename(
    disex = indsex
  )

# see first few cases and variables
head(us5)
head(us6)
head(us7)
head(us8)
head(us9)

names(us5)
names(us6)
names(us7)
names(us8)
names(us9)

# rename variables in each dataset to make them unique when merging
us5 <- rename_at(us5, vars(-idauniq), ~str_c(., "_5"))
us6 <- rename_at(us6, vars(-idauniq), ~str_c(., "_6"))
us7 <- rename_at(us7, vars(-idauniq), ~str_c(., "_7"))
us8 <- rename_at(us8, vars(-idauniq), ~str_c(., "_8"))
us9 <- rename_at(us9, vars(-idauniq), ~str_c(., "_9"))

# add a variable to each dataset to indicate wave number
us5 <- mutate(us5, present_5 = 1)
us6 <- mutate(us6, present_6 = 1)
us7 <- mutate(us7, present_7 = 1)
us8 <- mutate(us8, present_8 = 1)
us9 <- mutate(us9, present_9 = 1)

# check description of the data
glimpse(us5)
glimpse(us6)
glimpse(us7)
glimpse(us8)
glimpse(us9)

# merge all waves based on idauniq
us56 <- full_join(us5, us6, by = "idauniq")
us567 <- full_join(us56, us7, by = "idauniq")
us5678 <- full_join(us567, us8, by = "idauniq")
us56789 <- full_join(us5678, us9, by = "idauniq")
glimpse(us56789)

# to see how many people are present in all waves (it's 5665 that are present in all waves)
count(us56789, present_5, present_6, present_7, present_8, present_9)

# keep only those who are present in all waves
us <- filter(us56789, present_5 == 1, present_6 == 1, present_7 == 1, present_8 == 1, present_9 == 1)

# eliminate the "present" variables as they are not useful anymore
us <- select(us, -starts_with("present"))
glimpse(us)

# since we need to perform multilevel models for change, we need to have data in long format
usl <- pivot_longer(
  us,
  cols = !c(idauniq),
  names_sep = "_",
  names_to = c(".value", "wave")
)
glimpse(usl)

# check how many people we have overall and if the data is balanced
count(usl, wave)

# removing unwanted values from dhwork (keep only those who are in paid employment)
usl <- subset(usl, !(dhwork %in% c(-9, -8, -1, 2)))
count(usl, dhwork)

# removing unwanted values from sex and map 1 to Male and 2 to Female
usl <- subset(usl, !is.na(disex))
usl$disex <- ifelse(usl$disex == 1, "Male", ifelse(usl$disex == 2, "Female", usl$disex))
count(usl, disex)

# Convert the 'wave' variable to numeric if it's not already
usl$wave <- as.numeric(usl$wave)

# removing unwanted values from iasinc
usl <- subset(usl, !(iasinc %in% c(-9, -8, -1)))
count(usl, iasinc)


# Calculate average iasinc for each individual across waves
avg_iasinc_individual <- usl %>%
  group_by(idauniq, wave) %>%
  summarise(avg_iasinc = mean(iasinc, na.rm = TRUE))

# Plot boxplot for average iasinc by individual
ggplot(avg_iasinc_individual, aes(x = factor(wave), y = avg_iasinc, fill = factor(wave))) +
  geom_boxplot() +
  labs(title = "Boxplot of Average Income by Individual Across Waves",
       x = "Wave",
       y = "Average Income",
       fill = "Wave") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Calculate IQR for each wave
iqr_by_wave <- avg_iasinc_individual %>%
  group_by(wave) %>%
  summarise(Q1 = quantile(avg_iasinc, 0.25),
            Q3 = quantile(avg_iasinc, 0.75))

# Define upper and lower bounds for each wave
iqr_by_wave <- iqr_by_wave %>%
  mutate(lower_bound = Q1 - 1.5 * (Q3 - Q1),
         upper_bound = Q3 + 1.5 * (Q3 - Q1))

# Remove outliers from usl dataset
usl <- usl %>%
  left_join(avg_iasinc_individual, by = c("idauniq", "wave")) %>%
  filter(avg_iasinc >= lower_bound & avg_iasinc <= upper_bound) %>%
  select(-avg_iasinc)

# Plot boxplot without outliers
ggplot(usl, aes(x = factor(wave), y = iasinc, fill = factor(wave))) +
  geom_boxplot() +
  labs(title = "Boxplot of Average Income by Individual Across Waves (Outliers Removed)",
       x = "Wave",
       y = "Average Income",
       fill = "Wave") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Filter the dataset to keep only the minimum number of observations per wave, to balance dataset
min_obs <- min(table(usl$wave))
usl <- usl %>%
  group_by(wave) %>%
  slice(1:min_obs) %>%
  ungroup()
count(usl, wave)


# -----
# Model
# -----
model2 <- lmer(iasinc ~ 1 + wave + (1 | idauniq), data = usl)
summary(model2)

# The qqplot
qqmath(ranef(model2, condVar = TRUE))


# Extract the random effects from the model
random_effects <- ranef(model2)$idauniq[,1]
# Create a histogram of the random effects
hist(random_effects, main = "Histogram of Random Effects", xlab = "Random Effects")


# Calculate average Gross earned income amount per wave
avg_iasinc_per_wave <- usl %>%
  group_by(wave) %>%
  summarize(avg_iasinc = mean(iasinc, na.rm = TRUE))
# Plotting
ggplot(avg_iasinc_per_wave, aes(x = wave, y = avg_iasinc)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Average Gross Earnings Annually Over Time",
       x = "Wave",
       y = "Average Gross Earnings Annually") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
