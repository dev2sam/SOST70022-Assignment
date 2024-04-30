# set working directory
setwd("C:/Users/ShaheerAhmed/Desktop/Manchester/Data Science/Material/Longitudinal Data Analysis/Coursework/Analysis")

# install packages
install.packages("tidyverse")
install.packages("lme4")
install.packages("plm")
install.packages("haven")
install.packages("dplyr")
install.packages("tidyr")

# load packages
library("tidyverse")
library("lme4")
library("plm")
library(haven)
library(dplyr)
library(tidyr)


# -------------
# Merging waves
# -------------

# make objects with the variable names we want
vars5 <- c("idauniq", "samptyp", "iintdatm", "iintdaty", "couple", "dhwork", "scworki", "heiqk", "wphjob", "disex", "hepsyde", "hepsyan", "scptrd", "scptrc", "scptra", "scworka", "scqolr")
vars6 <- c("idauniq", "samptyp", "iintdatm", "iintdaty", "couple", "DhWork", "scworki", "Heiqk", "WpHjob", "DiSex", "hepsyde", "hepsyan", "scptrd", "scptrc", "scptra", "scworka", "scqolr")
vars7 <- c("idauniq", "samptyp", "iintdatm", "iintdaty", "couple", "DhWork", "scworki", "Heiqk", "WpHjob", "DiSex", "hepsyde", "hepsyan", "scptrd", "scptrc", "scptra", "scworka", "scqolr")
vars8 <- c("idauniq", "samptyp", "iintdatm", "iintdaty", "couple", "dhwork", "scworki", "heiqk", "wphjob", "indsex", "hepsyde", "hepsyan", "scprtd", "scprtc", "scprta", "scworka", "scqolr")
vars9 <- c("idauniq", "samptyp", "iintdatm", "iintdaty", "couple", "dhwork", "scworki", "heiqk", "wphjob", "indsex", "hepsyde", "hepsyan", "scprtd", "scprtc", "scprta", "scworka", "scqolr")

# import data using relative link and select some variables
us5 <- read_dta("./data/wave_5.dta", col_select = vars5)
us6 <- read_dta("./data/wave_6.dta", col_select = vars6)
us7 <- read_dta("./data/wave_7.dta", col_select = vars7)
us8 <- read_dta("./data/wave_8.dta", col_select = vars8)
us9 <- read_dta("./data/wave_9.dta", col_select = vars9)

# see first few cases and variables
head(us5)
head(us6)
head(us7)
head(us8)
head(us9)

# rename variables so all datatsets follow same naming conventions
us6 <- us6 %>%
  rename(
    dhwork = DhWork,
    heiqk = Heiqk,
    wphjob = WpHjob,
    disex = DiSex
  )
us7 <- us7 %>%
  rename(
    dhwork = DhWork,
    heiqk = Heiqk,
    wphjob = WpHjob,
    disex = DiSex
  )
us8 <- us8 %>%
  rename(
    scptrd = scprtd, 
    scptrc = scprtc, 
    scptra = scprta,
    disex = indsex
  )
us9 <- us9 %>%
  rename(
    scptrd = scprtd, 
    scptrc = scprtc, 
    scptra = scprta,
    disex = indsex
  )
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

# format of samptyp_5 is not same as other waves, it should be encoded in numbers by mapping
label_mapping <- c("-1 New partner, not known at time of sampling" = -1, "CM" = 1, "CP" = 2, "NP" = 3, "YP" = 4, "OP" = 5, "SM" = 6)
us$samptyp_5 <- label_mapping[us$samptyp_5]

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


# -------------
# Data cleaning
# -------------

# remove -1 values from samptyp
usl <- usl[usl$samptyp != -1, ]
count(usl, samptyp)

# removing unwanted values from dhwork
usl <- subset(usl, dhwork != -9 & dhwork != -8 & dhwork != -1)
count(usl, dhwork)

# removing unwanted values from scworki
usl <- subset(usl, !(scworki %in% c(-9, -3, -2, -1)))
count(usl, scworki)

# removing unwanted values from wphjob
usl <- subset(usl, !(wphjob %in% c(-8, -1)))
count(usl, wphjob)

# removing unwanted values from sex and map 1 to Male and 2 to Female
usl <- subset(usl, !is.na(disex))
usl$disex <- ifelse(usl$disex == 1, "Male", ifelse(usl$disex == 2, "Female", usl$disex))
count(usl, disex)

# remove unwanted values from "hepsyde", "hepsyan", "scptrd", "scptrc", "scptra", "scworka", "scqolr"
count(usl, hepsyan)

# save the marged and cleaned long format data
save(usl, file = "./data/us_clean.RData")

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
