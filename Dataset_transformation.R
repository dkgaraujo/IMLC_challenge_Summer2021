# This code documents how the ML challenge dataset was created.
# The dataset used in this challenge is a simplified version of a more complete Olympics dataset.
# The original source for this data is found here: https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results
# (obs.: a free Kaggle subscription is needed to access this page).
# You need to download the original dataset and save in the same folder a this script.

library(tidyverse)

olympics_full_ds <- read_csv("athlete_events.csv")

ds <- olympics_full_ds %>%
  filter(Season == "Summer") %>%
  mutate(Athlete = paste(Name, Sport, sep = "_")) %>%
  group_by(Athlete, Year) %>%
  summarise(NOC = unique(NOC)[1],
            Participation = 1,
            NumEvents = n(),
            FemaleSex = mean(ifelse(Sex == "F", 1, 0)),
            Age = mean(Age, na.rm = TRUE) %>% as.integer(),
            Height = mean(Height, na.rm = TRUE) %>% as.integer(),
            Weight = mean(Weight, na.rm = TRUE) %>% as.integer(),
            MedalAchievement = sum(!is.na(Medal)) > 0,
            GoldMedal = sum("Gold" %in% Medal) > 0,
            SilverMedal = sum("Silver" %in% Medal) > 0,
            BronzeMedal = sum("Bronze" %in% Medal) > 0
  ) %>%
  ungroup() %>%
  group_by(Athlete) %>%
  mutate(OlympicExperience = cumsum(Participation),
         pastAchiev = lag(MedalAchievement, order_by = Year),
         pastAchiev = ifelse(is.na(pastAchiev), FALSE, pastAchiev),
         pastMedals = lag(cumsum(MedalAchievement), order_by = Year),
         pastMedals = ifelse(is.na(pastMedals), 0, pastMedals)) %>%
  ungroup() %>%
  group_by(NOC, Year) %>%
  summarise(NumAthletes = n(),
            PctFemale = sum(FemaleSex) / n(),
            AvgAge = mean(Age, na.rm = TRUE),
            AvgHeight = mean(Height, na.rm = TRUE),
            AvgWeight = mean(Weight, na.rm = TRUE),
            AvgOlympicExp = mean(OlympicExperience, na.rm = TRUE),
            PctAchievPrevGames = mean(pastAchiev, na.rm = TRUE),
            PctPastMedals = mean(pastMedals, na.rm = TRUE),
            Target = sum(GoldMedal, SilverMedal, BronzeMedal) / NumAthletes)

leaderboard_ds <- ds %>% filter(Year == 2016)
ds <- ds %>% filter(Year < 2016)

write_csv(ds, "olympics_challenge_dataset.csv")
write_csv(leaderboard_ds, "olympics_challenge_lb_dataset.csv")

