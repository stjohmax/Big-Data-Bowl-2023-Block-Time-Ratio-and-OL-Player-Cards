library(tidyverse)

games = read_csv("Data/games.csv")
PFFScoutingData = read_csv("Data/PFFScoutingData.csv")
players = read_csv("Data/players.csv") %>%
  select(nflId, officialPosition, displayName)
plays = read_csv("Data/plays.csv")

trackingData = tibble()
for(i in 1:8){
  data = read_csv(paste0("Data/week", i, ".csv"))
  trackingData = bind_rows(trackingData, data)
}

# Joining data into single tibble

# Joining plays and PFF data
plays_pff_joined = full_join(plays,
                              PFFScoutingData)

# Joining plays, PFF, and game data
games_plays_pff_joined = full_join(games,
                                    plays_pff_joined)

# Joining plays, PFF, and game data with tracking data
trackingData = trackingData %>%
  full_join(games_plays_pff_joined) %>%
  full_join(players)

# Remove unmerged data
remove(games, games_plays_pff_joined, PFFScoutingData, plays, plays_pff_joined, data)

# Cleaning tracking data

# Reorienting plays so data has them moving left to right
trackingData <- trackingData %>%
  mutate(x = ifelse(playDirection == "left", 120-x, x),
         y = ifelse(playDirection == "left", 160/3 - y, y),
         o = ifelse(playDirection == "left", (360- o)*(pi/180), o*(pi/180)),
         dir = ifelse(playDirection == "left", (360- dir)*(pi/180), dir*(pi/180)))

footballLocations = trackingData %>%
  filter(is.na(nflId))
