# Add binary variable for if player is on offense
trackingData = trackingData %>%
  mutate(offense = ifelse(team == possessionTeam, T, F))

# Find closest defender if player is pass blocking

# Find separation from all defenders for pass blockers
addSeparation <- function(data){
  offenseLocationData <- data %>%
    filter(offense == T
           # , pff_role == "Pass Block"
           ) %>%
    dplyr::select(gameId, playId, frameId, offId = nflId, offX = x, offY = y)
  
  defenseLocationData <- data %>%
    filter(offense == F) %>%
    dplyr::select(gameId, playId, frameId, defId = nflId, defX = x, defY = y, defO = o)
  
  locationMerged <- inner_join(defenseLocationData, offenseLocationData,
                       by = c("gameId" = "gameId",
                              "playId" = "playId",
                              "frameId" = "frameId"))
  
  distanceToOppDf <- locationMerged %>%
    mutate(distToOpponent = sqrt((offX - defX)^2 + (offY - defY)^2))
  
  return(distanceToOppDf)
  
}

separationData = addSeparation(trackingData)

# Find closest defender for pass blockers and merge with tracking data
minSepDef = separationData %>%
  group_by(gameId, playId, frameId, defId) %>%
  filter(distToOpponent == min(distToOpponent)) %>%
  select(-c(defX, defY))

trackingData = trackingData %>%
  full_join(minSepDef,
            by = c("gameId" = "gameId",
                   "playId" = "playId",
                   "frameId" = "frameId",
                   "nflId" = "defId"))

secondMinSepDef = separationData %>%
  group_by(gameId, playId, frameId, defId) %>%
  filter(distToOpponent == min(distToOpponent[distToOpponent != min(distToOpponent)])) %>%
  select(gameId, playId, frameId, defId, offId2 = offId, x2 = offX, y2 = offY, distToOpponent)

trackingData = trackingData %>%
  full_join(secondMinSepDef,
            by = c("gameId" = "gameId",
                   "playId" = "playId",
                   "frameId" = "frameId",
                   "nflId" = "defId")) %>%
  rename(distToOpponent1 = distToOpponent.x,
         distToOpponent2 = distToOpponent.y)

remove(minSepDef, separationData)

# Merging QB location for each frame
qbTrackingData = trackingData %>%
  select(gameId, playId, frameId, possessionTeam, defensiveTeam, officialPosition, qbX = x, qbY = y) %>%
  filter(officialPosition == "QB") %>%
  select(-officialPosition)

# Distance between QB and Players
trackingData = trackingData %>%
  inner_join(qbTrackingData) %>%
  mutate(distToQB = sqrt((qbX - x)^2 + (qbY - y)^2),
         off1DistToQB = sqrt((qbX - offX)^2 + (qbY - offY)^2),
         off2DistToQB = sqrt((qbX - x2)^2 + (qbY - y2)^2),
         off1CloserToQB = ifelse(off1DistToQB < distToQB, T, F),
         off2CloserToQB = ifelse(off2DistToQB < distToQB, T, F))

remove(qbTrackingData)

