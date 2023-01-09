library(fmsb)
colorsLogos = nflfastR::teams_colors_logos %>%
  select(team_abbr, team_color, team_logo_wikipedia)

PRwinData = trackingData %>%
  filter(pff_role == "Pass Rush",
         frameId >= 6) %>%
  mutate(engaged1 = ifelse(distToOpponent1 <= 1.5, T, F),
         engaged2 = ifelse(distToOpponent2 <= 1, T, F),
         doubleTeam = ifelse(engaged1 + engaged2 == 2, T, F),
         proxToQB = ifelse(distToQB < 5, T, F),
         PRWin = ifelse(proxToQB == F, F,
                        ifelse(doubleTeam == T & off1CloserToQB == F & off2CloserToQB == F, T,
                  ifelse(doubleTeam == F & off1CloserToQB == F & engaged1 == T, T, F)))) %>%
  select(gameId, playId, week, possessionTeam, defensiveTeam, nflId, officialPosition, displayName,
         frameId, x, y, offId:off2CloserToQB, engaged1, engaged2, doubleTeam, PRWin)

playerColorData = PRwinData %>%
  select(gameId, offId, week, possessionTeam, defensiveTeam) %>%
  unique() %>%
  inner_join(colorsLogos, by = c("possessionTeam" = "team_abbr")) %>%
  rename(possTeamColor = team_color, possTeamLogo = team_logo_wikipedia) %>%
  inner_join(colorsLogos, by = c("defensiveTeam" = "team_abbr")) %>%
  rename(defTeamColor = team_color, defTeamLogo = team_logo_wikipedia, nflId = offId)

defPlayerColorData = PRwinData %>%
  select(nflId, defensiveTeam) %>%
  unique() %>%
  inner_join(colorsLogos, by = c("defensiveTeam" = "team_abbr")) 

blockTypeData = trackingData %>%
  filter(!is.na(pff_blockType)) %>%
  select(gameId, playId, nflId, frameId, pff_blockType)

PRWinBlockType = PRwinData %>%
  full_join(blockTypeData,
             by = c("gameId" = "gameId",
                    "playId" = "playId",
                    "frameId" = "frameId",
                    "offId" = "nflId"))

playerPerformance = PRWinBlockType %>%
  group_by(gameId, playId, nflId, displayName, offId) %>%
  mutate(firstEngagedTime = min(frameId[engaged1 == T]),
         lastEngagedTime = max(frameId[engaged1 == T]),
         frameBeat = min(frameId[PRWin == T]),
         lastFrame = max(frameId)) %>%
  filter(frameId >= firstEngagedTime,
         frameBeat >= frameId) %>%
  summarise(n = n(), timeDT = sum(doubleTeam),PRWin = sum(PRWin), firstFrame = min(frameId),
            lastFrame = max(frameId),
            blockType = pff_blockType[frameId == firstFrame])

playerPerformanceFiltered = playerPerformance %>%
  group_by(gameId, playId, nflId) %>%
  mutate(winFrame = min(lastFrame[PRWin == 1])) %>%
  filter(lastFrame <= winFrame) %>%
  unique()


blockTimeAwarded = playerPerformanceFiltered %>%
  filter(PRWin <= 1) %>%
  mutate(diffInFrame = lastFrame - firstFrame + 1) %>%
  filter(diffInFrame == n) %>%
  group_by(gameId, playId, offId, blockType) %>%
  filter(lastFrame <= min(winFrame[PRWin == 1])) %>%
  summarise(n = sum(n), timeDT = sum(timeDT)/2, PRWin = min(sum(PRWin), 1), n = n - timeDT,
            blockTimeAwarded = ifelse(PRWin == 0 & n <= 25, n/10,
                                   ifelse(PRWin == 0 & n > 25,n/10,
                                   ifelse(PRWin == 1 & n >= 25, n/10, 2.5 - n/10))),
            blockTimeAwarded = min(blockTimeAwarded, 5))

rushTimeAwarded = playerPerformanceFiltered %>%
  filter(PRWin <= 1) %>%
  mutate(diffInFrame = lastFrame - firstFrame + 1) %>%
  filter(diffInFrame == n) %>%
  group_by(gameId, playId, nflId) %>%
  filter(lastFrame <= min(winFrame[PRWin == 1])) %>%
  summarise(n = sum(n), timeDT = sum(timeDT)/2, PRWin = min(sum(PRWin), 1), n = n - timeDT,
            rushTimeAwarded = ifelse(PRWin == 0 & n <= 25, n/10,
                                      ifelse(PRWin == 0 & n > 25, n/10,
                                             ifelse(PRWin == 1 & n >= 25, n/10, 2.5 - n/10))),
            rushTimeAwarded = min(rushTimeAwarded, 5))

overallPerformanceRushers = rushTimeAwarded %>%
  group_by(nflId) %>%
  summarise(blocks = n(), rushPerformance = (sum(rushTimeAwarded[PRWin == 0]) +
                                                   sum(rushTimeAwarded[PRWin == 1 & n >= 25]))/
              (sum(rushTimeAwarded[PRWin == 0]) +
                 sum(rushTimeAwarded[PRWin == 1 & n >= 25]) +
                 sum(rushTimeAwarded[PRWin == 1 & n < 25]))) %>%
  filter(blocks >= 70)

defensivePlayerScores = players %>%
  inner_join(overallPerformanceRushers) %>%
  filter(officialPosition %in% c("DE", "DT", "NT", "OLB")) %>%
  mutate(interiorRusher = ifelse(officialPosition %in% c("DT", "NT"), T, F))


blockPerformance = blockTimeAwarded %>%
  filter(blockType %in% c("PA", "PP", "SW")) %>%
  group_by(offId, blockType) %>%
  summarise(blocks = n(), blockPerformance = (sum(blockTimeAwarded[PRWin == 0]) + 
              sum(blockTimeAwarded[PRWin == 1 & n >= 25]))/
              (sum(blockTimeAwarded[PRWin == 0]) + 
                 sum(blockTimeAwarded[PRWin == 1 & n >= 25]) +
                 sum(blockTimeAwarded[PRWin == 1 & n < 25]))) %>%
  filter(blocks >= 15) %>%
  pivot_wider(id_cols = offId, names_from = blockType, values_from = c("blocks", "blockPerformance"))


overallPerformance = blockTimeAwarded %>%
  # inner_join(defensivePlayerScores, by = c("nflId" = "nflId")) %>%
  group_by(offId) %>%
  summarise(blocks = n(), blockPerformance = (sum(blockTimeAwarded[PRWin == 0]) + 
                                                sum(blockTimeAwarded[PRWin == 1 & n >= 25]))/
              (sum(blockTimeAwarded[PRWin == 0]) + 
                 sum(blockTimeAwarded[PRWin == 1 & n >= 25]) +
                 sum(blockTimeAwarded[PRWin == 1 & n < 25]))
            # , opponentStrength = mean(rushPerformance)
            ) %>%
  filter(blocks >= 30)

performanceMerged = players %>%
  inner_join(overallPerformance,
             by = c("nflId" = "offId")) %>%
  inner_join(blockPerformance,
             by = c("nflId" = "offId")) %>%
  filter(officialPosition %in% c("T", "C", "G")) %>%
  mutate(isTackle = ifelse(officialPosition == "T", T, F))

overallPerformanceByPosition = performanceMerged %>%
  group_by(isTackle) %>%
  mutate(Overall_Grade = percentile(blockPerformance)) %>%
  ungroup() %>%
  select(nflId, officialPosition, displayName, isTackle, blocks, blockPerformance,
         Overall_Grade)

PPPerformanceByPosition = performanceMerged %>%
  select(nflId, officialPosition, displayName, isTackle, blocks_PP, blockPerformance_PP) %>%
  na.omit %>%
  group_by(isTackle) %>%
  mutate(PP_Grade = percentile(blockPerformance_PP)) %>%
  ungroup()

PAPerformanceByPosition = performanceMerged %>%
  select(nflId, officialPosition, displayName, isTackle, blocks_PA, blockPerformance_PA) %>%
  na.omit %>%
  group_by(isTackle) %>%
  mutate(PA_Grade = percentile(blockPerformance_PA)) %>%
  ungroup()

SWPerformanceByPosition = performanceMerged %>%
  select(nflId, officialPosition, displayName, isTackle, blocks_SW, blockPerformance_SW) %>%
  na.omit %>%
  group_by(isTackle) %>%
  mutate(SW_Grade = percentile(blockPerformance_SW)) %>%
  ungroup()

finalPerformance = overallPerformanceByPosition %>%
  full_join(PPPerformanceByPosition) %>%
  full_join(PAPerformanceByPosition) %>%
  full_join(SWPerformanceByPosition)

tacklePerformance = finalPerformance %>%
  filter(officialPosition == "T")

guardCenterPerformance = finalPerformance %>%
  filter(officialPosition != "T")

## Game by Game Breakdown

overallPerformanceByGame = blockTimeAwarded %>%
  group_by(gameId, offId) %>%
  summarise(blocks = n(), blockPerformance = (sum(blockTimeAwarded[PRWin == 0]) + 
                                                sum(blockTimeAwarded[PRWin == 1 & n >= 25]))/
              (sum(blockTimeAwarded[PRWin == 0]) + 
                 sum(blockTimeAwarded[PRWin == 1 & n >= 25]) +
                 sum(blockTimeAwarded[PRWin == 1 & n < 25]))) %>%
  filter(blocks >= 5)

performanceMergedByGame = players %>%
  inner_join(overallPerformanceByGame,
             by = c("nflId" = "offId")) %>%
  filter(officialPosition %in% c("T", "C", "G")) %>%
  mutate(isTackle = ifelse(officialPosition == "T", T, F))

performanceByPositionByGame = performanceMergedByGame %>%
  group_by(isTackle) %>%
  mutate(Game_Grade = percentile(blockPerformance),
         Opponent_Strength = percentile(blockPerformance)) %>%
  inner_join(playerColorData, by = c("gameId" = "gameId", "nflId" = "nflId"))

tacklePerformanceByGame = performanceByPositionByGame %>%
  filter(officialPosition == "T")

guardCenterPerformanceByGame = performanceByPositionByGame %>%
  filter(officialPosition != "T")

positionData = finalPerformance %>%
  select(nflId, isTackle)

doubleTeamRate = playerPerformanceFiltered %>%
  filter(PRWin <= 1) %>%
  mutate(diffInFrame = lastFrame - firstFrame + 1) %>%
  filter(diffInFrame == n) %>%
  mutate(dtBlock = ifelse(timeDT/n >= .5, T, F)) %>%
  group_by(gameId, playId, offId) %>%
  filter(firstFrame == min(firstFrame)) %>%
  ungroup() %>%
  group_by(offId) %>%
  summarise(n = n(), dtRate = round(sum(dtBlock)/n, 2)) %>%
  filter(n >= 100)

nSnaps = trackingData %>%
  filter(frameId == 1) %>%
  select(gameId, playId, nflId) %>%
  group_by(nflId) %>%
  summarise(snaps = n())

timeToThrow = trackingData %>%
  select(gameId, playId, nflId, frameId) %>%
  ungroup() %>%
  group_by(gameId, playId) %>%
  filter(frameId == max(frameId)) %>%
  ungroup() %>%
  group_by(nflId) %>%
  summarise(meanTimeToThrow = mean(frameId) - 10)

additionalScores = players %>%
  inner_join(doubleTeamRate, by = c("nflId" = "offId")) %>%
  inner_join(nSnaps) %>%
  inner_join(timeToThrow) %>%
  mutate(isTackle = ifelse(officialPosition == "T", T, F)) %>%
  filter(officialPosition != "TE") %>%
  group_by(isTackle) %>%
  mutate(dtGrade = percentile(dtRate),
         snapGrade = percentile(snaps),
         tttGrade = percentile(meanTimeToThrow)) 

# Adjusting for Matchup Difficulty

performanceOppMerged = playerPerformanceFiltered %>%
  filter(PRWin <= 1) %>%
  mutate(diffInFrame = lastFrame - firstFrame + 1) %>%
  filter(diffInFrame == n) %>%
  select(gameId, playId, nflId, offId) %>%
  ungroup() %>%
  full_join(overallPerformanceRushers %>% select(-blocks))

avgRushPerformance = sum(overallPerformanceRushers$blocks * overallPerformanceRushers$rushPerformance)/
  sum(overallPerformanceRushers$blocks)

blockDifficultySummary = performanceOppMerged %>%
  mutate(rushPerformance = ifelse(is.na(rushPerformance), avgRushPerformance, rushPerformance)) %>%
  ungroup() %>%
  group_by(gameId, playId, offId) %>%
  mutate(n = 1/n(), rushPerformance = rushPerformance * n) %>%
  summarise(rushPerformance = sum(rushPerformance)) %>%
  ungroup() %>%
  group_by(offId) %>%
  summarise(meanOppStrength = mean(rushPerformance))

adjustedBlockDifficulty = overallPerformance %>%
  inner_join(blockDifficultySummary) %>%
  mutate(adjustedBlockPerformance = blockPerformance - meanOppStrength) %>%
  filter(blocks >= 150)

adjustedBlockDifficultyNames = players %>%
  inner_join(adjustedBlockDifficulty, by = c("nflId" = "offId"))

tackleAdjustedBlockDifficulty = adjustedBlockDifficultyNames %>%
  filter(officialPosition == "T") %>%
  mutate(meanOppStrength = meanOppStrength - 0.0279225,
         adjustedBlockPerformance = blockPerformance - meanOppStrength,
         rawScore = percentile(blockPerformance),
         assignmentDifficultyScore = percentile(meanOppStrength),
         overallScore = percentile(adjustedBlockPerformance)) %>%
  inner_join(playerColorData %>% select(nflId, possTeamColor) %>% unique())


guardCenterAdjustedBlockDifficulty = adjustedBlockDifficultyNames %>%
  filter(officialPosition %in% c("G", "C")) %>%
  mutate(rawScore = percentile(blockPerformance),
         assignmentDifficultyScore = percentile(meanOppStrength),
         overallScore = percentile(adjustedBlockPerformance)) %>%
  inner_join(playerColorData %>% select(nflId, possTeamColor) %>% unique())

mergedAdjustedBlockDifficulty = bind_rows(tackleAdjustedBlockDifficulty, guardCenterAdjustedBlockDifficulty) %>%
  mutate(assignmentDifficultyScore = abs(assignmentDifficultyScore - 100))

# By game

blockDifficultySummaryGame = performanceOppMerged %>%
  mutate(rushPerformance = ifelse(is.na(rushPerformance), avgRushPerformance, rushPerformance)) %>%
  ungroup() %>%
  group_by(gameId,offId) %>%
  summarise(meanOppStrength = mean(rushPerformance) )

adjustedBlockDifficultyGame = blockDifficultySummaryGame %>%
  inner_join(overallPerformance) %>%
  mutate(adjustedBlockPerformance = blockPerformance - meanOppStrength)

adjustedBlockDifficultyByGame = players %>%
  inner_join(adjustedBlockDifficultyGame, by = c("nflId" = "offId")) %>%
  inner_join(playerColorData, by = c("gameId" = "gameId", "nflId" = "nflId")) %>%
  filter(officialPosition %in% c("T", "C", "G")) %>%
  mutate(meanOppStrength = ifelse(officialPosition == "T", meanOppStrength - 0.0279225, meanOppStrength),
         adjustedBlockPerformance = blockPerformance - meanOppStrength)

  

