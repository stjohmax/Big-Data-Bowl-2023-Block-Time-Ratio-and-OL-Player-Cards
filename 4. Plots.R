library(ggridges)
overallPerformanceForPlots = blockTimeAwarded %>%
  group_by(offId) %>%
  summarise(blocks = n(), blockPerformance = (sum(blockTimeAwarded[PRWin == 0]) + 
                                                sum(blockTimeAwarded[PRWin == 1 & n >= 25]))/
              (sum(blockTimeAwarded[PRWin == 0]) + 
                 sum(blockTimeAwarded[PRWin == 1 & n >= 25]) +
                 sum(blockTimeAwarded[PRWin == 1 & n < 25]))
            # , opponentStrength = mean(rushPerformance)
  )

performanceMergedForPlots = players %>%
  inner_join(overallPerformanceForPlots,
             by = c("nflId" = "offId")) %>%
  filter(officialPosition %in% c("T", "C", "G")) %>%
  mutate(isTackle = ifelse(officialPosition == "T", T, F)) %>%
  inner_join(singlePlayerColorLogo)

positionList = c()
blockPerfList = c()

for(i in 1:length(performanceMergedForPlots$nflId)){
  position = rep(performanceMergedForPlots$officialPosition[i], performanceMergedForPlots$blocks[i])
  blockPerformance = rep(performanceMergedForPlots$blockPerformance[i], performanceMergedForPlots$blocks[i])
  positionList = c(positionList, position)
  blockPerfList = c(blockPerfList, blockPerformance)
}

distTibble = tibble(`Position` = positionList, `Block Time` = blockPerfList)

BTRposDist = ggplot(distTibble, aes(`Block Time`, y = `Position`, height = ..density.., fill = `Position`)) +
  geom_joy(scale = 1.5) +
  xlim(.5, 1) +
  theme_joy() +
  xlab("Block Time Ratio") +
  ggtitle("Block Time Ratio Distributions by Position") +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = .5),
        axis.title.y = element_text(hjust = .5),
        plot.title = element_text(face = "bold", hjust = 0.5))

epaData = read_csv("/Users/maxstjohn/Downloads/rbsdm.comstats (7).csv") %>%
  select(Abbr, `Dropback EPA`)

epaBlockData = performanceMergedForEPA %>%
  mutate(nTimesBlockPerformance = blocks*blockPerformance) %>%
  group_by(possessionTeam, possTeamLogo) %>%
  summarise(n = sum(blocks), perf = sum(nTimesBlockPerformance),
            overallBlockTime = perf/n) %>%
  inner_join(epaData, by = c("possessionTeam" = "Abbr"))

#cor(y$overallBlockTime, y$`Dropback EPA`)

EPAvsBlockTimePlot = ggplot(epaBlockData, aes(overallBlockTime, `Dropback EPA`)) +
  geom_image(aes(image = possTeamLogo), size = 0.04, by = "width", asp = 1.618) +
  geom_smooth(method = "lm", linetype = "dashed", se = F, color = "black") +
  xlab("Overall Block Time Ratio") +
  theme_bw() +
  ggtitle("Dropback EPA vs Overall Block Time Ratio") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Tackle Bar Chart
tackleBar = ggplot(tackleAdjustedBlockDifficulty %>% top_n(15, blockPerformance),
                   aes(x = reorder(displayName, blockPerformance),
                       y = blockPerformance)) +
  geom_bar(stat = "identity", aes(fill = possTeamColor, color = "black")) +
  geom_text(aes(x = displayName, y = blockPerformance - .025, label = round(blockPerformance, 2)),
            color = "white") +
  scale_color_identity(aesthetics = c("color", "fill")) +
  coord_flip() +
  xlab("Player Name") +
  ylab("Block Time Ratio") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  ggtitle("Top 15 Tackle Block Time Ratio")



defensivePlayerScoresColors = defensivePlayerScores %>%
  inner_join(defPlayerColorData)

RTRbar = ggplot(defensivePlayerScoresColors %>% top_n(-15, rushPerformance), 
       aes(x = reorder(displayName, -rushPerformance),
           y = rushPerformance)) +
  geom_bar(stat = "identity", aes(fill = team_color, color = "black")) +
  geom_text(aes(x = displayName, y = rushPerformance - .025, label = round(rushPerformance, 2)),
            color = "white") +
  scale_color_identity(aesthetics = c("color", "fill")) +
  coord_flip() +
  xlab("Player Name") +
  ylab("Rush Time Ratio") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  ggtitle("Top 15 Rush Time Ratio")

tackleScatter = ggplot(tackleAdjustedBlockDifficulty,
                       aes(x = meanOppStrength, y = adjustedBlockPerformance)) +
  geom_point(aes(color = possTeamColor)) +
  scale_color_identity() +
  ggrepel::geom_text_repel(aes(label = displayName),
                           box.padding = .25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(x = .78, y = -.11, label = " More Difficult \n Matchups", color = "black")) +
  geom_text(aes(x = .84, y = -.11, label = " Easier \n Matchups", color = "black")) +
  xlab("Matchup Difficulty") +
  ylab("Adjusted Block Time") +
  ylim(c(-.125, .125)) +
  ggtitle("Tackle Adjusted Block Time vs Matchup Difficulty") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))