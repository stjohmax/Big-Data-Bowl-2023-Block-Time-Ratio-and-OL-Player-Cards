library(ggimage)
library(ggpubr)
scoreBarChart = function(id){
  playerData = mergedAdjustedBlockDifficulty %>%
    filter(nflId == id)
  overallScore = playerData$overallScore
  rawScore = playerData$rawScore
  assignmentDifficultyScore = playerData$assignmentDifficultyScore
  data = tibble(metric = c("Overall Score", "Raw Score", "Assignment \n Difficulty Score"),
                score = c(overallScore, rawScore, assignmentDifficultyScore))
  
  blank = ggplot() +
    theme_nothing()
  
  plot = ggplot() +
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 1.25, ymax = 2.25, fill = overallScore)) +
    geom_rect(aes(xmin = -.75, xmax = .25, ymin = 0, ymax = 1, fill = rawScore)) +
    geom_rect(aes(xmin = .75, xmax = 1.75, ymin = 0, ymax = 1, fill = assignmentDifficultyScore)) +
    scale_fill_gradientn(colors = c("#FF0000","#D3D3D3", "#004FBF"),
                         limits = c(0, 100),
                         breaks = c(0, 50, 100),
                         guide = "none") +
    annotate("text", x = .5, y = 1.75, label = paste0(overallScore, "%"), 
             color = "white", size = 7.5, fontface = "bold") +
    annotate("text", x = -.25, y = .5, label = paste0(rawScore, "%"), 
             color = "white", size = 7.5, fontface = "bold") +
    annotate("text", x = 1.25, y = .5, label = paste0(assignmentDifficultyScore, "%"), 
             color = "white", size = 7.5, fontface = "bold") +
    annotate("text", x = .5, y = 2.35, label = "Overall Score", size = 3.5, fontface = "bold") +
    annotate("text", x = -.25, y = 1.1, label = "Raw Score", size = 3.5, fontface = "bold") +
    annotate("text", x = 1.25, y = 1.1, label = "Matchup Difficulty", size = 3.5, fontface = "bold") +
    scale_color_identity() +
    theme_nothing()
  
  x = ggarrange(NULL, plot, NULL, ncol = 3, nrow = 1, widths = c(.25, 1, .25))
  ggarrange(NULL, x, NULL, ncol = 1, nrow = 3, heights = c(.1, 1, .1))
    
}

gameByGameChart = function(id){
  playerData = adjustedBlockDifficultyByGame %>% 
    filter(nflId == id)
  
  ggplot(playerData, aes(week, adjustedBlockPerformance)) +
    geom_segment(aes(x = 1, xend = 8, y = 0, yend = 0), linetype = "dashed") +
    geom_line(aes()) +
    ggimage::geom_image(aes(image = defTeamLogo), size = 0.07, by = "width", asp = 1.618) +
    theme_bw()+
    scale_y_continuous(limits = c(-.15, .15)) +
    scale_x_continuous(limits = c(1, 8), breaks = c(1:8)) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    ylab("Adjusted Block Time") +
    ggtitle("Adjusted Block Time by Game")
}

blockGradeLollipop = function(id){
  playerData = finalPerformance %>% filter(nflId == id)
  PP_Grade = playerData$PP_Grade
  PA_Grade = playerData$PA_Grade
  SW_Grade = playerData$SW_Grade
  color = mergedAdjustedBlockDifficulty$possTeamColor[mergedAdjustedBlockDifficulty$nflId == id]
  plotTibble = tibble(blockType = c("Standard Block", "PA Block", "Switch Block"),
                      blockGrade = c(PP_Grade, PA_Grade, SW_Grade))
  
  p1 = ggplot(plotTibble %>% filter(blockType == "Standard Block"), aes(blockType, blockGrade)) +
    geom_segment(aes(x = blockType, xend = blockType, y = -1, yend = 101),
                 color = "gray", lwd = 1) +
    geom_point(aes(x = blockType, y = -1), color = "gray", size = 2) +
    geom_point(aes(x = blockType, y = 50), color = "gray", size = 2) +
    geom_point(aes(x = blockType, y = 101), color = "gray", size = 2) +
    geom_point(size = 5.5, pch = 21, aes(fill = blockGrade)) +
    scale_fill_gradientn(colors = c("#FF0000","#D3D3D3", "#004FBF"),
                         limits = c(0, 100),
                         breaks = c(0, 50, 100),
                         guide = "none") +
    geom_text(aes(label = blockGrade), color = ifelse(PP_Grade >= 75 | PP_Grade <= 25, "white", "black"), 
              size = 2.5, fontface = "bold") +
    geom_text(aes(y = 50,label = blockType), color = "black", vjust = -2, fontface = "bold") +
    geom_text(aes(y = 4, label = "Bad"), color = "black", vjust = -2.25, size = 3, fontface = "italic") +
    geom_text(aes(y = 96, label = "Good"), color = "black", vjust = -2.25, size = 3, fontface = "italic") +
    coord_flip() +
    theme_nothing()
  
  p2 = ggplot(plotTibble %>% filter(blockType == "PA Block"), aes(blockType, blockGrade)) +
    geom_segment(aes(x = blockType, xend = blockType, y = -1, yend = 101),
                 color = "gray", lwd = 1) +
    geom_point(aes(x = blockType, y = -1), color = "gray", size = 2) +
    geom_point(aes(x = blockType, y = 50), color = "gray", size = 2) +
    geom_point(aes(x = blockType, y = 101), color = "gray", size = 2) +
    geom_point(size = 5.5, pch = 21, aes(fill = blockGrade)) +
    scale_fill_gradientn(colors = c("#FF0000","#D3D3D3", "#004FBF"),
                         limits = c(0, 100),
                         breaks = c(0, 50, 100),
                         guide = "none") +
    geom_text(aes(label = blockGrade), color = ifelse(PA_Grade >= 75 | PA_Grade <= 25, "white", "black"), 
              size = 2.5, fontface = "bold") +
    geom_text(aes(y = 50,label = blockType), color = "black", vjust = -2, fontface = "bold") +
    coord_flip() +
    scale_color_identity() +
    theme_nothing()
  
  p3 = ggplot(plotTibble %>% filter(blockType == "Switch Block"), aes(blockType, blockGrade)) +
    geom_segment(aes(x = blockType, xend = blockType, y = -1, yend = 101),
                 color = "gray", lwd = 1) +
    geom_point(aes(x = blockType, y = -1), color = "gray", size = 2) +
    geom_point(aes(x = blockType, y = 50), color = "gray", size = 2) +
    geom_point(aes(x = blockType, y = 101), color = "gray", size = 2) +
    geom_point(size = 5.5, pch = 21, aes(fill = blockGrade)) +
    scale_fill_gradientn(colors = c("#FF0000","#D3D3D3", "#004FBF"),
                         limits = c(0, 100),
                         breaks = c(0, 50, 100),
                         guide = "none") +
    geom_text(aes(label = blockGrade), color = ifelse(SW_Grade >= 75 | SW_Grade <= 25, "white", "black"), 
              size = 2.5, fontface = "bold") +
    geom_text(aes(y = 50,label = blockType), color = "black", vjust = -2, fontface = "bold") +
    coord_flip() +
    scale_color_identity() +
    theme_nothing()
  
  ggarrange(p1, p2, p3, ncol = 1, nrow = 3)
  
}

additionalStatsLollipop = function(id){
  playerData = additionalScores %>% filter(nflId == id)
  dtGrade= playerData$dtGrade
  snapGrade = playerData$snapGrade
  tttGrade = playerData$tttGrade
  dtRate = paste0(round(playerData$dtRate, 2) * 100, "%")
  snaps = playerData$snaps
  ttt = round(playerData$meanTimeToThrow/10, 1)
  color = mergedAdjustedBlockDifficulty$possTeamColor[mergedAdjustedBlockDifficulty$nflId == id]
  plotTibble = tibble(metric = c("Snaps", "Protection Time", "Double Team Rate"),
                      rawData = c(snaps, ttt, dtRate),
                      grade = c(snapGrade, tttGrade, dtGrade)) 
  
  p1 = ggplot(plotTibble %>% filter(metric == "Snaps"), aes(metric, grade)) +
    geom_segment(aes(x = metric, xend = metric, y = -1, yend = 101),
                 color = "gray", lwd = 1) +
    geom_point(aes(x = metric, y = -1), color = "gray", size = 2) +
    geom_point(aes(x = metric, y = 50), color = "gray", size = 2) +
    geom_point(aes(x = metric, y = 101), color = "gray", size = 2) +
    geom_point(size = 5.5, pch = 21, aes(fill = "black")) +
    geom_text(aes(label = rawData), color = "white", 
              size = 2.5, fontface = "bold") +
    geom_text(aes(y = 50,label = metric), color = "black", vjust = -2, fontface = "bold") +
    geom_text(aes(y = 4, label = "Low"), color = "black", vjust = -2.25, size = 3, fontface = "italic") +
    geom_text(aes(y = 96, label = "High"), color = "black", vjust = -2.25, size = 3, fontface = "italic") +
    coord_flip() +
    scale_fill_identity() +
    theme_nothing()
  
  p2 = ggplot(plotTibble %>% filter(metric == "Protection Time"), aes(metric, grade)) +
    geom_segment(aes(x = metric, xend = metric, y = -1, yend = 101),
                 color = "gray", lwd = 1) +
    geom_point(aes(x = metric, y = -1), color = "gray", size = 2) +
    geom_point(aes(x = metric, y = 50), color = "gray", size = 2) +
    geom_point(aes(x = metric, y = 101), color = "gray", size = 2) +
    geom_point(size = 5.5, pch = 21, aes(fill = "black")) +
    geom_text(aes(label = rawData), color = "white", 
              size = 2.5, fontface = "bold") +
    geom_text(aes(y = 50,label = metric), color = "black", vjust = -2, fontface = "bold") +
    coord_flip() +
    scale_fill_identity() +
    theme_nothing()
  
  p3 = ggplot(plotTibble %>% filter(metric == "Double Team Rate"), aes(metric, grade)) +
    geom_segment(aes(x = metric, xend = metric, y = -1, yend = 101),
                 color = "gray", lwd = 1) +
    geom_point(aes(x = metric, y = -1), color = "gray", size = 2) +
    geom_point(aes(x = metric, y = 50), color = "gray", size = 2) +
    geom_point(aes(x = metric, y = 101), color = "gray", size = 2) +
    geom_point(size = 5.5, pch = 21, aes(fill = "black")) +
    geom_text(aes(label = rawData), color = "white", 
              size = 2.5, fontface = "bold") +
    geom_text(aes(y = 50,label = metric), color = "black", vjust = -2, fontface = "bold") +
    coord_flip() +
    scale_fill_identity() +
    theme_nothing()
  
  ggarrange(p1, p2, p3, ncol = 1, nrow = 3)
  
}

tacklePlot = ggplot(tackleAdjustedBlockDifficulty, 
                    aes(x = meanOppStrength, y = adjustedBlockPerformance)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Matchup Difficulty") +
  ylab("Adjusted Block Time") +
  ggtitle("Adjusted Block Time vs Matchup Difficulty") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

guardCenterPlot = ggplot(guardCenterAdjustedBlockDifficulty, 
                         aes(x = meanOppStrength, y = adjustedBlockPerformance)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Matchup Difficulty") +
  ylab("Adjusted Block Time") +
  ggtitle("Adjusted Block Time vs Matchup Difficulty") +
  theme_bw()

compPlot = function(id){
  isTackle = finalPerformance$isTackle[finalPerformance$nflId == id]
  if(isTackle == T){
    return(tacklePlot +
             geom_point(aes(color = ifelse(nflId == id, "red", "black"),
                            size = ifelse(nflId == id, 5, 1))) +
             scale_color_identity() +
             theme(legend.position = "none",
                   plot.title = element_text(hjust = 0.5)))
  }
  else{
    return(guardCenterPlot +
             geom_point(aes(color = ifelse(nflId == id, "red", "black"),
                            size = ifelse(nflId == id, 5, 1))) +
             scale_color_identity() +
             theme(legend.position = "none",
                   plot.title = element_text(hjust = 0.5)))
  }
}

singlePlayerColorLogo = playerColorData %>%
  select(nflId, possessionTeam, possTeamColor, possTeamLogo) %>%
  unique()

playerCardFunc = function(id){
  name = players$displayName[players$nflId == id]
  pos = players$officialPosition[players$nflId == id]
  image = singlePlayerColorLogo$possTeamLogo[singlePlayerColorLogo$nflId == id]
  nflLogo = "https://raw.githubusercontent.com/nflverse/nflverse-pbp/master/NFL.png" # From nflReadR
  l1 = scoreBarChart(id)
  l2 = gameByGameChart(id)
  l3 = ggarrange(NULL, blockGradeLollipop(id), NULL, additionalStatsLollipop(id), NULL, nrow = 1, ncol = 5, 
                 widths = c(.1, 1, .1, 1, .1))
  l4 = compPlot(id)
  plot = ggarrange(l1, l2, l3, l4, nrow = 2, ncol = 2, heights = c(1, .75))
  annotate_figure(plot, top = ggpubr::text_grob(paste(name, "Player Card", paste0("(", pos, ")")), face = "bold", size = 20)) +
    geom_image(aes(x = 0.03, y = .97, image = image)) +
    geom_image(aes(x = 0.97, y = .97, image = nflLogo))
}

p1_p2Intersect = intersect(mergedAdjustedBlockDifficulty$nflId, adjustedBlockDifficultyByGame$nflId)
p3Intersect = intersect(p1_p2Intersect, finalPerformance$nflId)
p4Intersect = intersect(p3Intersect, additionalScores$nflId)

idTibble = tibble(nflId = p4Intersect)

playersForCards = idTibble %>%
  inner_join(players) %>%
  inner_join(singlePlayerColorLogo %>% select(nflId, possessionTeam)) %>%
  arrange(possessionTeam)

# 12.6 x 9.29 in image

teamIds = unique(playersForCards$possessionTeam)
for(i in 1:32){
  teamId = teamIds[i]
  playerData = playersForCards %>%
    filter(possessionTeam == teamId)
  for(j in 1:length(playerData$nflId)){
    name = playerData$displayName[j]
    playerCard = playerCardFunc(playerData$nflId[j])
    ggsave(paste0("Player Cards/",teamId, "/", name, "_PlayerCard.jpeg"),
           plot = playerCard)
  }
  print(i)
}
