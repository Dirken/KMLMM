

match$result[match$home_team_goal == match$away_team_goal] <- "D"
match$result[match$home_team_goal > match$away_team_goal] <- "W"
match$result[match$home_team_goal < match$away_team_goal] <- "L"

table(match$result)
df <- data.frame(
  group = c("Win", "Draw", "Lose"),
  value = c(11917, 6596, 7466)
)
bp<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
pie <- bp + coord_polar("y", start=0) + ggtitle("What teams do when playing home?")
#% of W/L/D of the local team
pie

#are goals correlated with w/d/l?
matchData <- dbGetQuery(con,"SELECT *
                        FROM Match
                        JOIN Country on Country.id = Match.country_id
                        JOIN League on League.id = Match.league_id")
matchData$result[matchData$home_team_goal == matchData$away_team_goal] <- "D"
matchData$result[matchData$home_team_goal > matchData$away_team_goal] <- "W"
matchData$result[matchData$home_team_goal < matchData$away_team_goal] <- "L"

plot(as.factor(matchData$result), matchData$home_team_goal)
matchData$diff_goals <- matchData$home_team_goal - matchData$away_team_goal


#RESULTS BY GOAL:
goals <- NULL
goals$localLose <- table(matchData$home_team_goal[matchData$result == "L"], as.factor(matchData$result[matchData$result == "L"]))
goals$awayLose <- table(matchData$away_team_goal[matchData$result == "W"], as.factor(matchData$result[matchData$result == "W"]))
goals$draw <- table(matchData$home_team_goal[matchData$result == "D"], as.factor(matchData$result[matchData$result == "D"]))
goals$result[matchData$home_team_goal[matchData$result == "D"]] <- "D"
goals$result[matchData$home_team_goal[matchData$result == "L"]] <- "L"
goals$result[matchData$away_team_goal[matchData$result == "W"]] <- "W"

plot(goals$localLose)
plot(goals$awayLose)
plot(goals$draw)

df <- data.frame(
  group = c(0, 1, 2, 3, 4, 5),
  localLose = c(3918, 2743, 717, 78, 10,0),
  awayLose = c(6709,4112,972,118,6,0),
  draw = c(1978,3014,1310,264,27,2)
)




ggplot(df) + 
  geom_line(data=df, aes(y = localLose, x = group, colour="Home Loses"), size=1) +
  geom_line(data=df, aes(y = awayLose, x = group, colour="Away Loses"), size=1) +
  geom_line(data=df, aes(y = draw, x = group, colour="Draws"), size=1) + 
  ggtitle("Number of goals when Losing")

table(matchData$home_team_goal[matchData$result == "W"], as.factor(matchData$result[matchData$result == "W"]))
table(matchData$away_team_goal[matchData$result == "L"], as.factor(matchData$result[matchData$result == "L"]))

df$localWin <- c(2643,4312,2946,1348,455,160)
df$AwayWin <- c(1863,2864,1763,685,213,62)
ggplot(df) + 
  geom_line(data=df, aes(y = localWin, x = group, colour="Home Wins"), size=1) +
  geom_line(data=df, aes(y = AwayWin, x = group, colour="Away Wins"), size=1) +
  geom_line(data=df, aes(y = draw, x = group, colour="Draws"), size=1) + 
  ggtitle("Number of goals when Winning")

##does this happen across countries?
matchData <- dbGetQuery(con,"SELECT Country.name AS country_name,  League.name, count(*) as contador
                        FROM Match
                        JOIN Country on Country.id = Match.country_id
                        JOIN League on League.id = Match.league_id
                        LEFT JOIN Team AS HT on HT.team_api_id = Match.home_team_api_id
                        LEFT JOIN Team AS AT on AT.team_api_id = Match.away_team_api_id
                        GROUP BY Country.name, League.name")

#matches in each league by season
#we can see some inbalances between leagues
ggplot(data=matchData, aes(x=country_name, y=contador, group=name)) +
  geom_bar(stat="identity")


leagueSummary <- dbGetQuery(con,
                            "SELECT Country.name AS country_name, 
                            League.name AS league_name, 
                            season,
                            count(distinct stage) AS number_of_stages,
                            count(distinct HT.team_long_name) AS number_of_teams,
                            avg(home_team_goal) AS avg_home_team_scors, 
                            avg(away_team_goal) AS avg_away_team_goals, 
                            avg(home_team_goal-away_team_goal) AS avg_goal_dif, 
                            avg(home_team_goal+away_team_goal) AS avg_goals, 
                            sum(home_team_goal+away_team_goal) AS total_goals                                       
                            FROM Match
                            JOIN Country on Country.id = Match.country_id
                            JOIN League on League.id = Match.league_id
                            LEFT JOIN Team AS HT on HT.team_api_id = Match.home_team_api_id
                            LEFT JOIN Team AS AT on AT.team_api_id = Match.away_team_api_id
                            GROUP BY Country.name, League.name, season
                            HAVING count(distinct stage) > 10
                            ORDER BY Country.name, League.name, season DESC
                            ;")
#goals per season in all leagues:
ggplot(leagueSummary, aes(x=season, y=avg_goals, color=league_name, group = league_name))+
  geom_point() +
  geom_line()

leagueSummary <- dbGetQuery(con,
                            "SELECT Country.name AS country_name, 
                            League.name AS league_name, 
                            season,
                            count(distinct stage) AS number_of_stages,
                            count(distinct HT.team_long_name) AS number_of_teams,
                            avg(home_team_goal) AS avg_home_team_scors, 
                            avg(away_team_goal) AS avg_away_team_goals, 
                            avg(home_team_goal-away_team_goal) AS avg_goal_dif, 
                            avg(home_team_goal+away_team_goal) AS avg_goals, 
                            sum(home_team_goal+away_team_goal) AS total_goals                                       
                            FROM Match
                            JOIN Country on Country.id = Match.country_id
                            JOIN League on League.id = Match.league_id
                            LEFT JOIN Team AS HT on HT.team_api_id = Match.home_team_api_id
                            LEFT JOIN Team AS AT on AT.team_api_id = Match.away_team_api_id
                            WHERE country_name in ('Spain', 'Germany', 'France', 'Italy', 'England')
                            GROUP BY Country.name, League.name, season
                            HAVING count(distinct stage) > 10
                            ORDER BY Country.name, League.name, season DESC
                            ;")

#average goals per game over time on major leagues
ggplot(leagueSummary, aes(x=season, y=avg_goals, color=league_name, group = league_name))+
  geom_point() +
  geom_line()

#gonna focus in this major leagues from now on for clarity in the graphics.

#Average goals difference between home - out, we can see that in any case home is bigger
ggplot(leagueSummary, aes(x=season, y=avg_home_team_scors - avg_away_team_goals, color=league_name, group = league_name))+
  geom_point() +
  geom_line()

ggplot(leagueSummary, aes(x=season, y=avg_home_team_scors - avg_away_team_goals, color=league_name, group = league_name))+
  geom_point() +
  geom_line() + ggtitle("Difference of goals scored and received by local team")


#is there a correlation between home form and away form? a good team is always a good team?
league <- select(league, id, name, country_id) %>% rename(league_id = id, league_name = name)
team   <- select(team, team_api_id, team_long_name, team_short_name)
match  <- select(match, league_id, home_team_api_id, away_team_api_id, home_team_goal, away_team_goal)

points <- match %>% 
  mutate(home_team_points = if_else((home_team_goal > away_team_goal),3,if_else((home_team_goal == away_team_goal),1,0))) %>%
  mutate(away_team_points = if_else((home_team_goal > away_team_goal),0,if_else((home_team_goal == away_team_goal),1,3))) 

localPoints <- points %>%
  select(league_id, team_api_id = home_team_api_id, home_team_points) %>%
  group_by(league_id, team_api_id) %>%
  summarize(avgHome = mean(home_team_points))

awayPoints <- points %>%
  select(league_id, team_api_id = away_team_api_id, away_team_points) %>%
  group_by(league_id, team_api_id) %>%
  summarize(avgAway = mean(away_team_points))

points <- left_join(localPoints, awayPoints, by = c("league_id", "team_api_id"))
points <- points %>%
  mutate(avgPointsGame = (avgHome + avgAway)/2)
points <- left_join(points, league, by = "league_id")
points <- left_join(points, team, by = "team_api_id")

home_away <- points %>%
  mutate(ratioHA = avgHome / avgAway) %>%
  select(team_short_name, league_id, league_name, avgPointsGame, ratioHA) %>%
  as.data.frame()


ggplot(home_away, aes(x = ratioHA, y = avgPointsGame)) + 
  geom_point() +
  ylim(0,3) +
  geom_point(aes(x = 1, y = 3), color="red") +
  geom_text(aes(x = 1, y = 3), hjust = 0, nudge_x = 0.05, size = 3, label = "Perfection") +
  labs(title = "Comparison of Average Points Per Game (PPG) for each Team\nin Home and Away Games",
       x = "Home to Away Points Ratio",
       y = "Average PPG") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(home_away, aes(x = league_name, y = ratioHA)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Comparison of Home to Away Points Ratio for each League",
       x = "Leage Name",
       y = "Home to Away Points Ratio") +
  theme(plot.title = element_text(hjust = 0.5))



##betting odds:

matchData <- dbGetQuery(con,"SELECT *
                        FROM Match
                        JOIN Country on Country.id = Match.country_id
                        JOIN League on League.id = Match.league_id")

matchData$B365R <- ifelse(matchData$B365H > matchData$B365A, "A", ifelse( matchData$B365A > matchData$B365D, "D", "H"))
matchData$BWR <- ifelse(matchData$BWJ > matchData$BWA, "A",  ifelse(matchData$BWA > matchData$BWD, "D", "H")) #HAS NA's
matchData$IWR <- ifelse(matchData$IWH > matchData$IWA, "A",  ifelse(matchData$IWA > matchData$IWD, "D", "H"))
matchData$LBR <- ifelse(matchData$LBH > matchData$LBA, "A",  ifelse(matchData$LBA > matchData$LBD, "D", "H"))
matchData$PSR <- ifelse(matchData$PSH > matchData$PSA, "A",  ifelse(matchData$PSA > matchData$PSD, "D", "H"))
matchData$WHR <- ifelse(matchData$WHH > matchData$WHA, "A",  ifelse(matchData$WHA > matchData$WHD, "D", "H"))
matchData$SJR <- ifelse(matchData$SJH > matchData$SJA, "A",  ifelse(matchData$SJA > matchData$SJD, "D", "H"))
matchData$VCR <- ifelse(matchData$VCH > matchData$VCA, "A",  ifelse(matchData$VCA > matchData$VCD, "D", "H"))
matchData$GBR <- ifelse(matchData$GBH > matchData$GBA, "A",  ifelse(matchData$GBA > matchData$GBD, "D", "H"))
matchData$BSR <- ifelse(matchData$BSH > matchData$BSA, "A",  ifelse(matchData$BSA > matchData$BSD, "D", "H"))


matchData$result[matchData$home_team_goal == matchData$away_team_goal] <- "D"
matchData$result[matchData$home_team_goal > matchData$away_team_goal] <- "H"
matchData$result[matchData$home_team_goal < matchData$away_team_goal] <- "A"

scores <- NULL
scores$B365 <- length(matchData$result[(matchData$B365R == matchData$result)]) / length(matchData$B365R) 

scores$IW <- length(matchData$result[(matchData$IWR == matchData$result)]) / length(matchData$IWR) 
scores$LB <- length(matchData$result[(matchData$LBR == matchData$result)]) / length(matchData$LBR) 
scores$PS <-length(matchData$result[(matchData$PSR == matchData$result)]) / length(matchData$PSR) 
scores$WH <-length(matchData$result[(matchData$WHR == matchData$result)]) / length(matchData$WHR) 
scores$SJ <-length(matchData$result[(matchData$SJR == matchData$result)]) / length(matchData$SJR) 
scores$VC <-length(matchData$result[(matchData$VCR == matchData$result)]) / length(matchData$VCR) 
scores$GB <-length(matchData$result[(matchData$GBR == matchData$result)]) / length(matchData$GBR) 
scores$BS <-length(matchData$result[(matchData$BSR == matchData$result)]) / length(matchData$BSR) 

df <- data.frame(
  group = c("B365" , "IW", "LB", "PS", "WH", "SJ", "VC", "GB", "BS") ,
  value = c(scores$B365, scores$IW, scores$LB, scores$PS, scores$WH, scores$SJ, scores$VC, scores$GB, scores$BS) 
)
barplot(df$value, names.arg=df$group,  main = 'Accuracy of predicting odds')


