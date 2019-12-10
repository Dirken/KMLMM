#KMLMM: implementation football forecasting.
#Dataset: https://www.kaggle.com/hugomathien/soccer
library(ggplot2) # Data visualization
library(ggrepel) # Labels on plots
library(dplyr)
library(RSQLite)
library(mice)
library(VIM)

##########################################
#Dataset
##########################################
con <- dbConnect(SQLite(), dbname="database.sqlite")
dbListTables(con)
league <- tbl_df(dbGetQuery(con,"SELECT * FROM League"))
team   <- tbl_df(dbGetQuery(con,"SELECT * FROM Team"))
match  <- tbl_df(dbGetQuery(con,"SELECT * FROM Match"))
players <- tbl_df(dbGetQuery(con, "SELECT * FROM Player"))
player_attrs <- tbl_df(dbGetQuery(con, "SELECT * FROM Player_Attributes"))
countries <- tbl_df(dbGetQuery(con, "SELECT * FROM Country")) #not relevant at all
team_attrs <- tbl_df(dbGetQuery(con, "SELECT * FROM Team_Attributes"))


##########################################
#EDA:
##########################################
#do we have a lot of NA's?


md.pattern(team) #unredeable
md.pattern(league)
md.pattern(team) 
md.pattern(match)
md.pattern(players) 
md.pattern(player_attrs) #most missings
md.pattern(countries)
md.pattern(team_attrs)
matrixplot(match) #nice one

#best ones tbh
matrixplot(team)
matrixplot(league)
matrixplot(players)
matrixplot(player_attrs)
matrixplot(countries)
matrixplot(team_attrs)

#not showing well
scattmatrixMiss(team)
scattmatrixMiss(league)
scattmatrixMiss(players)
scattmatrixMiss(player_attrs)
scattmatrixMiss(countries)
scattmatrixMiss(team_attrs)

aggr(match, numbers =T, prop=F) #more readable but not the rhs
aggr(team, numbers =T, prop=F)

#Better see there:
source(league-matches.R)
source(players.R)

#Final data cleaning


matchData <- dbGetQuery(con,"SELECT *
                        FROM Match
                        JOIN Country on Country.id = Match.country_id
                        JOIN League on League.id = Match.league_id")

match_league_country <- dbGetQuery(con,"SELECT * 
                FROM Match natural inner join Player
                JOIN Country on Country.id = Match.country_id
                JOIN League on League.id = Match.league_id
                
                LEFT JOIN Team AS HT ON HT.team_api_id = Match.home_team_api_id
                LEFT JOIN Team AS AT ON AT.team_api_id = Match.away_team_api_id

                                   
                                   ")


