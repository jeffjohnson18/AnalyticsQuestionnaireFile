library(dplyr)
library(ggplot2)
library(caret)
library(plotly)
library(readr)

# load my data set
data <- read_csv("C:/Users/jeffr/Downloads/Questionnaire Data/AnalyticsQuestionnairePitchData.csv")

#---------------------
# hitting stats game 1 
#---------------------

# store every player who got on base in game 1
# select the result (pitchcall), batter id, inning, istop, and pitcher id
players_on_base_g1 <- data %>%
  filter(GamePk == 1 & PitchCall %in% c("single", "double", "triple", "home_run", "walk")) %>%
  select(PitchCall, BatterId, Inning, IsTop, PitcherId)

# display the table
print(players_on_base_g1, n = Inf)


# store data into a bar graph to view how many times each player got on base
players_on_base_summary <- players_on_base_g1 %>%
  group_by(BatterId) %>%
  summarize(OnBaseCount = n())

# plot the graph
ggplot(players_on_base_summary, aes(x = as.factor(BatterId), y = OnBaseCount)) +
  geom_bar(stat = "identity") +
  labs(x = "Batter ID", y = "On Base Count", title = "Number of Times Each Player Reached Base") +
  theme_minimal()

#top hitters:
#players 21 and 24 reached base 4 teams
#30 and 34 reached 3 times

#-----------------:
# pitching stats game 1:
#-----------------:

# store the amount of base runners each pitcher allowed
pitchers_base_runners <- players_on_base_g1 %>%
  group_by(PitcherId) %>%
  summarize(BaseRunnersAllowed = n())

# print the table
print(pitchers_base_runners, n = Inf)

#notable performances:
#pitcher 1 allowed 7 baserunners
#pitcher 11 allowed 9 baserunners
#total of 10 pitchers in game 1 by both teams

#---------------------
# hitting stats game 2 
#---------------------


# store every player who got on base in game 2
# repeating same process but game 2 instead of 1
players_on_base_g2 <- data %>%
  filter(GamePk == 2 & PitchCall %in% c("single", "double", "triple", "home_run", "walk")) %>%
  select(PitchCall, BatterId, Inning, IsTop, PitcherId)

# display the table
print(players_on_base_g2, n = Inf)


# store data into a bar graph to view how many times each player got on base
players_on_base_g2_summary <- players_on_base_g2 %>%
  group_by(BatterId) %>%
  summarize(OnBaseCount = n())

# plot the graph
ggplot(players_on_base_g2_summary, aes(x = as.factor(BatterId), y = OnBaseCount)) +
  geom_bar(stat = "identity") +
  labs(x = "Batter ID", y = "On Base Count", title = "Number of Times Each Player Reached Base") +
  theme_minimal()

#top hitters:
#player 5 reached base 3 times
#players 4, 21, and 34 got on base twice

#-----------------:
# pitching stats game 2:
#-----------------:

# store the amount of base runners each pitcher allowed
pitchers_base_runners_g2 <- players_on_base_g2 %>%
  group_by(PitcherId) %>%
  summarize(BaseRunnersAllowed = n())

# print the table
print(pitchers_base_runners_g2, n = Inf)

#notable performances:
#great pitching matchup, not a lot of baserunners
#pitcher 7 allowed 6 baserunners
#pitcher 9 allowed only 1 baserunner
