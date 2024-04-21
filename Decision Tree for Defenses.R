# Decision Tree for NFL Team Playcalling
'''
Want to take a teams playcalls (hopefully from new pbp data) and create a decision tree output that can take info from things like down, distance, yardline, time in game, wp, opp personnel, opp efficiency in said personnel, formation, etc. to provide an expectation for what we expect a playcall to be based on those inputs. Would be sick to create these for the games this weekend and see if they match up.
'''

# Load Data ---------------------------------------------------------------


pbp <- load_pbp(seasons = 2023)
participation <- load_participation(seasons = 2023)

all_pbp <- pbp %>% 
  left_join(participation)


# Data Prep ---------------------------------------------------------------

full_pbp <- all_pbp %>% 
  mutate(OffWP = if_else(posteam == home_team, home_wp, away_wp)) %>% 
  filter(!is.na(defense_coverage_type)) %>% 
  filter(defteam == "DET")

# Decision Tree -----------------------------------------------------------
library(rpart)
library(rpart.plot)


tree_model <- rpart(defense_coverage_type ~ down + ydstogo + yardline_100 + offense_formation + offense_personnel, data = full_pbp, method = "class")


rpart.plot(tree_model, fallen.leaves = F)


check <- full_pbp %>% 
  group_by(offense_formation) %>% 
  summarise(Count = n())

summary(tree_model)



# Rough Summaries ---------------------------------------------------------

PositionalCheck <- all_pbp %>% 
  filter(posteam == "BAL",
         !is.na(defense_coverage_type)) %>% 
  group_by(defense_coverage_type, offense_personnel, offense_formation) %>% 
  summarise(Count = n(),
            SuccessRate = mean(success),
            EPA = mean(epa)) %>% 
  filter(Count >= 5)

