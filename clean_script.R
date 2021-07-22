
# Load Libraries and Data ----------------------------------------------------------


library(tidyverse)
library(tidytext)   #reorder_within() and scale_x_reordered()
library(padr)       #best package for mean imputation
library(ggplot2)
library(ggrepel)
library(lme4)
library(patchwork)
library(randomForest)
library(caret)

GameInfo <- read.csv("https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2021/main/Data/GameInfo.csv")

pbp <- read.csv("https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2021/main/Data/PlayByPlay.csv")

PlayerTotalPoints <- read.csv("https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2021/main/Data/PlayerTotalPoints.csv")

SkillPositionPlayers <- read.csv("https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2021/main/Data/SkillPositionPlayers.csv")


# Data Cleaning -----------------------------------------------------------


routes <- SkillPositionPlayers %>%
  mutate(run_fake = ifelse(Route == "Run Fake", 1 ,0),
         wr_blocker = ifelse(Route == "Blocking" & OnFieldPosition %in% c("WR", "SWR"), 1, 0)) %>%
  filter(!(Route %in% c("Blocking", "Run Fake"))) %>%
  mutate(route_category = case_when(
    Route %in% c("Curl", "Comeback", "Jerk", "Out", 
                 "Over Ball", "Whip") ~ "Short",
    Route %in% c("Go/Fly", "Fade", "Fade - Back Shoulder", "Seam", 
                 "Post", "Corner") ~ "Vertical",
    Route %in% c("Drag", "Dig", "Deep Cross", "Slant") ~ "Crossing",
    Route %in% c("Angle", "Check & Release", "Chip", "Chip - Curl",
                 "Chip - Drag", "Chip - Flat", "Chip - Seam", "Flat - Right",
                 "Flat - Left", "Leak", "Swing - Left", "Swing - Right", 
                 "Wheel") ~ "Interior",
    Route %in% c("Beneath", "Screen - Beneath", "Screen - Bubble", "Screen - Drag",
                 "Screen - Quick", "Screen - RB", "Screen - Shovel", "Screen - TE",
                 "Screen - Tunnel", "Jet Sweep Pass", "Quick") ~ "Screen",
    Route %in% c("Corner Post", "Post Corner", "Hitch & Go", "Out & Up", "Sluggo",
                 "Stick - Nod") ~ "DoubleMove",
    T ~ "Other"
  ))


routes_wide_nontarget <- routes %>%
  filter(Target == 0) %>%
  group_by(GameID, EventID, route_category) %>%
  summarise(n = n()) %>%
  pivot_wider(id_cols = GameID:EventID, names_from = route_category, 
              values_from = n, values_fn = sum) %>%
  padr::fill_by_value(value = 0)

routes_wide_target <- routes %>%
  filter(Target == 1) %>%
  select(GameID, EventID, route_category) %>%
  rename(target_route = route_category)


routes_wide <- left_join(routes_wide_nontarget, routes_wide_target, by = c("GameID", "EventID")) %>%
  filter(Other != 6, !is.na(target_route))



df_Mod <- pbp %>%
  filter(Attempt != "NULL",
         FumbleByPasser != "1", 
         FumbleByRusher != "1") %>%
  select(-c(Season, EventType, FirstDown, Touchdown, Safety, Turnover, Attempt,
            Completion, Spike, ThrowAway, ThrowDepth, PressureOnPlay, FumbleByPasser,
            FumbleByRusher, FumbleByReceiver, OffensiveYardage, PlayDesc, SackOnPlay,
            InterceptionOnPlay, PassBreakupOnPlay))  

targets <- SkillPositionPlayers %>%
  filter(Target == 1)

receptions <- SkillPositionPlayers %>%
  group_by(GameID, EventID) %>%
  summarise(success = max(Reception)) %>%
  ungroup()

df_Mod <- inner_join(df_Mod, targets, by = c("GameID", "EventID")) %>%
  select(-c(Name, OnFieldPosition, IsBlocking, Reception, Target)) %>%
  mutate(score_diff = OffTeamScoreBefore - DefTeamScoreBefore) %>%
  select(-OffTeamScoreBefore, -DefTeamScoreBefore) %>%
  na.omit() %>%
  filter(Route != "Pick") %>%
  mutate(route_category = case_when(
    Route %in% c("Curl", "Comeback", "Jerk", "Out", 
                 "Over Ball", "Whip") ~ "Short",
    Route %in% c("Go/Fly", "Fade", "Fade - Back Shoulder", "Seam", 
                 "Post", "Corner") ~ "Vertical",
    Route %in% c("Drag", "Dig", "Deep Cross", "Slant") ~ "Crossing",
    Route %in% c("Angle", "Check & Release", "Chip", "Chip - Curl",
                 "Chip - Drag", "Chip - Flat", "Chip - Seam", "Flat - Right",
                 "Flat - Left", "Leak", "Swing - Left", "Swing - Right", 
                 "Wheel") ~ "Interior",
    Route %in% c("Beneath", "Screen - Beneath", "Screen - Bubble", "Screen - Drag",
                 "Screen - Quick", "Screen - RB", "Screen - Shovel", "Screen - TE",
                 "Screen - Tunnel", "Jet Sweep Pass", "Quick") ~ "Screen",
    Route %in% c("Corner Post", "Post Corner", "Hitch & Go", "Out & Up", "Sluggo",
                 "Stick - Nod") ~ "Double Move",
    T ~ "Other"
  )) %>%
  mutate(SideOfCenter = ifelse(SideOfCenter == "NULL", "C", SideOfCenter),
         Order_OutsideToInside = ifelse(Order_OutsideToInside == "NULL", 0, 
                                        Order_OutsideToInside),
         Shotgun = as.factor(Shotgun),
         Week = as.factor(Week),
         DropType = case_when(
           DropType %in% c("RPO", "RPO Move")~ "RPO",
           DropType %in% c("Designed Rollout Left", "Designed Rollout Right") ~ "Designed Rollout",
           T ~ DropType
         ),
         DropType = as.factor(DropType)) %>%
  left_join(receptions, by = c("GameID", "EventID")) %>%
  select(-EPA)

table(df_Mod$DropType)


# Modeling ----------------------------------------------------------------


idx = unlist(createDataPartition(df_Mod$success, p = .7))
train_mod <- df_Mod[idx, ] 
test_mod <- df_Mod[-idx, ]


glm_mod <- glmer(factor(success) ~ Down + ToGo + SideOfField + 
                   StartYard + Shotgun + DropType + FastMotion +
                   (1 | OffensiveTeam) + (1 | DefensiveTeam) + (1 | route_category),
                 data = train_mod,
                 family = binomial()) 

summary(glm_mod)
pred_glm <- predict(glm_mod, test_mod, type = "response")

MLmetrics::LogLoss(pred_glm, test_mod$success)
MLmetrics::LogLoss(rep(.67, length(test_mod$success)), test_mod$success)
ModelMetrics::logLoss(test_mod$success, pred_glm)
ModelMetrics::logLoss(test_mod$success, rep(.67, length(test_mod$success)))


glm_mod_final <- glmer(factor(success) ~ Down + ToGo + SideOfField + 
                         StartYard + Shotgun + DropType + FastMotion +
                         (1 | OffensiveTeam) + (1 | DefensiveTeam)  + (1 | route_category),
                       data = df_Mod,
                       family = binomial()) 
summary(glm_mod_final)


## coverage and target model

glm_mod_coverage <- glmer(factor(success) ~ Down + SideOfField + 
                            StartYard + Shotgun + DropType + (1 | OffensiveTeam) +
                            (1 | DefensiveTeam) + FastMotion + (1| CoverageScheme) + 
                            (1 | route_category),
                          data = train_mod,
                          family = binomial()) 
summary(glm_mod_coverage)
pred_glm_coverage <- predict(glm_mod_coverage, test_mod, type = "response")

MLmetrics::LogLoss(pred_glm_coverage, test_mod$success)
ModelMetrics::logLoss(test_mod$success, pred_glm_coverage)



glm_mod_coverage_final <- glmer(factor(success) ~ Down + SideOfField + 
                            StartYard + Shotgun + DropType + (1 | OffensiveTeam) +
                            (1 | DefensiveTeam) + FastMotion + (1| CoverageScheme) + 
                            (1 | route_category),
                          data = df_Mod,
                          family = binomial()) 
summary(glm_mod_coverage_final)

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}


df_Mod$pred <- predict(glm_mod_final, df_Mod, type = "response")
df_Mod$pred_coverage <- predict(glm_mod_coverage_final, df_Mod, type = "response", allow.new.levels = T)



# Analysis ----------------------------------------------------------------


df_Mod %>%
  mutate(success_OE = success - pred_coverage) %>%
  group_by(CoverageScheme, route_category) %>%
  summarise(mean_success_OE = mean(success_OE),
            n = n(), 
            sd_success_OE = sd(success_OE),
            low = mean_success_OE - 1.96*sd_success_OE,
            high = mean_success_OE + 1.96*sd_success_OE) %>%
  arrange(-n) %>%
  filter(n >= 20) %>%
  mutate(combo = paste0(CoverageScheme, " : ", route_category)) %>%
  ggplot(., aes(x = mean_success_OE, 
                y = reorder(combo, mean_success_OE),
                xmin = low,
                xmax = high)
  ) +
  geom_col(width = .5,
           fill = "#C63F32") +
  labs(title = "Best Coverage:Route Combinations",
       subtitle = "Combinations with 20 or More Occurences ",
       x = "\nCompletions Over Expected per Play",
       y = "Coverage Type : Route Category\n") +
  theme_light()


targetroutes_by_coverage <- df_Mod %>%
  mutate(success_OE = success - pred) %>%
  group_by(CoverageScheme, route_category) %>%
  summarise(mean_success_OE = mean(success_OE),
            n = n(), 
            sd_success_OE = sd(success_OE),
            low = mean_success_OE - sd_success_OE,
            high = mean_success_OE + sd_success_OE) %>%
  filter(n >= 20) %>%
  ggplot(., aes(y = reorder_within(route_category, mean_success_OE, CoverageScheme), 
                x = mean_success_OE,
                xmin = low,
                xmax = high)) +
   geom_col(width = .4, 
            fill = "#C63F32"
   ) +
  scale_y_reordered() +
  labs(title = "Target Receiver Routes - Completions Over Expected per Play",
       subtitle = "Combinations with 20 or More Occurences ",
       x = "\nCompletions Over Expected per Play",
       y = "Target Receiver Route Type\n") +
  facet_wrap(~CoverageScheme, as.table = T, scale = "free_y") +
  theme_light() +
  #scale_fill_gradient2(high = "#314686", low = "#C63F32", mid = "darkgrey", labels = scales::percent_format(), name = "Play Success Over") +
  theme(strip.text = element_text(color = "black", face = "bold"),
        strip.background = element_rect(fill = "white")) 

ggsave("targetroutes_by_coverage.png", targetroutes_by_coverage, width = 11, height = 7)



routes_wide_preds <- left_join(routes_wide, 
                               df_Mod %>% select(GameID, EventID, success, pred_coverage), 
                               by = c("GameID", "EventID")) %>%
  rowwise() %>%
  mutate(n_routes = sum(across(Other:DoubleMove)))


supportingroutes_by_target <- routes_wide_preds %>%
  #filter(target_route == "Crossing") %>%
  group_by(target_route) %>%
  pivot_longer(cols = Other:DoubleMove, names_to = "Supporting_Route") %>%
  mutate(success_OE = success - pred_coverage,
         success_OE = success_OE * (value/n_routes)) %>%
  group_by(target_route, Supporting_Route) %>%
  filter(target_route != "Other") %>%
  summarise(success_OE = mean(success_OE, na.rm = T)) %>% 
  ggplot(., aes(x = reorder_within(Supporting_Route, success_OE, target_route), y = success_OE)) +
  geom_col(width = .3, fill = "#C63F32") +
  coord_flip() +
  scale_x_reordered() +
  #expand_limits(y = .07) +
  theme_light() +
  labs(title = "Best Supporting Routes for Each Target Receiver Route",
       y = "\nWeighted Completions Over Expected per Play",
       x = "Non-Receiver Route Type") +
  facet_wrap(~target_route, scale = "free_y", as.table = T) +
  geom_hline(yintercept = 0) +
  theme(strip.text = element_text(face = "bold", color = "black"),
        strip.background = element_rect(fill = "white"))

ggsave("supportingroutes_by_target.png", supportingroutes_by_target, width = 11, height = 7)


SkillPositionPlayers %>%
  mutate(route_category = case_when(
    Route %in% c("Curl", "Comeback", "Jerk", "Out", 
                 "Over Ball", "Whip") ~ "Short",
    Route %in% c("Go/Fly", "Fade", "Fade - Back Shoulder", "Seam", 
                 "Post", "Corner") ~ "Vertical",
    Route %in% c("Drag", "Dig", "Deep Cross", "Slant") ~ "Crossing",
    Route %in% c("Angle", "Check & Release", "Chip", "Chip - Curl",
                 "Chip - Drag", "Chip - Flat", "Chip - Seam", "Flat - Right",
                 "Flat - Left", "Leak", "Swing - Left", "Swing - Right", 
                 "Wheel") ~ "Interior",
    Route %in% c("Beneath", "Screen - Beneath", "Screen - Bubble", "Screen - Drag",
                 "Screen - Quick", "Screen - RB", "Screen - Shovel", "Screen - TE",
                 "Screen - Tunnel", "Jet Sweep Pass", "Quick") ~ "Screen",
    Route %in% c("Corner Post", "Post Corner", "Hitch & Go", "Out & Up", "Sluggo",
                 "Stick - Nod") ~ "Double Move",
    T ~ "Other"
  )) %>%
  filter(route_category == "Other") %>%
  count(Route)

