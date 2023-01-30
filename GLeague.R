library(tidyverse)
library(caret)
library(gridExtra)

# 2022 G League player bios
bios22 <- read_csv("Desktop/NBA Projects/Data/bios22.csv")
# Select important columns and convert height to total inches
bios22 <- bios22 %>%
  select(Player, Pos, HT, WT, `NBA Draft Status`) %>%
  separate(HT, c('HT', 'inches'), sep = '-', convert = TRUE, remove = FALSE) %>%
  mutate(HT = 12 * HT + inches) %>%
  select(-inches)

# Extract draft round from draft status
draft_round <- function(x) {
  str_extract(x, "Undrafted|Rnd 1|Rnd 2")
}
bios22 <- bios22 %>%
  mutate(Draft = draft_round(`NBA Draft Status`)) %>%
  select(-`NBA Draft Status`)
View(bios22)
table(bios22$Draft)

# 2022 G League data and call ups
gleague2022 <- read_csv("Desktop/NBA Projects/Data/gleague2022.csv")
gleague2022 <- gleague2022[-1]
View(gleague2022)
gleague2022 %>% filter(Player == "Mac McClung")
summary(gleague2022['GP'])
hist(gleague2022$GP)
callups2022 <- read_csv("Desktop/NBA Projects/Data/callups2022.csv")
View(callups2022)

# Create called up variable
callup_names22 <- unique(callups2022$Player)
length(callup_names22)
calledUp22 <- ifelse(gleague2022$Player %in% callup_names22, 1, 0)
sum(calledUp22) # Some call ups do not show up because they did not qualify for statistics (didn't play enough)
gleague2022$CalledUp <- calledUp22
table(gleague2022$CalledUp) # 29.8% of players called up

# Differentiate the two Justin Jacksons
bios22 <- bios22 %>%
  mutate(Player = replace(Player, Player == "Justin Jackson", c("Justin Jackson (1)", "Justin Jackson (2)")))
gleague2022 <- gleague2022 %>%
  mutate(Player = replace(Player, Player == "Justin Jackson", c("Justin Jackson (2)", "Justin Jackson (1)")))

# Join bios22 to gleague22
gleague2022 <- left_join(gleague2022, bios22, by = "Player")
gleague2022 <- gleague2022 %>%
  mutate(WT = as.numeric(WT))

# Called up % by position
gleague2022 %>%
  group_by(Pos) %>%
  summarize(Prop = mean(CalledUp), Count = n()) %>%
  arrange(desc(Prop)) # Sorted by prop
gleague2022 %>%
  group_by(Pos) %>%
  summarize(Prop = mean(CalledUp), Count = n()) %>%
  arrange(desc(Count)) # Sorted by total count

# Called up % by draft round
gleague2022 %>%
  group_by(Draft) %>%
  summarize(Prop = mean(CalledUp)) %>%
  arrange(desc(Prop))
chisq.test(gleague2022$Draft, gleague2022$CalledUp)

# Difference in HT & WT by called up
gleague2022 %>%
  group_by(CalledUp) %>%
  summarize_at(c("HT", "WT"), mean)

# Check distribution of players by team
gleague2022 %>%
  group_by(Team) %>%
  summarize(count = n()) %>%
  print(n = 28)

# Called Up proportion by team
gleague2022 %>% 
  group_by(Team) %>%
  summarize(Prop = mean(CalledUp)) %>% 
  arrange(desc(Prop)) %>%
  top_n(10)

# Visualizations
library(ggthemes)
theme_set(theme_economist())
p1 <- gleague2022 %>% 
  group_by(Team) %>%
  summarize(Prop = mean(CalledUp)) %>%
  mutate(Prop = round(Prop, 2)) %>%
  ggplot(aes(x = reorder(Team, Prop), y = Prop)) + geom_bar(stat = "identity", fill = "#00BFC4") +
  geom_text(aes(label = Prop),  hjust = 1.6, color = "black", size = 5) + coord_flip() +
  labs(subtitle = "For the 2021-2022 G-league Season", 
       y = "Proportion of Players Called Up", 
       x = "Team", 
       title = "Proportion of Players Called Up by Team")
p1

# train-test split
set.seed(24)
train_idx <- sample(1:356, 267)
train <- gleague2022[train_idx, ]
test <- gleague2022[-train_idx, ]

# Baseline models
fic_mdl <- glm(CalledUp ~ FIC, data = train, family = "binomial")
per_mdl <- glm(CalledUp ~ PER, data = train, family = "binomial")
full <- glm(CalledUp ~ . - Team, data = train[-1], family = "binomial")
summary(fic_mdl)
summary(per_mdl)
summary(full)

# Full model train and test error (62.8%)
predicted <- ifelse(predict(full, train, type="response") > 0.5, 1, 0)
mean(predicted == train$CalledUp)
predicted <- ifelse(predict(full, test[-1], type="response") > 0.5, 1, 0)
mean(predicted == test$CalledUp)

# FIC model train and test error (71.9%)
predicted <- ifelse(predict(fic_mdl, train, type="response") > 0.5, 1, 0)
mean(predicted == train$CalledUp)
predicted <- ifelse(predict(fic_mdl, test[-1], type="response") > 0.5, 1, 0)
mean(predicted == test$CalledUp)

# PER model train and test error (65.1%)
predicted <- ifelse(predict(per_mdl, train, type="response") > 0.5, 1, 0)
mean(predicted == train$CalledUp)
predicted <- ifelse(predict(per_mdl, test[-1], type="response") > 0.5, 1, 0)
mean(predicted == test$CalledUp)

# Null model for forward AIC/BIC
null_mdl <- glm(CalledUp ~ FIC, data = train, family = 'binomial')

# Forward step
forwardAIC <- step(
  null_mdl, 
  scope = list(lower = null_mdl, upper = full),
  direction = "forward",
)
forwardBIC <- step(
  null_mdl, 
  scope = list(lower = null_mdl, upper = full),
  direction = "forward",
  k = log(419)
)

# Backward step
backwardAIC <- step(full, direction = 'backward')
backwardBIC <- step(full, direction = 'backward', k = log(419))

# BackAIC test error: 70.8%
# BackBIC test error: 73.0%
# ForwardAIC test error: 71.9%
# ForwardBIC test error: 73.0%
mdl <- glm(CalledUp ~ FIC + `USG%`, data = train, family = "binomial") # Back BIC model
summary(mdl)

predicted <- ifelse(predict(mdl, train, type="response") > 0.5, 1, 0)
mean(predicted == train$CalledUp)
predicted <- ifelse(predict(mdl, test[-1], type="response") > 0.5, 1, 0) 
mean(predicted == test$CalledUp) # 73.0%

# Post-hoc t-tests
t.test(FIC ~ CalledUp, data = gleague2022)
t.test(`USG%` ~ CalledUp, data = gleague2022)

# Post-hoc visualizations
team_df <- gleague2022 %>%
  group_by(Team) %>%
  summarize(FIC_mean = mean(FIC), USG_sd = sd(`USG%`), CallUp_prop = mean(CalledUp))
gleague2022 %>%
  mutate(CalledUp = as.factor(CalledUp)) %>%
  ggplot(aes(x = FIC, y = `USG%`, color = CalledUp)) +  
    geom_point(size = 3) + geom_smooth(method = lm)
ggplot(gleague2022, aes(x = FIC)) + 
  geom_histogram(data = subset(gleague2022, CalledUp == 0),fill = "#F8766D", alpha = 0.5) +
  geom_histogram(data = subset(gleague2022, CalledUp == 1),fill = "#00BFC4", alpha = 0.5)
ggplot(gleague2022, aes(x = `USG%`)) + 
  geom_histogram(data = subset(gleague2022, CalledUp == 0),fill = "#F8766D", alpha = 0.5) +
  geom_histogram(data = subset(gleague2022, CalledUp == 1),fill = "#00BFC4", alpha = 0.5)
p2 <- team_df %>%
  mutate(FIC_mean = round(FIC_mean, 3)) %>%
  ggplot(aes(x = reorder(Team, FIC_mean), y = FIC_mean)) + geom_bar(stat = "identity", fill = "#F8766D") +
  geom_text(aes(label = FIC_mean),  hjust = 1.6, color = "black", size = 5) + coord_flip() +
  labs(subtitle = "For the 2021-2022 G-league Season", 
       y = "Average FIC", 
       x = "Team", 
       title = "Average FIC by Team")
p3 <- team_df %>%
  mutate(USG_sd = round(USG_sd, 3)) %>%
  ggplot(aes(x = reorder(Team, USG_sd), y = USG_sd)) + geom_bar(stat = "identity", fill = "#7CAE00") +
  geom_text(aes(label = USG_sd),  hjust = 1.6, color = "black", size = 5) + coord_flip() +
  labs(subtitle = "For the 2021-2022 G-league Season", 
       y = "USG% Standard Deviation", 
       x = "Team", 
       title = "USG% Standard Deviation by Team")
grid.arrange(p1, p2, ncol = 2)
grid.arrange(p3, p2, ncol = 2)
grid.arrange(p1, p3, ncol = 2)
# Scatter plots
p4 <- team_df %>%
  ggplot(aes(x = FIC_mean, y = USG_sd)) + geom_point(aes(size = CallUp_prop), alpha = 0.7, color = "#00BFC4") +
  scale_size(range = c(10, 20), name = "Call Up Proportion") + geom_text(label = team_df$Team) +
  labs(subtitle = "Size by the proportion of call ups for each team", 
       y = "USG% Standard Deviation", 
       x = "Average FIC", 
       title = "Average FIC vs USG% Standard Deviation by Team") +
  theme(legend.position="right") + 
  guides(colour = guide_legend(override.aes = list(size=10)))
p5 <- team_df %>%
  ggplot(aes(x = USG_sd, y = CallUp_prop)) + geom_point(size = 15, alpha = 0.7, color = "#F8766D") +
  geom_text(label = team_df$Team) +
  labs(x = "USG% Standard Deviation", 
       y = "Proportion of Call Ups", 
       title = "USG% Standard Deviation vs Proportion of Call Ups by Team")
p6 <- team_df %>%
  ggplot(aes(x = FIC_mean, y = CallUp_prop)) + geom_point(size = 15, alpha = 0.7, color = "#7CAE00") +
  geom_text(label = team_df$Team) +
  labs(x = "Average FIC", 
       y = "Proportion of Call Ups", 
       title = "Average FIC vs Proportion of Call Ups by Team")
grid.arrange(p5, p6, ncol = 2)

# PIC
pic_df <- gleague2022 %>%
  mutate(pic = GP * (PPG + FGM + FTM - FGA - FTA + DRB + ORB / 2 + APG + SPG + BPG / 2 - PF - TOV))
summary(pic_df$pic)
hist(pic_df$pic)
pic_train <- pic_df[train_idx, ] # Train-test split
pic_test <- pic_df[-train_idx, ]
pic_baseline <- glm(CalledUp ~ pic, data = pic_train, family = "binomial") # PIC model baseline (2 Betas): test error = 73.0%
summary(pic_baseline)
predicted <- ifelse(predict(pic_baseline, pic_train, type="response") > 0.5, 1, 0)
mean(predicted == pic_train$CalledUp)
predicted <- ifelse(predict(pic_baseline, pic_test, type="response") > 0.5, 1, 0)
mean(predicted == pic_test$CalledUp)
table(predicted, pic_test$CalledUp)
pic_mdl <- glm(CalledUp ~ pic:Pos:Draft, data = pic_train, family = "binomial") # PIC model by Pos and Draft Round (28 Betas): test error = 77.5%
summary(pic_mdl)
predicted <- ifelse(predict(pic_mdl, pic_train, type="response") > 0.5, 1, 0)
mean(predicted == pic_train$CalledUp)
predicted <- ifelse(predict(pic_mdl, pic_test, type="response") > 0.5, 1, 0)
mean(predicted == pic_test$CalledUp)
table(predicted, pic_test$CalledUp)
t.test(pic ~ CalledUp, data = pic_df)
pic_df %>%
  group_by(CalledUp, Pos, Draft) %>%
  summarize(`Mean PIC` = mean(pic)) %>%
  arrange(Pos, CalledUp) %>%
  print(n = 45)

# PIC Visualizations
team_df <- pic_df %>%
  group_by(Team) %>%
  summarize(PIC_mean = mean(pic), CallUp_prop = mean(CalledUp))
p2 <- team_df %>% # By team
  mutate(PIC_mean = round(PIC_mean, 3)) %>%
  ggplot(aes(x = reorder(Team, PIC_mean), y = PIC_mean)) + geom_bar(stat = "identity", fill = "#F8766D") +
  geom_text(aes(label = PIC_mean),  hjust = 1.6, color = "black", size = 5) + coord_flip() +
  labs(subtitle = "For the 2021-2022 G-league Season", 
       y = "Average PIC", 
       x = "Team", 
       title = "Average PIC by Team")
grid.arrange(p1, p2, ncol = 2)
p3 <- team_df %>% # Scatter plots
  ggplot(aes(x = PIC_mean, y = CallUp_prop)) + geom_point(size = 15, alpha = 0.7, color = "#00BFC4") +
  geom_text(label = team_df$Team) +
  labs(x = "Average PIC", 
       y = "Proportion of Call Ups", 
       title = "Average PIC vs Proportion of Call Ups by Team")
p3
theme_set(theme_grey())
pic_df %>% # Visualize PIC and Call Ups with Draft and Pos
  group_by(Pos, Draft, CalledUp) %>%
  summarize(PIC_mean = mean(pic)) %>%
  mutate(Pos = as.factor(Pos), Draft = as.factor(Draft), CalledUp = as.factor(CalledUp)) %>%
  filter(Pos %in% c("G", "F", "SG")) %>%
  ggplot(aes(x = Draft, y = PIC_mean, group = CalledUp, color = CalledUp)) + geom_line(size = 1.5) + 
  geom_point(size = 5, shape = 21, fill = "white") + facet_grid(Pos ~ .) + 
  labs(x = "Draft Round", 
       y = "Average PIC", 
       title = "Draft Round vs Average PIC",
       subtitle = "Grouped by Position and Call Up Status")

# Jeremy Lin

# 2021 G League player bios
bios21 <- read_csv("Desktop/NBA Projects/Data/bios21.csv")
# Select important columns and convert height to total inches
bios21 <- bios21 %>%
  select(Player, Pos, HT, WT, `NBA Draft Status`) %>%
  separate(HT, c('HT', 'inches'), sep = '-', convert = TRUE, remove = FALSE) %>%
  mutate(HT = 12 * HT + inches) %>%
  select(-inches)

# Extract draft round from draft status
bios21 <- bios21 %>%
  mutate(Draft = draft_round(`NBA Draft Status`)) %>%
  select(-`NBA Draft Status`)
View(bios21)
table(bios21$Draft)

# 2021 G League data and call ups
gleague2021 <- read_csv("Desktop/NBA Projects/Data/gleague2021.csv")
gleague2021 <- gleague2021[-1]
View(gleague2021)
gleague2021 %>% filter(Player == "Jeremy Lin")
summary(gleague2021['GP'])
hist(gleague2021$GP)
callups2021 <- read_csv("Desktop/NBA Projects/Data/callups2021.csv")
View(callups2021)

# Create called up variable
callup_names21 <- unique(callups2021$Player)
length(callup_names21)
calledUp21 <- ifelse(gleague2021$Player %in% callup_names21, 1, 0)
sum(calledUp21) # Some call ups do not show up because they did not qualify for statistics (didn't play enough)
gleague2021$CalledUp <- calledUp21
table(gleague2021$CalledUp)

# Join bios21 to gleague21
gleague2021 <- left_join(gleague2021, bios21, by = "Player")
str(gleague2021)

pic_df21 <- gleague2021 %>%
  mutate(pic = GP * (PPG + FGM + FTM - FGA - FTA + DRB + ORB / 2 + APG + SPG + BPG / 2 - PF - TOV))
summary(pic_df21$pic)
hist(pic_df21$pic)

predict(pic_mdl, pic_df21 %>% filter(Player == "Jeremy Lin"), type = "response")
df1 <- pic_df21 %>% 
  filter(Player == "Jeremy Lin") %>% 
  select(Player, Pos, Draft, Team, pic, FIC, `USG%`, GP, PPG, APG, RPG, CalledUp)
pic_df21 %>%
  group_by(CalledUp, Pos, Draft) %>%
  summarize(PIC_mean = mean(pic)) %>%
  filter(Pos == "G" & Draft == "Undrafted")
pic_df21 %>%
  filter(Pos == "G" & Draft == "Undrafted" & CalledUp) %>%
  select(Player, Pos, Draft, Team, pic, FIC, `USG%`, GP, PPG, APG, RPG, CalledUp) %>%
  rbind(df1) %>%
  mutate(FIC = FIC / GP, pic = pic / GP)

pic_df %>%
  filter(Pos == "G" & Draft == "Undrafted" & CalledUp) %>%
  select(Player, Pos, Draft, Team, pic, FIC, `USG%`, GP, PPG, APG, RPG, CalledUp) %>%
  rbind(df1) %>%
  mutate(FIC = FIC / GP, pic = pic / GP) %>%
  arrange(desc(pic))

write.csv(pic_df, file = "Desktop/NBA Projects/Data/final_df22.csv") # Save final data sets
write.csv(pic_df21, file = "Desktop/NBA Projects/Data/final_df21.csv")
