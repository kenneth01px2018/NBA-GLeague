library(tidyverse)
library(caret)
library(gridExtra)
library(ggthemes)

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

# Called up prop. by position
gleague2022 %>%
  group_by(Pos) %>%
  summarize(Prop = mean(CalledUp), Count = n()) %>%
  arrange(desc(Prop)) # Sorted by prop
gleague2022 %>% # Visualize with bar chart
  group_by(Pos) %>%
  summarize(Prop = mean(CalledUp), Count = n()) %>%
  arrange(desc(Count)) %>% # Sorted by total count 
  ggplot(aes(x = reorder(Pos, Count), y = Count, fill = Pos)) + geom_bar(stat = "identity") +
    scale_fill_brewer(palette = "Oranges") + 
    geom_text(aes(label = Count),  hjust = 1.6, color = "black", size = 5) + coord_flip() +
    labs(subtitle = "For the 2021-2022 G-league Season", 
         y = "Number of Players Called Up", 
         x = "Position", 
         title = "Number of Players Called Up by Position") + theme_fivethirtyeight()

# Called up prop. by draft round
gleague2022 %>%
  group_by(Draft) %>%
  summarize(Prop = mean(CalledUp)) %>%
  arrange(desc(Prop)) %>%
  mutate(Prop = round(Prop, 2)) %>%
  ggplot(aes(x = Draft, y = Prop, fill = Draft)) + geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Greens") + 
  geom_text(aes(label = Prop),  vjust = 1.6, color = "grey27", size = 7) +
  labs(subtitle = "For the 2021-2022 G-league Season", 
       y = "Proportion of Players Called Up", 
       x = "Draft Round", 
       title = "Proportion of Players Called Up by Draft Round") + theme_fivethirtyeight() + ylim(0, 0.5)

# Player distribution by draft round
gleague2022 %>%
  group_by(Draft) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count)) %>%
  mutate(prop = round(Count / sum(Count), 2)) %>%
  mutate(ypos = cumsum(prop) - 0.5 * prop ) %>%
  ggplot(aes(x = "", y = prop, fill = Draft)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_fivethirtyeight() + 
  geom_text(aes(y = ypos, label = prop), color = "black", size = 5) +
  scale_fill_brewer(palette = "Blues") + 
  labs(subtitle = "For the 2021-2022 G-league Season", 
       title = "Distribution of G League Players by Draft Round")

# Test of Equal or Given Proportions
totals <- gleague2022 %>% group_by(Draft) %>% summarize(Count = n())
props <- gleague2022 %>% filter(CalledUp == 1) %>% group_by(Draft) %>% summarize(Count = n())
prop.test(props$Count, totals$Count)

# Difference in HT & WT by called up
gleague2022 %>%
  group_by(CalledUp) %>%
  summarize_at(c("HT", "WT"), mean)
t.test(HT ~ CalledUp, data = gleague2022)
t.test(WT ~ CalledUp, data = gleague2022)

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

# Called Up count by team
gleague2022 %>% 
  group_by(Team) %>%
  filter(CalledUp == 1) %>%
  summarize(Count = n()) %>% 
  arrange(desc(Count)) %>%
  top_n(10)

# Team-level visualizations
theme_set(theme_economist())
team_df <- gleague2022 %>% 
  group_by(Team) %>%
  summarize(Prop = mean(CalledUp)) %>%
  mutate(Prop = round(Prop, 2))
p1 <- team_df %>% # Prop.
  ggplot(aes(x = reorder(Team, Prop), y = Prop)) + geom_bar(stat = "identity", fill = "#00BFC4") +
  geom_text(aes(label = Prop),  hjust = 1.1, color = "black", size = 5) + coord_flip() +
  labs(subtitle = "For the 2021-2022 G-league Season", 
       y = "Proportion of Players Called Up", 
       x = "Team", 
       title = "Proportion of Players Called Up by Team")
p1_b <- gleague2022 %>% # Count
  group_by(Team) %>%
  filter(CalledUp == 1) %>%
  summarize(Count = n()) %>% 
  right_join(team_df, by = "Team") %>% 
  mutate(Count = ifelse(is.na(Count), 0, Count)) %>%
  ggplot(aes(x = reorder(Team, Prop), y = Count)) + geom_bar(stat = "identity", fill = "#F8766D") +
  geom_text(aes(label = Count),  hjust = 1.6, color = "black", size = 5) + coord_flip() +
  labs(subtitle = "For the 2021-2022 G-league Season", 
       y = "Number of Players Called Up", 
       x = "Team", 
       title = "Number of Players Called Up by Team")
p1
p1_b

# train-test split (75-25)
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
full_train_acc <- mean(predicted == train$CalledUp)
full_train_acc
predicted <- ifelse(predict(full, test[-1], type="response") > 0.5, 1, 0)
full_test_acc <- mean(predicted == test$CalledUp)
full_test_acc

# FIC model train and test error (71.9%)
predicted <- ifelse(predict(fic_mdl, train, type="response") > 0.5, 1, 0)
fic_train_acc <- mean(predicted == train$CalledUp)
fic_train_acc
predicted <- ifelse(predict(fic_mdl, test[-1], type="response") > 0.5, 1, 0)
fic_test_acc <- mean(predicted == test$CalledUp)
fic_test_acc

# PER model train and test error (65.1%)
predicted <- ifelse(predict(per_mdl, train, type="response") > 0.5, 1, 0)
per_train_acc <- mean(predicted == train$CalledUp)
per_train_acc
predicted <- ifelse(predict(per_mdl, test[-1], type="response") > 0.5, 1, 0)
per_test_acc <- mean(predicted == test$CalledUp)
per_test_acc

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
bic_mdl <- glm(CalledUp ~ FIC + `USG%`, data = train, family = "binomial") # Back BIC model
summary(bic_mdl)

# FIC+USG% model train and test error (73.0%)
predicted <- ifelse(predict(bic_mdl, train, type="response") > 0.5, 1, 0)
bic_train_acc <- mean(predicted == train$CalledUp)
bic_train_acc
predicted <- ifelse(predict(bic_mdl, test[-1], type="response") > 0.5, 1, 0) 
bic_test_acc <- mean(predicted == test$CalledUp)
bic_test_acc

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
  geom_point(size = 3) + geom_smooth(method = lm) + 
  scale_color_manual(values = c("firebrick", "salmon")) + 
  labs(subtitle = "Colored by Call-Ups", 
       x = "FIC",
       y = "USG%",
       title = "FIC vs USG%") + theme_fivethirtyeight()
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
  ggplot(aes(x = FIC_mean, y = USG_sd)) + geom_point(aes(color = CallUp_prop),  size = 15) +
  geom_text(label = team_df$Team, color = "white") + guides(color = guide_legend(title = "Call-Up Prop.")) + 
  scale_color_continuous(trans = 'reverse') + 
  labs(subtitle = "Colored by the proportion of call-ups for each team", 
       y = "USG% Standard Deviation", 
       x = "Average FIC", 
       title = "Average FIC vs USG% Standard Deviation by Team")
p4
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

# PIC (Player Impact Counter)
pic_df <- gleague2022 %>%
  mutate(pic = GP * (PPG + FGM + FTM - FGA - FTA + DRB + ORB / 2 + APG + SPG + BPG / 2 - PF - TOV))
summary(pic_df$pic)
hist(pic_df$pic)

# Train-test split
pic_train <- pic_df[train_idx, ] 
pic_test <- pic_df[-train_idx, ]

# PIC model train and test error (73.0%)
pic_baseline <- glm(CalledUp ~ pic, data = pic_train, family = "binomial")
summary(pic_baseline)
predicted <- ifelse(predict(pic_baseline, pic_train, type="response") > 0.5, 1, 0)
pic_train_acc <- mean(predicted == pic_train$CalledUp)
pic_train_acc
predicted <- ifelse(predict(pic_baseline, pic_test, type="response") > 0.5, 1, 0)
pic_test_acc <- mean(predicted == pic_test$CalledUp)
pic_test_acc
table(predicted, pic_test$CalledUp)

# PIC:Pos:Draft model train and test error (77.5%)
pic_mdl <- glm(CalledUp ~ pic:Pos:Draft, data = pic_train, family = "binomial")
summary(pic_mdl)
predicted <- ifelse(predict(pic_mdl, pic_train, type="response") > 0.5, 1, 0)
mdl_train_acc <- mean(predicted == pic_train$CalledUp)
mdl_train_acc
predicted <- ifelse(predict(pic_mdl, pic_test, type="response") > 0.5, 1, 0)
mdl_test_acc <- mean(predicted == pic_test$CalledUp)
mdl_test_acc
table(predicted, pic_test$CalledUp)

t.test(pic ~ CalledUp, data = pic_df)

# Find average PIC by Pos and Draft
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

# Visualize PIC and Call Ups with Draft and Pos
pic_df %>% 
  group_by(Pos, Draft, CalledUp) %>%
  summarize(PIC_mean = mean(pic)) %>%
  mutate(Pos = as.factor(Pos), Draft = as.factor(Draft), CalledUp = as.factor(CalledUp)) %>%
  filter(Pos %in% c("G", "F", "SG")) %>%
  ggplot(aes(x = Draft, y = PIC_mean, group = CalledUp, color = CalledUp)) + geom_line(size = 1.5, alpha = 0.7) + 
  geom_point(size = 5, shape = 21, fill = "white") + facet_grid(Pos ~ .) + scale_color_manual(values = c("steelblue2", "steelblue4")) + 
  labs(x = "Draft Round", 
       y = "Average PIC", 
       title = "Draft Round vs Average PIC",
       subtitle = "Grouped by Position and Call-Up Status") + theme_gray()

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

# Create PIC variable
pic_df21 <- gleague2021 %>%
  mutate(pic = GP * (PPG + FGM + FTM - FGA - FTA + DRB + ORB / 2 + APG + SPG + BPG / 2 - PF - TOV))
summary(pic_df21$pic)
hist(pic_df21$pic)

# Save JLin obs (2021)
jlin21 <- pic_df21 %>%
  filter(Player == "Jeremy Lin")

# Save JLin obs (2022)
jlin22 <- pic_df21 %>% 
  filter(Player == "Jeremy Lin") %>%
  mutate(pic = pic / GP * 24) # Multiply PIC per game by 2022 median GP

pic_mdl22 <- glm(CalledUp ~ pic:Pos:Draft, data = pic_df, family = "binomial") # Fit model on the entire 2022 data set
predict(pic_mdl22, jlin22, type = "response") # Get model probability for JLin in 2022
pic_mdl21 <- glm(CalledUp ~ pic:Pos:Draft, data = pic_df21, family = "binomial") # Fit model on the entire 2021 data set
predict(pic_mdl21, jlin21, type = "response") # Get model probability for JLin in 2021

# Find mean PIC for both call-up groups (2021)
pic_df21 %>%
  group_by(CalledUp, Pos, Draft) %>%
  summarize(PIC_mean = mean(pic)) %>%
  filter(Pos == "G" & Draft == "Undrafted")

# Find top PIC players with the same Pos (2021 data)
pic_df21 %>%
  filter(Pos == "G") %>%
  mutate(PIC = pic) %>%
  arrange(desc(PIC)) %>%
  select(Player, Pos, Team, PIC, GP, PPG, APG, RPG)

# Compare Lin with called-up players (2021 data)
pic_df21 %>% 
  filter(CalledUp == 1) %>%
  rbind(jlin21) %>%
  mutate(PIC = pic) %>%
  arrange(desc(PIC)) %>%
  select(Player, Pos, Team, PIC, GP, PPG, APG, RPG, CalledUp)

# Compare Lin with other Gs in 2022 (2022 data)
pic_df %>% 
  filter(Pos == "G") %>%
  rbind(jlin22) %>% # Append JLin Data
  mutate(PIC = pic) %>%
  arrange(desc(PIC)) %>%
  select(Player, Pos, Team, PIC, PPG, APG, RPG)

# PIE Analysis
pie22 <- read_csv("Desktop/NBA Projects/Data/pie_df22.csv")

# Clean player names for data joining
nba_names <- c(
  "Derrick Alston Jr.", "Robert Baker", "James Banks III", "Brandon Boston Jr.", "Charlie Brown Jr.",
  "Chaundee Brown Jr.", "TJ Cline", "Jeff Dowtin Jr.", "David Duke Jr.", "Vincent Edwards", "LJ Figueroa",
  "Melvin Frazier Jr.", "Michael Gbinije", "Deng Geu", "RaiQuan Gray", "TJ Haws", "Wes Iwundu",
  "Ra'shad James", "BJ Johnson", "ShawnDre Jones", "Walt Lemon Jr.", "Frank Mason III", "Daxter Miles Jr.", 
  "RJ Nembhard Jr.", "EJ Onu", "John Petty Jr.", "Paul Reed", "Kerwin Roach", "Galen Robinson Jr.", 
  "Trevon Scott", "Chris Smith", "DJ Steward", "DJ Stewart", "JT Thor",              
  "MJ Walker", "Derrick Walton Jr."
)
realgm_names <- c(
  "Derrick Alston, Jr.", "Robert Baker, Jr.", "James Banks", "B.J. Boston, Jr.", "Charlie Brown, Jr.",  "Chaundee Brown, Jr.",   
  "T.J. Cline", "Jeff Dowtin", "David Duke", "Vince Edwards", "L.J. Figueroa", "Melvin Frazier, Jr.", "Michael Patrick Gbinije", 
  "Deng John Geu", "Raiquan Gray", "T.J. Haws", "Wesley Iwundu", "Ra'Shad James",           
  "B.J. Johnson", "ShawnDre' Jones", "Walt Lemon, Jr.", "Frank Mason", "Daxter Miles, Jr.", "R.J. Nembhard", "E.J. Onu", "John Petty, Jr.",        
  "Paul Reed, Jr.", "Kerwin Roach, Jr.", "Galen Robinson, Jr.", "Tre Scott", "Sean Christian Smith", "D.J. Steward", "D.J. Stewart", "J.T. Thor",              
  "M.J. Walker", "Derrick Walton, Jr."
)
for (i in 1:36) { 
  pie22 <- pie22 %>% 
    mutate(Player = replace(Player, Player == nba_names[i], realgm_names[i]))
}
pie22 <- pie22 %>% 
  mutate(Player = replace(Player, Player == "Justin Jackson", c("Justin Jackson (1)", "Justin Jackson (2)")))
pie22 <- gleague2022 %>%
  left_join(pie22 %>% select(Player, PIE), by = "Player")
View(pie22)
summary(pie22$PIE)  

# Train-test split
pie_train <- pie22[train_idx, ] 
pie_test <- pie22[-train_idx, ]

# PIE model train and test error (65.2%)
pie_baseline <- glm(CalledUp ~ PIE, data = pie_train, family = "binomial") 
summary(pie_baseline)
predicted <- ifelse(predict(pie_baseline, pie_train, type="response") > 0.5, 1, 0)
pie_train_acc <- mean(predicted == pie_train$CalledUp)
pie_train_acc
predicted <- ifelse(predict(pie_baseline, pie_test, type="response") > 0.5, 1, 0)
pie_test_acc <- mean(predicted == pie_test$CalledUp)
pie_test_acc
table(predicted, pie_test$CalledUp)

# Accuracy table
train_accs <- round(c(full_train_acc, fic_train_acc, bic_train_acc, pie_train_acc, per_train_acc, pic_train_acc, mdl_train_acc), 2)
test_accs <- round(c(full_test_acc, fic_test_acc, bic_test_acc, pie_test_acc, per_test_acc, pic_test_acc, mdl_test_acc), 2)
mdl_names <- c("Full", "FIC", "FIC+USG", "PIE", "PER", "PIC", "PIC:Pos:Draft")
tibble("Model" = mdl_names, "Train_Accuracy" = train_accs, "Test_Accuracy" = test_accs) %>% arrange(desc(Test_Accuracy))

# 2022 players with highest PIC (no call-ups)
pic_df %>%
  filter(CalledUp == 0) %>%
  arrange(desc(pic)) %>%
  select(Player, Pos, Team, GP, PPG, APG, RPG, pic) %>% print(n = 5)

# 2022 players with highest PIC (all players)
pic_df %>%
  arrange(desc(pic)) %>%
  select(Player, Pos, Team, GP, PPG, APG, RPG, pic) %>% print(n = 5)

# 2022 players with highest FIC (all players)
pic_df %>%
  arrange(desc(FIC)) %>%
  select(Player, Pos, Team, GP, PPG, APG, RPG, FIC) %>% print(n = 5)

# 2023 G League data (as of 2/7/2023)
gleague2023 <- read_csv("Desktop/NBA Projects/Data/gleague2023.csv")
gleague2023 <- gleague2023[-2:-1]

# 2023 players with highest PIC
pic_df23 <- gleague2023 %>%
  mutate(PIC = GP * (PPG + FGM + FTM - FGA - FTA + DRB + ORB / 2 + APG + SPG + BPG / 2 - PF - TOV))
pic_df23 %>%
  arrange(desc(PIC)) %>%
  select(Player, Team, GP, PPG, APG, RPG, PIC) %>% print(n = 10)

# Add PIE to PIC data for final data set
final_df22 <- pic_df %>% 
  add_column(PIE = pie22$PIE)
str(final_df22)

# Write final data sets
write.csv(final_df22, file = "Desktop/NBA Projects/Data/final_df22.csv") 
write.csv(pic_df21, file = "Desktop/NBA Projects/Data/final_df21.csv")
write.csv(pic_df23, file = "Desktop/NBA Projects/Data/final_df23.csv")
