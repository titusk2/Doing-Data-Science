#Titus Karuri

soccer_player = read.csv("M:/Titus/SMU Data Science/Doing Data Science/FIFA Players.csv")

df = soccer_player %>% filter(Position == "LM" | Position == "LF")


df %>% select(Acceleration,Agility,Position) %>% ggpairs(aes(color =Position))

df %>% group_by(Position) %>% summarize(mean_agility = mean(Agility))

df %>% ggplot(aes(x = Agility, fill = Position)) + geom_histogram() + facet_wrap(~Position)

ball_control_factor = cut(fifa$BallControl, breaks = c(1,50,70,99),labels = c("Average","Good","Elite"))

fifa %>% mutate(ball_control_factor = ball_control_factor) %>% select(Preferred.Foot ,ball_control_factor,Finishing,ShotPower) %>% 
  ggpairs(mapping = aes(color = ball_control_factor,binwidth=5))

fifa %>% mutate(ball_control_factor = ball_control_factor) %>%filter(!is.na(ball_control_factor)) %>% 
  group_by(ball_control_factor) %>% summarize(median = median(Finishing))

fifa %>% ggplot(aes(y = Finishing, color = ball_control_factor)) + geom_boxplot() + ggtitle("relationship between ball control and finishing")

fifa %>% ggplot(aes(x = ShotPower, y = Finishing)) + geom_point()

fifa %>%  mutate(ball_control_factor = ball_control_factor) %>% filter(!is.na(ball_control_factor)) %>% 
  ggplot(aes(x = ball_control_factor, y = Finishing))+ geom_point()

2 * pt(-0.53, df=10)

#turn the agility ratings taken from the fifa players csv file into data frames
LM_agility  <- data.frame(position = "LM", agility=c(76,91,93,86,74,92))
LF_agility  <- data.frame(position = "LF", agility=c(95,91,79,89,87,84))

#combine the data frames into one data frame
agility_score <- rbind(LM_agility,LF_agility)

#histogram with agility ratings
ggplot(agility_score,aes(x = agility, fill = position)) + geom_histogram(binwidth = 5, color = "black") + facet_wrap(~position) +
ggtitle("Agility ratings by position") + labs(y = "Frequency")


fifa %>% mutate(ball_control_factor = ball_control_factor) %>% 
  group_by(ball_control_factor) %>% summarize(median = median(Finishing))

