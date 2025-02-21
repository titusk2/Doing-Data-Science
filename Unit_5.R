#Unit 5 
#Titus Karuri

#Part 1
ball_players = read.csv("M:/Titus/SMU Data Science/Doing Data Science/PlayersBBall.csv")
ball_players

ball_players$height <- str_extract_all(ball_players$height, "\\d+", simplify = TRUE)
ball_players$Total_Inches <- as.numeric(ball_players$height[,1])*12 + as.numeric(ball_players$height[,2])

 ball_players %>% ggplot(aes(x=Total_Inches, fill = position)) + geom_histogram(stat = "count") + ggtitle("Height diferences between positions") +
  ylab("Total_Numbers")

#Part 2
soccer_player = read.csv("M:/Titus/SMU Data Science/Doing Data Science/FIFA Players.csv")
soccer_player

soccer_player$Height <- str_extract_all(soccer_player$Height, "\\d", simplify = TRUE)
soccer_player$total_height <- as.numeric(soccer_player$Height[,1])*12 + as.numeric(soccer_player$Height[,2])

soccer_player$Weight <- str_extract_all(soccer_player$Weight, "\\d+", simplify = TRUE)
soccer_player$total_weight <- as.numeric(soccer_player$Weight[,1])

soccer <- na.omit(soccer_player)

soccer  %>%  ggplot(aes(x=total_weight, y = total_height, color = Position)) +  geom_point(show.legend = FALSE) + facet_wrap(~Position) + 
  ggtitle("Relationship between Height and Weight at different positions")

soccer_LM_LF = soccer_player %>% filter(Position == "LM" | Position == "LF") 
soccer_LM_LF %>% select(total_weight,total_height,Position) %>% ggpairs(aes(color =Position))


#Part 3
df <- read.table("M:/Titus/SMU Data Science/Doing Data Science/yob2016.txt",sep = ";" , header = FALSE, col.names = c("Name", "Gender", "Amount_with_same_name"))
summary(df)

misspelled_name <- df[grep("yyy",df$Name),]
print(misspelled_name)

misspelled_name_index <- grep("yyy", df$Name)

y2016 <- df[-misspelled_name_index,]
y2016[212,]

y2015 <- read.table("M:/Titus/SMU Data Science/Doing Data Science/yob2015.txt",sep = "," , header = FALSE, col.names = c("Name", "Gender", "Amount_with_same_name"))
tail(y2015,10)

final <- merge(y2016,y2015, by = "Name")
print(final)

tail(final,10)

final$Total <- as.numeric(final$Amount_with_same_name.x) + as.numeric(final$Amount_with_same_name.y)
sum(final$Total)
arrange(final, desc(Total))

final_girls_only <- filter(final, final$Gender.x == "F" & final$Gender.y == "F" )
arrange(final_girls_only, desc(Total))

top_10_girl_names <- arrange(final_girls_only, desc(Total))

write.csv(top_10_girl_names[1:10,c("Name","Total")],"Top_10_Girl_names.csv", row.names = FALSE)

top_Girl_names <- top_10_girl_names[1:10,]

 ggplot(top_Girl_names, aes(x=reorder(Name, desc(Total)), y = Total, fill = Name)) + geom_bar(stat = "identity", show.legend = FALSE) +
   labs(title = "Top 10 Most Popular Girl Names in 2015 and 2016", x = "Name", y = "Total",
        caption ="Source: yob2015.txt and yob2016.txt") 
 
 final_boys_only <- filter(final, final$Gender.x == "M" & final$Gender.y == "M" )
 arrange(final_boys_only, desc(Total))
 
 top_10_boy_names <- arrange(final_boys_only, desc(Total))
  top_Boy_names <- top_10_boy_names[1:10,]
   
 ggplot(top_Boy_names, aes(x=reorder(Name, desc(Total)), y = Total, fill = Name)) + geom_bar(stat = "identity", show.legend = FALSE) +
   labs(title = "Top 10 Most Popular Boy Names in 2015 and 2016", x = "Name", y = "Total",
        caption ="Source: yob2015.txt and yob2016.txt") 
