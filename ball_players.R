ggplot(data = mpg,aes(x=hwy,y=cty)) + geom_point()

mpg %>% ggplot(aes(x = hwy, y = cty)) + geom_point()

ball_players = read.csv("C:/Users/Newze/R/basketball.csv")
ball_players

p = ggplot(ball_players,aes(x=position)) + geom_bar(stat = "count") + ggtitle("Number of basketball players in each position") #question 1
ggplotly(p)

ball_players %>% ggplot(aes(x=position , y= weight)) + geom_boxplot(color = "blue", fill = "red") #question 2

ball_players %>% ggplot(aes(x=position , y= height)) + geom_point(color = "blue", fill = "red") #question 3

ball_players %>% ggplot(aes(x=position , y= height)) + geom_point(color = "blue", fill = "red") + facet_grid(~position) #question 4

t = ball_players %>% ggplot(aes(x=height, fill = position)) + geom_histogram(color = "blue",stat = "count") + ggtitle("Height diferences between positions") #question 5
ggplotly(t)

ball_players %>%ggplot(aes(x=height,y= weight, linetype = position, color = position)) + geom_point() + geom_smooth() #question 6

ball_players %>%ggplot(aes(x=year_start , y = height , linetype = position , color = position)) + geom_point() #question 8

p <- plot_ly(ball_players, x = ~height, y = ~weight , z = ~year_start, color = ~position) %>% add_markers() %>% layout(scene = list(xaxis = list(title = "height"), yaxis = list(title = "weight"), zaxis=list(title = "start year"))) #question 9
ggplotly(p)

education_income = read.csv("C:/Users/Newze/R/Education_Income.csv")
education_income

education_income %>% ggplot(aes(x=Educ, y = Income2005, color = Educ)) + geom_point() + ggtitle("income based on education level") #question 9

pie <- ggplot(ball_players, aes(x = "", fill = factor(position))) +
  geom_bar(width = 1) +
  theme(axis.line = element_blank(),
        plot.title = element_text(hjust=0.5)) +
  labs(fill="position",
       x=NULL,
       y=NULL,
       title="Number of Basketball players at each position",
       caption="Source: basketball.csv")

pie + coord_polar(theta = "y", start=0)
