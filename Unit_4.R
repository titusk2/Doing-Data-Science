#titus Karuri
install.packages("XML")
library(XML)
library(dplyr)
library(tidyr)
library(stringi)
library(rvest) #html_table, html_node
library(ggplot2)
library(RCurl) #getURL

# Method 1: XML

data <-getURL("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml")
doc <- xmlParse(data)
rootNode <- xmlRoot(doc)
xmlSApply(rootNode, xmlValue)
name <- xpathSApply(doc,"//name",xmlValue)
zip_code <- xpathSApply(doc,"//zipcode",xmlValue)
council_district <- xpathSApply(doc,"//councildistrict",xmlValue)
Baltimore_restaurant = data.frame(name,zip_code,council_district)
Baltimore_restaurant

Baltimore_restaurant$name
length(grep("SUSHI",Baltimore_restaurant$name))

downtown = Baltimore_restaurant %>% filter(council_district == "11")
length(grep("SUSHI",downtown$name))

Baltimore_restaurant %>% arrange(council_district,)

Baltimore_restaurant$council_district <- factor(as.numeric(Baltimore_restaurant$council_district))
Baltimore_restaurant$council_district
Baltimore_restaurant %>%  ggplot(aes(x=council_district, fill = council_district)) + geom_bar() + ggtitle("Number of restaurants in each council district")

sum(grepl("14",Baltimore_restaurant$council_district))
grepl("SUSHI",Baltimore_restaurant$name)
sum(grepl("SUSHI",Baltimore_restaurant$name))
which(grepl("covered",Baltimore_restaurant$description))
as.num
