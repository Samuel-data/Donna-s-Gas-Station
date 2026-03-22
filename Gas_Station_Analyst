library(tidyverse)
library(dplyr)})
library(ggplot2)
library(janitor)
Donna <- read.csv("DonnaGasStation.csv", header = TRUE, sep = ",", quote = '"')

#1.) Data Cleaning

clean_Donna <- clean_names(Donna)

names(clean_Donna)[1]<- "week_num"
names(clean_Donna)[2]<- "Weekday"
names(clean_Donna)[3]<- "D_Price"
names(clean_Donna)[4]<- "competitor_price"
names(clean_Donna)[5]<- "D_Gal_sold"
> colnames(clean_Donna)
[1] "week_num"           "weekday"            "D's_Sales"          "competitor's_price"
[5] "D's_Sales" 

> unique(clean_Donna$weekday)
[1] "Monday"    "Tuesday"   "Wednesday" "Thursday"  "Friday"    "Saturday"  "Sunday"   
> length(unique(clean_Donna$weekday))
[1] 7
> unique(clean_Donna$week_num)
[1] 1 2 3 4 5 6 7 8
> length(unique(clean_Donna$week_num))
[1] 8

New_clean_Donna <- na.omit(clean_Donna)
New_clean_Donna

duplicated(New_clean_Donna)
[1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
[16] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
[31] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
[46] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

is.na(New_clean_Donna)
sum(is.na(New_clean_Donna))
[1] 0


#2.) Five Number Summary
summary(clean_Donna)
fivenum(New_clean_Donna$D_Price)
[1] 3.270 3.405 3.495 3.590 3.710
fivenum(New_clean_Donna$D_Gal_sold)
[1]  817.03  936.14  968.45  997.52 1092.33
fivenum(New_clean_Donna$competitor_price)
[1] 3.27 3.41 3.50 3.59 3.71
sapply(New_clean_Donna[c('D_Price', 'D_Gal_sold', 'competitor_price')], fivenum)
      D_Price   D_Gal_sold  competitor_price
[1,]   3.270     817.03             3.27
[2,]   3.405     936.14             3.41
[3,]   3.495     968.45             3.50
[4,]   3.590     997.52             3.59
[5,]   3.710    1092.33             3.71


#3.) tabulation table
Donna_categorical <- table(New_clean_Donna$week_num, New_clean_Donna$weekday)
Donna_categorical
Friday Monday Saturday Sunday Thursday Tuesday Wednesday
1      1      1        1      1        1       1         1
2      1      1        1      1        1       1         1
3      1      1        1      1        1       1         1
4      1      1        1      1        1       1         1
5      1      1        1      1        1       1         1
6      1      1        1      1        1       1         1
7      1      1        1      1        1       1         1
8      1      1        1      1        1       1         1

#4.) BOTH histograms & boxplots (2 numerical col)
#Histogram
ggplot(data = New_clean_Donna) + geom_histogram(mapping=aes(x=D_Price, fill = weekday), bins = 10)
ggplot(data = New_clean_Donna) + geom_histogram(mapping=aes(x=D_Gal_sold, fill = weekday), bins = 10)

#Boxplot
boxplot(New_clean_Donna$D_Price)
boxplot(New_clean_Donna$D_Gal_sold)
boxplot(New_clean_Donna$competitor_price)

#5.) histogram categorized by at least one categorical variable, with COLOR
ggplot(data = New_clean_Donna) + geom_histogram(aes(x= week_num, fill = "weekday"), bins = 20)

#6.) Create a properly labeled bar graph for one categorical column
ggplot(data = New_clean_Donna) + geom_bar(mapping=aes(x= weekday, fill = weekday))

#7.) Create a properly labeled stacked or dodge bar graph for 2 categorical variables
ggplot(data = New_clean_Donna) + geom_bar(mapping=aes(x = week_num), position="dodge")
ggplot(data = New_clean_Donna) + geom_bar(mapping=aes(x = weekday), position="dodge")

#8.) Create a properly labeled Scatter plot of two appropriate variables: Include trend line
#scatterplot
ggplot(data = New_clean_Donna, mapping = aes(x = D_Price, y = D_Gal_sold)) + geom_point(mapping = aes(color = weekday))+ geom_smooth()
ggplot(data = New_clean_Donna, mapping = aes(x = competitor_price, y = D_Gal_sold)) + geom_point(mapping = aes(color = weekday))+ geom_smooth()

#9.) 
ggplot(data=New_clean_Donna)+geom_point(mapping=aes(x = D_Price, y = D_Gal_sold,color=weekday))+facet_wrap(~weekday)

