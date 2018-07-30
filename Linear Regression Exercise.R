library(ggplot2)
library(ggthemes)
library(dplyr)
library(caTools)

bike <- read.csv('bikeshare.csv')

correlation <- cor(bike$temp,bike$count)

boxplot <- ggplot(bike,aes(x = factor(season),y = count)) + geom_boxplot(aes(color = factor(season)))

#convert datetime into POSIXct
bike$datetime <- as.POSIXct(bike$datetime)

#creating hour column
bike$hour <- sapply(bike$datetime,function(x){format(x, '%H')})

scatter <- ggplot(filter(bike,workingday == 0),aes(x = hour,y = count)) + geom_point(position=position_jitter(w=1, h=0),aes(color = temp),alpha = 0.5) + scale_color_gradientn(colors = c('dark blue','blue','light blue','light green','yellow','orange','red'))

#modeling and predictions
temp.model <- lm(count ~ temp, bike)
summary(temp.model)

bike$hour <- sapply(bike$hour,as.numeric)

final.summary <- lm(count ~ . - casual - registered - datetime - atemp,bike)
