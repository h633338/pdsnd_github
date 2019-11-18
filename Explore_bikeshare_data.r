
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(wash)

head(chi)

#Most common day of the week
# Chicago
library(ggplot2)
library(lubridate)
date_Start.Time <- as.Date(chi$Start.Time)
date <- c(date_Start.Time)
x <- ymd(date)
dwka <- format(x , "%a")
dwkn <- as.numeric( format(x , "%w") )
hist( dwkn , breaks= -.5+0:7, labels= unique(dwka[order(dwkn)]), main = "Chicago most common day of the week")

# Wash
library(ggplot2)
library(lubridate)
date_Start.Time <- as.Date(wash$Start.Time)
date <- c(date_Start.Time)
x <- ymd(date)
dwka <- format(x , "%a")
dwkn <- as.numeric( format(x , "%w") )
hist( dwkn , breaks= -.5+0:7, labels= unique(dwka[order(dwkn)]), main = "Washington most common day of the week")

# NY
library(ggplot2)
library(lubridate)
date_Start.Time <- as.Date(ny$Start.Time)
date <- c(date_Start.Time)
x <- ymd(date)
dwka <- format(x , "%a")
dwkn <- as.numeric( format(x , "%w") )
hist( dwkn , breaks= -.5+0:7, labels= unique(dwka[order(dwkn)]), main = "New York most common day of the week")





#What are the counts of each user type?

table(chi$User.Type)
table(wash$User.Type)
table(ny$User.Type)

#What are the counts of each gender (only available for NYC and Chicago)?

table(chi$Gender)
table(wash$Gender)
table(ny$Gender)


#What are the earliest, most recent, most common year of birth (only available for NYC and Chicago)?

summary(ny$Birth.Year)
summary(chi$Birth.Year)

library(ggplot2)
ggplot(data = ny, aes(x = ny$Birth.Year)) +
  geom_histogram(binwidth = 1)+
  scale_x_continuous(limits = c(1920, 2001), breaks = seq(1920, 2001, 10)) + labs(title="Distribution of user by birth year in New York", x ="Birth year", y = "Count")


ggplot( data = chi, aes(x = chi$Birth.Year)) +
  geom_histogram(binwidth = 1)+
  scale_x_continuous(limits = c(1920, 2001), breaks = seq(1920, 2001, 10)) + labs(title="Distribution of user by birth year in Chicago", x ="Birth year", y = "Count")




The first 3 tables show the break-down user by type in Chicago, Washington and NY. The next 2 tables show the break-down of users by gender in Chicago and NY. In NY, the oldest user was born in 1932 and the youngest in 2001. In Chicago, the oldest user was born in 1930 and the youngest in 2001.

library(ggplot2)

#3 Trip duration

#What is the total travel time for users in different cities?

#What is the average travel time for users in different cities?

summary(chi$Trip.Duration)
summary(wash$Trip.Duration)
summary(ny$Trip.Duration)

ggplot(data = chi, aes(x = chi$Trip.Duration)) +
geom_histogram(binwidth = 30, color = 'black', fill = '#099DD9') +
scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 100)) + labs(title="Distribution of trip duration in Chicago", x ="Trip duration", y = "Count")

qplot(x = User.Type, y = Trip.Duration, data = subset(chi, !is.na(User.Type)), geom = 'boxplot', ylim = c(0, 10000)) + labs(title="Plot of trip duration by user type in Chicago", x ="User type", y = "Trip duration")


ggplot(data = wash, aes(x = wash$Trip.Duration)) +
  geom_histogram(binwidth = 30, color = 'black', fill = '#099DD9') +
scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 100)) + labs(title="Distribution of trip duration in Washington", x ="Trip duration", y = "Count")


qplot(x = User.Type, y = Trip.Duration, data = subset(wash, !is.na(User.Type)), geom = 'boxplot', ylim = c(0, 10000)) + labs(title="Plot of trip duration by user type in Washington", x ="User type", y = "Trip duration")


ggplot(data = ny, aes(x = ny$Trip.Duration)) +
  geom_histogram(binwidth = 30, color = 'black', fill = '#099DD9') +
scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 100)) + labs(title="Distribution of trip duration in New York", x ="Trip duration", y = "Count")


qplot(x = User.Type, y = Trip.Duration, data = subset(ny, !is.na(User.Type)), geom = 'boxplot', ylim = c(0, 10000)) + labs(title="Plot of trip duration by user type in New York", x ="User type", y = "Trip duration")


system('python -m nbconvert Explore_bikeshare_data.ipynb')
