#Question 1
library(readr)
day <- read_csv("Documents/Maryville_University/DSCI_502/day.csv")
View(day)

#Question 2A
day$season <- as.character(day$season)
for (i in c(1:nrow(day))){
  if (day[i,'season'] == 1){
    day[i, 'season'] <- 'spring'
  }
  if (day[i,'season'] == 2){
    day[i, 'season'] <- 'summer'
  }
  if (day[i,'season'] == 3){
    day[i, 'season'] <- 'fall'
  }
  if (day[i,'season'] == 4){
    day[i, 'season'] <- 'winter'
  }
}

#Question 2B
day$weathersit <- as.character(day$weathersit)
for (i in c(1:nrow(day))){
  if (day[i,'weathersit'] == 1){
    day[i, 'weathersit'] <- 'good'
  }
  if (day[i,'weathersit'] == 2){
    day[i, 'weathersit'] <- 'mist'
  }
  if (day[i,'weathersit'] == 3){
    day[i, 'weathersit'] <- 'bad'
  }
  if (day[i,'weathersit'] == 4){
    day[i, 'weathersit'] <- 'severe'
  }
}

#Question 3
day$season <- as.factor(day$season)
day$holiday <- as.factor(day$holiday)
day$workingday <- as.factor(day$workingday)
day$weathersit <- as.factor(day$weathersit)

is.factor(day$season)
is.factor(day$holiday)
is.factor(day$workingday)
is.factor(day$weathersit)

#Question 4
min(day$cnt)
max(day$cnt)
mean(day$cnt)
median(day$cnt)
sd(day$cnt)
quantile(day$cnt, probs = c(0.25, 0.5, 0.75))

#Question 5
min(day$registered)
max(day$registered)
mean(day$registered)
median(day$registered)
sd(day$registered)
quantile(day$registered, probs = c(0.25, 0.5, 0.75))

#Question 6
cor(day$registered, day$cnt)

#Question 7
table(day$season)

#Question 8
xtab_season_weather <- xtabs(~ season + weathersit, data = day)
prop.table(xtab_season_weather, margin = 1) #row
prop.table(xtab_season_weather, margin = 2) #column

#Question 9
library(ggplot2)
ggplot(data = day, aes(x=cnt)) + 
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white") +
  geom_density(alpha=.2, fill="pink") + 
  geom_vline(aes(xintercept=mean(cnt)), color="green", linetype="dashed", linewidth=1)

#Question 10
ggplot(data = day, aes(x = registered, y=cnt)) +
  geom_point() + 
  geom_smooth()

#Question 11
ggplot(data = day, aes(x=season, y= ..count..)) + 
    geom_bar(aes(fill = weathersit), position = "dodge")

#Question 12
ggplot(data=day, aes(x=weathersit, y=cnt)) + 
  geom_boxplot(aes(col= weathersit ), notch = TRUE)
ggsave("C:\\Users\\symphonyhopkins\\Documents\\Maryville University\\DSCI_502\\cntweather.jpg", width = 20, height = 15, units = "cm")

#Question 13A
lm_result_13a <- lm(cnt ~ season + weathersit + atemp + registered, data= day)
summary(lm_result_13a)

#Question 13B
lm_result_13b <- lm(cnt ~ season + workingday + weathersit + atemp + registered, data= day)
summary(lm_result_13b)

#Question 13C
lm_result_13c <- lm(cnt ~ season + holiday + workingday + weathersit + atemp + hum + windspeed + registered, data= day)
summary(lm_result_13c)

#Question 14A
library(pscl)
glm_result_14a <- glm(holiday ~ cnt + season + registered, family=binomial, data=day) 
summary(glm_result_14a)
pR2(glm_result_14a)

#Question 14B
glm_result_14b <- glm(holiday ~ cnt + season + weathersit + registered, family=binomial, data=day) 
summary(glm_result_14b)
pR2(glm_result_14b)

#Question 14C
glm_result_14c <- glm(holiday ~ cnt + season + weathersit + workingday + registered, family=binomial, data=day) 
summary(glm_result_14c)
pR2(glm_result_14c)

