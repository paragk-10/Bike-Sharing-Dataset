rm(list=ls(all=TRUE))
library(data.table) 
library(gradDescent) 
library(bigmemory)
library(ggplot2)
library(reshape2)
library(ggExtra)
library(bigalgebra)
library(tidyverse)
library(corrplot)
options(bigalgebra.mixed_arithmetic_returns_R_matrix=FALSE)

# Importing the dataset
context = fread('hour.csv')
summary(context)

# Look for missing values if present
table(is.na(context))

# Look at the types of Variable
str(context)

# create a copy of context and change all int to numeric
context2 <- as.data.frame(context)
context2$yr=as.numeric(context2$yr)
context2$mnth=as.numeric(context2$mnth)
context2$hr=as.numeric(context2$hr)
context2$season=as.numeric(context2$season)
context2$holiday=as.numeric(context2$holiday)
context2$weekday=as.numeric(context2$weekday)
context2$workingday=as.numeric(context2$workingday)
context2$weathersit=as.numeric(context2$weathersit)
context2$casual=as.numeric(context2$casual)
context2$registered=as.numeric(context2$registered)
context2$cnt=as.numeric(context2$cnt)
str(context2)

# Plot histogram for all variables
par(mfrow=c(4,3))
par(mar=rep(3,4))
hist(context2$season)
hist(context2$holiday)
hist(context2$weekday)
hist(context2$workingday)
hist(context2$weathersit)
hist(context2$temp)
hist(context2$atemp)
hist(context2$hum)
hist(context2$windspeed)
hist(context2$casual)
hist(context2$registered)
hist(context2$cnt)

# create a copy of context2 and correlation matrix
context3=context2[,-c(1,2,15,16)]
corrplot(cor(context3))

# generate frequency tables for variables: season and weathersit
table(context2$season)
table(context2$weathersit)

# create boxplot for various variables
boxplot(cnt~season, data=context2, main="Different boxplots for each season", xlab="Season", ylab="Count", col="orange")
boxplot(cnt~weathersit, data=context2, main="Different boxplots for each weather", xlab="Weather", ylab="Count", col="orange")

ggplot(data = context2, mapping = aes(x = season, y = cnt,fill = factor(season))) +
  geom_boxplot()

ggplot(data = context2, mapping = aes(x = weathersit, y = cnt,fill = factor(weathersit))) +
  geom_boxplot()

ggplot(data = context2, mapping = aes(x = mnth, y = cnt,fill = factor(mnth))) +
  geom_boxplot()

ggplot(data = context2, mapping = aes(x = hr, y = cnt,fill = factor(hr))) +
  geom_boxplot()

ggplot(data = context2, mapping = aes(x = workingday, y = cnt,fill = factor(workingday))) +
  geom_boxplot()

ggplot(data = context2, mapping = aes(x = weekday, y = cnt,fill = factor(weekday))) +
  geom_boxplot()

# Look at count of registered and casual users according to different variables
df1 <- data.frame(context2$registered, context2$casual, context2$weekday)
df2 <- melt(df1, id.vars='context2.weekday')
ggplot(df2, aes(x=context2.weekday, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+
  xlab('weekdays') +
  ylab('hourly count')+
  ggtitle('Hourly count of registered and casual users according to days of the week')

df3 <- data.frame(context2$registered, context2$casual, context2$holiday)
df4 <- melt(df3, id.vars='context2.holiday')
ggplot(df4, aes(x=context2.holiday, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+
  xlab('Holiday') +
  ylab('hourly count')+
  ggtitle('Hourly count of registered and casual users according to holiday')

df5 <- data.frame(context2$registered, context2$casual, context2$mnth)
df6 <- melt(df5, id.vars='context2.mnth')
ggplot(df6, aes(x=context2.mnth, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+
  xlab('month') +
  ylab('hourly count')+
  ggtitle('Hourly count of registered and casual users according to months')

# Look at hourly count vs different variables
ggplot(context2,aes(x=atemp,y=cnt))+
  geom_point(alpha=0.07, color='green')+
  xlab('feels like temperature') +
  ylab('hourly count')+
  ggtitle('Plot of atemp vs cnt') +
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'black')

ggplot(context2,aes(x=temp,y=cnt))+
  geom_point(alpha=0.5, color='green')+
  xlab('Temperature') +
  ylab('hourly count')+
  ggtitle('Plot of temp vs cnt') +
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'black')

# Look at how count changed in both the years based on different variables (found in yearly_counts variable)
yearly_counts <- context2 %>%
  count(yr, season)
ggplot(data = yearly_counts, mapping = aes(x = yr, y = n)) +
  geom_line()+
  facet_wrap(~ season)

yearly_counts2 <- context2 %>%
  count(yr, hr)
ggplot(data = yearly_counts2, mapping = aes(x = yr, y = n)) +
  geom_line()+
  facet_wrap(~ hr)

# Linear Regression
model <- lm(data = context3, cnt~mnth+hr+temp+atemp+hum+windspeed)
summary(model)

# Generalized Linear Regression
model2 <- glm(data = context3, cnt~as.factor(mnth)+as.factor(hr)+as.factor(weathersit)+temp+atemp+hum+windspeed)
summary(model2)
1-(213525876/571761591)

model3 <- glm(data = context3, cnt~as.factor(season)+as.factor(mnth)+as.factor(hr)+as.factor(weathersit)+temp+hum+windspeed)
summary(model3)
1-(211126755/571761591)

model4 <- glm(data = context3, cnt~as.factor(season)+as.factor(yr)+as.factor(holiday)+as.factor(weekday)+as.factor(workingday)+as.factor(mnth)+as.factor(hr)+as.factor(weathersit)+temp+atemp+hum+windspeed)
summary(model4)
1-(179328746/571761591)

model5 <- glm(data = context3, cnt~as.factor(season)+as.factor(yr)+as.factor(weekday)+as.factor(workingday)+as.factor(mnth)+as.factor(hr)+as.factor(weathersit)+temp+atemp+hum+windspeed)
summary(model5)
1-(179328746/571761591)

model6 <- glm(data = context3, cnt~as.factor(hr)+as.factor(yr)+as.factor(weekday)+as.factor(workingday)+as.factor(mnth)+as.factor(weathersit)+temp+atemp+hum+windspeed)
summary(model6)
1-(181727213/571761591)


# Method 2 using dummy variables
library(dummies)
df11 <- context3
df11 <- cbind(df11, dummy(df11$season, sep = "season"))
df11 <- cbind(df11, dummy(df11$yr, sep = "yr"))
df11 <- cbind(df11, dummy(df11$mnth, sep = "mnth"))
df11 <- cbind(df11, dummy(df11$hr, sep = "hr"))
df11 <- cbind(df11, dummy(df11$holiday, sep = "holiday"))
df11 <- cbind(df11, dummy(df11$weekday, sep = "weekday"))
df11 <- cbind(df11, dummy(df11$workingday, sep = "workingday"))
df11 <- cbind(df11, dummy(df11$weathersit, sep = "weathersit"))
df11 <- df11[,-c(1:8)]

model6 <- lm(data = df11, cnt~.)
summary(model6)

#####################################
#Gradient descent function
# gradient_desc <- function(context3, alpha=0.1, maxIter=10, seed=NULL){
#   context3 <- matrix(unlist(context3), ncol=ncol(context3), byrow=FALSE)
#   set.seed(seed)
#   context3 <- context3[sample(nrow(context3)), ]
#   set.seed(NULL)
#   theta <- theta_list(ncol(context3), seed=seed)
#   context3 <- cbind(1, context3)
#   context11 <- context3[,1:ncol(context3)-1]
#   context12 <- context3[,ncol(context3)]
#   Theta2 <- matrix(ncol=length(theta), nrow=1)
#   updateRule <- matrix(0, ncol=length(theta), nrow=1)
#   rwlength <- nrow(context3)
#   for(iteration in 1:maxIter){
#     error <- (context11 %*% t(theta)) - context12
#     for(column in 1:length(theta)){
#       term <- error * context11[,column] 
#       gradient <- sum(term) / rwlength
#       updateRule[1,column] <- 0.05*updateRule[1,column] + (alpha*gradient)
#       Theta2[1,column] = theta[1,column] - updateRule[1,column] 
#     }
#     theta <- Theta2
#   }
#   ret <- theta
#   return(ret)
# }
# 
# theta_list <- function(col, min=0, max=1, seed=NULL){
#   set.seed(seed)
#   thetas <- runif(col, min=min, max=max)
#   set.seed(seed)
#   ret <- matrix(unlist(thetas), ncol=col, nrow=1, byrow=FALSE)
#   return(ret)
# }
# 
# running linear model using gradient_desc function
# model7 <- gradient_desc(context3, alpha = 0.2, maxIter = 1000, seed = 1)
# cbind(model5$coefficients, as.vector(model7))
