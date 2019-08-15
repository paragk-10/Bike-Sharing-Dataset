rm(list=ls(all=TRUE))
library(data.table) 
library(ggplot2)
library(reshape2)
library(tidyverse)
library(corrplot)

context = fread('day.csv')
summary(context)
table(is.na(context))
str(context)
context2 <- as.data.frame(context)

context2$yr=as.numeric(context2$yr)
context2$mnth=as.numeric(context2$mnth)
context2$season=as.numeric(context2$season)
context2$holiday=as.numeric(context2$holiday)
context2$weekday=as.numeric(context2$weekday)
context2$workingday=as.numeric(context2$workingday)
context2$weathersit=as.numeric(context2$weathersit)
context2$casual=as.numeric(context2$casual)
context2$registered=as.numeric(context2$registered)
context2$cnt=as.numeric(context2$cnt)
str(context2)

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

context3=context2[,-c(1,2,15,14)]
corrplot(cor(context3))

table(context2$season)
table(context2$weathersit)

boxplot(cnt~season, data=context2, main="Different boxplots for each season", xlab="Season", ylab="Count", col="orange")

boxplot(cnt~weathersit, data=context2, main="Different boxplots for each season", xlab="Season", ylab="Count", col="orange")

ggplot(data = context2, mapping = aes(x = season, y = cnt,fill = factor(season))) +
  geom_boxplot()

ggplot(data = context2, mapping = aes(x = weathersit, y = cnt,fill = factor(weathersit))) +
  geom_boxplot()

ggplot(data = context2, mapping = aes(x = mnth, y = cnt,fill = factor(mnth))) +
  geom_boxplot()

ggplot(data = context2, mapping = aes(x = workingday, y = cnt,fill = factor(workingday))) +
  geom_boxplot()

ggplot(data = context2, mapping = aes(x = weekday, y = cnt,fill = factor(weekday))) +
  geom_boxplot()

# ggplot(data = context2, mapping = aes(x = weekday, y = registered)) +
# geom_bar(stat="identity",fill="blue")
# ggplot(data = context2, mapping = aes(x = weekday, y = casual)) +
#   geom_bar(stat="identity",fill="blue")

df1 <- data.frame(context2$registered, context2$casual, context2$weekday)
df2 <- melt(df1, id.vars='context2.weekday')
ggplot(df2, aes(x=context2.weekday, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')

df3 <- data.frame(context2$registered, context2$casual, context2$holiday)
df4 <- melt(df3, id.vars='context2.holiday')
ggplot(df4, aes(x=context2.holiday, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')

df5 <- data.frame(context2$registered, context2$casual, context2$mnth)
df6 <- melt(df5, id.vars='context2.mnth')
ggplot(df6, aes(x=context2.mnth, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')

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

yearly_counts <- context2 %>%
  count(yr, season)
ggplot(data = yearly_counts, mapping = aes(x = yr, y = n)) +
  geom_line()+
  facet_wrap(~ season)


model <- lm(data = context3, cnt~mnth+temp+atemp+hum+windspeed)
summary(model)

model2 <- glm(data = context3, cnt~as.factor(mnth)+as.factor(weathersit)+temp+atemp+hum+windspeed)
summary(model2)
1-(1201819598/2739535392)

model3 <- glm(data = context3, cnt~as.factor(season)+as.factor(mnth)+as.factor(weathersit)+temp+hum+windspeed)
summary(model3)
1-(1147923776/2739535392)

model4 <- glm(data = context3, cnt~as.factor(season)+as.factor(yr)+as.factor(holiday)+as.factor(weekday)+as.factor(workingday)+as.factor(mnth)+as.factor(weathersit)+temp+atemp+hum+windspeed)
summary(model4)
1-(415401688/2739535392)

model5 <- glm(data = context3, cnt~as.factor(yr)+as.factor(weekday)+as.factor(workingday)+as.factor(mnth)+as.factor(weathersit)+temp+atemp+hum+windspeed)
summary(model5)
1-(467112813/2739535392)

library(dummies)
df11 <- context3
df11 <- cbind(df11, dummy(df11$season, sep = "season"))
df11 <- cbind(df11, dummy(df11$yr, sep = "yr"))
df11 <- cbind(df11, dummy(df11$mnth, sep = "mnth"))
df11 <- cbind(df11, dummy(df11$holiday, sep = "holiday"))
df11 <- cbind(df11, dummy(df11$weekday, sep = "weekday"))
df11 <- cbind(df11, dummy(df11$workingday, sep = "workingday"))
df11 <- cbind(df11, dummy(df11$weathersit, sep = "weathersit"))
df11 <- df11[,-c(1:7)]

model6 <- lm(data = df11, cnt~.)
summary(model6)

