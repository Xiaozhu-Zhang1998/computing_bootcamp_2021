library("ggplot2")
library("GGally")
library("caret")
library("ggfortify")


set.seed(12345)
# load data
df <- read.csv("PATH_TO_THE_DATA_SET/wine.csv")
head(df, 10)
summary(df)
sum(is.na(df['Price']))

trainIndex <- createDataPartition(df$Year, p=0.8, list=FALSE, times=1)
df_train = df[trainIndex, ]
df_test = df[-trainIndex, ]


# plot data
ggpairs(df_train, title="correlogram with ggpairs()") 


qplot(Price, Age, data=df_train)

qplot(Price, Age, data=df_train) + geom_smooth(method='lm', formula= y~x)

qplot(Price, data=df_train)
qplot(exp(Price), data=df_train)


# linear model fitting
lmWine <- lm(Price ~ ., data = df_train)
summary(lmWine)
lmWine <- lm(Price ~ .-Year, data = df_train)
summary(lmWine)
lmWine <- lm(Price ~ Age + AGST + FrancePop + HarvestRain + WinterRain, data = df_train)
summary(lmWine)

# MSE
mean(lmWine$residuals^2)
sqrt(mean(lmWine$residuals^2))


# predict for a new data point
weather <- data.frame(Age = 10, FrancePop=45000, WinterRain = 100, HarvestRain = 40, AGST = 1)
# Prediction of the mean
predict(lmWine, newdata = weather)

mean((df_test$Price - predict(lmWine, newdata = df_test))^2)

# residuals
df_lmWine <- fortify(lmWine)
ggplot(df_lmWine, aes(x = .fitted, y = .resid)) + geom_point()


# from ggfortify
autoplot(lmWine)

