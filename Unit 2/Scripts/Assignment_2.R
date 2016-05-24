setwd("~/Dropbox/Education/edX/AnalyticsEdge/Unit 2")

##### CLIMATE CHANGE #####
### Problem 1 - Creating Our First Model ###
clmt = read.csv("Data/climate_change.csv")
clmt_train = subset(clmt, clmt$Year <= 2006)
clmt_test = subset(clmt, clmt$Year > 2006)

lm1 = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = clmt_train)
summary(lm1)

### Problem 2 - Understanding the Model ###
cor(clmt_train)

### Problem 3 - Simplifying the Model ###
lm2 = lm(Temp ~ MEI + N2O + TSI + Aerosols, data = clmt_train)
summary(lm2)

### Problem 4 - Automatically Building the Model ###
lm3 = step(lm1)
summary(lm3)

### Problem 5 - Testing on Unseen Data ###
pred = predict(lm3, newdata = clmt_test)
SSE = sum((clmt_test$Temp - pred)^2) # RSS
SST = sum((clmt_test$Temp - mean(clmt_train$Temp))^2) # mean calculated on train set --> baseline model
R2 = 1 - SSE/SST

##### READING TEST SCORES #####
### Problem 1.1 - Dataset size ###
pisaTrain = read.csv("Data/pisa2009train.csv")
pisaTest = read.csv("Data/pisa2009test.csv")
str(pisaTrain)

### Problem 1.2 - Summarizing the dataset ###
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

### Problem 1.3 - Locating missing values ###
summary(pisaTrain)

### Problem 1.4 - Removing missing values ###
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
str(pisaTrain)
str(pisaTest)

### Problem 2.1 - Factor variables ###
str(as.factor(pisaTrain$grade))
str(as.factor(pisaTrain$male))
str(as.factor(pisaTrain$raceeth))

### Problem 2.2 - Unordered factors in regression models ###
summary(as.factor(pisaTrain$raceeth))

### Problem 3.1 - Building a model ###
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(readingScore ~ ., data = pisaTrain)
summary(lmScore)

### Problem 3.2 - Computing the root-mean squared error of the model ###
SSE = sum((lmScore$residuals)^2)
RMSE = sqrt(SSE / nrow(pisaTrain))

### Problem 3.3 - Comparing predictions for similar students ###
# 2 * 29.542707 = 59.09

### Problem 4.1 - Predicting on unseen data ###
pred = predict(lmScore, newdata = pisaTest)
summary(pred)
range = max(pred) - min(pred)

### Problem 4.2 - Test set SSE and RMSE ###
SSE = sum((pisaTest$readingScore - pred)^2)
RMSE = sqrt(SSE / nrow(pisaTest))

### Problem 4.3 - Baseline prediction and test-set SSE ###
baseline = mean(pisaTrain$readingScore)
SST = sum((pisaTest$readingScore - baseline)^2)

### Problem 4.4 - Test-set R-squared ###
R2 = 1 - SSE/SST

##### DETECTING FLU EPIDEMICS VIA SEARCH ENGINE QUERY DATA #####
### Problem 1 - Understanding the Data ###
FluTrain = read.csv("Data/FluTrain.csv")
subset(FluTrain, ILI == max(ILI))
FluTrain$Week[which.max(FluTrain$ILI)]

subset(FluTrain, Queries == max(Queries))

hist(FluTrain$ILI)
plot(FluTrain$Queries, log(FluTrain$ILI))

### Problem 2 - Linear Regression Model ###
FluTrend1 = lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)

r = cor(FluTrain$Queries, log(FluTrain$ILI))
r^2
log(1/r)
exp(-0.5*r)

### Problem 3.1 - Performance on the Test Set ###
FluTest = read.csv("Data/FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

FluTest[which(FluTest$Week == "2012-03-11 - 2012-03-17"), ]
PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]

relativeError = (FluTest$ILI - PredTest1)/FluTest$ILI
relativeError[which(FluTest$Week == "2012-03-11 - 2012-03-17")]

SSE = sum((FluTest$ILI - PredTest1)^2)
RMSE = sqrt(SSE / nrow(FluTest))

### Problem 4 - Training a Time Series Model ###
library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain)

plot(log(FluTrain$ILI), log(FluTrain$ILILag2))

FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)

### Problem 5.1 - Evaluating the Time Series Model in the Test Set ###
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest$ILILag2)

FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]

PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE = sum((FluTest$ILI - PredTest2)^2)
RMSE = sqrt(SSE / nrow(FluTest))

##### STATE DATA (OPTIONAL) #####
### Problem 1 - Data Exploration ###
data(state)

statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,
                  state.division, state.name, state.region)

plot(statedata$x, statedata$y)

tapply(statedata$HS.Grad, statedata$state.region, mean)

plot(statedata$state.region, statedata$Murder)

NortheastData = subset(statedata, state.region == "Northeast")
NortheastData$state.abb[which.max(NortheastData$Murder)]

### Problem 2 - Predicting Life Expectancy - An Initial Model ###
lm1 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata)
summary(lm1)

plot(statedata$Income, statedata$Life.Exp)

### Problem 3 - Predicting Life Expectancy - Refining the Model and Analyzing Predictions ###
lm2 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost, data = statedata)
summary(lm2)

lm3 = lm(Life.Exp ~ Population + Income + Murder + HS.Grad + Frost, data = statedata)
summary(lm3)

lm4 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
summary(lm4)

predTrain = predict(lm4)
sort(predTrain)
statedata$state.name[which.min(statedata$Life.Exp)]
sort(predTrain)
statedata$state.name[which.max(statedata$Life.Exp)]

sort(abs(lm4$residuals))
# sort(abs(statedata$Life.Exp - predict(lm4)))

##### FORECASTING ELANTRA SALES (OPTIONAL) #####
### Problem 1 - Loading the Data ###
elantra = read.csv("Data/elantra.csv")
elantraTrain = subset(elantra, elantra$Year <= 2012)
elantraTest = subset(elantra, elantra$Year > 2012)

### Problem 2 - A Linear Regression Model ###
lm1 = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, data = elantraTrain)
summary(lm1)

### Problem 3 - Modeling Seasonality ###
lm2 = lm(ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy + Queries, data = elantraTrain)
summary(lm2)

### Problem 4 - A New Model ###
elantraTrain$Month = as.factor(elantraTrain$Month)
elantraTest$Month = as.factor(elantraTest$Month)

lm3 = lm(ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy + Queries, data = elantraTrain)
summary(lm3)

### Problem 5 - Multicolinearity ###
elantraTrain$Month = as.numeric(elantraTrain$Month)
cor(elantraTrain[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])
elantraTrain$Month = as.factor(elantraTrain$Month)

### Problem 6.1 - A Reduced Model ###
lm3 = lm(ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy + Queries, data = elantraTrain)
summary(lm3)

lm4 = lm(ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy, data = elantraTrain)
summary(lm4)

### Problem 6.2 - Test Set Predictions ###
pred = predict(lm4, newdata = elantraTest)
SSE = sum((elantraTest$ElantraSales - pred)^2)
baseline = mean(elantraTrain$ElantraSales)
SST = sum((elantraTest$ElantraSales - baseline)^2)
R2 = 1 - SSE/SST

errors = elantraTest$ElantraSales - pred
sort(abs(errors))

elantraTest[which.max(errors), ]