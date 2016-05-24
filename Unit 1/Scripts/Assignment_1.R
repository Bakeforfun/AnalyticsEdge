setwd("~/Dropbox/Education/edX/AnalyticsEdge/Unit 1")

##### AN ANALYTICAL DETECTIVE #####
### Problem 1 - Loading the Data ###
mvt = read.csv("Data/mvtWeek1.csv")
str(mvt)
summary(mvt)

max(mvt$ID)
min(mvt$Beat)

table(mvt$Arrest)

nrow(mvt[mvt$LocationDescription == "ALLEY",])

### Problem 2 - Understanding Dates in R ###
mvt$Date[1]
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)

mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

which.min(table(mvt$Month))
which.max(table(mvt$Weekday))

which.max(table(mvt$Month, mvt$Arrest)[,2])

### Problem 3 - Visualizing Crime Trends ###
hist(mvt$Date, breaks=100)
boxplot(mvt$Date)

mvt$Year = as.numeric(format(mvt$Date, "%Y")) 
table(mvt$Year, mvt$Arrest)
table(mvt$Year, mvt$Arrest)[1,2] / (table(mvt$Year, mvt$Arrest)[1,1] + table(mvt$Year, mvt$Arrest)[1,2])
table(mvt$Year, mvt$Arrest)[7,2] / (table(mvt$Year, mvt$Arrest)[7,1] + table(mvt$Year, mvt$Arrest)[7,2])
table(mvt$Year, mvt$Arrest)[12,2] / (table(mvt$Year, mvt$Arrest)[12,1] + table(mvt$Year, mvt$Arrest)[12,2])

### Problem 4 - Popular Locations ###
sort(table(mvt$LocationDescription))

Top5 = subset(mvt, mvt$LocationDescription == "STREET" |
                   mvt$LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" |
                   mvt$LocationDescription == "ALLEY" |
                   mvt$LocationDescription == "GAS STATION" |
                   mvt$LocationDescription == "DRIVEWAY - RESIDENTIAL")

Top5$LocationDescription = factor(Top5$LocationDescription)
summary(Top5$LocationDescription)

table(Top5$LocationDescription, Top5$Arrest)
table(Top5$LocationDescription, Top5$Arrest)[, 2] / 
  (table(Top5$LocationDescription, Top5$Arrest)[, 1] + table(Top5$LocationDescription, Top5$Arrest)[, 2])



##### STOCK DYNAMICS #####
### Problem 1 - Summary Statistics ###
IBM = read.csv("data/IBMStock.csv")
GE = read.csv("data/GEStock.csv")
ProcterGamble = read.csv("data/ProcterGambleStock.csv")
CocaCola = read.csv("data/CocaColaStock.csv")
Boeing = read.csv("data/BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

summary(IBM$Date)
mean(IBM$StockPrice)
min(GE$StockPrice)
max(CocaCola$StockPrice)
median(Boeing$StockPrice)
sd(ProcterGamble$StockPrice)

### Problem 2 - Visualizing Stock Dynamics ###
plot(CocaCola, type='l', main = 'Coca-Cola and IBM stock prices', ylab = '', xlab = 'Date', col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)

plot(CocaCola, type='l', main = 'Coca-Cola and IBM stock prices', ylab = '', xlab = 'Date', col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
abline(v=as.Date(c("1983-03-01")), lwd=2)

### Problem 3 - Visualizing Stock Dynamics 1995-2005 ###
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210),
     main = 'Stock prices', ylab = '', xlab = 'Date')
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="blue")
lines(GE$Date[301:432], GE$StockPrice[301:432], col="green")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="purple")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="orange")
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-01")), lwd=2)

sort(tapply(IBM$StockPrice, months(IBM$Date), mean))
mean(IBM$StockPrice)

sort(tapply(GE$StockPrice, months(GE$Date), mean))
sort(tapply(CocaCola$StockPrice, months(CocaCola$Date), mean))

##### DEMOGRAPHICS AND EMPLOYMENT IN THE UNITED STATES #####
### Problem 1 - Loading and Summarizing the Dataset ###
CPS = read.csv('data/CPSData.csv')
str(CPS)
summary(CPS)

sort(table(CPS$State)) 

(table(CPS$Citizenship)[1] + table(CPS$Citizenship)[2]) / sum(table(CPS$Citizenship))

table(CPS$Race, CPS$Hispanic)

### Problem 2 - Evaluating Missing Values ###
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

table(CPS$State, is.na(CPS$MetroAreaCode))

ftable(CPS$Region, is.na(CPS$MetroAreaCode))

sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

### Problem 3 - Integrating Metropolitan Area Data ###
MetroAreaMap = read.csv('data/MetroAreaCodes.csv')
CountryMap = read.csv('data/CountryCodes.csv')
str(MetroAreaMap)
summary(MetroAreaMap)
str(CountryMap)
summary(CountryMap)

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
sum(is.na(CPS$MetroArea))

sort(table(CPS$MetroArea))

sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))
sort(tapply(CPS$Race == 'Asian', CPS$MetroArea, mean))

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm = TRUE))

### Problem 4 - Integrating Country of Birth Data ###
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
sum(is.na(CPS$Country))

sort(table(CPS$Country))

tapply(CPS$Country != 'United States', CPS$MetroArea, mean, na.rm = TRUE)

sort(tapply(CPS$Country == 'India', CPS$MetroArea, sum, na.rm = TRUE))
sort(tapply(CPS$Country == 'Brazil', CPS$MetroArea, sum, na.rm = TRUE))
sort(tapply(CPS$Country == 'Somalia', CPS$MetroArea, sum, na.rm = TRUE))

##### INTERNET PRIVACY POLL #####
### Problem 1 - Loading and Summarizing the Dataset ###
poll = read.csv('data/AnonymityPoll.csv')
str(poll)
summary(poll)

sum(poll$Smartphone, na.rm = TRUE)
sum(!poll$Smartphone, na.rm = TRUE)
sum(is.na(poll$Smartphone))

table(poll$State, poll$Region)

### Problem 2 - Internet and Smartphone Users ###
sum(!poll$Internet.Use & !poll$Smartphone, na.rm = TRUE)
sum(poll$Internet.Use & poll$Smartphone, na.rm = TRUE)
sum(poll$Internet.Use & !poll$Smartphone, na.rm = TRUE)
sum(!poll$Internet.Use & poll$Smartphone, na.rm = TRUE)

limited = subset(poll, poll$Internet.Use | poll$Smartphone)

### Problem 3 - Summarizing Opinions about Internet Privacy ###
str(limited)
summary(limited)

mean(limited$Info.On.Internet)

sum(limited$Info.On.Internet == 0)
sum(limited$Info.On.Internet == 11)

mean(limited$Worry.About.Info, na.rm = TRUE)
mean(limited$Anonymity.Possible, na.rm = TRUE)
mean(limited$Tried.Masking.Identity, na.rm = TRUE)
mean(limited$Privacy.Laws.Effective, na.rm = TRUE)

### Problem 4 - Relating Demographics to Polling Results ###
hist(limited$Age)

plot(limited$Age, limited$Info.On.Internet)
table(limited$Age, limited$Info.On.Internet)
max(table(limited$Age, limited$Info.On.Internet))

plot(jitter(limited$Age), jitter(limited$Info.On.Internet))
plot(jitter(limited$Age), (limited$Info.On.Internet))

tapply(limited$Info.On.Internet, limited$Smartphone, mean)
tapply(limited$Tried.Masking.Identity, limited$Smartphone, mean, na.rm = TRUE)
