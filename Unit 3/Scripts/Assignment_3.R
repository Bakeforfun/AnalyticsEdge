setwd("~/Dropbox/Education/edX/AnalyticsEdge/Unit 3")

##### POPULARITY OF MUSIC RECORDS #####
### Problem 1 - Understanding the Data ###
songs = read.csv("Data/songs.csv")
table(songs$year)
table(songs$artistname)
songs$songtitle[songs$Top10 == 1 & songs$artistname == 'Michael Jackson']
summary(as.factor(songs$timesignature))
table(songs$timesignature)
songs$songtitle[which.max(songs$tempo)]

### Problem 2 - Creating Our Prediction Model ###
SongsTrain = subset(songs, year <= 2009)
SongsTest = subset(songs, year > 2009)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

### Problem 3 - Beware of Multicollinearity Issues! ###
cor(SongsTrain$loudness, SongsTrain$energy)

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

### Problem 4 - Validating Our Model ###
SongsPred = predict(SongsLog3, newdata = SongsTest, type = "response")
table(SongsTest$Top10, SongsPred >= 0.45)
# Accuracy: (19 + 309) / (19 + 309 + 5 + 40)

table(SongsTest$Top10, SongsPred >= 1)
# Baseline: 314 / (314 + 59)

# Sensitivity: 19 / (19 + 40)
# Specificity: 309 / (309 + 5)
