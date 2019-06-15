library(ggplot2)

# Load the data set
rawData <- read.csv("d:\\data\\athlete_events.csv")

# There were a handful of Figure Skating events that took place in the Summer
# For the purpose of this study, we will move these to the Winter, as it is now
# traditionally known as a Winter event.
rawData[which(rawData$Season =="Summer" & rawData$Sport == "Figure Skating"),]$Season <- "Winter"

# Print Summary
print(str(rawData))

## Simple exploratory Box Plots for our athlete data
# Height
boxplot(rawData$Height, horizontal = TRUE, axes = FALSE)
text(x=fivenum(rawData$Height), labels=fivenum(rawData$Height), y= .75)
title("Distribution of Athlete Height (cm)")

# Weight
boxplot(rawData$Weight, horizontal = TRUE, axes = FALSE)
text(x=fivenum(rawData$Weight), labels=fivenum(rawData$Weight), y= .75)
title("Distribution of Athlete Weight (kg)")

# Age
boxplot(rawData$Age, horizontal = TRUE, axes = FALSE)
text(x=fivenum(rawData$Age), labels=fivenum(rawData$Age), y= .75)
title("Distribution of Age")

## Deeper boxplot for height data per sport
# First make a new data frame with the data we want
heightData <- data.frame(rawData$Height, rawData$Sport, rawData$Season, rawData$Sex)
colnames(heightData) <- c("Height","Sport","Season","Sex")
# Drop NA's
heightData <- na.omit(heightData)

# Clean up data for height graph
# First, remove any sport with less than 5 known heights
sportCounts <- table(heightData$Sport)
heightData <- heightData[heightData$Sport %in% names(sportCounts[sportCounts >= 5]),]
# Also, remove Art Competitions, as we are mainly concerned with athletic events
heightData <- heightData[-which(heightData$Sport == "Art Competitions"),]

# Create the height boxplot
heightBox <- ggplot(heightData, aes(x=reorder(Sport, Height, mean),y=Height)) + geom_boxplot()
heightBox <- heightBox + theme(axis.text.x = element_text(angle = 90, hjust = 1))
heightBox <- heightBox + facet_grid(rows = vars(Sex), cols = vars(Season), scales="free", space = "free_x")
heightBox <- heightBox + theme(strip.background = element_rect(fill="red",color="blue"))
heightBox <- heightBox + theme(strip.text = element_text(color = "white", face ="bold", size = 12))
heightBox <- heightBox + labs(title = "Olympic Athlete Height By Sport") + xlab("Sport")
print(heightBox)

# Gather aggregates statistics on the spread of each sport
# for deeper analysis
aggFiveNum <- do.call(data.frame, aggregate(heightData$Height, list(heightData$Sport,heightData$Sex), FUN = function(x) c(five = fivenum(x), count = length(x))))
names(aggFiveNum) <- c("Sport", "Sex", "Min", "25thPerc", "Median", "75thPerc", "Max", "Count")
aggFiveNum$Range <- aggFiveNum$Max - aggFiveNum$Min
aggFiveNum$QuartileRange <- aggFiveNum$'75thPerc' - aggFiveNum$'25thPerc'
aggFiveNum <- aggFiveNum[order(-aggFiveNum$QuartileRange),]
print(aggFiveNum)

## Deeper boxplot for weight data per sport
# First make a new data frame with the data we want
weightData <- data.frame(rawData$Weight, rawData$Sport, rawData$Season, rawData$Sex)
colnames(weightData) <- c("Weight","Sport","Season","Sex")
# Drop NAs
weightData <- na.omit(weightData)

# Clean up data for height graph
# First, remove any sport with less than 5 known heights
sportCounts <- table(weightData$Sport)
weightData <- weightData[weightData$Sport %in% names(sportCounts[sportCounts >= 5]),]
# Also, remove Art Competitions, as we are mainly concerned with athletic events
weightData <- weightData[-which(weightData$Sport == "Art Competitions"),]

# Create the height boxplot
weightBox <- ggplot(weightData, aes(x=reorder(Sport, Weight, mean),y=Weight)) + geom_boxplot()
weightBox <- weightBox + theme(axis.text.x = element_text(angle = 90, hjust = 1))
weightBox <- weightBox + facet_grid(rows = vars(Sex), cols = vars(Season), scales="free", space = "free_x")
weightBox <- weightBox + theme(strip.background = element_rect(fill="red",color="blue"))
weightBox <- weightBox + theme(strip.text = element_text(color = "white", face ="bold", size = 12))
weightBox <- weightBox + labs(title = "Olympic Athlete Weight By Sport") + xlab("Sport")
print(weightBox)

# Gather aggregates statistics on the spread of each sport
# for deeper analysis
aggFiveNum <- do.call(data.frame, aggregate(weightData$Weight, list(weightData$Sport,weightData$Sex), FUN = function(x) c(five = fivenum(x), count = length(x))))
names(aggFiveNum) <- c("Sport", "Sex", "Min", "25thPerc", "Median", "75thPerc", "Max", "Count")
aggFiveNum$Range <- aggFiveNum$Max - aggFiveNum$Min
aggFiveNum$QuartileRange <- aggFiveNum$'75thPerc' - aggFiveNum$'25thPerc'
aggFiveNum <- aggFiveNum[order(-aggFiveNum$QuartileRange),]
print(aggFiveNum)

## Deeper boxplot for Age data per sport
# First make a new data frame with the data we want
ageData <- data.frame(rawData$Age, rawData$Sport, rawData$Season, rawData$Sex)
colnames(ageData) <- c("Age","Sport","Season","Sex")
# Drop NAs
ageData <- na.omit(ageData)

# Clean up data for height graph
# First, remove any sport with less than 5 known heights
sportCounts <- table(ageData$Sport)
ageData <- ageData[ageData$Sport %in% names(sportCounts[sportCounts >= 5]),]
# Also, remove Art Competitions, as we are mainly concerned with athletic events
ageData <- ageData[-which(ageData$Sport == "Art Competitions"),]

# Create the height boxplot
ageBox <- ggplot(ageData, aes(x=reorder(Sport, Age, mean),y=Age)) + geom_boxplot()
ageBox <- ageBox + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ageBox <- ageBox + facet_grid(rows = vars(Sex), cols = vars(Season), scales="free", space = "free_x")
ageBox <- ageBox + theme(strip.background = element_rect(fill="red",color="blue"))
ageBox <- ageBox + theme(strip.text = element_text(color = "white", face ="bold", size = 12))
ageBox <- ageBox + labs(title = "Olympic Athlete Age By Sport") + xlab("Sport")
print(ageBox)

# Gather aggregates statistics on the spread of each sport
# for deeper analysis
aggFiveNum <- do.call(data.frame, aggregate(ageData$Age, list(ageData$Sport,ageData$Sex), FUN = function(x) c(five = fivenum(x), count = length(x))))
names(aggFiveNum) <- c("Sport", "Sex", "Min", "25thPerc", "Median", "75thPerc", "Max", "Count")
aggFiveNum$Range <- aggFiveNum$Max - aggFiveNum$Min
aggFiveNum$QuartileRange <- aggFiveNum$'75thPerc' - aggFiveNum$'25thPerc'
aggFiveNum <- aggFiveNum[order(-aggFiveNum$QuartileRange),]
print(aggFiveNum)

hist(aggFiveNum$QuartileRange, xlab = "Quartile Range")