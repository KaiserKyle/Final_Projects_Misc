library(e1071)
library(ggplot2)

# Load the data set
rawData <- read.csv("d:\\data\\athlete_events.csv")

# There were a handful of Figure Skating events that took place in the Summer
# For the purpose of this study, we will move these to the Winter, as it is now
# traditionally known as a Winter event.
rawData[which(rawData$Season =="Summer" & rawData$Sport == "Figure Skating"),]$Season <- "Winter"

# Print Summary
print(str(rawData))

# Prep for SVM modeling

# Select 'modern' data, starting in 1970
svmData <- rawData[which(rawData$Year >= 1970),]
svmData <- svmData[c("Height", "Weight", "Age", "Sex", "NOC", "Sport")]
svmData <- na.omit(svmData)

# Split data into male and female datasets
maleSvmData <- svmData[which(svmData$Sex == "M"),]
femaleSvmData <- svmData[which(svmData$Sex =="F"),]

# (Comment out one of these two lines in order to make a model)
# Create female model
#modelData <- femaleSvmData
# Create male model
modelData <- maleSvmData

# Pick only sports with over 100 participants
sportCounts <- as.data.frame(table(modelData$Sport))
sportCounts <- sportCounts[which(sportCounts$Freq >= 100),]
finalSvmData <- modelData[modelData$Sport %in% sportCounts$Var1,]
finalSvmData$Sport <- droplevels(finalSvmData$Sport)

# Split into test and training data set
randIndex <- sample(1:dim(finalSvmData)[1])
cutPoint <- floor(2 * dim(finalSvmData)[1] / 3)
trainData <- finalSvmData[randIndex[1:cutPoint],]
testData <- finalSvmData[randIndex[(cutPoint+1):dim(finalSvmData)[1]],]

# Create the models
svmOutput <- svm(Sport ~ Height + Weight + Age + NOC, data=trainData, probability=TRUE)
svmNoNOCOutput <- svm(Sport ~ Height + Weight + Age, data=trainData, probability=TRUE)

# Test the model
svmPrediction <- predict(svmOutput, testData, probability=TRUE)
results <- attr(svmPrediction,"probabilities")

svmNoPrediction <- predict(svmNoNOCOutput, testData, probability=TRUE)
resultsNoNoc <- attr(svmNoPrediction,"probabilities")

# Function to predict best sports for given
# model/height/weight/age/NOC
predictIndividual <- function(model, height, weight, age, NOC) {
  df <- data.frame(Height = height, Weight = weight, Age = age, NOC = NOC)
  prediction <- predict(model, df, probability=TRUE)
  res <- attr(prediction, "probabilities")
  
  return(res)
}

# Predict results for myself
meResults <- predictIndividual(svmOutput, 188, 77, 33, "USA")
print(meResults)

# This function returns the match list for our test data based on the
# first n results in the probability
getAccuracy <- function(results, testData, n) {
  # Grab the top n most probable sports for each test row
  resultsList <- list()
  index <- 1
  
  for (row in 1:nrow(testData)) {
    resultsList[[index]] <- colnames(results)[order(results[index,], decreasing = TRUE)][1:n]
    index <- index + 1
  }
  resultTop5 <- data.frame(matrix(unlist(resultsList), nrow=nrow(testData), byrow=T))
  
  matches <- c()
  # Check if the actual sport is in the top five
  for (row in 1:nrow(testData)) {
    matches <- append(matches, as.character(testData$Sport[row]) %in% resultsList[[row]])
  }
  
  return(matches)
}

matchSvmPercents <- c()
matchSvmNoNocPercents <- c()
randMatchPercents <- c()

for (i in 1:44) {
  temp <- getAccuracy(results, testData, i)
  # Get the match percent for the TOP i by probability in our SVM model
  matchSvmPercents <- append(matchSvmPercents, length(temp[temp == TRUE]) / length(temp))
  
  temp <- getAccuracy(resultsNoNoc, testData, i)
  # Get the match percent for the TOP i by probability in our NB model
  matchSvmNoNocPercents <- append(matchSvmNoNocPercents, length(temp[temp == TRUE]) / length(temp))
  
  # Get the match percent if we just random picked i sports
  randMatchPercents <- append(randMatchPercents, i / 44)
}

accDf <- data.frame(matchSvmPercents, matchSvmNoNocPercents, randMatchPercents)
matchPlot <- ggplot(data = accDf, aes(x = as.numeric(row.names(accDf)), y = matchSvmPercents))
matchPlot <- matchPlot + geom_line(aes(color="SVM - With Country"), size = 2) + geom_point(color = "mediumblue", size = 3)
matchPlot <- matchPlot + geom_line(aes(y=randMatchPercents, color="Random Chance"), size = 2)
matchPlot <- matchPlot + geom_line(aes(y=matchSvmNoNocPercents, color = "SVM - No Country"), size = 2) + geom_point(aes(y=matchSvmNoNocPercents), color = "darkgreen", size = 3)
matchPlot <- matchPlot + xlab("Number of Sports Picked") + ylab("Correctness on Test Data") + ggtitle("Success Rates for SVM With and Without Country")
matchPlot <- matchPlot + scale_color_manual("Model", values=c("SVM - With Country"="cadetblue", "SVM - No Country"="darkolivegreen3", "Random Chance"="orange"))
matchPlot <- matchPlot + theme(legend.position="bottom")

print(matchPlot)
