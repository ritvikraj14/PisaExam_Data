# Reading testing and training dataset
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

# calculate number of row
nrow(pisaTrain)
str(pisaTrain)

# average reading score of male and female
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

# Check features which have NA's
summary(pisaTrain)

# Remove NA values 
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

# New observations
summary(pisaTrain)

# Look into the dataset and check which one is ordered and unordered

# For unordered data, refeence level is set is 1 and all other to 0

# Reset reference level as the most occuring one
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

summary(pisaTrain)
summary(pisaTest)

# Build model and read R squared
lmScore = lm(readingScore~., data=pisaTrain)
summary(lmScore)
# Predicting score based on the available features is difficult, which is making R squared very low.

# calculated SSE and RMSE
SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE / nrow(pisaTrain))
# Alternate way of calculating RMSE: sqrt(mean(lmScore$residuals^2))

View(RMSE)

# The coefficient 29.54 on grade is the difference in reading score between two students who are identical other than having a difference in grade of 1. Because A and B have a difference in grade of 2, the model predicts that student A has a reading score that is 2*29.54 larger.

# check which variable is significant and which is not
summary(lmScore)

# range between the maximum and minimum predicted reading score on the test set
predTest = predict(lmScore, newdata=pisaTest)
summary(predTest)

# Calculate SSE and RMSE of lmScore on the testing set
sum((predTest-pisaTest$readingScore)^2)
sqrt(mean((predTest-pisaTest$readingScore)^2))

# predicted test score used in the baseline model
baseline = mean(pisaTrain$readingScore)

# calculate SST
sum((baseline-pisaTest$readingScore)^2)

# The test-set R^2 is defined as 1-SSE/SST
# For this model, the R^2 is then computed to be 1-5762082/7802354



