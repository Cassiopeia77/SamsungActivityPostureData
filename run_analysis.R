# STEP 1. READ IN COLUMN/FEATURE MEASUREMENT NAMES FROM "features.txt" and TIDY UP 
# Read in table of feature names (column names) from the file "features.txt"
featureNames <- read.table("features.txt")

# Extract a vector of feature names which will be the column names for feature measurments
featureNames <- featureNames[, "V2"]
# This vector of character names has brackets, dashes and commas so make the names R-compliant using make.names() function. 
featureNames <- make.names(featureNames, unique=TRUE, allow_=TRUE)


# 2. READ TEST SET DATA INTO R and INTO A SINGLE DATA-FRAME: 
# Note: The test set contains data for subjects 2, 4, 9, 10, 12, 13, 18, 20 and 24.

# Read in table of test subjects and call column "Subject". 
# This loads in a data frame of 2947 observations of 1 variable (subject).  
subjectTest <- read.table("subject_test.txt", col.names="Subject")

# Read in table of test activity and call column "Activity"
# This loads in a data frame of 2947 observations of 1 variable (activity)
activityTest <- read.table("y_test.txt", col.names = "Activity_Code")

# Read in table of feature measurements (test) and call columns according to the vector "featureNames" 
# This loads in a data frame of 2947 observations and 561 variables (feature measurments)
featuresTest <- read.table("X_test.txt", col.names = featureNames)

# Now to merge all the test data together: use cbind()
testData <- cbind(subjectTest, activityTest)
testData <- cbind(testData, featuresTest)


# 3. READ TRAINING SET DATA INTO R and INTO A SINGLE DATA-FRAME: 
# Note: The test set contains data for subjects 1, 3, 5, 6, 7, 8, 11, 14, 15, 16, 17, 19, 21, 22, 23, 25, 26, 27, 28, 29 and 30.
# Read in table of traiing subjects and call column "Subject". 
# This loads in a data frame of 7352 observations of 1 variable (subject).  
subjectTrain <- read.table("subject_train.txt", col.names="Subject")

# Read in table of training activity and call column "Activity"
# This loads in a data frame of 7352 observations of 1 variable (activity)
activityTrain <- read.table("y_train.txt", col.names = "Activity_Code")

# Read in table of feature measurements (train) and call columns according to the vector "featureNames" 
# This loads in a data frame of 7352 observations and 561 variables (feature measurments)
featuresTrain <- read.table("X_train.txt", col.names = featureNames)

# Now to merge all the train data together: use cbind()
trainData <- cbind(subjectTrain, activityTrain)
trainData <- cbind(trainData, featuresTrain)


# 4. NOW MERGE THE TEST AND TRAINING DATA-FRAMES TOGETHER using rbind() 
allData <- rbind(trainData, testData)
# You can check the intersection of the merge using:
allData[7350:7355, 1:4]


# 5. LOOSE THE VARIABLES THAT ARE NOT TO DO WITH MEAN() or STD() using select() from dplyr package. 
# This select function keeps Subject and Activity_Codes columns, any columns containing "mean" or "std" and removes any columns containing "meanFreq" or "angle"
allData <- select(allData, Subject, Activity_Code, contains("mean", ignore.case=TRUE), contains("std", ignore.case=TRUE), -contains("meanFreq", ignore.case=TRUE), -contains("angle", ignore.case=TRUE))

# 7. MAP ACTIVITY_CODES (1-6) TO ACTIVITY NAMES and cbind() this column "Activity" to allData
activities <- c('WALKING', 'WALKING_UPSTAIRS', 'WALKING_DOWNSTAIRS', 'SITTING', 'STANDING', 'LAYING')
activity_number <- allData[ ,2]
Activity <- activities[activity_number]
Activity <- as.data.frame(Activity)
allData <- cbind(Activity, allData)


# 7. SORT THE DATA FRAME ON SUBJECT 1..30 (and then ACTIVITY 1..6) using arrange() in dplyr package
allData <- arrange(allData, Subject, Activity_Code)

# 8 REARRANGE DATAFRAME so that Subject is FIRST and ACTIVITY second and ACTIVTY_CODES is lost 
# Should be able to do this in one line but for now it is mulitple as I'm not sure how to do it efficiently
subject_activity <- allData[, c("Subject", "Activity")]
allData <- select(allData, -Activity, -Subject, -Activity_Code)
allData <- cbind(subject_activity, allData)

# 9 Group by Subject and Activity and then find the mean for each variable. 
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Uses the group_by() function in dplyr to group the data by Subject and Activity
groupedData <- group_by(allData, Subject, Activity)
# Then uses the summarize_each() function in dplyr to find the mean for each column of each group. 
# The resulting data frame has 180 observations for 68 variables (Subject, Activity and feature measurements)
meanSumData <- summarise_each(groupedData,funs(mean))
