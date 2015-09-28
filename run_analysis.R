# STEP 1. READ IN COLUMN/FEATURE MEASUREMENT NAMES FROM "features.txt" and TIDY UP 
# Read in table of feature names from the file "features.txt"
featureNames <- read.table("features.txt")
# Extract a vector of feature names that will be the column names for feature measurments
featureNames <- featureNames[, "V2"]
# This vector of character names has brackets, dashes and commas so make the names R-compliant using make.names() function. 
featureNames <- make.names(featureNames, unique=TRUE, allow_=TRUE)


# 2. READ TEST SET DATA INTO R and COMPILE INTO A SINGLE DATA-FRAME called "testData": 
# Note: The test set contains data for subjects 2, 4, 9, 10, 12, 13, 18, 20 and 24.

# Read in table of test subjects from "subject.txt" and call column "Subject". 
# This loads in a data frame of 2947 observations of 1 variable (subject).  
subjectTest <- read.table("subject_test.txt", col.names="Subject")

# Read in table of test activity codes from "y_test.txt" and call column "Activity_Code"
# This loads in a data frame of 2947 observations of 1 variable (Activity_Code)
activityTest <- read.table("y_test.txt", col.names = "Activity_Code")

# Read in table of feature measurements (test) from "X_test.txt" and call columns according to the vector "featureNames" 
# This loads in a data frame of 2947 observations and 561 variables (feature measurments)
featuresTest <- read.table("X_test.txt", col.names = featureNames)

# Now MERGE all the test data together: use cbind()
testData <- cbind(subjectTest, activityTest)
testData <- cbind(testData, featuresTest)


# 3. READ TRAINING SET DATA INTO R and COMPILE INTO A SINGLE DATA-FRAME called "trainData": 
# Note: The test set contains data for subjects 1, 3, 5, 6, 7, 8, 11, 14, 15, 16, 17, 19, 21, 22, 23, 25, 26, 27, 28, 29 and 30.
# Read in table of training subjects from "subject_train.txt" and call column "Subject". 
# This loads in a data frame of 7352 observations of 1 variable (subject).  
subjectTrain <- read.table("subject_train.txt", col.names="Subject")

# Read in table of training activity codes from "y_train.txt" and call column "Activity_Code"
# This loads in a data frame of 7352 observations of 1 variable (Activity_Code)
activityTrain <- read.table("y_train.txt", col.names = "Activity_Code")

# Read in table of feature measurements (train) from "X_train.txt" and call columns according to the vector "featureNames" 
# This loads in a data frame of 7352 observations and 561 variables (feature measurments)
featuresTrain <- read.table("X_train.txt", col.names = featureNames)

# Now to MERGE all the train data together: use cbind()
trainData <- cbind(subjectTrain, activityTrain)
trainData <- cbind(trainData, featuresTrain)


# 4. NOW MERGE THE TEST AND TRAINING DATA-FRAMES TOGETHER using rbind() 
allData <- rbind(trainData, testData)
# You can check the intersection of the merge using: > allData[7350:7355, 1:4]


# 5. OMIT THE VARIABLES THAT ARE NOT TO DO WITH mean() or std() using the select() function from the dplyr package. 
# This select function keeps Subject and Activity_Code columns, any columns containing "mean" or "std" and removes any columns containing "meanFreq" or "angle"
allData <- select(allData, Subject, Activity_Code, contains("mean", ignore.case=TRUE), contains("std", ignore.case=TRUE), -contains("meanFreq", ignore.case=TRUE), -contains("angle", ignore.case=TRUE))


# 7. MAP THE ACTIVITY_CODE (1-6) TO ACTIVITY NAMES, CONVERT TO A SUITABLE FORM (with "Activity" as variable name) to cbind() to the DATA-BASE "allData"
activities <- c('WALKING', 'WALKING_UPSTAIRS', 'WALKING_DOWNSTAIRS', 'SITTING', 'STANDING', 'LAYING')
activity_number <- allData[ ,2]
Activity <- activities[activity_number]
Activity <- as.data.frame(Activity)
allData <- cbind(Activity, allData)

# 8. SORT THE DATA FRAME ON SUBJECT 1..30 (and then ACTIVITY 1..6) using arrange() in dplyr package
allData <- arrange(allData, Subject, Activity_Code)

# 9 RE-ARRANGE the DATA FRAME so that "Subject" is 1ST and "Activity" is 2ND and "Activity_Code" is OMITTED 
subject_activity <- allData[, c("Subject", "Activity")]
allData <- select(allData, -Activity, -Subject, -Activity_Code)
allData <- cbind(subject_activity, allData)

# 10 GROUP by "Subject" AND "Activity" AND THEN FIND THE MEAN FOR EACH VARIABLE
# Uses the group_by() function in dplyr to group the "alldata" by Subject and Activity
groupedData <- group_by(allData, Subject, Activity)
# Then use the summarize_each() function in dplyr to find the mean for each column of each group in "groupedData" 
# The resulting data frame has 180 observations for 68 variables (Subject, Activity and mean of each feature measurement)
meanSumData <- summarise_each(groupedData,funs(mean))
