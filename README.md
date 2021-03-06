# SamsungActivityPostureData
Smartphone-Based Recognition of Human Activities and Postural Transitions Data Set 

README FILE
Coursera Data Science Course - Getting and Cleaning Data - PROJECT ASSIGNMENT

Source Data:
The database used in this analysis represents data collected from the accelerometers from the Samsung Galaxy S Smartphone and was obtained from the UCI Machine Learning Repository. You can find this database at the URL:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Study Design:
The purpose of this study was to generate data from the accelerometer and gyroscope located in a smartphone device and associate these measurements with a number of activities that were carried out by device wearers. The hope being that this data would help build signature device reading patterns for each activity that could then be used so the device would know what activity  an individual was performing without being informed. 
30 volunteers (ages 19-48 years old) performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a Samsung Galaxy S Smartphone on their waist. Using the devices accelerometer and gyroscope, 3-axial linear acceleration and 3-axial angular velocity at a constant rate were captured. Video-Recordings were also made to allow data labelling. The sensor signal were pre-processed and filtered. 

Downloaded Data Files:
The data from this study contains the files used in producing the tidyData set generated by “run_analysis.R” and can be found at:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The link above will prompt download of a .zip file containing a number of files. Some of these files (located in the “Intertial Signals” folders) contained raw sample data and these were not used in analysis. 
For the purpose of this project the following files were placed in the working directory for R: (Note - Some were uploaded into R for use and others were used for reference)

“activity_labels.txt” - this file was used for reference to determine which number in the “y_test.txt and “y_train.txt” files corresponded to which of the 6 named activities. 

“features_info.txt” - This file contains details of the signals and filters and functions used on them to generate variables that are included in the “features.txt” file. 

“features.txt” - This file lists the variable names of the 561 variables. This file was used as the basis for labelling variable names for the data loaded in from “X-test.txt” and “X_train.txt” files. 

“subject_test.txt” - This file provided data for 30% of the participants. It gave the assigned number (between 1 and 30) of the subject from which each row of data in the “X-test. txt” was derived.  

“subject_train.txt” - This file provided data for 70% of the participants. It gave the assigned number (between 1 and 30) of the subject from which each row of data in the “X-train.txt” was derived.  

“X_test.txt” - This file provided data for 30% of the participants in the test set. It provides all the feature measurements recorded for the variables listed in “features.txt” (561 variables in total) for each participant performing each activity.

“X_train.txt” - This file provided data for 70% of the participants in the training data set. It provides all the feature measurements recorded for the variables listed in “features.txt” (561 variables in total) for each participant performing each activity.

“y_test.txt” - This file provided data for 30% of the participants. It gave the assigned number (between 1 and 6) of the activity from which each row of data in the “X-test.txt” was derived.  

“y_train.txt” - This file provided data for 70% of the participants. It gave the assigned number (between 1 and 6) of the activity from which each row of data in the “X-train.txt” was derived.

Aim of Project:
The aim of this project was to create 
(i) A single TIDY DATA SET containing all the mean and std related feature measurements data produced from all 30 participants for each activity they carried out. 
(ii) Create a second INDEPENDENT TIDY DATA SET with the average of each feature measurement variable for each activity and each subject. 
The later of these steps is achieved through the run_analysis.R program provided in this repo. To create the initial TIDY DATA SET run_analysis should be truncated to include lines 1 to 72. All analysis was run using RStudio (version 0.99.441) on a Mac running OS X Yosemite (Version 10.10.4)

ANALYSIS STEPS - see run_analysis.R
STEP 1: Read the "features.txt" into R using the read.table() function. This provides a table from which a vector of feature names can be extracted. As this vector of names contains brackets, dashes and commas there are not R-compliant variable names. In order to use them as such the vector was passed through the make.names() function. This vector was used later to label the columns of data-frames generated from data in “X_test.txt” and “X_train.txt” files. 

STEP 2: The test data to be included in the data-frame is contained in 3 files: “subject_test.txt”, “y_test.txt” and “”X_test.txt”. These 3 files were loaded into R separately using the read.table() function and given column names “Subject”, “Activity_Code” and names derived from the vector featureNames (generated in Step 1), respectively. The individual data-frames were then merged together sequentially, using the cbind() function to produce the data frame testData. 

STEP 3: The train data to be included in the data-frame is contained in 3 files: “subject_train.txt”, “y_train.txt” and “”X_train.txt”. These 3 files were loaded into R separately using the read.table() function and given column names “Subject”, “Activity_Code” and names derived from the vector featureNames (generated in Step 1), respectively. The individual data-frames were then merged together sequentially, using the cbind() function to produce the data frame trainData. 

STEP 4: In order to get the data for all 30 participants into one data frame the testData and trainData data frames were merged using rbind. This produces the data frame allData. 

STEP 5: The dataFrame allData contains information for each 10922 observations and 563 variable (Subject, Activity_Code and 561 feature measurement variables). A requirement of the project was to “extract only the measurements on the mean and standard deviation for each measurement”. This was open to interpretation as many feature measurement variable names contained the term “mean”. This script takes a harsh view of what the a measurement on the mean and standard deviation is and included only the variables where a mean() or std() function was implied. This meant that other feature measurements such as those containing the word “freqmean” and “angle” had to be excluded. The inclusion and exclusion of columns was carried out on the allData data frame using the select() function from the dplyer package. 

STEP 6: The “Activity_Code” variable in the allData data frame is a categorical variable and as such it is confusing to have its values as numbers 1-6. For clarity and to avoid mix-up the “Activity_Code” (1-6) data from allData was mapped to a vector of activity names (“activities”). The resulting vector was then coerced into a data-frame and merged onto the allData data frame using the cbind() function to give a new variable “Activity”. 

STEP 7: The allData data frame was then sorted first on the “Subject “ variable and then on “Activity_Code” variable using the arrange() function in the deployer package. 

STEP 8: The resulting re-ordered allData data-frame was then tidied further so that the first column of the data frame was “Subject” and the second column was “Activity”. In the process, the now redundant “Activity_Code” variable was removed from the data-frame. Up to step 8 concludes the generation of the TIDY DATA SET containing all the mean and std related feature measurements data produced from all 30 participants for each activity they carried out.

STEP 9: To generate a second INDEPENDENT TIDY DATA SET with the average of each feature measurement variable for each activity and each subject this additional step was carried out: The allData data-frame was grouped by “Subject” and “Activity” variables using the group_by() function in deployer to create a data frame called GroupedData. The summarise_each() function was then applied to the GroupedData to find the mean of each column for each group in the data frame. The resulting data frame, named, meanSumData has 180 observations for 68 variables (Subject, Activity and mean of each feature measurement). 


