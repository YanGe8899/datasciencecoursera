########################
# Course Project for Getting and Cleaning Data

########################################################################################################
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, 
#    creates a second, independent tidy data set with the average of each variable for each activity and each subject.
########################################################################################################
library("data.table")
library("reshape2")
## Download, unzip data
file.dir <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(file.dir,  "./ProjectdataFiles.zip")
unzip(zipfile = "ProjectdataFiles.zip")

## read activity labels, feature
activitylabels <- fread("UCI HAR Dataset/activity_labels.txt",col.names = c("class","activityName"))
features <- fread("UCI HAR Dataset/features.txt",col.names = c("index","featureName"))

## read training data
# extract only mean, std meansurements
Xtrain <- fread("UCI HAR Dataset/train/X_train.txt",col.names = features$featureName)
ytrain <- fread("UCI HAR Dataset/train/y_train.txt",col.names = "activityClass")
subjecttrain <- fread("UCI HAR Dataset/train/subject_train.txt",col.names = "subjectNum")
train <- cbind(ytrain,subjecttrain,Xtrain)

## read test data
Xtest <- fread("UCI HAR Dataset/test/X_test.txt",col.names = features$featureName)
ytest <- fread("UCI HAR Dataset/test/y_test.txt",col.names = "activityClass")
subjecttest <- fread("UCI HAR Dataset/test/subject_test.txt",col.names = "subjectNum")
test <- cbind(ytest,subjecttest,Xtest)

## Merges the training and the test sets to create one data set.
MergeSet <- rbind(train,test)

## Uses descriptive activity names to name the activities in the data set
MergeSet$activityClass <- activitylabels$activityName[MergeSet$activityClass]
## Appropriately labels the data set with descriptive variable names
colnames(MergeSet)[1] <- "activityName"
colnames(MergeSet)[3:ncol(MergeSet)] <- gsub('[()]', '', colnames(MergeSet)[3:ncol(MergeSet)])

## Extracts only the measurements on the mean and standard deviation for each measurement.
featureChoose <- grep("(mean|std)\\(\\)",features$featureName) 
MergeSetChoose <- MergeSet[ ,c(1,2,(2+featureChoose)),with=FALSE]
write.table(MergeSetChoose,"tidyData_all.txt",row.names = FALSE)


## creates a second, independent tidy data set with the average of each variable for each activity and each subject.
MergeSetChoose.second <- MergeSetChoose

MergeSetChoose.second <- melt(data = MergeSetChoose.second, id = c("subjectNum", "activityName"))
MergeSetChoose.second <- dcast(data = MergeSetChoose.second, subjectNum + activityName ~ variable, mean)

write.table(MergeSetChoose.second,"tidyData_average.txt",row.names = FALSE)

