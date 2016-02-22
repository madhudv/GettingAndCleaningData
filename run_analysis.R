library(dplyr)
library(data.table)
library(tidyr)
setwd("./UCI HAR Dataset")

## step 1: Read data from the files
testActivities <- tbl_df(read.table("test/y_test.txt", col.names="activity"))
testSubjects <- tbl_df(read.table("test/subject_test.txt", col.names="subject"))
testData <- tbl_df(read.table("test/X_test.txt"))
trainActivities <- tbl_df(read.table("train/y_train.txt", col.names="activity"))
trainSubjects <- tbl_df(read.table("train/subject_train.txt", col.names="subject"))
trainData <- tbl_df(read.table("train/X_train.txt"))

## Merge test and train data together
allData <- rbind(testData, trainData)

# Rename column names (variables) for the feature they represent 
# e.g.(V1 = "tBodyAcc-mean()-X")
features <- tbl_df(read.table("features.txt"))
colnames(allData) <- features$V2

## Bind data
allActivities <- rbind(testActivities, trainActivities)
allSubjects <- rbind(testSubjects, trainSubjects)
data <- cbind (allSubjects, allActivities, allData)

## Step 2: mean and std only
# only retain features of mean and standard deviation
meanStdFeature <- features[grep("mean\\(\\)|std\\(\\)", features$V2),]
meanStdData <- data[, c(1, 2, meanStdFeature$V1+2)]

## step 3: read the labels (activities)
activities <- read.table("activity_labels.txt", stringsAsFactors=FALSE)
# replace activities in data with activity names
meanStdData$activity <- activities[meanStdData$activity, 2]

## Step 4: label the dataset with descriptive names
mynames <- union("subject", "activity")
mynames <- union(mynames, meanStdFeature$V2)
mynames <- gsub("^t", "time", mynames)
mynames <- gsub("^f", "frequency", mynames)
mynames <- gsub("std()", "SD", mynames)
mynames <- gsub("mean()", "MEAN", mynames)
mynames <- gsub("[^[:alpha:]]", "", mynames)
colnames(meanStdData) <- mynames

## Step 5:
## We have 180 sets (30 subjects and 6 activities)
tidyData <- aggregate(meanStdData[, 3:ncol(meanStdData)],
                       by=list(subject = meanStdData$subject, 
                               activity = meanStdData$activity),
                       mean)
## Write the tidy data in the text file
## Generate the codebook
write.table(tidyData, "tidyData.txt")
Write(codebook(tidyData), file="codebook.md")
