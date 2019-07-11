##############  David Wagner  ########### Getting and Cleaning Data Project

## Load toolkit libraries and set working directory

setwd("~/GitHub/datasciencecoursera/Getting and Cleaning data")
library(dplyr)

## Download and unzip the dataset:

filename <- "dwnld_dataset.zip"
if (!file.exists(filename)){
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
    download.file(fileURL, filename, method ="curl" )
}  
if (!file.exists("UCI HAR Dataset")) { 
    unzip(filename)
}

## Confirm Unzip results:

list.files(path = ".", pattern = NULL, all.files = T,
           full.names = T, recursive = T,
           ignore.case = F, include.dirs = T, no.. = F)

## Read in data files: Training, Test, Features and Activities

datapath <- "UCI HAR Dataset"

## Test data
training_subjects <- read.table(file.path(datapath, "train", "subject_train.txt"))
training_values <- read.table(file.path(datapath, "train", "X_train.txt"))
training_activity <- read.table(file.path(datapath, "train", "y_train.txt"))

## Training data
test_subjects <- read.table(file.path(datapath, "test", "subject_test.txt"))
test_values <- read.table(file.path(datapath, "test", "X_test.txt"))
test_activity <- read.table(file.path(datapath, "test", "y_test.txt"))

## Features (maintained as characters)
features <- read.table(file.path(datapath, "features.txt"), as.is = T)

## Activities
activities <- read.table(file.path(datapath, "activity_labels.txt"))
colnames(activities) <- c("activity_id", "activity_label")

## Combine and merge data files into a single table

subject_activity <- rbind(
    cbind(training_subjects, training_values, training_activity),
    cbind(test_subjects, test_values, test_activity)
)

## Remove individual data files to save memory
rm(training_subjects, training_values, training_activity, 
    test_subjects, test_values, test_activity)

## Assign column names
colnames(subject_activity) <- c("subject", features[, 2], "activity")

## Parse out columns to keep

## Find and keep columns subject, activity, and those that measure mean and standard deviation
columns_to_keep <- grepl("subject|activity|mean|std", colnames(subject_activity))

## Keep data in these columns only
subject_activity <- subject_activity[, columns_to_keep]


## Use descriptive activity names to name the activities in the data set

## Replace activity values with named factor levels
subject_activity$activity <- factor(subject_activity$activity, 
          levels = activities[, 1], labels = activities[, 2])



## Appropriately label the data set with descriptive variable names

## Get column names
subject_activity_cols <- colnames(subject_activity)

## Remove special characters
subject_activity_cols <- gsub("[\\(\\)-]", "", subject_activity_cols)

# Expand abbreviations and clean up names
subject_activity_cols <- gsub("^f", "frequency_domain", subject_activity_cols)
subject_activity_cols <- gsub("^t", "time_domain", subject_activity_cols)
subject_activity_cols <- gsub("Acc", "Accelerometer", subject_activity_cols)
subject_activity_cols <- gsub("Gyro", "Gyroscope", subject_activity_cols)
subject_activity_cols <- gsub("Mag", "Magnitude", subject_activity_cols)
subject_activity_cols <- gsub("Freq", "Frequency", subject_activity_cols)
subject_activity_cols <- gsub("mean", "Mean", subject_activity_cols)
subject_activity_cols <- gsub("std", "Standard_deviation", subject_activity_cols)

## Correct typo
subject_activity_cols <- gsub("BodyBody", "Body", subject_activity_cols)

## Use new labels as column names
colnames(subject_activity) <- subject_activity_cols



## Create a second, independent tidy set with the average of each
## variable for each activity and each subject

## Group by subject and activity as well as summarize using mean to find the average for each variable
subject_activityMeans <- subject_activity %>% 
  group_by(subject, activity) %>%
  summarize_each(list(mean))

## Output to file "tidy_data.txt"
write.table(subject_activityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)



