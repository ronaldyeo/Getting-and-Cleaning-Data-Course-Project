####################################################################
# Peer-graded Assignment: Getting and Cleaning Data Course Project #
####################################################################

#Download and extract data from link

library(data.table)

fileurl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if (!dir.exists('./UCI HAR Dataset')){
  download.file(fileurl,'./UCI HAR Dataset.zip', mode = 'wb')
  unzip("UCI HAR Dataset.zip", exdir = getwd())
  unlink("UCI HAR Dataset.zip")
} else {
  unlink("UCI HAR Dataset.zip")}


#Read data

features <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
features <- as.character(features[,2])


data.set.train <- read.table('./UCI HAR Dataset/train/X_train.txt')
activity.train <- read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
subject.train <- read.csv('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')

data.train <-  data.frame(subject.train, activity.train, data.set.train)
names(data.train) <- c(c('subject', 'activity'), features)

data.set.test <- read.table('./UCI HAR Dataset/test/X_test.txt')
activity.test <- read.csv('./UCI HAR Dataset/test/y_test.txt', header = FALSE, sep = ' ')
subject.test <- read.csv('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ')

data.test <- data.frame(subject.test, activity.test, data.set.test)
names(data.test) <- c(c('subject', 'activity'), features)


#1. Merges the training and the test sets to create one data set.

data.all <- rbind(data.train, data.test)


#2. Extracts only the measurements on the mean and standard deviation for each measurement.

data.extracts <- data.all[,c(1, 2, grep("mean|std",names(data.all)))]


#3. Uses descriptive activity names to name the activities in the data set.
library(dplyr)

activity.labels <- read.csv('./UCI HAR Dataset/activity_labels.txt', header = FALSE, sep = ' ')
data.extracts.temp <- left_join(data.extracts, activity.labels, by = c("activity" = "V1"))
data.extracts$activity <- data.extracts.temp$V2
rm(data.extracts.temp)


#4. Appropriately labels the data set with descriptive variable names.

name.new <- names(data.extracts)
name.new <- gsub("[(][)]", "", name.new)
name.new <- gsub("^t", "TimeDomain_", name.new)
name.new <- gsub("^f", "FrequencyDomain_", name.new)
name.new <- gsub("Acc", "Accelerometer", name.new)
name.new <- gsub("Gyro", "Gyroscope", name.new)
name.new <- gsub("Mag", "Magnitude", name.new)
name.new <- gsub("-mean-", "_Mean_", name.new)
name.new <- gsub("-std-", "_StandardDeviation_", name.new)
name.new <- gsub("-", "_", name.new)
names(data.extracts) <- name.new

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

data.tidy <- aggregate(data.extracts[,3:ncol(data.extracts)], by = list(activity = data.extracts$activity, subject = data.extracts$subject),FUN = mean)
write.table(x = data.tidy, file = "data_tidy.txt", row.names = FALSE)