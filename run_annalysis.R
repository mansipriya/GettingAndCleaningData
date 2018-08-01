#run_analysis.R that does the following.
#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement.
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names.
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#load dplyr package
library(dplyr)

#reading training set
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")

#reading test set
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

#merging both the data set
subject_final <- rbind(subject_test,subject_train)
X_final <- rbind(X_test,X_train)
Y_final <- rbind(Y_test,Y_train)

#reading features.txt
features <- read.table("./UCI HAR Dataset/features.txt")

# read activity labels
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

#final merging
dataset <- cbind(subject_final,X_final,Y_final)

#extracts mean and standard deviation from the measurement
selected_var <- features[grep("mean\\(\\)|std\\(\\)",features[,2]),]
X_final <- X_final[,selected_var[,1]]

#Uses descriptive activity names to name the activities in the data set
colnames(Y_final) <- "activity"
Y_final$activitylabel <- factor(Y_final$activity, labels = as.character(activity_labels[,2]))
activitylabel <- Y_final[,-1]

#Appropriately labels the data set with descriptive variable names.
colnames(X_final) <- features[selected_var[,1],2]

# From the data set in step 4, creates a second, independent tidy data set with the average
# of each variable for each activity and each subject.
colnames(subject_final) <- "subject"
total <- cbind(X_final, activitylabel, subject_final)
total_mean <- total %>% group_by(activitylabel, subject) %>% summarize_all(funs(mean))
write.table(total_mean, file = "./UCI HAR Dataset/tidydata.txt", row.names = FALSE, col.names = TRUE)
