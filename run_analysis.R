##########################################################################################################

## Coursera Getting and Cleaning Data Course Project
## Author: Swetha KN

#Description:

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

## Date: 07- 25- 2018
##########################################################################################################

# 1.  Merges the training and the test sets to create one data set.

#setwd to source folder
setwd("~/Documents/swetha/Learning/Data_Science")
features     = read.table("./UCI HAR Dataset/features.txt",header=FALSE) #imports features.txt
activityType = read.table("./UCI HAR Dataset/activity_labels.txt",header=FALSE) #imports activity_labels.txt

X_test <- read.table("./UCI HAR Dataset/test/X_test.txt",header = FALSE) #imports X_test.txt
Y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt",header = FALSE) #imports Y_test.txt
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt",header = FALSE) #imports subject_test.txt

#rename the column names
colnames(activityType) = c('activityId','activityType') 
colnames(subject_test)  = "subjectId"
colnames(X_test)        = features[,2]
colnames(Y_test)        = "activityId"

#combine the test data
test_DT<- cbind(X_test,Y_test,subject_test)


X_train <- read.table("./UCI HAR Dataset/train/X_train.txt",header = FALSE) #imports X_train.txt
Y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt",header = FALSE) #imports Y_train.txt
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt",header = FALSE) #imports subject_train.txt

#rename the column names
colnames(subject_train)  = "subjectId"
colnames(X_train)       = features[,2]
colnames(Y_train)       = "activityId"

#combine the train data
train_DT<- cbind(X_train,Y_train,subject_train)

#combine the train and test data as final data
Final_DT <- rbind(test_DT,train_DT)

#2. Extracts only the measurements on the mean and standard deviation for each measurement
#save the column names 
Col_names_vect <- colnames(Final_DT)

# Create a logical Vector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
index_logi_vect = (grepl("activity..",Col_names_vect) | grepl("subject..",Col_names_vect) | grepl("-mean..",Col_names_vect) & !grepl("-meanFreq..",Col_names_vect) & !grepl("mean..-",Col_names_vect) | grepl("-std..",Col_names_vect) & !grepl("-std()..-",Col_names_vect));

# Subset Final_DT table based on the logical Vector to keep only desired columns
Final_DT = Final_DT[index_logi_vect==TRUE]

# 3. Uses descriptive activity names to name the activities in the data set
# Merge the Final_DT set with the acitivityType table to include descriptive activity names
Final_DT = merge(Final_DT,activityType,by='activityId',all.x=TRUE)

# 4. Appropriately labels the data set with descriptive variable names.
#save the column names 
Col_names_vect <- colnames(Final_DT)

for (i in 1:length(Col_names_vect)) 
{
        Col_names_vect[i] = gsub("\\()","",Col_names_vect[i])
        Col_names_vect[i] = gsub("-std$","StdDev",Col_names_vect[i])
        Col_names_vect[i] = gsub("-mean","Mean",Col_names_vect[i])
        Col_names_vect[i] = gsub("^(t)","time",Col_names_vect[i])
        Col_names_vect[i] = gsub("^(f)","freq",Col_names_vect[i])
        Col_names_vect[i] = gsub("([Gg]ravity)","Gravity",Col_names_vect[i])
        Col_names_vect[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",Col_names_vect[i])
        Col_names_vect[i] = gsub("[Gg]yro","Gyro",Col_names_vect[i])
        Col_names_vect[i] = gsub("AccMag","AccMagnitude",Col_names_vect[i])
        Col_names_vect[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",Col_names_vect[i])
        Col_names_vect[i] = gsub("JerkMag","JerkMagnitude",Col_names_vect[i])
        Col_names_vect[i] = gsub("GyroMag","GyroMagnitude",Col_names_vect[i])
}

# reassign the changed column names to Final_DT column names
colnames(Final_DT) = Col_names_vect

# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

# Melt the dataset to make it skinny by activity and each subject
melted <- melt(Final_DT, id=c("subjectId","activityType"))

#dcast the melted dataset to get the average of each variable for each activity and each subject
tidy_DT <- dcast(melted, subjectId+activityType ~ variable, mean)

# write the tidy dataset to a txt file
write.table(tidy_DT, './tidyData.txt',row.names=FALSE, sep='\t')
