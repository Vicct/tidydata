# Tidydata
## Getting and Cleaning Data Course Project
### The work directory is defined and configured
setwd("C:/Project Files/CURSOS/DataScience-Hopkins/getdata_projectfiles_UCI HAR Dataset (1)/UCI HAR Dataset/")

### The libraries are loaded
library(plyr)
library(data.table)

## The dowloaded information is read.
### In the features file are the data descriptions of the Body Acceleration, Gravity Acceleration, Body Acceleration Jerk, Body Gyro, Body Gyro Jerk, Body Acceleration Mag,
### Body Acceleration Jerk Mag, Body Acceleration Gyro Mag, Body Gyro Jerk Mag
## The information is read for later identification of the tidy data.
features <- read.table('./features.txt', col.names = c("n","functions"))

## In the activity_labels file are the descriptions of the activities executed by the subjects.
activities <- read.table("./activity_labels.txt", col.names = c("code", "activity"))

## The Train Data is read for later merge with the Test data.
### The collected data was executed by 30 individuals. In the subject_train data file are the subjects numbers and the number to times executing their activities. 
subject_Train = read.table('./train/subject_train.txt',header=FALSE)

### Subject´s X positions executing their training activities.
Train_x = read.table('./train/x_train.txt',header=FALSE)

### Subject´s Y positions executing their training activities. 
Train_y = read.table('./train/y_train.txt',header=FALSE)

### Subject´s X positions executing their test activities.
subject_Test = read.table('./test/subject_test.txt',header=FALSE)

### Subject´s X positions executing their test activities.
Test_x = read.table('./test/x_test.txt',header=FALSE)

### Subject´s Y positions executing their test activities. 
Test_y = read.table('./test/y_test.txt',header=FALSE)

### Merging train and test data in X position.
DataSet_x <- rbind(Train_x, Test_x)

### Merging train and test data in Y position.
DataSet_y <- rbind(Train_y, Test_y)

# 1.Merges the training and the test sets to create one data set.
## Merging subject dataset
DataSet_subject <- rbind(subject_Train, subject_Test)
Merged <- cbind(DataSet_subject, DataSet_x, DataSet_y)

head(DataSet_subject)

# 2.Extracts only the measurements on the mean and standard deviation for each measurement.
### Using the merged dataset and using the features file to select only the mean & standard information. 
DataSet_x_mean_std <- DataSet_x[, grep("-(mean|std)\\(\\)", read.table("features.txt")[, 2])]

### Defining the names for the mean and standard in the data set.
names(DataSet_x_mean_std) <- read.table("features.txt")[grep("-(mean|std)\\(\\)", read.table("features.txt")[, 2]), 2] 
View(DataSet_x_mean_std)

dim(DataSet_x_mean_std)

# 3.Uses descriptive activity names to name the activities in the data set.
### According the activities executed by the subjects, the description is added to the data set.
DataSet_y[, 1] <- read.table("activity_labels.txt")[DataSet_y[, 1], 2]
names(DataSet_y) <- "Activity"
View(DataSet_y)

# 4.Appropriately labels the data set with descriptive variable names.
### Subject label identification is added the data set.
names(DataSet_subject) <- "Subject"
summary(DataSet_subject)

### Organizing and combining all data sets into single one.
AllDataSet <- cbind(DataSet_x_mean_std, DataSet_y, DataSet_subject)

### Defining descriptive names for all variables.
names(AllDataSet) <- make.names(names(AllDataSet))
names(AllDataSet) <- gsub('Acc',"Acceleration",names(AllDataSet))
names(AllDataSet) <- gsub('GyroJerk',"AngularAcceleration",names(AllDataSet))
names(AllDataSet) <- gsub('Gyro',"AngularSpeed",names(AllDataSet))
names(AllDataSet) <- gsub('Mag',"Magnitude",names(AllDataSet))
names(AllDataSet) <- gsub('^t',"TimeDomain.",names(AllDataSet))
names(AllDataSet) <- gsub('^f',"FrequencyDomain.",names(AllDataSet))
names(AllDataSet) <- gsub('\\.mean',".Mean",names(AllDataSet))
names(AllDataSet) <- gsub('\\.std',".StandardDeviation",names(AllDataSet))
names(AllDataSet) <- gsub('Freq\\.',"Frequency.",names(AllDataSet))
names(AllDataSet) <- gsub('Freq$',"Frequency",names(AllDataSet))

View(AllDataSet)

#  5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
### Writing the names of the whole data set.
names(AllDataSet)

### Identification of activities average by subject. 
DataMean<-aggregate(. ~Subject + Activity, AllDataSet, mean)
DataMean<-DataMean[order(DataMean$Subject,DataMean$Activity),]

### Write the output in a txt file.
write.table(DataMean, file = "DataMean.txt",row.name=FALSE)


