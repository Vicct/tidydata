# Tidydata - #escription of all the variables, data and transformations.
features <- Variable to load the features file description of the Body Acceleration, Gravity Acceleration, Body Acceleration Jerk, Body Gyro, Body Gyro Jerk, Body Acceleration Mag, Body Acceleration Jerk Mag, Body Acceleration Gyro Mag, Body Gyro Jerk Mag.
features <- read.table('./features.txt', col.names = c("n","functions"))

#activities <- Variable to assign the activity_labels file are the descriptions of the activities executed by the subjects.
activities <- read.table("./activity_labels.txt", col.names = c("code", "activity"))

#subject_Train <- Variable to assign the data executed by 30 individuals. In the subject_train data file are the subjects numbers and the number to times executing their activities.
subject_Train = read.table('./train/subject_train.txt',header=FALSE)

#Train_x <- Variable to assign subject´s X positions executing their training activities.
Train_x <- read.table('./train/x_train.txt',header=FALSE)

#Train_y <- Variable to assign subject´s Y positions executing their training activities. 
Train_y <- read.table('./train/y_train.txt',header=FALSE)

#subject_Test <- Variable to assign subject´s X positions executing their test activities.
subject_Test = read.table('./test/subject_test.txt',header=FALSE)

#Test_x <- Variable to assign subject´s X positions executing their test activities.
Test_x <- read.table('./test/x_test.txt',header=FALSE)

#Test_y <- variable to assign subject´s Y positions executing their test activities. 
Test_y <- read.table('./test/y_test.txt',header=FALSE)
DataSet_y <- rbind(Train_y, Test_y)

#DataSet_x <- Merging train and test data in X position.
DataSet_x <- rbind(Train_x, Test_x)

#DataSet_y <- Merging train and test data in Y position.
DataSet_y <- rbind(Train_y, Test_y)

# 1.Merges the training and the test sets to create one data set.

#DataSet_subject <- Variable to assign the merging subject dataset info.
DataSet_subject <- rbind(subject_Train, subject_Test)

#Merged <- Variable to combine the subject information and their X & Y positions.
Merged <- cbind(DataSet_subject, DataSet_x, DataSet_y)

# 2.Extracts only the measurements on the mean and standard deviation for each measurement.
#DataSet_x_mean_std <- Variable to assign the merged dataset and using the features file to select only the mean & standard information. 
DataSet_x_mean_std <- DataSet_x[, grep("-(mean|std)\\(\\)", read.table("features.txt")[, 2])]

#names(DataSet_x_mean_std) <- Defining the names for the mean and standard in the data set.
names(DataSet_x_mean_std) <- read.table("features.txt")[grep("-(mean|std)\\(\\)", read.table("features.txt")[, 2]), 2]

# 3.Uses descriptive activity names to name the activities in the data set.
#DataSet_y[, 1] <- According the activities executed by the subjects, the description is added to the data set.
DataSet_y[, 1] <- read.table("activity_labels.txt")[DataSet_y[, 1], 2]
names(DataSet_y) <- "Activity"

# 4.Appropriately labels the data set with descriptive variable names.
#names(DataSet_subject) <- Adding the subject label identification is added the data set.
names(DataSet_subject) <- "Subject"

#AllDataSet <- Variable used to organize and combine all data sets into single one.
AllDataSet <- cbind(DataSet_x_mean_std, DataSet_y, DataSet_subject) Organizing into only one dataset all data sets into single one.

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

#DataMean <- Variable to identify the activities average by subject. 
DataMean<-aggregate(. ~Subject + Activity, AllDataSet, mean)
DataMean<-DataMean[order(DataMean$Subject,DataMean$Activity),]

# Write the output in a txt file.
write.table(DataMean, file = "DataMean.txt",row.name=FALSE)


