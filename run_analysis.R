# Junyang Liu
# Getting and Cleaning Data Project

# 1. Merge the training and the test sets to create one data set.

#set working directory
setwd('/Users/kevin/Downloads/UCI HAR Dataset/');

# Read data from files
    features = read.table('./features.txt',header=FALSE); #imports features.txt
activityType = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
      xTrain = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
      yTrain = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt

# Assigin column names to the data
 colnames(activityType) = c('activityID','activityType');
 colnames(subjectTrain) = "subjectID";
       colnames(xTrain) = features[,2]; 
       colnames(yTrain) = "activityID";

# Create the final training set: merging yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain);

# Read test data
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
      xTest = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
      yTest = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Assign column names to the test data
colnames(subjectTest) = "subjectID";
      colnames(xTest) = features[,2]; 
      colnames(yTest) = "activityID";

# Create the final test set: merging the xTest, yTest and subjectTest data
testData = cbind(yTest,subjectTest,xTest);


# Create a final data set
finalData = rbind(trainingData,testData);

# Create a vector for the column names from the finalData
colNames  = colnames(finalData); 

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table
finalData = finalData[logicalVector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set
finalData = merge(finalData,activityType,by='activityID');

# Updating the colNames vector
colNames  = colnames(finalData); 

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoActivity without the activityType column
finalDataNoActivity  = finalData[,names(finalData) != 'activityType'];

# Summarizing the finalDataNoActivity table to include just the mean of each variable for each activity and each subject
tidyData = aggregate(finalDataNoActivity[,names(finalDataNoActivity) != c('activityID','subjectID')],by=list(activityID=finalDataNoActivity$activityID,subjectID = finalDataNoActivity$subjectID),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData = merge(tidyData,activityType,by='activityID');

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');
