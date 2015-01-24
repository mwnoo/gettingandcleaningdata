# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(reshape2)

#Download files
if(!file.exists("./Dataset.zip")) {
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl, destfile = "Dataset.zip", method = "curl")
    
    dateDownloaded <- date()
}

#Unzip the file
if(!file.exists("./UCI HAR Dataset")) {
    unzip("Dataset.zip")
}

# 1. Merges the training and the test sets to create one data set.

#Read the test data into R
test_data <-  read.table("./UCI HAR Dataset/test/X_test.txt")
test_labels <-  read.table("./UCI HAR Dataset/test/y_test.txt", col.names="labels")
test_subjects <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names="subjects")

#Read the train data into R
train_data <-  read.table("./UCI HAR Dataset/train/X_train.txt")
train_labels <-  read.table("./UCI HAR Dataset/train/y_train.txt", col.names="labels")
train_subjects <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names="subjects")

#Read the labels and replace "-" in the variable names with "_"
colLabels <- read.table("./UCI HAR Dataset/features.txt")[,2]
colLabels <- gsub("-","_",colLabels)

#Add the labels to the data
names(test_data) <- colLabels
names(train_data) <- colLabels

#Combine the components of each dataset
dataTest <- cbind(test_subjects, test_labels, test_data)
dataTrain <- cbind(train_subjects, train_labels, train_data)

#Merge train and test datasets
dataMerged <- rbind(dataTest, dataTrain)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

#Find the columns that have "mean()" or "std()" in their name
colIndx <- grepl("mean\\(\\)|std\\(\\)", colLabels)
#sum(grepl("mean\\(\\)|std\\(\\)", colLabels)) ## 66 columns

#Select the relevant columns of the data using the colIndx vector.
#Combine the first to columns (subjects and labels) with the relevant columns
data <- cbind(dataMerged[,1:2],dataMerged[, -(1:2)][colIndx])

# 3. Uses descriptive activity names to name the activities in the data set

#Read the activty label names
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]

data$labels <- factor(data$labels, labels = activity_labels)

# 4. Appropriately labels the data set with descriptive variable names. 

#Rename columns
names(data)[1] <- "SubjectID"
names(data)[2] <- "Activity"

new_colname <- gsub("BodyBody", "Body", names(data))
new_colname <- gsub("mean()", "Mean", new_colname, fixed=TRUE)
new_colname <- gsub("std()", "Std", new_colname, fixed=TRUE)

#Use the corrected column names 
names(data) <- new_colname

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Melt the data: unique id variabels (SubjectID, Activity)
#All non-id variables get their on row in the column "variable"
#dataMelt will have 66 * 10299 = 679734 obs.
id_var <- c("SubjectID","Activity")
dataMelt <- melt(data, id=id_var)

#Cast the data into a tidy format
#formula: x_var1 + x_var2 ~ y_var1
tidy_data <- dcast(dataMelt, SubjectID + Activity ~ variable, mean)

#Write tidy dataset to txt file
write.table(tidy_data, "./tidydata.txt", row.names = FALSE)

