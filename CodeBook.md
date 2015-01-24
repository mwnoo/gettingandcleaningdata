Codebook
--------
### Introduction
This codebook describes the data, variables and all the steps that were performed to clean the original dataset and create a tidy dataset.

### Data source
This dataset was obtained from 'Human Activity Recognition Using Smartphones Data Set'

<http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones>

### Dataset information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

### The following files of the original dataset were used:



- 'features_info.txt': Shows information about the variables used on the feature vector.



- 'features.txt': List of all features.



- 'activity_labels.txt': Links the class labels with their activity name.



- 'train/X_train.txt': Training set.



- 'train/y_train.txt': Training labels.



- 'test/X_test.txt': Test set.



- 'test/y_test.txt': Test labels.



The following files are available for the train and test data. Their descriptions are equivalent. 



- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

### Features of the original dataset
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 



Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 



Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 



These signals were used to estimate variables of the feature vector for each pattern:  

'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.



tBodyAcc-XYZ

tGravityAcc-XYZ

tBodyAccJerk-XYZ

tBodyGyro-XYZ

tBodyGyroJerk-XYZ

tBodyAccMag

tGravityAccMag

tBodyAccJerkMag

tBodyGyroMag

tBodyGyroJerkMag

fBodyAcc-XYZ

fBodyAccJerk-XYZ

fBodyGyro-XYZ

fBodyAccMag

fBodyAccJerkMag

fBodyGyroMag

fBodyGyroJerkMag



The set of variables that were estimated from these signals are: 



mean(): Mean value  

std(): Standard deviation   

*Notes*  
- Features are normalized and bounded within [-1,1].  

- Each feature vector is a row on the text file.

### Data transformations

The data was transformed in 5 steps:
  
1. Merged the training and the test sets to create one data set.
	- The merged dataset contains 10299 observations and 563 variables
	- The first 2 columns contain subject and activity data
2. Extracted only the measurements on the mean and standard deviation for each measurement.
	- The dataset of only the mean and standard deviation measurements contains 10299 observations and 68 variables
3. Used descriptive activity names to name the activities in the data set
	- Used the labels in "activity_labels.txt" 
4. Renamed some variables in the data set into more descriptive variable names
	- Renamed mean() and std() into Mean and Std respectively
	- Fixed in issue with variable names containing "BodyBody" in their name
5. From the data set in step 4, an independent tidy data set was created with the average of each variable for each activity and each subject.

### Resulting tidy dataset

- The tidy dataset contains 180 observations (30 subjects x 6 activity levels) and 68 variables
- The first 2 columns contain subjectID and activity labels
- The other 66 variables are the average for each activity and each subject (for details on these variables see "Features of the original dataset")
