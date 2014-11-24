==================================================================
Summarized version of the Human Activity Recognition Using Smartphones Dataset
Version 1.0


Based upon the data collected by 
==================================================================
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws
==================================================================
Brief description of the source of the data
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 
The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 
[SOURCE DATA]
The source data contained data for each subject
For each record it is provided:
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The source dataset includes the following files:
=========================================
- 'features.txt': List of all features.
- 'activity_labels.txt': Links the class labels with their activity name.
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Training labels.
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Test labels.

The feature vectors X_train and X_test contain  the calculated features from the experiment as follows:

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
mad(): Median absolute deviation 
max(): Largest value in array
min(): Smallest value in array
sma(): Signal magnitude area
energy(): Energy measure. Sum of the squares divided by the number of values. 
iqr(): Interquartile range 
entropy(): Signal entropy
arCoeff(): Autorregresion coefficients with Burg order equal to 4
correlation(): correlation coefficient between two signals
maxInds(): index of the frequency component with largest magnitude
meanFreq(): Weighted average of the frequency components to obtain a mean frequency
skewness(): skewness of the frequency domain signal 
kurtosis(): kurtosis of the frequency domain signal 
bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

gravityMean
tBodyAccMean
tBodyAccJerkMean
tBodyGyroMean
tBodyGyroJerkMean


[TRANSFORMATION]
The data was transformed using the following script which also describes the transformation

# Read in Data

activity_labels <- read.table("activity_labels.txt", quote="\"")
features <- read.table("features.txt", quote="\"")
subject_train <- read.table("train/subject_train.txt", quote="\"")
X_train <- read.table("train/X_train.txt", quote="\"")
y_train <- read.table("train/y_train.txt", quote="\"")
subject_test <- read.table("test/subject_test.txt", quote="\"")
X_test <- read.table("test/X_test.txt", quote="\"")
y_test <- read.table("test/y_test.txt", quote="\"")

library(dplyr)

# need to add column to define group  "train" or"test" to both train test for y
y_train<-mutate(y_train, group="train")
y_test<-mutate(y_test, group="test")


# Merge X_train and X_test to get single X_data set using rowbind
X<-rbind_list(X_train, X_test)

# Merge y_train and y_test to get single y_data set using rowbind
y<-rbind_list(y_train, y_test)

# Merge subject_train and subject_test to get single subject set using rowbind
subject<-rbind_list(subject_train, subject_test)

#Clean up feature names removing "(", ")" and "-"/ 
features[,2]<-gsub( "\\(|/|\\-|\\)" , "" , features[,2])
# further Clean up of feature names removing "," and replacing with "_"
features[,2]<-gsub( "\\," , "_" , features[,2])

# Add the feature labels to the X set
names(X)<-features[,2]


# Add name to y_data
names(y)[1]<-"activity_num"

# Add name to subject_data
names(subject)[1]<-"subject"

# keep only the mean and std features using grep() to return a vector of cols containing text
X<-X[,c(grep( "Mean",names(X),ignore.case=TRUE),grep( "STD",names(X),ignore.case=TRUE))]

# column bind the subject, X  and y to form combined
data_set<-cbind(subject, X, y )


# replace the activity # with activity names in y data set
names(activity_labels)<-c("activity_num", "activity_name")

data_set<-
  	data_set %>%
 	 merge(activity_labels,by.x="activity_num", by.y="activity_num",all=TRUE ) %>%
 	 select(-activity_num) %>%
  	arrange(desc(group),subject,activity_name)

# create a second, independent tidy data set with the average of each variable for each 
# activity and each subject.

# Take the data_set, remove the group column, and then group by subject and activity.  
# Summarise each col of the grouped data using the mean function to delivery the 
summary_set output
summary_set<-
  	data_set %>%
  	select(-group)%>%
  	group_by(subject, activity_name)%>%
 	 summarise_each(funs(mean))


[OUTPUT]
The output file "summary_set" is a wide tidy data set of dimension 180 rows by 88 columns It has the following columns:
subject
activity_name
tBodyAccmeanX
tBodyAccmeanY
tBodyAccmeanZ
tGravityAccmeanX
tGravityAccmeanY
tGravityAccmeanZ
tBodyAccJerkmeanX
tBodyAccJerkmeanY
tBodyAccJerkmeanZ
tBodyGyromeanX
tBodyGyromeanY
tBodyGyromeanZ
tBodyGyroJerkmeanX
tBodyGyroJerkmeanY
tBodyGyroJerkmeanZ
tBodyAccMagmean
tGravityAccMagmean
tBodyAccJerkMagmean
tBodyGyroMagmean
tBodyGyroJerkMagmean
fBodyAccmeanX
fBodyAccmeanY
fBodyAccmeanZ
fBodyAccmeanFreqX
fBodyAccmeanFreqY
fBodyAccmeanFreqZ
fBodyAccJerkmeanX
fBodyAccJerkmeanY
fBodyAccJerkmeanZ
fBodyAccJerkmeanFreqX
fBodyAccJerkmeanFreqY
fBodyAccJerkmeanFreqZ
fBodyGyromeanX
fBodyGyromeanY
fBodyGyromeanZ
fBodyGyromeanFreqX
fBodyGyromeanFreqY
fBodyGyromeanFreqZ
fBodyAccMagmean
fBodyAccMagmeanFreq
fBodyBodyAccJerkMagmean
fBodyBodyAccJerkMagmeanFreq
fBodyBodyGyroMagmean
fBodyBodyGyroMagmeanFreq
fBodyBodyGyroJerkMagmean
fBodyBodyGyroJerkMagmeanFreq
angletBodyAccMean_gravity
angletBodyAccJerkMean_gravityMean
angletBodyGyroMean_gravityMean
angletBodyGyroJerkMean_gravityMean
angleX_gravityMean
angleY_gravityMean
angleZ_gravityMean
tBodyAccstdX
tBodyAccstdY
tBodyAccstdZ
tGravityAccstdX
tGravityAccstdY
tGravityAccstdZ
tBodyAccJerkstdX
tBodyAccJerkstdY
tBodyAccJerkstdZ
tBodyGyrostdX
tBodyGyrostdY
tBodyGyrostdZ
tBodyGyroJerkstdX
tBodyGyroJerkstdY
tBodyGyroJerkstdZ
tBodyAccMagstd
tGravityAccMagstd
tBodyAccJerkMagstd
tBodyGyroMagstd
tBodyGyroJerkMagstd
fBodyAccstdX
fBodyAccstdY
fBodyAccstdZ
fBodyAccJerkstdX
fBodyAccJerkstdY
fBodyAccJerkstdZ
fBodyGyrostdX
fBodyGyrostdY
fBodyGyrostdZ
fBodyAccMagstd
fBodyBodyAccJerkMagstd
fBodyBodyGyroMagstd
fBodyBodyGyroJerkMagstd

Column Descriptions
1) subject : the number of the subject {1:30}
2) activity_name: the label for the activity the subject was conducting
these 2 columns are followed by 86 feature columns which contain the MEAN of the measured values for the subject whilst conducting the named  activity.
The features are labeled based on the same labeling convention of the input file, but with the special characters "(", ")" and "-" removed and the "," character replaced with "_" to make the data set easier to use for analysis and to also be more readable.
Of the original features, only the features relating to the means and standard deviations were kept, resulting in the 86 feature variables below.
 
Feature Ref	Original Feature Name	Cleaned Up Feature Name	   
1	tBodyAcc-mean()-X	tBodyAccmeanX	   
2	tBodyAcc-mean()-Y	tBodyAccmeanY	   
3	tBodyAcc-mean()-Z	tBodyAccmeanZ	   
41	tGravityAcc-mean()-X	tGravityAccmeanX	   
42	tGravityAcc-mean()-Y	tGravityAccmeanY	   
43	tGravityAcc-mean()-Z	tGravityAccmeanZ	   
81	tBodyAccJerk-mean()-X	tBodyAccJerkmeanX	   
82	tBodyAccJerk-mean()-Y	tBodyAccJerkmeanY	   
83	tBodyAccJerk-mean()-Z	tBodyAccJerkmeanZ	   
121	tBodyGyro-mean()-X	tBodyGyromeanX	   
122	tBodyGyro-mean()-Y	tBodyGyromeanY	   
123	tBodyGyro-mean()-Z	tBodyGyromeanZ	   
161	tBodyGyroJerk-mean()-X	tBodyGyroJerkmeanX	   
162	tBodyGyroJerk-mean()-Y	tBodyGyroJerkmeanY	   
163	tBodyGyroJerk-mean()-Z	tBodyGyroJerkmeanZ	   
201	tBodyAccMag-mean()	tBodyAccMagmean	   
214	tGravityAccMag-mean()	tGravityAccMagmean	   
227	tBodyAccJerkMag-mean()	tBodyAccJerkMagmean	   
240	tBodyGyroMag-mean()	tBodyGyroMagmean	   
253	tBodyGyroJerkMag-mean()	tBodyGyroJerkMagmean	   
266	fBodyAcc-mean()-X	fBodyAccmeanX	   
267	fBodyAcc-mean()-Y	fBodyAccmeanY	   
268	fBodyAcc-mean()-Z	fBodyAccmeanZ	   
294	fBodyAcc-meanFreq()-X	fBodyAccmeanFreqX	   
295	fBodyAcc-meanFreq()-Y	fBodyAccmeanFreqY	   
296	fBodyAcc-meanFreq()-Z	fBodyAccmeanFreqZ	   
345	fBodyAccJerk-mean()-X	fBodyAccJerkmeanX	   
346	fBodyAccJerk-mean()-Y	fBodyAccJerkmeanY	   
347	fBodyAccJerk-mean()-Z	fBodyAccJerkmeanZ	   
373	fBodyAccJerk-meanFreq()-X	fBodyAccJerkmeanFreqX	   
374	fBodyAccJerk-meanFreq()-Y	fBodyAccJerkmeanFreqY	   
375	fBodyAccJerk-meanFreq()-Z	fBodyAccJerkmeanFreqZ	   
424	fBodyGyro-mean()-X	fBodyGyromeanX	   
425	fBodyGyro-mean()-Y	fBodyGyromeanY	   
426	fBodyGyro-mean()-Z	fBodyGyromeanZ	   
452	fBodyGyro-meanFreq()-X	fBodyGyromeanFreqX	   
453	fBodyGyro-meanFreq()-Y	fBodyGyromeanFreqY	   
454	fBodyGyro-meanFreq()-Z	fBodyGyromeanFreqZ	   
503	fBodyAccMag-mean()	fBodyAccMagmean	   
513	fBodyAccMag-meanFreq()	fBodyAccMagmeanFreq	   
516	fBodyBodyAccJerkMag-mean()	fBodyBodyAccJerkMagmean	   
526	fBodyBodyAccJerkMag-meanFreq()	fBodyBodyAccJerkMagmeanFreq	   
529	fBodyBodyGyroMag-mean()	fBodyBodyGyroMagmean	   
539	fBodyBodyGyroMag-meanFreq()	fBodyBodyGyroMagmeanFreq	   
542	fBodyBodyGyroJerkMag-mean()	fBodyBodyGyroJerkMagmean	   
552	fBodyBodyGyroJerkMag-meanFreq()	fBodyBodyGyroJerkMagmeanFreq	   
555	angle(tBodyAccMean,gravity)	angletBodyAccMean_gravity	   
556	angle(tBodyAccJerkMean),gravityMean)	angletBodyAccJerkMean_gravityMean	   
557	angle(tBodyGyroMean,gravityMean)	angletBodyGyroMean_gravityMean	   
558	angle(tBodyGyroJerkMean,gravityMean)	angletBodyGyroJerkMean_gravityMean	   
559	angle(X,gravityMean)	angleX_gravityMean	   
560	angle(Y,gravityMean)	angleY_gravityMean	   
561	angle(Z,gravityMean)	angleZ_gravityMean	   
4	tBodyAcc-std()-X	tBodyAccstdX	   
5	tBodyAcc-std()-Y	tBodyAccstdY	   
6	tBodyAcc-std()-Z	tBodyAccstdZ	   
44	tGravityAcc-std()-X	tGravityAccstdX	   
45	tGravityAcc-std()-Y	tGravityAccstdY	   
46	tGravityAcc-std()-Z	tGravityAccstdZ	   
84	tBodyAccJerk-std()-X	tBodyAccJerkstdX	   
85	tBodyAccJerk-std()-Y	tBodyAccJerkstdY	   
86	tBodyAccJerk-std()-Z	tBodyAccJerkstdZ	   
124	tBodyGyro-std()-X	tBodyGyrostdX	   
125	tBodyGyro-std()-Y	tBodyGyrostdY	   
126	tBodyGyro-std()-Z	tBodyGyrostdZ	   
164	tBodyGyroJerk-std()-X	tBodyGyroJerkstdX	   
165	tBodyGyroJerk-std()-Y	tBodyGyroJerkstdY	   
166	tBodyGyroJerk-std()-Z	tBodyGyroJerkstdZ	   
202	tBodyAccMag-std()	tBodyAccMagstd	   
215	tGravityAccMag-std()	tGravityAccMagstd	   
228	tBodyAccJerkMag-std()	tBodyAccJerkMagstd	   
241	tBodyGyroMag-std()	tBodyGyroMagstd	   
254	tBodyGyroJerkMag-std()	tBodyGyroJerkMagstd	   
269	fBodyAcc-std()-X	fBodyAccstdX	   
270	fBodyAcc-std()-Y	fBodyAccstdY	   
271	fBodyAcc-std()-Z	fBodyAccstdZ	   
348	fBodyAccJerk-std()-X	fBodyAccJerkstdX	   
349	fBodyAccJerk-std()-Y	fBodyAccJerkstdY	   
350	fBodyAccJerk-std()-Z	fBodyAccJerkstdZ	    
427	fBodyGyro-std()-X	fBodyGyrostdX	   
428	fBodyGyro-std()-Y	fBodyGyrostdY	   
429	fBodyGyro-std()-Z	fBodyGyrostdZ	   
504	fBodyAccMag-std()	fBodyAccMagstd	   
517	fBodyBodyAccJerkMag-std()	fBodyBodyAccJerkMagstd	   
530	fBodyBodyGyroMag-std()	fBodyBodyGyroMagstd	   
543	fBodyBodyGyroJerkMag-std()	fBodyBodyGyroJerkMagstd	   
			 



