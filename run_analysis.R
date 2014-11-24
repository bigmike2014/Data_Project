# Read in Data
setwd("~/Data Science/Coursera/Data Science Foundation/Course 3 - Getting and Cleaning Data/UCI HAR Dataset")

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

# create a second, independent tidy data set with the average of each variable for each activity and each subject.

# Take the data_set, remove the group column, and then group by subject and activity.  
# Summarise each col of the grouped data using the mean function to delivery the summary_set output
summary_set<-
  data_set %>%
  select(-group)%>%
  group_by(subject, activity_name)%>%
  summarise_each(funs(mean))
