# PURPOSE : This script extracts a tidy data set from the test and training data sets,
# as requested and provided in the programming assignment for the Getting and Cleaning Data course.

# DATA AND WORKING DIRECTORY : This script assumes the content of the provided zip file
# (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)
# has been extracted locally and that the R working directory has been set up to point to the extracted 
# UCI HAR Dataset folder, which in turn is the parent for the test and train sub-folders.

## 01. Loading data frames with common information ( activity labels and variable names )
activity_labels<-read.table("activity_labels.txt")
variablenames<-read.table("features.txt")

## 02. Loading data frames with test information ( test set, test activities and test subjects )
setwd('./test')
test<-read.table("X_test.txt")
test_labels<-read.table("y_test.txt")
subject_test<-read.table("subject_test.txt")
setwd('..')

## 03. Loading data frames with training information ( training set, training activities and training subjects )
setwd('./train')
train<-read.table("X_train.txt")
train_labels<-read.table("y_train.txt")
subject_train<-read.table("subject_train.txt")
setwd('..')

## 04. Changing variable name in both subjects data frames
names(subject_test)<-c("subject")
names(subject_train)<-c("subject")

## 05. Changing variable name in both activities data frames
names(test_labels)<-c("activitycode")
names(train_labels)<-c("activitycode")

## 06. Changing variable names in activity labels data frame
names(activity_labels)<-c("activitycode","activityname")

## 07. Changing all 561 variable names, for both test and training data sets. Names are taken from variablenames data frame ( features.txt )
names(test)<-variablenames$V2
names(train)<-variablenames$V2

## 08. Column binding subject,labels and set data frames for test and training
test<-cbind(subject_test,test_labels,test)
train<-cbind(subject_train,train_labels,train)

## 09. Row binding test and training into a new data frame (full)
full<-rbind(test,train)

## 10. Merging activity labels with full data frame
full<-merge(activity_labels,full)

## 11. Identifying variable names containing std() and mean()
indexmean<-grep("mean\\(",variablenames$V2)
indexstd<-grep("std\\(",variablenames$V2)
indexmeanstd<-c(indexmean,indexstd)
indexmeanstd<-sort(indexmeanstd)

## 12. Adding 3 to each member of the index list to account for the 3 initial fields ( activitycode, activityname and subject )
indexmeanstd<-sapply(indexmeanstd,FUN=function(t){t+3})

## 13. Adding indexes 2 and 3 at the beginning of the index list. These correspond to the activityname and subject fields
indexmeanstd<-c(2,3,indexmeanstd)

## 14. Extracting only required columns ( activityname, subject and those with mean() and std() )
full<-full[,indexmeanstd]

## 15. Changing variable names to full descriptive
names(full)<-gsub("Acc","Acceleration",names(full))
names(full)<-gsub("mean","MeanValue",names(full))
names(full)<-gsub("std","StandardDeviation",names(full))
names(full)<-gsub("\\(","",names(full))
names(full)<-gsub("\\)","",names(full))
names(full)<-sub("X$","Xaxis",names(full))
names(full)<-sub("Y$","Yaxis",names(full))
names(full)<-sub("Z$","Zaxis",names(full))
names(full)<-gsub("Gyro","Gyroscope",names(full))
names(full)<-gsub("Mag","Magnitude",names(full))
names(full)<-sub("^t","Time",names(full))
names(full)<-sub("^f","Frequency",names(full))
names(full)<-gsub("-","",names(full))

## 16. Grouping by activityname and subject. Calculating mean for each variable.
library(dplyr)
grpfull<-tbl_df(full)
grpfull<-group_by(grpfull,activityname,subject)
result<-summarize(grpfull,
          MeanOfTimeBodyAccelerationMeanValueXaxis                         =mean(TimeBodyAccelerationMeanValueXaxis),
          MeanOfTimeBodyAccelerationMeanValueYaxis                         =mean(TimeBodyAccelerationMeanValueYaxis),
          MeanOfTimeBodyAccelerationMeanValueZaxis                         =mean(TimeBodyAccelerationMeanValueZaxis),
          MeanOfTimeBodyAccelerationStandardDeviationXaxis                 =mean(TimeBodyAccelerationStandardDeviationXaxis),
          MeanOfTimeBodyAccelerationStandardDeviationYaxis                 =mean(TimeBodyAccelerationStandardDeviationYaxis),
          MeanOfTimeBodyAccelerationStandardDeviationZaxis                 =mean(TimeBodyAccelerationStandardDeviationZaxis),
          MeanOfTimeGravityAccelerationMeanValueXaxis                      =mean(TimeGravityAccelerationMeanValueXaxis),
          MeanOfTimeGravityAccelerationMeanValueYaxis                      =mean(TimeGravityAccelerationMeanValueYaxis),
          MeanOfTimeGravityAccelerationMeanValueZaxis                      =mean(TimeGravityAccelerationMeanValueZaxis),
          MeanOfTimeGravityAccelerationStandardDeviationXaxis              =mean(TimeGravityAccelerationStandardDeviationXaxis),
          MeanOfTimeGravityAccelerationStandardDeviationYaxis              =mean(TimeGravityAccelerationStandardDeviationYaxis),
          MeanOfTimeGravityAccelerationStandardDeviationZaxis              =mean(TimeGravityAccelerationStandardDeviationZaxis),
          MeanOfTimeBodyAccelerationJerkMeanValueXaxis                     =mean(TimeBodyAccelerationJerkMeanValueXaxis),
          MeanOfTimeBodyAccelerationJerkMeanValueYaxis                     =mean(TimeBodyAccelerationJerkMeanValueYaxis),
          MeanOfTimeBodyAccelerationJerkMeanValueZaxis                     =mean(TimeBodyAccelerationJerkMeanValueZaxis),
          MeanOfTimeBodyAccelerationJerkStandardDeviationXaxis             =mean(TimeBodyAccelerationJerkStandardDeviationXaxis),
          MeanOfTimeBodyAccelerationJerkStandardDeviationYaxis             =mean(TimeBodyAccelerationJerkStandardDeviationYaxis),
          MeanOfTimeBodyAccelerationJerkStandardDeviationZaxis             =mean(TimeBodyAccelerationJerkStandardDeviationZaxis),
          MeanOfTimeBodyGyroscopeMeanValueXaxis                            =mean(TimeBodyGyroscopeMeanValueXaxis),
          MeanOfTimeBodyGyroscopeMeanValueYaxis                            =mean(TimeBodyGyroscopeMeanValueYaxis),
          MeanOfTimeBodyGyroscopeMeanValueZaxis                            =mean(TimeBodyGyroscopeMeanValueZaxis),
          MeanOfTimeBodyGyroscopeStandardDeviationXaxis                    =mean(TimeBodyGyroscopeStandardDeviationXaxis),
          MeanOfTimeBodyGyroscopeStandardDeviationYaxis                    =mean(TimeBodyGyroscopeStandardDeviationYaxis),
          MeanOfTimeBodyGyroscopeStandardDeviationZaxis                    =mean(TimeBodyGyroscopeStandardDeviationZaxis),
          MeanOfTimeBodyGyroscopeJerkMeanValueXaxis                        =mean(TimeBodyGyroscopeJerkMeanValueXaxis),
          MeanOfTimeBodyGyroscopeJerkMeanValueYaxis                        =mean(TimeBodyGyroscopeJerkMeanValueYaxis),
          MeanOfTimeBodyGyroscopeJerkMeanValueZaxis                        =mean(TimeBodyGyroscopeJerkMeanValueZaxis),
          MeanOfTimeBodyGyroscopeJerkStandardDeviationXaxis                =mean(TimeBodyGyroscopeJerkStandardDeviationXaxis),
          MeanOfTimeBodyGyroscopeJerkStandardDeviationYaxis                =mean(TimeBodyGyroscopeJerkStandardDeviationYaxis),
          MeanOfTimeBodyGyroscopeJerkStandardDeviationZaxis                =mean(TimeBodyGyroscopeJerkStandardDeviationZaxis),
          MeanOfTimeBodyAccelerationMagnitudeMeanValue                     =mean(TimeBodyAccelerationMagnitudeMeanValue),
          MeanOfTimeBodyAccelerationMagnitudeStandardDeviation             =mean(TimeBodyAccelerationMagnitudeStandardDeviation),
          MeanOfTimeGravityAccelerationMagnitudeMeanValue                  =mean(TimeGravityAccelerationMagnitudeMeanValue),
          MeanOfTimeGravityAccelerationMagnitudeStandardDeviation          =mean(TimeGravityAccelerationMagnitudeStandardDeviation),
          MeanOfTimeBodyAccelerationJerkMagnitudeMeanValue                 =mean(TimeBodyAccelerationJerkMagnitudeMeanValue),
          MeanOfTimeBodyAccelerationJerkMagnitudeStandardDeviation         =mean(TimeBodyAccelerationJerkMagnitudeStandardDeviation ),
          MeanOfTimeBodyGyroscopeMagnitudeMeanValue                        =mean(TimeBodyGyroscopeMagnitudeMeanValue),
          MeanOfTimeBodyGyroscopeMagnitudeStandardDeviation                =mean(TimeBodyGyroscopeMagnitudeStandardDeviation),
          MeanOfTimeBodyGyroscopeJerkMagnitudeMeanValue                    =mean(TimeBodyGyroscopeJerkMagnitudeMeanValue),
          MeanOfTimeBodyGyroscopeJerkMagnitudeStandardDeviation            =mean(TimeBodyGyroscopeJerkMagnitudeStandardDeviation),
          MeanOfFrequencyBodyAccelerationMeanValueXaxis                    =mean(FrequencyBodyAccelerationMeanValueXaxis),
          MeanOfFrequencyBodyAccelerationMeanValueYaxis                    =mean(FrequencyBodyAccelerationMeanValueYaxis),
          MeanOfFrequencyBodyAccelerationMeanValueZaxis                    =mean(FrequencyBodyAccelerationMeanValueZaxis),
          MeanOfFrequencyBodyAccelerationStandardDeviationXaxis            =mean(FrequencyBodyAccelerationStandardDeviationXaxis),
          MeanOfFrequencyBodyAccelerationStandardDeviationYaxis            =mean(FrequencyBodyAccelerationStandardDeviationYaxis),
          MeanOfFrequencyBodyAccelerationStandardDeviationZaxis            =mean(FrequencyBodyAccelerationStandardDeviationZaxis),
          MeanOfFrequencyBodyAccelerationJerkMeanValueXaxis                =mean(FrequencyBodyAccelerationJerkMeanValueXaxis),
          MeanOfFrequencyBodyAccelerationJerkMeanValueYaxis                =mean(FrequencyBodyAccelerationJerkMeanValueYaxis),
          MeanOfFrequencyBodyAccelerationJerkMeanValueZaxis                =mean(FrequencyBodyAccelerationJerkMeanValueZaxis),
          MeanOfFrequencyBodyAccelerationJerkStandardDeviationXaxis        =mean(FrequencyBodyAccelerationJerkStandardDeviationXaxis),
          MeanOfFrequencyBodyAccelerationJerkStandardDeviationYaxis        =mean(FrequencyBodyAccelerationJerkStandardDeviationYaxis),
          MeanOfFrequencyBodyAccelerationJerkStandardDeviationZaxis        =mean(FrequencyBodyAccelerationJerkStandardDeviationZaxis),
          MeanOfFrequencyBodyGyroscopeMeanValueXaxis                       =mean(FrequencyBodyGyroscopeMeanValueXaxis),
          MeanOfFrequencyBodyGyroscopeMeanValueYaxis                       =mean(FrequencyBodyGyroscopeMeanValueYaxis),
          MeanOfFrequencyBodyGyroscopeMeanValueZaxis                       =mean(FrequencyBodyGyroscopeMeanValueZaxis),
          MeanOfFrequencyBodyGyroscopeStandardDeviationXaxis               =mean(FrequencyBodyGyroscopeStandardDeviationXaxis),
          MeanOfFrequencyBodyGyroscopeStandardDeviationYaxis               =mean(FrequencyBodyGyroscopeStandardDeviationYaxis),
          MeanOfFrequencyBodyGyroscopeStandardDeviationZaxis               =mean(FrequencyBodyGyroscopeStandardDeviationZaxis),
          MeanOfFrequencyBodyAccelerationMagnitudeMeanValue                =mean(FrequencyBodyAccelerationMagnitudeMeanValue ),
          MeanOfFrequencyBodyAccelerationMagnitudeStandardDeviation        =mean(FrequencyBodyAccelerationMagnitudeStandardDeviation),
          MeanOfFrequencyBodyBodyAccelerationJerkMagnitudeMeanValue        =mean(FrequencyBodyBodyAccelerationJerkMagnitudeMeanValue),
          MeanOfFrequencyBodyBodyAccelerationJerkMagnitudeStandardDeviation=mean(FrequencyBodyBodyAccelerationJerkMagnitudeStandardDeviation),
          MeanOfFrequencyBodyBodyGyroscopeMagnitudeMeanValue               =mean(FrequencyBodyBodyGyroscopeMagnitudeMeanValue),
          MeanOfFrequencyBodyBodyGyroscopeMagnitudeStandardDeviation       =mean(FrequencyBodyBodyGyroscopeMagnitudeStandardDeviation),
          MeanOfFrequencyBodyBodyGyroscopeJerkMagnitudeMeanValue           =mean(FrequencyBodyBodyGyroscopeJerkMagnitudeMeanValue),
          MeanOfFrequencyBodyBodyGyroscopeJerkMagnitudeStandardDeviation   =mean(FrequencyBodyBodyGyroscopeJerkMagnitudeStandardDeviation))

## 17. Writing result data frame to output file (csv format)
write.table(result,file="UCI_HAR_Full_Dataset_Means_Of_StdDeviations_and_MeanValues_By_Activity_and_Subject.txt",row.names=FALSE)









