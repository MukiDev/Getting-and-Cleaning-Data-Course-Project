# DATA DICTIONARY

## The data in this dataset comes from the original datasets (training and test) provided with the Human Activity Recognition Using Smartphones Dataset, Version 1.0.
## As requested :
## -the training and test datasets have been merged back into a single one.
## -only the mean and standard deviation for each measurement have been extracted.
## -descriptive names have been applied to activity names and extracted variable names.
## -the final data set includes the average for the extracted variables, grouped by activity name and subject.

# Variable Names
## The variable names used in the final data set are :
## 01. activityname                                               
## 02. subject                                                    
## 03. MeanOfTimeBodyAccelerationMeanValueXaxis                         
## 04. MeanOfTimeBodyAccelerationMeanValueYaxis                         
## 05. MeanOfTimeBodyAccelerationMeanValueZaxis                         
## 06. MeanOfTimeBodyAccelerationStandardDeviationXaxis                 
## 07. MeanOfTimeBodyAccelerationStandardDeviationYaxis                 
## 08. MeanOfTimeBodyAccelerationStandardDeviationZaxis                 
## 09. MeanOfTimeGravityAccelerationMeanValueXaxis                      
## 10. MeanOfTimeGravityAccelerationMeanValueYaxis                      
## 11. MeanOfTimeGravityAccelerationMeanValueZaxis                      
## 12. MeanOfTimeGravityAccelerationStandardDeviationXaxis              
## 13. MeanOfTimeGravityAccelerationStandardDeviationYaxis              
## 14. MeanOfTimeGravityAccelerationStandardDeviationZaxis              
## 15. MeanOfTimeBodyAccelerationJerkMeanValueXaxis                     
## 16. MeanOfTimeBodyAccelerationJerkMeanValueYaxis                     
## 17. MeanOfTimeBodyAccelerationJerkMeanValueZaxis                     
## 18. MeanOfTimeBodyAccelerationJerkStandardDeviationXaxis             
## 19. MeanOfTimeBodyAccelerationJerkStandardDeviationYaxis             
## 20. MeanOfTimeBodyAccelerationJerkStandardDeviationZaxis             
## 21. MeanOfTimeBodyGyroscopeMeanValueXaxis                            
## 22. MeanOfTimeBodyGyroscopeMeanValueYaxis                            
## 23. MeanOfTimeBodyGyroscopeMeanValueZaxis                            
## 24. MeanOfTimeBodyGyroscopeStandardDeviationXaxis                    
## 25. MeanOfTimeBodyGyroscopeStandardDeviationYaxis                    
## 26. MeanOfTimeBodyGyroscopeStandardDeviationZaxis                    
## 27. MeanOfTimeBodyGyroscopeJerkMeanValueXaxis                        
## 28. MeanOfTimeBodyGyroscopeJerkMeanValueYaxis                        
## 29. MeanOfTimeBodyGyroscopeJerkMeanValueZaxis                        
## 30. MeanOfTimeBodyGyroscopeJerkStandardDeviationXaxis                
## 31. MeanOfTimeBodyGyroscopeJerkStandardDeviationYaxis                
## 32. MeanOfTimeBodyGyroscopeJerkStandardDeviationZaxis                
## 33. MeanOfTimeBodyAccelerationMagnitudeMeanValue                     
## 34. MeanOfTimeBodyAccelerationMagnitudeStandardDeviation             
## 35. MeanOfTimeGravityAccelerationMagnitudeMeanValue                  
## 36. MeanOfTimeGravityAccelerationMagnitudeStandardDeviation          
## 37. MeanOfTimeBodyAccelerationJerkMagnitudeMeanValue                 
## 38. MeanOfTimeBodyAccelerationJerkMagnitudeStandardDeviation         
## 39. MeanOfTimeBodyGyroscopeMagnitudeMeanValue                        
## 40. MeanOfTimeBodyGyroscopeMagnitudeStandardDeviation                
## 41. MeanOfTimeBodyGyroscopeJerkMagnitudeMeanValue                    
## 42. MeanOfTimeBodyGyroscopeJerkMagnitudeStandardDeviation            
## 43. MeanOfFrequencyBodyAccelerationMeanValueXaxis                    
## 44. MeanOfFrequencyBodyAccelerationMeanValueYaxis                    
## 45. MeanOfFrequencyBodyAccelerationMeanValueZaxis                    
## 46. MeanOfFrequencyBodyAccelerationStandardDeviationXaxis            
## 47. MeanOfFrequencyBodyAccelerationStandardDeviationYaxis            
## 48. MeanOfFrequencyBodyAccelerationStandardDeviationZaxis            
## 49. MeanOfFrequencyBodyAccelerationJerkMeanValueXaxis                
## 50. MeanOfFrequencyBodyAccelerationJerkMeanValueYaxis                
## 51. MeanOfFrequencyBodyAccelerationJerkMeanValueZaxis                
## 52. MeanOfFrequencyBodyAccelerationJerkStandardDeviationXaxis        
## 53. MeanOfFrequencyBodyAccelerationJerkStandardDeviationYaxis        
## 54. MeanOfFrequencyBodyAccelerationJerkStandardDeviationZaxis        
## 55. MeanOfFrequencyBodyGyroscopeMeanValueXaxis                       
## 56. MeanOfFrequencyBodyGyroscopeMeanValueYaxis                       
## 57. MeanOfFrequencyBodyGyroscopeMeanValueZaxis                       
## 58. MeanOfFrequencyBodyGyroscopeStandardDeviationXaxis               
## 59. MeanOfFrequencyBodyGyroscopeStandardDeviationYaxis               
## 60. MeanOfFrequencyBodyGyroscopeStandardDeviationZaxis               
## 61. MeanOfFrequencyBodyAccelerationMagnitudeMeanValue                
## 62. MeanOfFrequencyBodyAccelerationMagnitudeStandardDeviation        
## 63. MeanOfFrequencyBodyBodyAccelerationJerkMagnitudeMeanValue        
## 64. MeanOfFrequencyBodyBodyAccelerationJerkMagnitudeStandardDeviation
## 65. MeanOfFrequencyBodyBodyGyroscopeMagnitudeMeanValue               
## 66. MeanOfFrequencyBodyBodyGyroscopeMagnitudeStandardDeviation       
## 67. MeanOfFrequencyBodyBodyGyroscopeJerkMagnitudeMeanValue           
## 68. MeanOfFrequencyBodyBodyGyroscopeJerkMagnitudeStandardDeviation   

# Terms
## The explanation for the terms used in the variable names is the following :
## -activityname 		: One of the six physical activities performed by the experiment subjects, as listed below under Activity Names
## -subject				: Integer identifying one of the thirty subjects participating in the experiment
## -MeanOf				: Mean value calculated over the variable named right after. All calculated values have been grouped by activity name and subject.
## -Time					: Time domain signal
## -Frequency			: Frequency domain signal
## -BodyAcceleration		: Body acceleration captured by the accelerometer attached to each participating subject. The original acceleration signal is separated into body and gravity accelerations.
## -GravityAcceleration	: Gravity acceleration captured by the accelerometer attached to each participating subject. The original acceleration signal is separated into body and gravity accelerations.
## -BodyGyroscope		: Angular velocity captured by the gyroscope attached to each participating subject.
## -MeanValue			: Mean value calculated for each signal
## -StandardDeviation	: Standard deviation calculated for each signal
## -Jerk					: Jerk signal, obtained by deriving in time either the body linear acceleration or the angular velocity.
## -Magnitude			: Magnitude calculated for the three dimensional signal using the Euclidean norm.
## -Xaxis				: Signal component in the X direction.
## -Yaxis				: Signal component in the Y direction.
## -Zaxis				: Signal component in the Z direction.

# Data Tidying
## A short description for each of the data tidying steps is included in the actual script file (run_analysis.R). A more detailed description is the following :

## 01. Loading data frames with common information : activity labels and variable names. 
### -This information is common to the provided test and training data sets. 
### -Uses read.table against the activity_labels.txt and features.txt files provided.

## 02. Loading data frames with test information ( test set, test activities and test subjects )
### -This information is unique to the provided test data set and located under the UCI HAR Dataset\test subfolder.
### -The working directory is temporarily set to this subfolder.
### -Uses read.table against the X_test.txt,y_test.txt and subject_test.txt files provided.

## 03. Loading data frames with training information ( training set, training activities and training subjects )
### -This information is unique to the provided training data set and located under the UCI HAR Dataset\train subfolder.
### -The working directory is temporarily set to this subfolder.
### -Uses read.table against the X_train.txt,y_train.txt and subject_train.txt files provided.

## 04. Changing variable name in both subjects data frames
### -From V1 to subject. 
### -Each of these columns will be later clipped with the correspondent datasets ( test or training) and then into a single dataset.

## 05. Changing variable name in both activities data frames
### -From V1 to activitycode. 
### -Each of these columns will be later clipped with the correspondent datasets ( test or training) and then into a single dataset.
### -It will be used to merge the activity names into the final single dataset.

## 06. Changing variable names in activity labels data frame
### -From V1,V2 to activitycode, activityname.
### -These new names will allow and facilitate merging the activityname column into the final single dataset.

## 07. Changing all 561 variable names, for both test and training data sets. 
### -Names are taken from variablenames data frame ( features.txt )
### -These new names will be used to identify the requested columns for the final dataset ( only median values and standard deviations taken over the measurements ).
### -These names will also be enhanced to be fully descriptive.

## 08. Column binding subject,labels and set data frames for test and training
### -This step clips together the subject, activity codes and measurement columns for test and training, respectively.

## 09. Row binding test and training into a new data frame (full)
### -This step clips together the test and training rows (obtained in the previous step) into a single dataset named "full"

## 10. Merging activity labels with full data frame
### -The activity names are merged with the full dataset.
### -Merge causes its resulting dataset to be reordered. By executing the merge after clipping columns and rows together, it does not affect the final dataset negatively.

## 11. Identifying variable names containing std() and mean()
### -This step identifies the positions of the elements containing "mean(" or "std(" within the vector of variable names. This vector comes from the data frame loaded with the features.txt file.
### -The positions will be used to select only the required fields for the final dataset.

## 12. Adding 3 to each member of the index list to account for the 3 initial fields ( activitycode, activityname and subject )
### -This step and the following two adapt the index list to the structure of the full data frame and extract the required fields only.
## 13. Adding indexes 2 and 3 at the beginning of the index list. These correspond to the activityname and subject fields
## 14. Extracting only required columns ( activityname, subject and those with mean() and std() )

## 15. Changing variable names to full descriptive
### -The gsub function is used repeatedly over the names list of the full data frame to convert abbreviations to complete words.
### -These conversions clarify the meaning and make the variable names fully descriptive, as requested. Intermediate dashes and parenthesis are removed.

## 16. Grouping by activityname and subject. Calculating mean for each variable.
### -The dplyr package is loaded to provide the group_by and summarize functions.
### -The full data frame is loaded into a table data frame with tbl_df, to allow for the required functions.
### -Grouping by activity name and subject is applied, then summarizing and calculating the mean for each variable.
### -The same variable names are used for calculated means in the result data set, but preceded each by the "MeanOf" prefix.

## 17. Writing result data frame to output file (txt format)
### -The result data frame is exported as a txt file. Row names are ommited.
### -Dimensions are 180 rows by 68 columns.






